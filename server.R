# SERVER ------
server <- function(input, output) {
  
  # Generate count data for 3 groups: neg. vs. pos. vs. exp. ----
  count_data <- reactive({ 
    if (input$data_type == "Count Data") {
      
      seed <- floor(input$set_seed)
      set.seed(seed)
      
      # # Generate 10 numbers greater than 0 -----
      # negative_control <- replicate(10, {
      #   num <- rnegbin(1, mu = 200, theta = 25)
      #   while (num <= 0) {
      #     num <- rnegbin(1, mu = 200, theta = 25)
      #   }
      #   num
      # })
      # 
      # positive_control <- replicate(10, {
      #   num <- rnegbin(1, mu = 175, theta = 10)
      #   while (num <= 0) {
      #     num <- rnegbin(1, mu = 175, theta = 10)
      #   }
      #   num
      # })
      # 
      # # treated <- c(treated[treated < 6])
      # 
      # control <- replicate(10, {
      #   num <- rnegbin(1, mu = 120, theta = 7)
      #   while (num <= 0) {
      #     num <-  rnegbin(1, mu = 120, theta = 7)
      #   }
      #   num
      # })
      # 
      # dat <-
      #   data.frame(Group = rep(c("negative_control", "positive_control", "control"), each = 10),
      #              Count = c(negative_control, positive_control, control))
      # dat$Group <- factor(dat$Group, levels = c("control", "negative_control", "positive_control"))
      # -----
      # set.seed(993)
      sample.size = c(8,8,8)
      dat <- data.frame(Group =  c(rep("Negative control",sample.size[1]),
                                   rep("Positive control",sample.size[2]),
                                   rep("Experimental group",sample.size[3])),
                        Count = c(rnegbin(sample.size[1], mu = 1000,theta=3), #The neg control should have
                                  rnegbin(sample.size[2], mu = 100,theta=3), #The pos control should have
                                  rnegbin(sample.size[3], mu = 925,theta=3)))  # control
      
      dat$Group <- factor(dat$Group, levels = c("Negative control", "Positive control", "Experimental group"))
      return(dat)
    } else {
      NULL
    }
  }) # -----
  
  # Generate random continuous data
  continuous_data <- reactive({
    if (input$data_type == "Continuous Data") {
      rnorm(100, mean = 5, sd = 2)
    } else {
      NULL
    }
  })
  
  # Render plot based on selected data type ----
  output$plot <- renderPlot({
    req(count_data())
    if (input$data_type == "Count Data") {
      ggplot(count_data(), aes(y = Count, x = Group, color = Group)) +
        scale_color_nejm() + 
        geom_boxplot() +
        geom_point(alpha = 0.7) + 
        ggtitle("Distribution of Cell Counts across groups") +
        theme_bw()
    } else {
      hist(continuous_data(), main = "Continuous Data", xlab = "Value")
    }
  })
  
  # GLM Negative Binomial ----
  output$glm_nb_result <- renderDataTable({
    if (input$show_glmnb_tests) {
      if (input$data_type == "Count Data") {
        options(scipen = 999)
        dat <- count_data()
        dat$Group <- factor(dat$Group, levels = c("Experimental group", "Positive control", "Negative control"))
        mms <- emmeans::emmeans(glm.nb(Count~Group, data = dat), pairwise ~ Group)
        mms_df <- data.frame(mms$contrasts, row.names = 'contrast') %>% round(10)
        mms_df <- mms_df %>% dplyr::select(-SE, -df)
        mms_df <- mms_df %>% mutate(P.Value = if_else(p.value < 0.00001, "p < 0.00001", sprintf("%.4f", p.value)))
        mms_df <- mms_df %>% dplyr::select(estimate, P.Value)
        # tidy_result <- round(tidy_result[2:3], 3)
        dt_tidy_result <-DT::datatable(mms_df, caption = "GLM Negative Binomial", rownames = T,
                                       options = list(
                                         lengthMenu = F,
                                         paging = F,
                                         info = F,
                                         searching = F
                                       ))
        
        return(dt_tidy_result)
      }
    }
  }) # ----
  
  # Kruskall Wallis Test -----
  output$kruskall_wallis_results <- renderDataTable({
    req(input$show_kw_tests)
    dat <- count_data()
    dat$Group <- factor(dat$Group, levels = c("Experimental group", "Positive control", "Negative control"))
    pair_wilc <- pairwise.wilcox.test(dat$Count, dat$Group, data = dat, var.equal = F, p.adjust.method = "holm")
    
    tt <- tidy(pair_wilc)
    tt <- tt  %>%  
      mutate(Contrast = paste(group2,"-", group1), 
             p.value = round(p.value, 6)) %>% 
      relocate(Contrast, p.value) %>% dplyr::select(-group1, -group2)
    
    dt_tidy_result <- DT::datatable(tt, caption = "Kruskal Wallis Test", options = list(lengthMenu = F, paging = F, info = F, searching = F))
    return(dt_tidy_result)
    
  })
  # ----
# Poisson ---- 
  # output$glm_pois_result <- renderDataTable({
  #   if (input$show_pois_tests) {
  #     if (input$data_type == "Count Data") {
  #       pois <- (glm(dat$Count ~ dat$Group, family = poisson))
  #       pois_tab <- round(data.frame(summary(pois)$coeff)[c(1,2,4)],3)
  #       colnames(pois_tab) <- c("Coeff. Est.", "Std.Error", "P.Value")
  #       rownames(pois_tab) <- c("Intercept", "Est. Trt Group - Ref. Cntrl Group")
  #       dt_tab <- DT::datatable(pois_tab, caption = "GLM Poisson Results", options = list(lengthMenu = F, paging = F, info = F, searching = F))
  #       return(dt_tab)
  #     }
  #   }
  # })
  #  -----
  
  # Welch's ANOVA results ----  
  output$welch_ANOVA_result <- renderDataTable({
    if (input$show_welch_ANOVA_tests) {
      if (input$data_type == "Count Data") {
        dat <- count_data()
        dat$Group <- factor(dat$Group, levels = c("Experimental group", "Positive control", "Negative control"))
        t <- pairwise.t.test(dat$Count, dat$Group, data = dat, var.equal = F)
        #tidied version
        tt <- tidy(t)
        tt <- tt  %>%  
          mutate(Contrast = paste(group2,"-", group1), p.value = round(p.value, 6)) %>% 
          relocate(Contrast, p.value) %>% dplyr::select(-group1, -group2)
        
        dt_tidy_result <- DT::datatable(tt, 
                                        caption = "Welch's ANOVA (not assuming equal variances)", 
                                        options = list(lengthMenu = F, paging = F, info = F, searching = F))
        return(dt_tidy_result)
      }
    }
    
  })
  
  ## Standard ANOVA ----
  output$std_ANOVA_result <- renderDataTable({
    req(input$show_std_ANOVA_tests)
    if(input$data_type == "Count Data"){ 
      dat <- count_data()
      dat$Group <- factor(dat$Group, levels = c("Experimental group", "Positive control", "Negative control"))
      aov_res <- aov(Count ~ Group, data = dat)
      mms <- emmeans::emmeans(aov_res, specs = pairwise ~ Group)
      mms_df <- data.frame(mms$contrasts, row.names = 'contrast') %>% round(6)
      mms_df <- mms_df %>% dplyr::select(-SE, -df)
      # tidy_result <- round(tidy_result[2:3], 3)
      dt_tidy_result <-DT::datatable(mms_df, caption = "Standard ANOVA", rownames = T,
                                     options = list(
                                       lengthMenu = F,
                                       paging = F,
                                       info = F,
                                       searching = F
                                     ))
      
      return(dt_tidy_result)
    }
    
    
  }) 
  
  
  # QQ Plot -----
  # lm <- reactive({
  #   req(count_data())
  #   aov(Count~ Group, data = count_data())
  # })
  # 
  # output$qqplot <- renderPlot({
  #   req(lm())
  #   req(input$show_std_ANOVA_tests)
  #   plot(lm(), which = 2)
  #   
  # }) # ----
  
  output$Note <- renderText({"Note how the p-values change as the statistical analyses change."})
  
}


