# UI

# Load necessary libraries
library(tidyverse)
options(bitmapType = "cairo")
library(shiny)
library(ggplot2)
library(MASS)
library(DT)
library(broom)
library(emmeans)
library(ggsci)


# For rnegbin()


ui <- fluidPage( # UI ------
                 
                 # Application title
                 titlePanel("T-Tests Are Not Meant for Count Data"),
                 
                 # Sidebar layout with input and output definitions
                 sidebarLayout(
                   sidebarPanel(
                     # Input for selecting data type
                     selectInput("data_type", "Select Data Type:",
                                 choices = c("Count Data", "Continuous Data")),
                     br(),
                     numericInput("set_seed", label = "Set seed", value = 993, min = 1, max = 1000000),
                     br(),
                     # Checkbox for showing t-test and negative binomial test
                     checkboxInput("show_welch_ANOVA_tests", "Show Welch's ANOVA Results"),
                     checkboxInput("show_std_ANOVA_tests", "Show Std. ANOVA Results"),
                     checkboxInput("show_glmnb_tests", "Show GLM Negative Binomial Results"),
                     checkboxInput("show_kw_tests", "Show Kruskall Wallis Results"),
                     HTML("<br><br>"),
                     textOutput("Note")
                   ),
                   mainPanel(
                     # Output: plot
                     plotOutput("plot"),
                     br(),
                     # Output: Test Results
                     # textOutput("test_result"),
                     splitLayout(wellPanel(dataTableOutput("welch_ANOVA_result", width = "70%"),
                                           HTML("<br><br>"),
                                           dataTableOutput("std_ANOVA_result", width = "70%"),
                                           #  plotOutput("qqplot"),
                                           HTML("<br><br>")),
                                 wellPanel(
                                   dataTableOutput("glm_nb_result", width = "70%"),
                                   HTML("<br><br>"),
                                   dataTableOutput("kruskall_wallis_results", width = "70%"),
                                   cellWidths = c("100%", "100%"))),
                     HTML("<br><br><br>")
                   )
                 )
)

