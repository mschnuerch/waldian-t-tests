library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(purrr)
library(dplyr)
library(ggplot2)
library(lemon)
library(scales)
library(crch)
library(kableExtra)
library(MBESS)
library(checkmate)
library(shinyalert)

ui <- fluidPage(
  
  useShinyalert(),
  useShinydashboard(),
  useShinyjs(),
  withMathJax(),
  
  titlePanel("Easy Application of Waldian t tests"),
  
  hr(),
  
  sidebarLayout(
    
    sidebarPanel(
      
      conditionalPanel(
        "input.tab == 'About'",
        
        div(img(src = "smip.jpg", width = "70%"),
            style="text-align: center;")),
      
      conditionalPanel(
        "input.tab == 'Prior Settings'",
        
        id = "prior-panel",
        
        p("Specify a prior on \\(\\delta\\)."),
        
        pickerInput(inputId = "h1", label = "Alternative hypothesis:",
                    choices = list("Negative effects" = "d_min",
                                   "Two-sided" = "d_0",
                                   "Positive effects" = "d_plus"),
                    selected = "d_0"),
        
        br(),
        
        pickerInput("prior", label = "Prior distribution:",
                    choices = list("Cauchy distribution" = "cauchy", 
                                   "Non-local alternative prior" = "nap",
                                   "Normal distribution" = "normal",
                                   "t distribution" = "t"),
                    selected = "cauchy"),
        
        conditionalPanel(
          "input.prior == 'cauchy'",
          textInput("gamma_c","\\( \\gamma \\) (prior scale)",
                    value = "1/sqrt(2)")
        ),
        
        conditionalPanel(
          "input.prior == 'nap'",
          textInput("tau2","\\( \\tau^2 \\) (prior scale)",
                    value = "0.045")
        ),
        
        conditionalPanel(
          "input.prior == 't'",
          textInput("mu_t","\\( \\mu \\) (prior location)",
                    value = "0"),
          textInput("gamma_t","\\( \\gamma \\) (prior scale)",
                    value = "1/sqrt(2)"),
          sliderInput("kappa","\\( \\kappa \\) (prior degrees of freedom)",
                      min = 1, max = 100, value = 1, step = 1,
                      ticks = FALSE)
        ),
        
        conditionalPanel(
          "input.prior == 'normal'",
          textInput("mu_n","\\( \\mu \\) (prior location)",
                    value = "0"),
          textInput("sigma","\\( \\sigma^2 \\) (prior variance)",
                    value = "1")
        ),
        
        br(),
        
        p("Plot options:"),
        
        numericRangeInput("ylim", "y-Axis range:", c(-1, 1)),
        
        fluidRow(
          column(12, align = "center",
                 actionButton("reset2", "Reset"))
        )
        
      ),
      
      conditionalPanel(
        "input.tab == 'Analysis'",
        
        id = "analysis-panel",
        
        prettyRadioButtons(inputId = "stoprule", label = "Procedure:",
                           choices = list("Error Probabilities" = "errors", 
                                          "Evidence Thresholds" = "thresholds"),
                           selected = "errors",
                           inline = TRUE),
        
        conditionalPanel(
          condition = "input.stoprule == 'errors'",
          textInput("alpha", "\\( \\alpha \\) (Type I error probability)",
                    value = ".05"),
          textInput("beta", "\\( \\beta \\) (Type II error probability)",
                    value = ".05")
        ),
        
        conditionalPanel(
          condition = "input.stoprule == 'thresholds'",
          textInput("A","Upper threshold"),
          textInput("B","Lower threshold")
        ),
        
        pickerInput("design", label = "Study Information:",
                    choices = list("One Sample" = "one", 
                                   "Independent Samples" = "two", 
                                   "Paired Samples" = "paired"),
                    selected = "two"),
        
        prettyRadioButtons(
          "info", "",choices = list("t Value" = "t","Means and SDs" = "means"),
          selected = "t", inline = TRUE),
        
        conditionalPanel(
          "input.design == 'one'",
          textInput("n", "Sample size")
        ),
        
        conditionalPanel(
          "input.design == 'paired'",
          textInput("np", "Sample size")
        ),
        
        conditionalPanel(
          "input.design == 'two'",
          
          fluidRow(
            column(6, textInput("n1", "Sample size Group 1")),
            
            column(6, textInput("n2", "Sample size Group 2"))
          )
        ),
        
        conditionalPanel(
          "input.info == 't'",
          textInput("t", "t Value")
        ),
        
        conditionalPanel(
          "input.info == 'means'",
          fluidRow(
            
            column(
              6,
              conditionalPanel(
                "input.design == 'one'",
                textInput("m", "Mean")
              ),
              conditionalPanel(
                "input.design != 'one'",
                textInput("m1", "Mean Group 1"),
                textInput("sd1", "SD Group 1")
              )
            ),
            
            column(
              6,
              conditionalPanel(
                "input.design == 'one'",
                textInput("sd", "SD")
              ),
              conditionalPanel(
                "input.design != 'one'",
                textInput("m2", "Mean Group 2"),
                textInput("sd2", "SD Group 2")
              )
            )
          ),
          
          conditionalPanel(
            "input.design == 'paired'",
            textInput("rxy", "Correlation between groups")
          )
        ),
        
        prettyRadioButtons(
          "bftype", "Output Options:", choices = c("BF\\(_{10}\\)" = "bf10",
                                    "BF\\(_{01}\\)" = "bf01",
                                    "log(BF\\(_{10}\\))" = "log"),
          selected = "bf10", inline = T),
        
        prettyRadioButtons(
          "estype", "", choices = c("Cohen's d" = "cohend",
                                    "Posterior Median (under \\(H_1\\))" = "posterior"),
          selected = "cohend", inline = T),
        
        numericInput(
          "ci", label = uiOutput("cilab"),
          value = "95"),
        
        br(),
        
        fluidRow(
          column(6, align = "center",
                 actionButton("do", "Analyze")),
          column(6, align = "center",
                 actionButton("reset3", "Reset"))
        )
      )
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        id = "tab",
        
        tabPanel(
          "About",
          h3("Welcome!"),
          p("This is a Shiny app for the convenient application of", strong("Waldian t tests."),
            "Waldian t tests are sequential Bayesian t tests with predefined thresholds",
            "to control error probabilities. You can find more information in",
            a("this preprint.", href = "https://psyarxiv.com/x4ybm")),
          p("To set up the sequential procedure, specify and visualize",
            "your prior on \\(\\delta\\) under", strong("Prior Settings."),
            "The app currently supports Cauchy distributions, t distributions,",
            "non-local alternative priors, and Normal distributions."),
          p("Under", strong("Analysis"), ", you can define the desired error",
            "probabilities of the test procedure and calculate threshold values",
            "(or vice versa). Finally, you can calculate Bayes factors for your",
            "observed data and check whether you can terminate or continue sampling."),
          p("If you have any questions, comments, or suggestions concerning this",
            "app or underlying scripts, please contact",
            a("Martin Schnuerch.", href="mailto:martin.schnuerch@gmail.com"),
            "You can find the underlying source code for this Shiny app on",
            a("Github.", href = "https://github.com/mschnuerch/waldian-t-tests")),
          strong("Enjoy this app!")
        ),
        
        tabPanel("Prior Settings",
                 h3("Prior Visualization"),
                 plotOutput("prior_plot")),
        
        tabPanel("Analysis",
                 
                 br(),
                 
                 fluidRow(
                   box(
                     id = "procedure",
                     uiOutput("procedure"),
                     title = "Procedure", status = "primary", 
                     solidHeader = FALSE, collapsible = TRUE,
                     width = 12
                   )
                 ),
                 
                 fluidRow(
                   box(
                     id = "results",
                     uiOutput("table"),
                     title = "Results", status = "primary", 
                     solidHeader = FALSE, collapsible = TRUE,
                     width = 12
                   )
                 )
            
        ),
        selected = "About"
      )
    )
  ),
  
  hr(),
  
  p("This app may be used for non-commercial purposes free of charge.",
    "Although considerable effort was put into developing and testing this app,",
    "there is no warranty whatsoever. Please address comments, questions, or suggestions",
    "concerning this Shiny app and underlying scripts to",
    a("Martin Schnuerch.", href="mailto:martin.schnuerch@gmail.com"),
    style = "font-size:12px; text-align:justify")
  
)