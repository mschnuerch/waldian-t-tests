
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
source("t_test_functions.R")
source("helper.R")

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
        "input.tab == 'Procedure'",
        
        id = "proc-panel",
        
        p("Define the parameters of the procedure."),
        
        prettyRadioButtons(inputId = "stoprule", label = "",
                           choices = list("Error Probabilities" = "errors", 
                                          "Thresholds" = "thresholds"),
                           selected = "errors",
                           inline = TRUE),
        
        conditionalPanel(
          condition = "input.stoprule == 'errors'",
          textInput("alpha", "\\( \\alpha \\) (Type I error probability)"),
          textInput("beta", "\\( \\beta \\) (Type II error probability)")
        ),
        
        conditionalPanel(
          condition = "input.stoprule == 'thresholds'",
          textInput("A","Upper threshold"),
          textInput("B","Lower threshold")
        )
      ),
      
      conditionalPanel(
        "input.tab == 'Prior Settings'",
        
        p("Specify a prior on \\(\\delta\\)."),
        
        pickerInput(inputId = "h1", label = "Alternative hypothesis:",
                    choices = list("Negative effects" = "d_min",
                                   "Two-sided" = "d_0",
                                   "Positive effects" = "d_plus"),
                    selected = "d_0"),
        
        br(),
        
        pickerInput("prior", label = "Prior distribution:",
                    choices = list("Cauchy distribution" = "cauchy", 
                                   "t distribution" = "t", 
                                   "Normal distribution" = "normal"),
                    selected = "cauchy"),
        
        conditionalPanel(
          "input.prior == 'cauchy'",
          textInput("gamma_c","\\( \\gamma \\) (prior scale)",
                    value = "1/sqrt(2)")
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
        
        numericRangeInput("ylim", "y-Axis range:", c(-1, 1))
        
      ),
      
      conditionalPanel(
        "input.tab == 'Analysis'",
        
        id = "analysis-panel",
        
        pickerInput("design", label = "Study design:",
                    choices = list("One Sample" = "one", 
                                   "Independent Samples" = "two", 
                                   "Paired Samples" = "paired"),
                    selected = "two"),
        
        br(),
        
        strong("Study information:"),
        
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
        
        br(),
        
        strong("Output options:"),
        
        prettyRadioButtons(
          "bftype", "", choices = c("BF\\(_{10}\\)" = "bf10",
                                    "BF\\(_{01}\\)" = "bf01",
                                    "log(BF\\(_{10}\\))" = "log"),
          selected = "bf10", inline = T),
        
        prettyRadioButtons(
          "estype", "", choices = c("Cohen's d" = "cohend",
                                    "Posterior Median" = "posterior"),
          selected = "cohend", inline = T),
        
        numericInput(
          "ci", label = uiOutput("cilab"),
          value = "95"),
        
        br(),
        
        fluidRow(
          column(6, align = "center",
                 actionButton("do", "Analyze")),
          column(6, align = "center",
                 actionButton("reset", "Reset"))
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
            "to control error probabilities. You can find more information in this paper",
            "by Schnuerch, Heck, and Erdfelder (2021)."),
          p("To set up the sequential procedure, specify the desired",
            "error probabilities to calculate threshold values (and vice versa)",
            "under", strong("Procedure."), "Define and visualize your prior on \\(\\delta\\)",
            "under", strong("Prior Settings."), "The app currently supports",
            "Cauchy distributions, t distributions, and Normal distributions."),
          p("Finally, you can calculate Bayes factors for your observed data under", 
            strong("Analysis"), "and check whether you can terminate or continue sampling.",
            "Bayes factor computations are based on an", a("R script", href = "https://osf.io/bsp6z/"),
            "by Gronau, Ly, and Wagenmakers (", a("2020", href = "https://doi.org/10.1080/00031305.2018.1562983"),
            ")."),
          p("If you have any questions, comments, or suggestions concerning this",
            "app or underlying scripts, please contact",
            a("Martin Schnuerch.", href="mailto:martin.schnuerch@gmail.com")),
          strong("Enjoy this app!")
        ),
        
        tabPanel("Procedure",
                 br(),
                 
                 p("Specify the desired error probabilities (\\(\\alpha, \\beta\\))",
                   "for your procedure to calculate the upper and lower threshold."),
                 p("Alternatively, if you follow a procedure with predefines boundaries,",
                   "you can set the threshold values to calculate associated",
                   "error probabilities."),
                 p(strong("Note:"), "Specify these parameters", em("before"), 
                   "you start data collection and do not change them afterwards."),
                 br(),
                 p("See", strong("Analysis"), "for resulting parameters.")
        ),
        
        tabPanel("Prior Settings",
                 h3("Prior Visualization"),
                 plotOutput("prior_plot")),
        
        tabPanel("Analysis",
                 
                 br(),
                 
                 fluidRow(
                   box(
                     fluidRow(
                       column(4, align = "center",
                              
                              p("\\(H_0\\): \\(\\delta = 0\\)"),
                              uiOutput("h1")
                              
                       ),
                       column(8, align = "center",
                              
                              p("Prior under \\(H_1\\)"),
                              uiOutput("prior")
                       )
                     ), title = "Hypotheses", status = "primary", 
                     solidHeader = FALSE, collapsible = TRUE,
                     width = 12
                   )
                 ),
                 
                 fluidRow(
                   box(
                     fluidRow(
                       column(6, align = "center",
                              strong("Error probabilities:"),
                              
                              fluidRow(
                                column(6, align = "center",
                                       p("Type I"),
                                       uiOutput("alpha")),
                                column(6, align = "center",
                                       p("Power"),
                                       uiOutput("power"))
                              )
                       ),
                       column(6, align = "center",
                              strong("Thresholds:"),
                              
                              fluidRow(
                                column(6, align = "center",
                                       p("lower (\\(H_0\\))"),
                                       uiOutput("B")),
                                column(6, align = "center",
                                       p("upper (\\(H_1\\))"),
                                       uiOutput("A"))
                              )
                       )
                     ),
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
    style = "font-size:15px; text-align:justify")
  
)


server <- function(input, output, session) {
  
  # Reactive Functions ------------------------------------------------------
  
  values <- reactiveValues(
    df = initial_df,
    messages = initial_message
  )
  
  alpha <- reactive({
    
    if(input$stoprule == "errors"){
      validate(
        need(is.numeric(try(eval(parse(text = input$alpha)))), 
             "Waiting...")
      )
      return(eval(parse(text = input$alpha)))
    }else{
      validate(
        need(is.numeric(try(eval(parse(text = input$A)))), 
             "Waiting...")
      )
      validate(
        need(is.numeric(try(eval(parse(text = input$B)))), 
             "Waiting...")
      )
      A <- eval(parse(text = input$A))
      B <- eval(parse(text = input$B))
      return((1 - B)/(A - B))
    }
  })
  
  power <- reactive({
    if(input$stoprule == "errors"){
      validate(
        need(is.numeric(try(eval(parse(text = input$beta)))), 
             "Waiting...")
      )
      return(1 - eval(parse(text = input$beta)))
    }else{
      validate(
        need(is.numeric(try(eval(parse(text = input$A)))), 
             "Waiting...")
      )
      validate(
        need(is.numeric(try(eval(parse(text = input$B)))), 
             "Waiting...")
      )
      
      A <- eval(parse(text = input$A))
      B <- eval(parse(text = input$B))
      return(1 - (A*B - B)/(A - B))
    }
  })
  
  A <- reactive({
    if(input$stoprule == "errors"){
      validate(
        need(is.numeric(try(eval(parse(text = input$alpha)))), 
             "Waiting...")
      )
      validate(
        need(is.numeric(try(eval(parse(text = input$beta)))), 
             "Waiting...")
      )
      a <- eval(parse(text = input$alpha))
      b <- eval(parse(text = input$beta))
      return((1 - b)/a)
    }else{
      validate(
        need(is.numeric(try(eval(parse(text = input$A)))), 
             "Waiting...")
      )
      return(eval(parse(text = input$A)))
    }
  })
  
  B <- reactive({
    if(input$stoprule == "errors"){
      validate(
        need(is.numeric(try(eval(parse(text = input$alpha)))), 
             "Waiting...")
      )
      validate(
        need(is.numeric(try(eval(parse(text = input$beta)))), 
             "Waiting...")
      )
      a <- eval(parse(text = input$alpha))
      b <- eval(parse(text = input$beta))
      return(b/(1 - a))
    }else{
      validate(
        need(is.numeric(try(eval(parse(text = input$B)))), 
             "Waiting...")
      )
      return(eval(parse(text = input$B)))
    }
  })
  
  check <- eventReactive(input$do, {
    
    check_input(alpha      = try(eval(parse(text = input$alpha))),
                beta       = try(eval(parse(text = input$beta))),
                A          = try(eval(parse(text = input$A))),
                B          = try(eval(parse(text = input$B))),
                mu         = try(eval(parse(text = ifelse(input$prior == "t",
                                                          input$mu_t, 
                                                          input$mu_n))), 
                                 silent = T),
                gamma      = try(eval(parse(text = ifelse(input$prior == "t",
                                                          input$gamma_t, 
                                                          input$gamma_c))), 
                                 silent = T),
                sigma      = try(eval(parse(text = input$sigma)), 
                                 silent = T),
                t          = try(eval(parse(text = input$t)), silent = T),
                nx         = try(eval(parse(text = ifelse(input$design == "one", 
                                                          input$n,
                                                          ifelse(input$design == "two", 
                                                                 input$n1, input$np)))), 
                                 silent = T),
                ny         = try(eval(parse(text = input$n2)), silent = T),
                mx         = try(eval(parse(text = ifelse(input$design == "one", 
                                                          input$m, 
                                                          input$m1))), 
                                 silent = T),
                my         = try(eval(parse(text = input$m2)), silent = T),
                sx         = try(eval(parse(text = ifelse(input$design == "one", 
                                                          input$sd, 
                                                          input$sd1))), 
                                 silent = T),
                sy         = try(eval(parse(text = input$sd2)), silent = T),
                rxy        = try(eval(parse(text = input$rxy)), silent = T),
                ci         = input$ci,
                prior      = input$prior,
                info       = input$info,
                design     = input$design)
  })
  
  res <- eventReactive(input$do, {
    
    waldian_t_test(A          = A(),
                   B          = B(),
                   design     = input$design,
                   h1         = input$h1,
                   prior      = input$prior,
                   mu         = eval(parse(text = ifelse(input$prior == "t",
                                                         input$mu_t, 
                                                         input$mu_n))),
                   gamma      = eval(parse(text = ifelse(input$prior == "t",
                                                         input$gamma_t, 
                                                         input$gamma_c))),
                   sigma      = eval(parse(text = input$sigma)),
                   kappa      = input$kappa,
                   info       = input$info,
                   t          = eval(parse(text = input$t)),
                   nx         = eval(parse(text = ifelse(input$design == "one", 
                                                         input$n,
                                                         ifelse(input$design == "two", 
                                                                input$n1, input$np)))),
                   ny         = eval(parse(text = input$n2)),
                   mx         = eval(parse(text = ifelse(input$design == "one", 
                                                         input$m, input$m1))),
                   my         = eval(parse(text = input$m2)),
                   sx         = eval(parse(text = ifelse(input$design == "one", 
                                                         input$sd, input$sd1))),
                   sy         = eval(parse(text = input$sd2)),
                   rxy        = eval(parse(text = input$rxy)),
                   ci         = input$ci)
  })
  
  # Render Output -----------------------------------------------------------
  
  output$alpha <- renderUI({
    withMathJax(round(alpha(), digits = 3))
  })
  
  output$power <- renderUI({
    withMathJax(round(power(), digits = 3))
  })
  
  output$A <- renderUI({
    A <- A()
    if(input$bftype == "bf01")
      A <- 1/A
    if(input$bftype == "log")
      A <- log(A)
    withMathJax(round(A, digits = 5))
  })
  
  output$B <- renderUI({
    B <- B()
    if(input$bftype == "bf01")
      B <- 1/B
    if(input$bftype == "log")
      B <- log(B)
    withMathJax(round(B, digits = 5))
  })
  
  output$cilab <- renderUI({
    tmp <- ifelse(input$estype == "cohend",
                  "Confidence",
                  "Credible")
    paste(tmp, "Interval (%):")
  })
  
  output$design <- renderText({
    tmp <- ifelse(input$design == "one", 
                  "one sample", 
                  ifelse(input$design == "two", 
                         "two independent samples", 
                         "two paired samples"))
    paste("Waldian t test for", tmp)
  })
  
  output$h1 <- renderUI({
    tmp <- ifelse(input$h1 == "d_0", 
                  "\\delta \\neq 0", 
                  ifelse(input$h1 == "d_min", 
                         "\\delta < 0", 
                         "\\delta > 0"))
    withMathJax(paste("\\(H_1\\!:", tmp, "\\)"))
  })
  
  output$prior <- renderUI({
    if(input$prior == "t"){
      mu <- try(eval(parse(text = input$mu_t)))
      gamma <- try(eval(parse(text = input$gamma_t)))
      tmp <- paste0("\\(t_{", input$kappa, "}\\)(", round(mu, digits = 2),
                    ",", round(gamma, digits = 3), ")")
    }
    if(input$prior == "normal"){
      mu <- try(eval(parse(text = input$mu_n)))
      sigma <- try(eval(parse(text = input$sigma)))
      tmp <- paste0("Normal(", round(mu, digits = 2),
                    ",", round(sigma, digits = 2), ")")
    }
    if(input$prior == "cauchy"){
      gamma <- try(eval(parse(text = input$gamma_c)))
      tmp <- paste0("Cauchy(", round(gamma, digits = 3), ")")
    }
    withMathJax(paste("\\(\\delta \\sim\\)", tmp))
  })
  
  output$prior_plot <- renderPlot({
    
    mu_n <- eval(parse(text = input$mu_n))
    mu_t <- eval(parse(text = input$mu_t))
    sd <- sqrt(eval(parse(text = input$sigma)))
    r <- eval(parse(text = input$gamma_c))
    gamma <- eval(parse(text = input$gamma_t))
    df <- eval(parse(text = input$kappa))
    
    if(input$prior == "normal"){
      if(input$h1 == "d_0")
        max_val <- dnorm(mu_n, mu_n, sd)
      else if(input$h1 == "d_plus")
        max_val <- dtnorm(mu_n, mu_n, sd, 0)
      else
        max_val <- dtnorm(mu_n, mu_n, sd, -Inf, 0)
    }
    
    if(input$prior == "t"){
      if(input$h1 == "d_0")
        max_val <- dtt(mu_t, mu_t, gamma, df)
      else if(input$h1 == "d_plus")
        max_val <- dtt(mu_t, mu_t, gamma, df, 0)
      else
        max_val <- dtt(mu_t, mu_t, gamma, df, -Inf, 0)
    }
    
    if(input$prior == "cauchy"){
      if(input$h1 == "d_0")
        max_val <- dtt(0, 0, r, 1)
      else if(input$h1 == "d_plus")
        max_val <- dtt(0, 0, r, 1, 0)
      else
        max_val <- dtt(0, 0, r, 1, -Inf, 0)
    }
    
    lim <- input$ylim
    
    lnwdth <- 1
    
    p <- ggplot(data.frame(x = 0), aes(x)) +
      geom_segment(aes(x = 0, xend = 0, y = 0, yend = max_val + max_val/10,
                       color = "h0"),
                   arrow = arrow(length = unit(.6, "cm")), lwd = lnwdth,
                   key_glyph = "point") +
      scale_color_manual(element_blank(),
                         limits = c("h0", "h1"),
                         values = c("firebrick", "dodgerblue2"),
                         labels = c(expression(H[0]), expression(H[1])),
                         guide = guide_legend(override.aes = list(linetype = "blank",
                                                                  shape = 19, size = 4))) +
      labs(x = expression(paste("Effect Size ", delta)),
           y = "Prior Density") +
      scale_x_continuous(breaks = breaks_extended(n = 8)) +
      theme_classic() +
      coord_capped_cart(left = "both", bottom = "both") +
      theme(panel.border = element_blank(), 
            axis.line = element_line(), 
            axis.ticks = element_line(color='black'),
            axis.ticks.length = unit(.25, "cm"),
            axis.text = element_text(color = "black"),
            axis.title.y = element_text(vjust = 2),
            legend.position = "bottom",
            text = element_text(size = 20))
    if(input$prior == "normal"){
      if(input$h1 == "d_0"){
        p +
          stat_function(mapping = aes(color = "h1"),
                        fun = dnorm, args = list(mean = mu_n, sd = sd),
                        xlim = lim, n = 1000, lwd = lnwdth)
      }else if(input$h1 == "d_plus"){
        p +
          stat_function(mapping = aes(color = "h1"),
                        fun = dtnorm, args = list(mean = mu_n, sd = sd,
                                                  left = 0),
                        xlim = lim, n = 1000, lwd = lnwdth)
      }else{
        p +
          stat_function(mapping = aes(color = "h1"),
                        fun = dtnorm, args = list(mean = mu_n, sd = sd,
                                                  right = 0),
                        xlim = lim, n = 1000, lwd = lnwdth)
      }
    }else if(input$prior == "t"){
      if(input$h1 == "d_0"){
        p +
          stat_function(mapping = aes(color = "h1"),
                        fun = dtt, args = list(location = mu_t, scale = gamma,
                                               df = df),
                        xlim = lim, n = 1000, lwd = lnwdth)
      }else if(input$h1 == "d_plus"){
        p +
          stat_function(mapping = aes(color = "h1"),
                        fun = dtt, args = list(location = mu_t, scale = gamma,
                                               df = df, left = 0),
                        xlim = lim, n = 1000, lwd = lnwdth)
      }else{
        p +
          stat_function(mapping = aes(color = "h1"),
                        fun = dtt, args = list(location = mu_t, scale = gamma,
                                               df = df, right = 0),
                        xlim = lim, n = 1000, lwd = lnwdth)
      }
    }else{
      if(input$h1 == "d_0"){
        p +
          stat_function(mapping = aes(color = "h1"),
                        fun = dtt, args = list(scale = r,
                                               df = 1),
                        xlim = lim, n = 1000, lwd = lnwdth)
      }else if(input$h1 == "d_plus"){
        p +
          stat_function(mapping = aes(color = "h1"),
                        fun = dtt, args = list(scale = r,
                                               df = 1, left = 0),
                        xlim = lim, n = 1000, lwd = lnwdth)
      }else{
        p +
          stat_function(mapping = aes(color = "h1"),
                        fun = dtt, args = list(scale = r,
                                               df = 1, right = 0),
                        xlim = lim, n = 1000, lwd = lnwdth)
      }
    }
  })
  
  output$table <- renderPrint({
    
    df <- values$df
    
    if(input$bftype == "bf01"){
      df$bf <- 1 / df$bf
      bf_label = "\\(\\mbox{BF}_{01}\\)"
    }else if(input$bftype == "log"){
      df$bf = log(df$bf)
      bf_label = "log(\\(\\mbox{BF}\\))"
    }else{
      bf_label = "\\(\\mbox{BF}_{10}\\)"
    }
    
    header1 = c("\\(n_1\\)", "\\(n_2\\)", "\\(t\\) Value", bf_label, "Decision",
                "Effect size\\(^a\\)", "Lower", "Upper")
    header2 = c(" " = 6, "tmp" = 2)
    
    if(input$estype == "cohend"){
      df <- dplyr::select(df, -c(post_d, post_ll, post_ul))
      names(header2)[2] <- paste0(input$ci, "% Confidence Interval")
    }
    if(input$estype == "posterior"){
      df <- dplyr::select(df, -c(cohen_d, cohen_ll, cohen_ul))
      names(header2)[2] <- paste0(input$ci, "% Credible Interval")
    }
    
    tab <- df %>%
      knitr::kable(format = "html", digits = 3, col.names = header1,
                   align = "c", escape = F) %>%
      kable_styling("striped", full_width = F) %>%
      add_header_above(header = header2) %>%
      add_footnote(ifelse(input$estype == "cohend",
                          "Cohen's d",
                          "Posterior Median"),
                   notation="alphabet",
                   escape = FALSE)
    
    # print(df)
    
    withMathJax(HTML(tab))
  })
  

# Observe -----------------------------------------------------------------

  observe({
    if(input$stoprule == "errors"){
      input$alpha
      input$beta
      updateTextInput(session, "A", value = A())
      updateTextInput(session, "B", value = B())
    }else{
      input$A
      input$B
      updateTextInput(session, "alpha", value = alpha())
      updateTextInput(session, "beta", value = 1 - power())
    }
  })
  
  observe({
    if(input$do == 0){
      return()
    }else{
      args_check <- check()
      if(args_check$check){
        alert_id <- shinyalert("Just a sec...", 
                               "Calculating Bayes factor and posterior statistics.",
                               "info",
                               closeOnEsc = FALSE, showConfirmButton = FALSE)
        tmp <- safely(quietly(res))()
        closeAlert(id = alert_id)
        if(!is.null(tmp$error)){
          error_message <- tmp$error$message
          values$df <- initial_df
          shinyalert("Error!", text = error_message, 
                     type = "error")
        }else{
          values$df <- tmp$result$result
          if(length(tmp$result$warnings) != 0){
            shinyalert("Warning", 
                       text = paste(unique(tmp$result$warnings), collapse = "\n"), 
                       type = "warning")
          }
        }
      }else{
        values$df <- initial_df
        shinyalert("Warning", 
                   text = paste(args_check$message, collapse = "\n"), 
                   type = "warning")
      }
    }
  })
  
  observe({
    if(input$reset == 0){
      return()
    }else{
      values$df <- initial_df
      shinyjs::reset("analysis-panel")
      shinyjs::reset("proc-panel")
    }
  })  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
