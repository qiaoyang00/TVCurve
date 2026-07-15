## See server.R for deployment guide.

require(shiny)
require(shinydashboard)
require(shinyWidgets)
require(shinyjs)
require(shinyauthr)
require(shinymeta)  # also need: shinyAce, clipr
require(shinyAce)
require(clipr)
require(DT)
require(tidyverse)
require(scales)
require(metR)
require(survival)
require(survminer)
require(nleqslv)
require(knitr)
require(pryr)


opts_knit$set(base.dir = "deploy")
opts_chunk$set(dpi = 100, fig.width = 7, fig.height = 5)

ref <- "1.	Beyersmann, J., Gastmeier, P., Wolkewitz, M., & Schumacher, M. An easy mathematical proof showed that time-dependent bias inevitably leads to biased effect estimation. Journal of clinical epidemiology, 61(12), 1216-1221. (2008).
2.	Therneau, T., Crowson, C., & Atkinson, E. Using time dependent covariates and time dependent coefficients in the cox model. Survival Vignettes, 2(3), 1-25. (2017).
3.	Mi, X., Hammill, B.G., Curtis, L.H., Lai, E.C.C., & Setoguchi, S. Use of the landmark method to address immortal person‐time bias in comparative effectiveness research: a simulation study. Statistics in medicine, 35(26), 4824-4836. (2016).
4.	Smith, A.R. et al.  Graphical representation of survival curves in the presence of time-dependent categorical covariates with application to liver transplantation. Journal of Applied Statistics, 46(9), 1702-1713. (2019).
5.	Li, Y. et al. Statistical considerations for analyses of time-to-event endpoints in oncology clinical trials: Illustrations with CAR-T immunotherapy studies. Clinical Cancer Research, 28(18), 3940-3949. (2022).
6.	Hay, K.A. et al. Factors associated with durable efs in adult b-cell all patients achieving mrd-negative cr after cd19 car t-cell therapy. Blood, The Journal of the American Society of Hematology, 133(15), 1652-1663. (2019).
7.	Summers, C. et al. Hematopoietic cell transplantation after CD19 chimeric antigen receptor T cell-induced acute lymphoblastic leukemia remission confers a leukemia-free survival advantage. Transplantation and Cellular Therapy, 28(1), 21-29. (2022).
8.	Park, J.H. et al. Long-term follow-up of CD19 CAR therapy in acute lymphoblastic leukemia. New England Journal of Medicine, 378(5), 449-459. (2018).
9.	Frey, N.V. et al. Optimizing chimeric antigen receptor T-cell therapy for adults with acute lymphoblastic leukemia. Journal of Clinical Oncology, 38(5), 415-422. (2020).
10.	Locke, F.L. et al. Long-term safety and activity of axicabtagene ciloleucel in refractory large B-cell lymphoma (ZUMA-1): a single-arm, multicentre, phase 1–2 trial. The lancet oncology, 20(1), 31-42. (2019).
11.	Plana, D., Fell, G., Alexander, B.M., Palmer, A.C., & Sorger, P.K. Cancer patient survival can be parametrized to improve trial precision and reveal time-dependent therapeutic effects. Nature communications, 13(1), 873. (2022).
12.	Messmer, B., Leachman, R., Nora, J., & Cooley, D. Survival-times after cardiac allografts. The Lancet, 293(7602), 954-956. (1969).
13.	Suissa, S. Immortal time bias in pharmacoepidemiology. American journal of epidemiology, 167(4), 492-499. (2008).
14.	Snapinn, S.M., Jiang, Q.I., & Iglewicz, B. Illustrating the impact of a time-varying covariate with an extended Kaplan-Meier estimator. The American Statistician, 59(4), 301-307. (2005).
15.	Jacoby, E. The role of allogeneic HSCT after CAR T cells for acute lymphoblastic leukemia. Bone Marrow Transplantation, 54(Suppl 2), 810-814. (2019).
16.	Shah, N.N. et al. Long-term follow-up of CD19-CAR T-cell therapy in children and young adults with B-ALL. Journal of Clinical Oncology, 39(15), 1650-1659. (2021).
17.	Taraseviciute A. et al.  What is the role of hematopoietic cell transplantation (HCT) for pediatric acute lymphoblastic leukemia (ALL) in the age of chimeric antigen receptor T-cell (CART) therapy?. Journal of pediatric hematology/oncology, 41(5), 337-344. (2019).
18.	Gardner, R.A. et al. Intent-to-treat leukemia remission by cd19 car t cells of defined formulation and dose in children and young adults. Blood, The Journal of the American Society of Hematology, 129(25), 3322-3331. (2017).
19.	Turtle,C.J., et al. CD19 CAR–T cells of defined CD4+: CD8+ composition in adult B cell ALL patients. The Journal of clinical investigation, 126(6), 2123-2138. (2016).
20.	Boyiadzis, M. et al. Impact of chronic graft-versus-host disease on late relapse and survival on 7,489 patients after myeloablative allogeneic hematopoietic cell transplantation for leukemia. Clinical Cancer Research, 21(9), 2020-2028. (2015).
21.	Leahy, A.B. et al. Unrelated donor α/β T cell–and B cell–depleted HSCT for the treatment of pediatric acute leukemia. Blood Advances, 6(4), 1175-1185. (2022).
22.	Horowitz, M.M. et al. Graft-versus-leukemia reactions after bone marrow transplantation. Blood.75(3):555-562. (1990)
23.	Huang, I, Baek G, Wu, Q.V., et al. High-intensity maintenance after autologous transplant is associated with improved event-free survival in multiple myeloma. To be submitted (2025)
24.	Li Y, Qiao Y, Gao F, Gauthier J, Zhang QE, Voutsinas JM, Leisenring WM, Gooley TA, Summers C, Hirayama AV, Turtle CJ. A Novel R Shiny Tool TVCurveTM for Survival Analysis with Time-Varying Covariate in Oncology Clinical Studies: Overcoming Biases and Enhancing Collaboration. Transplantation and Cellular Therapy, Official Publication of the American Society for Transplantation and Cellular Therapy. 2025 Feb 1;31(2):S23-4.
25.	Li Y et al. A Novel R Shiny Tool TVCurveTM for Visualizing Survival Analysis with Time-Varying Covariate in Clinical Studies. ENAR 2025 Spring Meeting, New Orleans, Louisiana, 2025."

header <- dashboardHeader(title = "TVCurve",
                          titleWidth = 280,
                          tags$li(
                            conditionalPanel(
                              "!output.user_auth",
                              dropdownButton(
                                inputId = "login_btn", label = "Log in", circle = F,
                                status = "info", icon = icon("sign-in"), right = T,
                                loginUI(id = "login", title = NULL))
                            ),
                            class = "dropdown", style = "padding: 8px;"
                          ),
                          tags$li(
                            logoutUI("logout", icon = icon("sign-out"), class = "btn-info"),
                            class = "dropdown", style = "padding: 8px;"
                          ),
                          tags$li(
                            tags$strong("Number of Visit", style = "color:white;"),
                            img(src = "https://hitwebcounter.com/counter/counter.php?page=7968056&style=0019&nbdigits=1&type=page&initCount=0", title = "Counter", Alt = "Counter", width = "31%"),
                            class = "dropdown")
)

sidebar <- dashboardSidebar(
  width = 280,
  sidebarMenu(
    id = "sidebar",
    menuItem("Welcome", tabName = "home", icon = icon("home")),
    menuItem("Real Data Analysis", icon = icon("database"), startExpanded = T,
             menuSubItem("Upload Data", tabName = "upload", icon = icon("upload")),
             menuSubItem(HTML(paste("Model:", span("Naive KM/Cox", style = "font-size: 13px;"))),
                         tabName = "model_naive", icon = icon("magnifying-glass")),
             menuSubItem(HTML(paste("Model:", span("Landmark KM/Cox", style = "font-size: 13px;"))),
                         tabName = "model_landmark", icon = icon("magnifying-glass")),
             menuSubItem(HTML(paste("Model:", span("Smith-Zee/Time-dependent Cox", style = "font-size: 13px;"))),
                         tabName = "model_timedependent", icon = icon("magnifying-glass")),
             menuSubItem(HTML(paste("Model:", span("Extended KM/Time-dependent Cox", style = "font-size: 13px;"))),
                         tabName = "model_extended", icon = icon("magnifying-glass")),
             menuSubItem("Model Summary", tabName = "model_summary", icon = icon("gear")),
             menuSubItem("Model Estimation", tabName = "model_estimation", icon = icon("calculator")),
             menuSubItem("Real Data Example", tabName = "example_realdata", icon = icon("file")),
             menuSubItem("More Complicated Examples", tabName = "example_more", icon = icon("file"))
    ),
    menuItem("Simulation", icon = icon("laptop-code"), startExpanded = T,
             menuSubItem("Simulate Data", tabName = "simulate", icon = icon("folder")),
             menuSubItem("Reference Case", tabName = "refcase", icon = icon("lightbulb"))
    ),
    menuItem("Reference", tabName = "reference", icon = icon("book")),
    menuItem("Introduction Video", tabName = "video", icon = icon("video")),
    textOutput("test"),
    switchInput("toggleSidebar", "sidebar", value = T, onLabel = "show", offLabel = "hide", size = "small")
  )
)

body <- dashboardBody(
  useShinyjs(),
  withMathJax(),
  sidebarLayout(
    {div(id = "sidebarPanel", sidebarPanel(
      width = 3,
      # verbatimTextOutput("test"),

      conditionalPanel(
        "input.sidebar == 'upload'",
        fileInput("datafile", "Upload .csv data file", multiple = F, accept = ".csv"),
        checkboxInput("datafile_header", "Header", value = T),
        if (Sys.info()[[1]] == "Darwin") actionButton("hack", h5("HACK!")),
        hr(),
        actionButton("run", h5("Click to Run")),
        hr(),
        uiOutput("ui_vars")
      ),
      conditionalPanel(
        "input.sidebar == 'model_landmark'",
        sliderInput("other_lmcox_prob", "Choose a probability and calculate the quantile",
                    min = 0, max = 1, value = 1, round = -2),
        numericInput("other_lmcox_landmark", "Enter a landmark (overide quantile input above)", value = NULL, step = .1),
        hr()
      ),
      conditionalPanel(
        "input.sidebar == 'model_timedependent' && input.model_timedependent == 'Smith-Zee Curve Plot'",
        selectizeInput("other_tdcox_prob", "Quartile value for plot",
                       choices = c(0.5, 0.75), selected = c(0.5, 0.75),
                       multiple = T, options = list(create = T, maxItems = 3)),
        htmlOutput("other_tdcox_quantile"),
        selectizeInput("other_tdcox_legend", "Legend of the above values",
                       choices = c("no", "at 50%", "at 75%"), selected = c("no", "at 50%", "at 75%"),
                       multiple = T, options = list(create = T, maxItems = 4)),
        hr()
      ),
      conditionalPanel(
        "input.sidebar == 'model_estimation'",
        numericInput("est_simu_M", "Number of replication (\\(M\\))", value = 10, min = 10, max = 100, step = 10),
        numericInput("est_simu_N", "Sample size (\\(N\\))", value = 100, min = 100, max = 1000, step = 10),
        numericInput("est_simu_maxtime", "Censoring time of event and treatment (maxtime)", value = 1500, min = 0),
        actionButton("run_est_simu", h5("Click to Simulate")),
        hr()
      ),
      conditionalPanel(
        "input.sidebar == 'simulate'",
        numericInput("simu_M", "Number of replication (\\(M\\))", value = 10, min = 10, max = 100, step = 10),
        numericInput("simu_N", "Sample size (\\(N\\))", value = 100, min = 100, max = 1000, step = 10),
        numericInput("simu_maxtime", "Censoring time of event and treatment (maxtime)", value = 1500, min = 0),
        numericInput("simu_alphaT", "Shape of Weibull distribution for treatment time (\\(\\alpha_T\\))", value = 1, min = 0),
        numericInput("simu_lambdaT", "Rate of Weibull distribution for treatment time (\\(\\lambda_T\\))", value = 1 / 50, min = 0),
        numericInput("simu_alphaE", "Shape of Weibull distribution for event time (\\(\\alpha_E\\))", value = 1, min = 0),
        numericInput("simu_lambdaE", "Rate of Weibull distribution for event time (\\(\\lambda_E\\))", value = 1 / 500, min = 0),
        selectizeInput("simu_expbeta", "Hazard ratio (\\(e^\\beta\\))",
                       choices = 1:4 / 2, selected = 1:4 / 2, multiple = T, options = list(create = T, maxItems = 5)),
        actionButton("run_simu", h5("Click to Simulate")),
        hr(),

        selectizeInput("other_simu_landmark_value", "Landmark", choices = 0.5, selected = 0.5,
                       multiple = T, options = list(create = T, maxItems = 2)),
        radioButtons("other_simu_landmark_type", NULL, choices = c("percentage", "fixed value"), inline = T),
        hr()
      ),
      conditionalPanel(
        # "['model', 'model_summary'].includes(input.sidebar).valueOf()",
        "input.sidebar == 'model_naive' && input.model_naive == 'KM Curve Plot' ||
        input.sidebar == 'model_landmark' && input.model_landmark == 'KM Curve Plot' ||
        input.sidebar == 'model_timedependent' && input.model_timedependent == 'Smith-Zee Curve Plot' ||
        input.sidebar == 'model_extended' && input.model_extended == 'KM Curve Plot' ||
        input.sidebar == 'model_summary' || input.sidebar == 'model_estimation' ||
        input.sidebar == 'simulate' || input.sidebar == 'refcase'",
        h4("Plot parameters:"),
        conditionalPanel("input.sidebar != 'model_estimation' && input.sidebar != 'simulate' && input.sidebar != 'refcase'",
                         numericInput("plot_breaktime", "Break time by", value = NULL, min = 1, step = 1),
                         sliderInput("plot_xlim", "X limit", min = 0, max = Inf, value = c(0, Inf), round = F, dragRange = T)),
        sliderInput("plot_fontsize", "Font size", 8, 24, value = 12, step = 1, round = 0, ticks = F)
      ),
      conditionalPanel(
        "input.sidebar == 'model_summary' || input.sidebar == 'example_realdata'",
        hr(),
        h4("Explanation:"),
        helpText("√: Good - recommended.", br(),
                 "×: Poor - not recommended.", br(),
                 "?: Risky - use with caution."),
      )

    ))},
    {mainPanel(
      width = 9,
      tabItems(
        {tabItem(
          "home",
          fluidPage(
            h1("Welcome to Vicky's Stat Corner!"),
            h3("Survival Analysis with Time-varying Covariates (TVCurve)"),
            helpText("Shiny App Developer: Yang Qiao, Vicky Wu.")
          )
        )},  # home
        {tabItem(
          "upload",
          tabsetPanel(
            tabPanel("An Example of Data",
                     div(h4("Description of the data"),
                         "It contains 20 records from 20 people, including the following 5 variables:", br(),
                         HTML('&emsp;'), "id: ID of object, can be number or string.", br(),
                         HTML('&emsp;'), "event: status of event, have to be coded in 0/1. 0 means censored, 1 means event occured.", br(),
                         HTML('&emsp;'), "time: time of event occured or censored.", br(),
                         HTML('&emsp;'), "XT_value: status of time-varying covariate, have to be coded in 0/1. 0 means the status never changed, 1 means the status changed.", br(),
                         HTML('&emsp;'), "XT_time: time to time-varying covariate changed from 0 to 1. If the status of time-varying covariate never changed, this should be NA or blank."),
                     hr(),
                     downloadButton("example_download", "Download"),
                     DTOutput("example")
            ),
            tabPanel("View of Uploaded Data",
                     DTOutput("data_real")
            )
          )
        )},  # upload
        {tabItem(
          "model_naive",
          tabsetPanel(
            id = "model_naive",
            tabPanel("Model Summary",
                     tableOutput("table_cox")
            ),
            tabPanel("KM Curve Plot",
                     downloadButton("plot_cox_download", "Download"),
                     plotOutput("plot_cox", height = "auto")
            ),
            tabPanel("Method Description"
            ),
            tabPanel("Source Code",
                     conditionalPanel("!output.user_auth", helpText("Please login to view the code.")),
                     actionButton("code_cox", label = "Show code", icon("code"))
            )
          )
        )},  # model_naive
        {tabItem(
          "model_landmark",
          tabsetPanel(
            id = "model_landmark",
            tabPanel("Choose Landmark",
                     textOutput("other_lmcox_text1"),
                     tableOutput("other_lmcox_table1"),
                     hr(),
                     textOutput("other_lmcox_text2"),
                     tableOutput("other_lmcox_table2")
            ),
            tabPanel("Model Summary",
                     tableOutput("table_lmcox")
            ),
            tabPanel("KM Curve Plot",
                     downloadButton("plot_lmcox_download", "Download"),
                     plotOutput("plot_lmcox", height = "auto")
            ),
            tabPanel("Method Description"
            ),
            tabPanel("Source Code",
                     conditionalPanel("!output.user_auth", helpText("Please login to view the code.")),
                     actionButton("code_lmcox", label = "Show code", icon("code"))
            )
          )
        )},  # model_landmark
        {tabItem(
          "model_timedependent",
          tabsetPanel(
            id = "model_timedependent",
            tabPanel("Model Summary",
                     tableOutput("table_tdcox")
            ),
            tabPanel("Smith-Zee Curve Plot",
                     downloadButton("plot_tdcox_download", "Download"),
                     plotOutput("plot_tdcox", height = "auto")
            ),
            tabPanel("Method Description"
            ),
            tabPanel("Source Code",
                     conditionalPanel("!output.user_auth", helpText("Please login to view the code.")),
                     actionButton("code_tdcox", label = "Show code", icon("code"))
            )
          )
        )},  # model_timedependent
        {tabItem(
          "model_extended",
          tabsetPanel(
            id = "model_extended",
            tabPanel("Model Summary",
                     tableOutput("table_kmcox")
            ),
            tabPanel("KM Curve Plot",
                     downloadButton("plot_kmcox_download", "Download"),
                     plotOutput("plot_kmcox", height = "auto")
            ),
            tabPanel("Method Description"
            ),
            tabPanel("Source Code",
                     conditionalPanel("!output.user_auth", helpText("Please login to view the code.")),
                     actionButton("code_kmcox", label = "Show code", icon("code"))
            )
          )
        )},  # model_extended
        {tabItem(
          "model_summary",
          tableOutput("table_model_summary"),
          plotOutput("plot_model_summary", height = "auto")
        )},  # model_summary
        {tabItem(
          "model_estimation",
          tabsetPanel(
            id = "model_estimation",
            tabPanel("Parameter Estimation",
                     tableOutput("table_est"),
                     box(title = "Summary of models",
                         status = "primary", width = NULL, collapsible = T,
                         tableOutput("table_est_simu_summary")),
                     box(title = "Summary plot of models",# align = "center",
                         status = "primary", width = NULL, collapsible = T,
                         plotOutput("plot_est_simu_summary_bias", height = "auto", width = "80%")),
                     box(title = "Summary plot of parameters",# align = "center",
                         status = "primary", width = NULL, collapsible = T,
                         plotOutput("plot_est_simu_summary_est", height = "auto", width = "80%"))
            ),
            tabPanel("Method Description"
            ),
            tabPanel("Source Code",
                     conditionalPanel("!output.user_auth", helpText("Please login to view the code.")),
                     actionButton("code_est", label = "Show code", icon("code"))
            )
          )
        )},  # model_estimation
        {tabItem(
          "example_realdata",
          tableOutput("table_realdata_example"),
          plotOutput("plot_realdata_example", height = "auto")
        )},  # example_realdata
        {tabItem(
          "example_more",
          HTML(markdown::mark_html(knitr::knit("deploy/More Examples TDCox.Rmd", output = "deploy/temp.md", quiet = T), template = F))
        )},  # example_more
        {tabItem(
          "simulate",
          tabsetPanel(
            tabPanel("Summary",
                     box(title = "Frequency of event and treatment",
                         status = "primary", width = NULL, collapsible = T,
                         tableOutput("table_simu_summary_stat")),
                     box(title = "Histogram of event and treatment time",
                         status = "primary", width = NULL, collapsible = T,
                         plotOutput("plot_simu_summary_histogram", height = "auto")),
                     box(title = "Summary of models",
                         status = "primary", width = NULL, collapsible = T,
                         tableOutput("table_simu_summary_avgest"),
                         tableOutput("table_simu_summary_rejrate"),
                         tableOutput("table_simu_summary_covrate")),
                     box(title = "Summary plot of models",# align = "center",
                         status = "primary", width = NULL, collapsible = T,
                         plotOutput("plot_simu_summary_bias", height = "auto", width = "80%")),
                     box(title = "Summary plot of parameters",# align = "center",
                         status = "primary", width = NULL, collapsible = T,
                         plotOutput("plot_simu_summary_est", height = "auto", width = "80%"))
            ),
            tabPanel("Show Case",
                     uiOutput("ui_simu_showcase"),
                     box(title = "Data overview",
                         status = "primary", width = NULL, collapsible = T,
                         DTOutput("simu_showcase_data")),
                     box(title = "Summary of models",
                         status = "primary", width = NULL, collapsible = T,
                         tableOutput("table_simu_showcase_summary")),
                     box(title = "Summary of parameters",
                         status = "primary", width = NULL, collapsible = T,
                         tableOutput("table_simu_showcase_est")),
                     box(title = "KM plots of models",
                         status = "primary", width = NULL, collapsible = T,
                         plotOutput("plot_simu_showcase", height = "auto"))
            ),
            tabPanel("Source Code",
                     conditionalPanel("!output.user_auth", helpText("Please login to view the code.")),
                     actionButton("code_simu_summary", label = "Show code", icon("code"))
            )
          )
        )},  # simulate
        {tabItem(
          "refcase",
          tabsetPanel(
            selected = "Grid-search Comparison", # Grid-search Comparison, Alpha Comparison, Lambda Comparison, N Comparison
            tabPanel("Grid-search Comparison",
                     downloadButton("simu_reference_grid_download", "Download Grid-search Data"),
                     hr(),
                     tableOutput("simu_reference_grid_setting"),
                     hr(),
                     box(title = "Bias Contour (\\(\\alpha\\))",
                         status = "primary", width = NULL, collapsible = T, collapsed = F,
                         uiOutput("ui_simu_reference_grid_bias_alpha"),
                         plotOutput("plot_simu_reference_grid_bias_alpha", height = "auto")),
                     box(title = "Bias Contour (\\(\\lambda\\))",
                         status = "primary", width = NULL, collapsible = T, collapsed = F,
                         uiOutput("ui_simu_reference_grid_bias_lambda"),
                         plotOutput("plot_simu_reference_grid_bias_lambda", height = "auto")),
                     box(title = "Coverage Contour (\\(\\alpha\\))",
                         status = "primary", width = NULL, collapsible = T, collapsed = F,
                         uiOutput("ui_simu_reference_grid_cov_alpha"),
                         plotOutput("plot_simu_reference_grid_cov_alpha", height = "auto")),
                     box(title = "Coverage Contour (\\(\\lambda\\))",
                         status = "primary", width = NULL, collapsible = T, collapsed = F,
                         uiOutput("ui_simu_reference_grid_cov_lambda"),
                         plotOutput("plot_simu_reference_grid_cov_lambda", height = "auto")),
                     box(title = "MSE Contour (\\(\\alpha\\))",
                         status = "primary", width = NULL, collapsible = T, collapsed = F,
                         uiOutput("ui_simu_reference_grid_mse_alpha"),
                         plotOutput("plot_simu_reference_grid_mse_alpha", height = "auto")),
                     box(title = "MSE Contour (\\(\\lambda\\))",
                         status = "primary", width = NULL, collapsible = T, collapsed = F,
                         uiOutput("ui_simu_reference_grid_mse_lambda"),
                         plotOutput("plot_simu_reference_grid_mse_lambda", height = "auto"))
            ),
            tabPanel("Alpha Comparison",
                     downloadButton("simu_reference_alpha_download", "Download Reference Data"),
                     hr(),
                     tableOutput("simu_reference_param_alpha"),
                     hr(),
                     box(title = "Bias (estimate - true)",
                         status = "primary", width = NULL, collapsible = T,
                         plotOutput("plot_simu_reference_bias_alpha", height = "auto")),
                     box(title = "P-value",
                         status = "primary", width = NULL, collapsible = T,
                         plotOutput("plot_simu_reference_pval_alpha", height = "auto")),
                     box(title = "Rejection rate at significance level 0.05",
                         status = "primary", width = NULL, collapsible = T,
                         plotOutput("plot_simu_reference_rej_alpha", height = "auto")),
                     box(title = "Coverage rate at significance level 0.05",
                         status = "primary", width = NULL, collapsible = T,
                         plotOutput("plot_simu_reference_cov_alpha", height = "auto"))
            ),
            tabPanel("Lambda Comparison",
                     downloadButton("simu_reference_lambda_download", "Download Reference Data"),
                     hr(),
                     tableOutput("simu_reference_param_lambda"),
                     hr(),
                     box(title = "Bias (estimate - true)",
                         status = "primary", width = NULL, collapsible = T,
                         plotOutput("plot_simu_reference_bias_lambda", height = "auto")),
                     box(title = "P-value",
                         status = "primary", width = NULL, collapsible = T,
                         plotOutput("plot_simu_reference_pval_lambda", height = "auto")),
                     box(title = "Rejection rate at significance level 0.05",
                         status = "primary", width = NULL, collapsible = T,
                         plotOutput("plot_simu_reference_rej_lambda", height = "auto")),
                     box(title = "Coverage rate at significance level 0.05",
                         status = "primary", width = NULL, collapsible = T,
                         plotOutput("plot_simu_reference_cov_lambda", height = "auto"))
            ),
            tabPanel("N Comparison",
                     downloadButton("simu_reference_n_download", "Download Reference Data"),
                     hr(),
                     tableOutput("simu_reference_param_N"),
                     hr(),
                     box(title = "Bias (estimate - true)",
                         status = "primary", width = NULL, collapsible = T,
                         plotOutput("plot_simu_reference_bias_N", height = "auto")),
                     box(title = "P-value",
                         status = "primary", width = NULL, collapsible = T,
                         plotOutput("plot_simu_reference_pval_N", height = "auto")),
                     box(title = "Rejection rate at significance level 0.05",
                         status = "primary", width = NULL, collapsible = T,
                         plotOutput("plot_simu_reference_rej_N", height = "auto")),
                     box(title = "Coverage rate at significance level 0.05",
                         status = "primary", width = NULL, collapsible = T,
                         plotOutput("plot_simu_reference_cov_N", height = "auto"))
            )
            # tabPanel("Source Code",
            #          conditionalPanel("!output.user_auth", helpText("Please login to view the code.")),
            #          actionButton("code_simu_reference", label = "Show code", icon("code"))
            # )
          )
        )},  # refcase
        {tabItem(
          "reference",
          uiOutput("debug"),
          HTML(gsub("\n", "<br>", ref))
        )},  # reference
        {tabItem(
          "video",
          tags$video(src = "TVCurve_1080p.mp4", type = "video/mp4", controls = T, width = "100%")
        )}  # video
      )
    )}
  )
)

ui <- dashboardPage(header, sidebar, body, title = "Vicky's Stat Corner: TVCurve")
