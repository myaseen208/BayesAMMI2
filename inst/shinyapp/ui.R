library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)

ui <- dashboardPage(
  dashboardHeader(
    title = tagList(span(class = "logo-lg", "BayesAMMI2 App", style = "color: maroon; font-size: inherit;"), icon("leaf")),
    userOutput("user")
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data", tabName = "tab-data", icon = icon("table")),
      menuItem("Bayesian AMMI", tabName = "tab-bayes_ammi", icon = icon("chart-bar")),
      menuItem("Plots", tabName = "tab-plot", icon = icon("chart-pie")),
      menuItem("GE Means", tabName = "tab-Means", icon = icon("chart-area")),
      menuItem("GE Effects", tabName = "tab-Effects", icon = icon("chart-line"))
    ),
    disable = c(TRUE, FALSE)[2],
    width = NULL,
    collapsed = c(TRUE, FALSE)[2]
  ),
  dashboardBody(
    tags$head(
      tags$style(
        "
        body, div, p, h1, h2, h3, h4, h5, h6 {
          font-family: 'Latin Modern Roman', serif;
          font-style: italic;
          font-weight: bold;
        }
        /* Enable both horizontal and vertical scrolling */
        .custom-table {
          height: 600px;  /* Adjust height */
          width: 100%;    /* Full width */
          overflow-x: auto;  /* Horizontal scroll */
          overflow-y: auto;  /* Vertical scroll */
          display: block;    /* Block display to manage scroll */
        }
        /* Adjust table to fit within its container */
        .dataTables_wrapper {
          width: 100%;  /* Ensure table takes up full width */
          overflow-x: auto;  /* Horizontal scrolling for wide tables */
          overflow-y: auto;  /* Vertical scrolling for long tables */
        }

        pre {
        font-size: 16px; /* Change to desired font size */
        line-height: 1.5; /* Optional: adjust line height */
        }

        "
      )
    ),
    tabItems(
      # Home Page
      tabItem(
        tabName = "home",
        h1(tags$span("BayesAMMI2 Analysis App", style = "color: green; font-size: inherit;"), align = "center"),
        br(),
        h4(
          "The", tags$span("BayesAMMI2 Analysis App", style = "color: green; font-size: inherit;"), "provides a comprehensive
         suite of tools for conducting", tags$span("Genotype by Environment Interaction (GEI)", style = "color: green; font-size: inherit;"), "analysis,
         using the", tags$span("Bayesian Additive Main Effect and Multiplicative Interaction (AMMI)", style = "color: green; font-size: inherit;"), "technique.
         It assists researchers in evaluating how different genotypes perform across various environments. This app includes advanced Bayesian AMMI methods and
         visualization tools that allow users to explore interaction effects, stability, and the adaptability of genotypes in multi-environment trials.
         By identifying which genotype performs best in specific environments, the app serves as a valuable resource for selecting winning genotypes under diverse conditions.",
          tags$span("Which Win Where!", style = "color: green; font-size: inherit;"),
          align = "justify"
        ),
        br(),
        br(),
        h4(
          tags$strong("Author/Maintainer:"),
          tags$a(href = "https://myaseen208.com/", "Muhammad Yaseen", target = "_blank"),
          align = "center"
        ),
        h4(
          tags$strong("Contributor(s):"),
          tags$a(href = "https://myaseen208.com/", "Diego Jarquin,", target = "_blank"),
          tags$a(href = "https://myaseen208.com/", "Julian Garcia Abadillo Velasco", target = "_blank"),
          align = "center"
        ),
        h4(
          tags$strong("Email:"),
          tags$a(href = "mailto:myaseen208@gmail.com", "myaseen208@gmail.com", target = "_blank"),
          align = "center"
        ),
        h4(
          tags$strong("Website:"),
          tags$a(href = "https://myaseen208.com/BayesAMMI2", "https://myaseen208.com/BayesAMMI2", target = "_blank"),
          align = "center"
        ),
        br(),
        br(),
        h4(tags$span("The key functions available in the BayesAMMI2 App include:", style = "color: green; font-size: inherit;")),
        tags$ul(
          style = "text-align: justify;",
          tags$li(
            tags$a(href = "https://myaseen208.com/BayesAMMI2/reference/bayes_ammi.html", target = "_blank", "bayes_ammi()"),
            ": Main function for Bayesian AMMI modeling."
          ),
          tags$li(
            tags$a(href = "https://myaseen208.com/BayesAMMI2/reference/plot.BayesAMMI.html", target = "_blank", "plot.BayesAMMI()"),
            ": Generates biplots from the BayesAMMI results."
          ),
          tags$li(
            tags$a(href = "https://myaseen208.com/BayesAMMI2/reference/biplots.html", target = "_blank", "biplots()"),
            ": Generates biplots from the BayesAMMI results."
          ),
          tags$li(
            tags$a(href = "https://myaseen208.com/BayesAMMI2/reference/e_eff.html", target = "_blank", "e_eff()"),
            ": Calculates environment effects."
          ),
          tags$li(
            tags$a(href = "https://myaseen208.com/BayesAMMI2/reference/g_eff.html", target = "_blank", "g_eff()"),
            ": Calculates genotype effects."
          ),
          tags$li(
            tags$a(href = "https://myaseen208.com/BayesAMMI2/reference/ge_ammi.html", target = "_blank", "ge_ammi()"),
            ": Fits AMMI model and calculates genotype × environment interaction effects."
          ),
          tags$li(
            tags$a(href = "https://myaseen208.com/BayesAMMI2/reference/ge_eff.html", target = "_blank", "ge_eff()"),
            ": Calculates genotype × environment interaction effects."
          ),
          tags$li(
            tags$a(href = "https://myaseen208.com/BayesAMMI2/reference/ge_means.html", target = "_blank", "ge_means()"),
            ": Computes genotype × environment means."
          ),
          tags$li(
            tags$a(href = "https://myaseen208.com/BayesAMMI2/reference/ge_model.html", target = "_blank", "ge_model()"),
            ": Fits the AMMI linear mixed-effects model."
          ),
          tags$li(
            tags$a(href = "https://myaseen208.com/stability/reference/ge_var.html", target = "_blank", "ge_var()"),
            ": Calculates genotype × environment variance."
          )
        ),
        br(),
        br(),
        h4(
          tags$span(
            "By using this app, researchers & plant breeders can assess the
            stability & adaptability of genotypes, select stable & high-yielding
            genotypes, and make informed decisions in breeding programs aimed at
            improving crop performance across diverse environmental conditions.",
            style = "color: green; font-size: inherit;"
          ),
          align = "justify"
        ),
        br(),
        br(),
        h5(
          tags$strong("If you find this app to be useful, please let us know at "),
          tags$a(href = "mailto:myaseen208@gmail.com", tags$strong("myaseen208@gmail.com.")),
          tags$strong("We can use this information to obtain more resources to make the app better. Thank you for your interest in our app!"),
          align = "justify"
        )
      ),

      # Data Page with Full Width Box
      tabItem(
        tabName = "tab-data",
        box(
          title = tagList(
            h3("Upload or Use Built-in Data"),
            actionButton(
              inputId = "run_data",
              label = "Load Data",
              icon = icon("upload"),
              style = "color: white; background-color: #007bff; border-color: red; float: right;"
            )
          ),
          status = "success", # Blue header bar
          solidHeader = TRUE,
          collapsible = TRUE,
          icon = icon("folder-open"),
          width = 12,
          sidebarLayout(
            sidebarPanel(
              fileInput("file", "Upload CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
              ),
              helpText("Upload a CSV file with columns: Env, Gen, Rep and Yield. If no file is uploaded, built-in ge_data will be used.")
            ),
            mainPanel(
              h3("Data Table"),
              DT::DTOutput("data_head")
            )
          )
        )
      ),
      tabItem(
        tabName = "tab-bayes_ammi",
        box(
          title = tagList(
            h3("Bayesian AMMI Analysis"),
            actionButton(
              inputId = "run_bayes_ammi",
              label = "Bayesian AMMI Analysis",
              icon = icon("play"),
              style = "color: white; background-color: #007bff; border-color: red; float: right;",
              title = "Click to run the analysis. If you have uploaded your own data, it will be used; otherwise, the built-in data will be used for the analysis."
            )
          ),
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12
        ),
        br(),
        # Display results with download options
        lapply(
          1:8,
          function(i) {
            box(
              title = paste("Result for Data Frame", i),
              status = "success",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              DT::DTOutput(paste0("bayes_ammi_result", i)),
              downloadButton(
                outputId = paste0("download_bayes_ammi", i),
                label = paste("Download Data Frame", i),
                style = "color: white; background-color: #28a745; border-color: green;"
              )
            )
          }
        )
      ),
      tabItem(
        tabName = "tab-plot",
        box(
          title = tagList(
            h3("Bayesian AMMI Plots"),
            actionButton(
              inputId = "run_plot",
              label = "Generate Bayesian AMMI Plots",
              icon = icon("play"),
              style = "color: white; background-color: #007bff; border-color: red; float: right;"
            )
          ),
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12
        ),
        br(),
        # Display individual plots with download buttons
        lapply(
          1:6,
          function(i) {
            box(
              title = paste("Plot Selection", i),
              status = "success",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              plotOutput(paste0("plot_result", i)),
              downloadButton(
                outputId = paste0("download_plot", i),
                label = paste("Download Plot", i),
                style = "color: white; background-color: #28a745; border-color: green;"
              )
            )
          }
        )
      ),
      tabItem(
        tabName = "tab-Means",
        h3("Calculate GE Means"),
        actionButton("run_ge_means", "Run"),
        DTOutput("ge_means_result")
      ),
      tabItem(
        tabName = "tab-Effects",
        h3("Calculate GE Effects"),
        actionButton("run_ge_eff", "Run"),
        verbatimTextOutput("ge_eff_result")
      )
    )
  )
)
