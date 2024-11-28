source("ui.R")

server <- function(input, output, session) {
  # Reactive data input
  data_input <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      return(Maiz) # Use built-in data
    } else {
      data <- read.csv(inFile$datapath)
      return(data)
    }
  })

  # Show the dataset
  output$data_head <- renderDT({
    DT::datatable(data_input(), options = list(pageLength = 20))
  })

  observeEvent(input$run_bayes_ammi, {
    data <- data_input()

    # Run Bayesian AMMI analysis
    bayes_result <- bayes_ammi(
      .data = data,
      .y = Yield,
      .gen = Gen,
      .env = Env,
      .rep = Rep,
      .nIter = 20
    )

    # Loop through the output data frames and display them
    lapply(1:8, function(i) {
      output[[paste0("bayes_ammi_result", i)]] <- DT::renderDT({
        datatable(bayes_result[[i]], options = list(pageLength = 5, autoWidth = TRUE))
      })

      # Add download handler for each data frame
      output[[paste0("download_bayes_ammi", i)]] <- downloadHandler(
        filename = function() {
          paste0("BayesAMMI_DataFrame_", i, ".csv")
        },
        content = function(file) {
          write.csv(bayes_result[[i]], file, row.names = FALSE)
        }
      )
    })
  })

  # Plot Bayesian AMMI
  observeEvent(input$run_plot, {
    data <- data_input()

    # Generate Bayesian AMMI results
    bayes_result <- bayes_ammi(
      .data = data,
      .y = Yield,
      .gen = Gen,
      .env = Env,
      .rep = Rep,
      .nIter = 20
    )

    # Generate and display individual plots
    lapply(1:6, function(i) {
      output[[paste0("plot_result", i)]] <- renderPlot({
        plot(bayes_result, plot_selection = i)
      })

      # Add download handler for each plot
      output[[paste0("download_plot", i)]] <- downloadHandler(
        filename = function() {
          paste0("Bayesian_AMMI_Plot_", i, ".png")
        },
        content = function(file) {
          png(file, width = 800, height = 600)
          plot(bayes_result, plot_selection = i)
          dev.off()
        }
      )
    })
  })


  # GE Means
  observeEvent(input$run_ge_means, {
    data <- data_input()
    ge_means_result <- ge_mean(
      .data = data,
      .y = Yield,
      .gen = Gen,
      .env = Env
    )
    output$ge_means_result <- renderDT({
      DT::datatable(ge_means_result$ge_means)
    })
  })

  # GE Effects
  observeEvent(input$run_ge_eff, {
    data <- data_input()
    ge_eff_result <- ge_eff(
      .data = data,
      .y = Yield,
      .gen = Gen,
      .env = Env
    )
    output$ge_eff_result <- renderPrint(ge_eff_result)
  })
}

shinyApp(ui = ui, server = server)
