#' @name    plot.BayesAMMI
#' @aliases plot.BayesAMMI
#' @title Plot Results from BayesAMMI Model
#' @description Generates diagnostic and visualization plots for the BayesAMMI model.
#'
#' @param x An object of class \code{BayesAMMI} containing results from a Bayesian AMMI analysis.
#' @param plot_selection A numeric value (1 to 6) or "all" (default) specifying which plots to generate:
#'   \itemize{
#'     \item 1: Trace and histogram plots for \eqn{\mu}.
#'     \item 2: Trace and histogram plots for \eqn{\sigma^2}.
#'     \item 3: AMMI method biplot.
#'     \item 4: Bayesian AMMI method biplot.
#'     \item 5: All trace and histogram plots.
#'     \item 6: All biplots.
#'     \item "all": All available plots.
#'   }
#' @param ... Additional arguments (not used).
#'
#' @details The function generates diagnostic and visualization plots based on the `plot_selection` argument.
#'
#' @return Generates ggplot objects and displays them sequentially.
#'
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#'
#' @examples
#' data(Maiz)
#' fm1 <- bayes_ammi(
#'     .data = Maiz
#'  ,  .y    = Yield
#'  ,  .gen  = Gen
#'  ,  .env  = Env
#'  ,  .rep  = Rep
#'  , .nIter = 20
#' )
#' 
#' plot(x = fm1, plot_selection = 1)
#' plot(x = fm1, plot_selection = 2)
#' plot(x = fm1, plot_selection = 3)
#' plot(x = fm1, plot_selection = 4)
#' plot(x = fm1, plot_selection = 5)
#' plot(x = fm1, plot_selection = 6)
#'
#'
#' @import ggplot2 ggrepel lme4 mvtnorm rlang rstiefel scales shiny shinyBS shinydashboardPlus tmvtnorm
#' @importFrom collapse fgroup_by funique fsubset fsummarise fmean pivot fselect
#' @importFrom data.table data.table
#' @importFrom dplyr filter select group_by count ungroup mutate n summarise bind_rows
#' @importFrom DT DTOutput datatable formatRound renderDT
#' @importFrom magrittr %>%
#' @importFrom MASS Null mvrnorm
#' @importFrom stats rexp rgamma rnorm runif sigma
#' @importFrom utils read.csv write.csv
#'
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "alphas1",
      "alphas2",
      "gammas1",
      "gammas2"
    )
  )
}

#' @export
#'
plot.BayesAMMI <- function(x, plot_selection = "all", ...) {
  # Helper function for line plots
  generate_line_plot <- function(data, y, x_label, y_label, title) {
    ggplot(data, aes(x = seq_along(data[[y]]), y = data[[y]])) +
      geom_line(color = "blue") +
      scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks()) +
      scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks()) +
      labs(x = x_label, y = y_label, title = title) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))
  }

  # Helper function for biplots
  generate_biplot <- function(alphas, gammas, title) {
    alphas_df <- as.data.frame(alphas)
    gammas_df <- as.data.frame(gammas)

    ggplot() +
      geom_point(data = alphas_df, aes(x = alphas1, y = alphas2)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_text(data = alphas_df, aes(x = alphas1, y = alphas2, label = rownames(alphas_df)),
                vjust = "inward", hjust = "inward") +
      geom_point(data = gammas_df, aes(x = gammas1, y = gammas2), color = "red") +
      geom_segment(data = gammas_df,
                   aes(x = 0, y = 0, xend = gammas1, yend = gammas2),
                   arrow = arrow(length = unit(0.2, "cm")), color = "red") +
      geom_text(data = gammas_df, aes(x = gammas1, y = gammas2, label = paste0("E", 1:nrow(gammas_df))),
                vjust = "inward", hjust = "inward") +
      labs(title = title, x = expression(PC[1]), y = expression(PC[2])) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))
  }

  # Generate plots based on `plot_selection`
  if (plot_selection == 1) {
    print(generate_line_plot(x$mu1, "mu", "Iterations", expression(mu), expression("Trace Plot of " ~ mu)))
  } else if (plot_selection == 2) {
    print(generate_line_plot(x$tau1, "tau", "Iterations", expression(sigma^2), expression("Trace Plot of " ~ sigma^2)))
  } else if (plot_selection == 3) {
    print(generate_line_plot(x$tao1, "tao1", "Iterations", expression(alpha[1]), expression("Trace Plot of " ~ alpha[1])))
  } else if (plot_selection == 4) {
    print(generate_line_plot(x$delta1, "delta1", "Iterations", expression(beta[1]), expression("Trace Plot of " ~ beta[1])))
  } else if (plot_selection == 5) {
    print(generate_biplot(x$alphas0, x$gammas0, "AMMI Method"))
  } else if (plot_selection == 6) {
    print(generate_biplot(x$alphas1, x$gammas1, "Bayesian AMMI Method"))
  } else {
    stop("Invalid `plot_selection` value. Choose a number between 1 and 6.")
  }
}
