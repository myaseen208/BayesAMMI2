#' @name    biplots
#' @aliases biplots
#' @title   Biplots
#' @description biplots
#'
#'
#' @param model Output from `bayes_ammi()`. This should contain the results of the Bayesian AMMI model, including all sampled iterations.
#' @param burnin Numeric. Percentage of iterations to discard as burn-in to avoid the effects of random initializations during sampling. For example, `burnin = 0.1` removes the first 10\% of iterations.
#' @param thin Numeric. Proportion of sampled iterations to retain for analysis. For example, `thin = 0.2` keeps 20\% of the iterations, selecting 1 out of every 5 iterations.
#' @param pb Numeric. Significance levels for the contours in the plot. Smaller values of `pb` result in wider contours, while higher values create smaller, more specific contours.
#' @param plot_stable Logical. If `TRUE`, stable instances are highlighted in the output plot.
#' @param plot_unstable Logical. If `TRUE`, unstable instances are highlighted in the output plot.
#' @param ncolors Integer. Specifies the number of distinct colors to use in the plot. Adjust this to control the visual differentiation of elements in the plot.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{plot}{A plot displaying the contours and final biplot values.}
#'   \item{contour_data}{A `data.frame` containing the data used to create the contours.}
#'   \item{biplot_data}{A `data.frame` containing the data used to recreate the final biplot values.}
#' }
#'
#' @author
#' \enumerate{
#'     \item Julian Garcia Abadillo Velasco (\email{garciaabadillo.j@ufl.edu})
#'     \item Diego Jarquin (\email{diego.jarquin@gmail.com})
#'    }
#'
#' @references
#'  Crossa, J., Perez-Elizalde, S., Jarquin, D., Cotes, J.M., Viele, K., Liu, G., and Cornelius, P.L. (2011)
#'  Bayesian Estimation of the Additive Main Effects and Multiplicative Interaction Model
#'  \emph{Crop Science}, \strong{51}, 1458–1469.
#'  (\href{https://acsess.onlinelibrary.wiley.com/doi/abs/10.2135/cropsci2010.06.0343}{doi: 10.2135/cropsci2010.06.0343})
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
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(Maiz)
#'
#' fm1 <-
#'   bayes_ammi(
#'     .data = Maiz,
#'     .y = Yield,
#'     .gen = Gen,
#'     .env = Env,
#'     .rep = Rep,
#'     .nIter = 200
#'   )
#'
#' library(ggplot2)
#'
#' output_05 <- biplots(model = fm1, plot_stable = TRUE, plot_unstable = TRUE, pb = 0.05)
#' output_05
#'
#' output_95 <- biplots(model = fm1, plot_stable = TRUE, plot_unstable = TRUE, pb = 0.95)
#' output_95
#' }
#'
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "alphas0",
      "c0",
      "delta0",
      "gammas0",
      "lambdas0",
      "ge_means0",
      "mu0",
      "n0",
      "tao0",
      "tau0",
      "prob",
      "byplot",
      "ID",
      "x",
      "y",
      "stable",
      "name"
    )
  )
}

biplots <- function(model, burnin = 0.3, thin = 0.2, pb = 0.05, plot_stable = TRUE, plot_unstable = TRUE, ncolors = 5) {
  UseMethod("biplots")
}

#' @export
#' @rdname biplots

biplots.default <-
  function(model, # bayes_ammi object
           burnin = 0.3, # percentage of iterations to burn
           thin = 0.2, # percentage of interations to sample
           pb = 0.05, # significance level for contorns
           plot_stable = TRUE, # plot stable instances
           plot_unstable = TRUE, # plot unstable instances
           ncolors = 5 # how many different colors for plotting instances?
  ) {

    # Process `alphas1`
    pre_alpha  <- model$alphas1 %>%  data.table()
    r          <- length(funique(pre_alpha[[2]]))
    niter      <- NROW(pre_alpha) / r
    burnin_idx <- ceiling(niter * burnin)
    thin_idx   <- seq(burnin_idx + 1, niter, by = 1 / thin)


    alpha <-
      pre_alpha %>%
      collapse::pivot(data = ., ids = 1, values = 3:NCOL(.), names = 2,  how = "wider") %>%
      fsubset(thin_idx)

    # Process `gammas1`
    pre_gamma <- model$gammas1 %>% data.table()
    gamma <-
      pre_gamma %>%
      collapse::pivot(data = ., ids = 1, values = 3:NCOL(.), names = 2,  how = "wider") %>%
      fsubset(thin_idx)

    # Combine `alpha` and `gamma`
    svd.mcmc <- cbind(alpha, gamma)
    nsim <- NROW(svd.mcmc)
    s <- (niter - burnin_idx)*thin

    return(
      list(
        alpha
      , gamma
      # , contorns = data.stable
      # , biplot_data = summary.stable
      # , biplot = base_plot
      )
      )
  }

