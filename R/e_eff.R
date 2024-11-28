#' @name e_eff
#' @aliases e_eff
#' @title Environment Effects
#' @description Calculates Environment Effects for a given dataset using Genotype, Environment, and Response variables.
#'
#' @param .data A data frame containing the dataset.
#' @param .y Response variable (numeric).
#' @param .gen Genotype factor variable.
#' @param .env Environment factor variable.
#'
#' @return A matrix of environment effects.
#'
#'
#' @author
#' \enumerate{
#'   \item Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' }
#'
#' @references
#' Crossa, J., Perez-Elizalde, S., Jarquin, D., Cotes, J.M., Viele, K., Liu, G., and Cornelius, P.L. (2011).
#' Bayesian Estimation of the Additive Main Effects and Multiplicative Interaction Model.
#' \emph{Crop Science}, \strong{51}, 1458–1469.
#' (\href{https://acsess.onlinelibrary.wiley.com/doi/abs/10.2135/cropsci2010.06.0343}{doi: 10.2135/cropsci2010.06.0343})
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
#' data(Maiz)
#' e_eff(
#'   .data = Maiz,
#'   .y = Yield,
#'   .gen = Gen,
#'   .env = Env
#' )

if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "EEffs"
    )
  )
}
e_eff <- function(.data, .y, .gen, .env) {
  UseMethod("e_eff")
}


#' @export
#' @rdname e_eff

e_eff.default <-
  function(.data, .y, .gen, .env) {
  .y <- deparse(substitute(.y))
  .gen <- deparse(substitute(.gen))
  .env <- deparse(substitute(.env))

  df1 <- data.table::data.table(
    Env = factor(.data[[.env]]),
    Gen = factor(.data[[.gen]]),
    Y = .data[[.y]]
  )

  e_effects <- df1 %>%
    collapse::fgroup_by(Env) %>%
    collapse::fsummarise(EMean = fmean(Y)) %>%
    collapse::fungroup() %>%
    collapse::fmutate(EEffs = EMean - mean(EMean)) %>%
    collapse::fungroup() %>%
    collapse::fselect(EEffs) %>%
    as.matrix()

  return(list(
      e_effects = e_effects
      ))
}
