#' @title BayesAMMI2APP
#' @name BayesAMMI2APP
#'
#' @description Launch the BayesAMMI2 Shiny App
#'
#' @return output of BayesAMMI2 R package
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
#' @examples
#' if(interactive()) {
#'     library(BayesAMMI2)
#'     BayesAMMI2APP()
#'   }
#'
#' @export
#'

BayesAMMI2APP <- function() {
  appDir <- system.file("shinyapp", package = "BayesAMMI2")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `BayesAMMI2`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
