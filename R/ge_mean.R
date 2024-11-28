#' @name    ge_mean
#' @aliases ge_mean
#' @title   Genotype by Environment Interaction Means
#' @description Calcuates Genotype by Environment Interaction Means
#'
#' @param .data  data.frame
#' @param .y     Response Variable
#' @param .gen   Genotypes Factor
#' @param .env   Environment Factor
#'
#' @return Genotype by Environment Interaction Means
#'
#' @author
#' \enumerate{
#'     \item Muhammad Yaseen (\email{myaseen208@@gmail.com})
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
#'
#' data(Maiz)
#' ge_mean(
#'     .data  = Maiz
#'      , .y    = Yield
#'      , .gen  = Gen
#'      , .env  = Env
#'    )
#'
#'


if(getRversion() >= "2.15.1"){
  utils::globalVariables(
    c(
      "Gen"
      , "Env"
      , "Y"
      , "GEMean"
      , "."
    )
  )
}

ge_mean <- function(.data, .y, .gen, .env) {
  UseMethod("ge_mean")
}


#' @export
#' @rdname ge_mean

ge_mean.default <-
  function(.data, .y, .gen, .env){

    .y    <- deparse(substitute(.y))
    .gen  <- deparse(substitute(.gen))
    .env  <- deparse(substitute(.env))

    df1 <-
      data.table::data.table(
        Env = factor(.data[[.env]])
      , Gen = factor(.data[[.gen]])
      , Y   = .data[[.y]]
    )

    ge_means <-
      df1 %>%
      collapse::fgroup_by(Gen, Env) %>%
      collapse::fsummarise(GEMean = fmean(Y)) %>%
      collapse::pivot(data = ., ids = "Gen", values = "GEMean", names = "Env",  how = "wider") %>%
      collapse::fselect(-Gen) %>%
      as.matrix()

    grand_mean <-
            df1 %>%
            collapse::fsummarise(grand_mean = fmean(Y))

    return(list(
        ge_means   = ge_means
      , grand_mean = grand_mean[[1]]
      ))
  }


