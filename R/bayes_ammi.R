#' @name    bayes_ammi
#' @aliases bayes_ammi.default
#' @title   Bayesian Estimation of the Additive Main Effects and Multiplicative Interaction Model
#' @description Performs Bayesian Estimation of the Additive Main Effects and Multiplicative Interaction Model
#'
#' @param .data  data.frame
#' @param .y     Response Variable
#' @param .gen   Genotypes Factor
#' @param .env   Environment Factor
#' @param .rep   Replication Factor
#' @param .nIter Number of Iterations
#'
#' @return Genotype by Environment Interaction Model
#'
#' @author
#' \enumerate{
#'     \item Muhammad Yaseen (\email{myaseen208@gmail.com})
#'     \item Jose Crossa (\email{j.crossa@cgiar.org})
#'     \item Sergio Perez-Elizalde (\email{sergiop@colpos.mx})
#'     \item Diego Jarquin (\email{diego.jarquin@gmail.com})
#'     \item Jose Miguel Cotes
#'     \item Kert Viele
#'     \item Genzhou Liu
#'     \item Paul L. Cornelius
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
#'
#' fm1 <-
#'   bayes_ammi(
#'     .data = Maiz,
#'     .y = Yield,
#'     .gen = Gen,
#'     .env = Env,
#'     .rep = Rep,
#'     .nIter = 20
#'   )
#' names(fm1)
#' fm1
#' 
#' plot(x = fm1, plot_selection = 1)
#' plot(x = fm1, plot_selection = 2)
#' plot(x = fm1, plot_selection = 3)
#' plot(x = fm1, plot_selection = 4)
#' plot(x = fm1, plot_selection = 5)
#' plot(x = fm1, plot_selection = 6)
#' 
#' biplots(fm1)
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


bayes_ammi <- function(.data, .y, .gen, .env, .rep, .nIter) {
  UseMethod("bayes_ammi")
}


#' @export
#' @rdname bayes_ammi

bayes_ammi.default <- function(.data, .y, .gen, .env, .rep, .nIter) {
  .y <- deparse(substitute(.y))
  .gen <- deparse(substitute(.gen))
  .env <- deparse(substitute(.env))
  .rep <- deparse(substitute(.rep))

  df1 <- data.table::data.table(
    Env = factor(.data[[.env]]),
    Gen = factor(.data[[.gen]]),
    Rep = factor(.data[[.rep]]),
    Y = .data[[.y]]
  )

  fm1 <- ge_ammi(.data = df1, .y = Y, .gen = Gen, .env = Env, .rep = Rep)

  Envs <- df1$Env
  Rep <- fm1$Rep
  alphas <- fm1$alphas
  gammas <- fm1$gammas
  ge_means <- fm1$ge_means$ge_means
  sigma2 <- fm1$sigma2
  tau <- fm1$tau
  tao <- fm1$tao
  lambdas <- fm1$lambdas
  delta <- fm1$delta
  mu <- fm1$ge_means$grand_mean
  g <- fm1$g
  e <- fm1$e
  k <- min(c(g, e)) - 1

  ge_variances <- ge_var(.data = df1, .y = Y, .gen = Gen, .env = Env)$ge_variances
  ssy <- sum(ge_variances, na.rm = TRUE)

  # Initial alphas0 and gammas0
  alphas0 <- alphas
  gammas0 <- gammas

  sigma2_u <- 1e+15
  grand_mean <- mu
  tao <- matrix(rowMeans(ge_means) - rowMeans(ge_means), ncol = 1)
  delta <- matrix(colMeans(ge_means) - colMeans(ge_means), ncol = 1)

  # Pre-allocate memory for outputs
  alphas1 <- matrix(0, ncol = k, nrow = g * .nIter)
  gammas1 <- matrix(0, ncol = k, nrow = e * .nIter)
  mu1 <- numeric(.nIter)
  tau1 <- numeric(.nIter)
  tao1 <- matrix(0, nrow = .nIter, ncol = g)
  delta1 <- matrix(0, nrow = .nIter, ncol = e)
  lambdas1 <- matrix(0, nrow = .nIter, ncol = k)

  for (time in 1:.nIter) {
    mu <- rnorm(
      n = 1,
      mean = (Rep * g * e * sigma2_u * grand_mean) / (Rep * g * e * sigma2_u + sigma2),
      sd = sqrt((sigma2 * sigma2_u) / (Rep * g * e * sigma2_u + sigma2))
    )

    tao <- MASS::mvrnorm(
      n = 1,
      mu = (Rep * e * sigma2 * tao + grand_mean) / (Rep * e + sigma2),
      Sigma = diag(g)
    )

    delta <- MASS::mvrnorm(
      n = 1,
      mu = (Rep * g * sigma2 * delta + grand_mean) / (Rep * g + sigma2),
      Sigma = diag(e)
    )

    lambdas <- rnorm(k, mean = mean(lambdas), sd = sd(lambdas))

    # Update pre-allocated matrices
    alphas1[((time - 1) * g + 1):(time * g), ] <- alphas
    gammas1[((time - 1) * e + 1):(time * e), ] <- gammas
    mu1[time] <- mu
    tau1[time] <- tau
    tao1[time, ] <- tao
    delta1[time, ] <- delta
    lambdas1[time, ] <- lambdas
  }

  # Convert outputs to data.table
  mu1 <- data.table(mu = mu1)
  tau1 <- data.table(tau = tau1)
  tao1 <- data.table(tao1) %>% data.table::setnames(paste0("tao", 1:ncol(tao1)))
  delta1 <- data.table(delta1) %>% data.table::setnames(paste0("delta", 1:ncol(delta1)))
  lambdas1 <- data.table(lambdas1)

  alphas1 <- data.table(
    .nIter = rep(1:.nIter, each = g),
    Gen = rep(1:g, times = .nIter),
    data.table::as.data.table(alphas1)
  ) %>% data.table::setnames(c(".nIter", "Gen", paste0("alphas", 1:k)))

  gammas1 <- data.table(
    .nIter = rep(1:.nIter, each = e),
    Env = rep(1:e, times = .nIter),
    data.table::as.data.table(gammas1)
  ) %>% data.table::setnames(c(".nIter", "Env", paste0("gammas", 1:k)))

  # Convert alphas0 and gammas0 to data.table
  alphas0 <-
    data.table::data.table(alphas0) %>%
    data.table::setnames(paste0("alphas", 1:k))

  gammas0 <-
    data.table::data.table(gammas0) %>%
    data.table::setnames(paste0("gammas", 1:k))

  Out1 <- list(
          mu1 = mu1,
          tau1 = tau1,
          tao1 = tao1,
          delta1 = delta1,
          lambdas1 = lambdas1,
          alphas1 = alphas1,
          gammas1 = gammas1,
          alphas0 = alphas0,
          gammas0 = gammas0
        )

  class(Out1) <- "BayesAMMI"

  return(Out1)
}
