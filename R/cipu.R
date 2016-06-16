#' Confidence interval for the population proportion when the population proportion is UNKNOWN
#'
#' @param x A vector of values in a distribution
#' @param c A value for the confidence level in whole number form...no decimals
#' @return A confidence interval for the population proportion
#' @examples
#' cipu(attenu$accel, 95)
#' cipu(attenu$accel, 99)
#' @export
#' @import stats methods datasets

cipu <- function(x, c) {
  pbar <- mean(x)
  n <- length(x)
  alpha <- ( (1 - (c / 100)))
  z <- qnorm(alpha / 2, mean = 0, sd = 1, lower.tail = FALSE)
  moe <- z * sqrt( (pbar * (1 - pbar)) / n)
  cil <- (pbar - moe)
  ciu <- (pbar + moe)
  cat("The ", c, "% confidence interval for the population proportion is: (", cil, ":", ciu, ")")
}
