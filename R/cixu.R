#' Confidence interval for the population mean when standard deviation is UNKNOWN
#'
#' @param x A vector of values in a distribution
#' @param c A value for the confidence level in whole number form...no decimals
#' @return A confidence interval for the population mean
#' @examples
#' cixu(attenu$accel, 95)
#' cixu(attenu$accel, 99)
#' @export
#' @import stats methods datasets

cixu <- function(x, c) {
  xbar <- mean(x)
  sd <- sd(x)
  n <- length(x)
  df <- n - 1
  alpha <- ( (1 - (c / 100)))
  t <- qt(alpha / 2, df = df, lower.tail = FALSE)
  moe <- t * (sd / sqrt(n))
  cil <- (xbar - moe)
  ciu <- (xbar + moe)
  cat("The ", c, "% confidence interval for the population mean is: (", cil, ":", ciu, ")")
}
