#' Tolerance interval for the population proportion when the population proportion is KNOWN
#'
#' @param x A vector of values in a distribution
#' @param p A value for the population parameter
#' @param c A value for the tolerance level in whole number form...no decimals
#' @return A tolerance interval for the population mean
#' @examples
#' tipk(dataset, 0.5, 95)
#' tipk(dataset, 0.75, 99)
#' @export
#' @import stats methods

tipk <- function(x, p, c) {
  n <- length(x)
  alpha <- ( (1 - (c / 100)))
  z <- qnorm(alpha / 2, mean = 0, sd = 1, lower.tail = FALSE)
  moe <- z * sqrt( (p * (1 - p)) / n)
  til <- (p - moe)
  tiu <- (p + moe)
  cat("The ", c, "% tolerance interval for the population proportion is: (", til, ":", tiu, ")")
}
