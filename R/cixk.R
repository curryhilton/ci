#' Confidence interval for the population mean when standard deviation is KNOWN
#'
#' @param x A vector of values in a distribution
#' @param sigma A value for the population standard deviation
#' @param c A value for the confidence level in whole number form...no decimals
#' @return A confidence interval for the population mean
#' @examples
#' cixk(Orange$age, 920, 95)
#' cixk(Orange$age, 920, 99)
#' @export
#' @import stats methods datasets

cixk <- function(x, sigma, c) {
xbar <- mean(x)
n <- length(x)
alpha <- ( (1 - (c / 100)))
z <- qnorm(alpha / 2, mean = 0, sd = 1, lower.tail = FALSE)
moe <- z * (sigma / sqrt(n))
cil <- (xbar - moe)
ciu <- (xbar + moe)
cat("The ", c, "% confidence interval for the population mean is: (", cil, ":", ciu, ")")
}
