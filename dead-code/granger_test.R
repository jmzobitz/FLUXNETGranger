#' @title Compute a Granger Causality test

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}, based on grangertest code from lmtest package

#' @description
#' Given an x (predictor) and y (response) variable, determine if x causes way at the given lag. Uses the F test statistic to compare/evaluate. The Null Hypothesis (H0) is that Time series x does not Granger-cause time series y, and the Alternative Hypothesis (HA): Time series x Granger-causes time series y
#' @param x Predictor variable (can be a vector)
#' @param y Response variable (can be a vector)
#' @param lag number of points in the timeseries that we lag the variables
#'
#'

#' @return A data frame that reports the F statistic and the p value associated with it

# changelog and author contributions / copyrights
#   John Zobitz (2021-07-31)
#     original creation



granger_test <- function (x, y, lag = 1, na.action = na.omit, ...)
{
  if ((NCOL(x) == 2) && missing(y)) {
    xnam <- colnames(x)[1]
    ynam <- colnames(x)[2]
    x <- as.zoo(x)
    y <- x[, 2]
    x <- x[, 1]
  }
  else {
    xnam <- deparse(substitute(x))
    ynam <- deparse(substitute(y))
    x <- zoo::as.zoo(x)
    y <- zoo::as.zoo(y)
    stopifnot((NCOL(x) == 1), (NCOL(y) == 1))
  }
  lagX <- do.call("merge", lapply(1:lag, function(k) stats::lag(x,
                                                           -k)))
  lagY <- do.call("merge", lapply(1:lag, function(k) stats::lag(y,
                                                           -k)))
  all <- merge(x, y, lagX, lagY)
  colnames(all) <- c("x", "y", paste("x", 1:lag, sep = "_"),
                     paste("y", 1:lag, sep = "_"))
  all <- na.action(all)
  y <- as.vector(all[, 2])
  lagX <- as.matrix(all[, (1:lag + 2)])
  lagY <- as.matrix(all[, (1:lag + 2 + lag)])

  fm <- lm(y ~ lagY + lagX)

  rval <- lmtest::waldtest(fm, 2, ...) %>%
    dplyr::rename(statistic=3,p.value=4) %>%
    dplyr::slice(2) %>%
    dplyr::select(statistic,p.value)

  return(rval)
}
