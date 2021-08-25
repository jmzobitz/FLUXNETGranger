#' @title Compute a Transfer Entropy Test

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}, based on code from Chandler Lutz, UCR 2009 (https://www.mathworks.com/matlabcentral/fileexchange/25467-granger-causality-test).

#' @description
#' Given an x (predictor) and y (response) variable, determine if x causes way at the given lag. Uses conditional mutual information to assess The Null Hypothesis (H0) is that Time series x does not cause time series y, and the Alternative Hypothesis (HA): Time series x causes time series y
#' @param x Predictor variable (can be a vector)
#' @param y Response variable (can be a vector)
#' @param alpha the significance level
#' @param max_lag the maximum number of lags to be considered

#'
#'

#' @return A T/F value - this means that the conditional information is less than the significance level alpha for all the considered lags.
#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2021-08-10)
#     original creation
#   Helpful hint from Mohamed Ombadi (personal correspondence)



transfer_entropy_test <- function (x, y, alpha = .05, max_lag = 1)
{

  # Make the data
  in_data <- tibble(x,y) %>%
    infotheo::discretize()

  # Create the output vector
  ci_values <- array(dim=max_lag)


  # Create a list of the lagged variables x and y
  lag_list <- lapply(1:max_lag, function(i) {
    out_val<-tibble(x=in_data$x,y=dplyr::lag(in_data$y,n=i,default=NA))
    })

  # Now loop through and do the conditional information

  for (i in seq_along(ci_values)) {
    ci_values[i] <- infotheo::condinformation(lag_list[[i]]$x,lag_list[[i]]$y, method="emp")
  }

  # Then determine if we have met significance:
  all_lags <- sum(ci_values < alpha)

  if(all_lags == max_lag) {
    return(TRUE)
  } else {
    return(FALSE)
  }


}
