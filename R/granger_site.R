#' @title Compute a Granger Causality test at a given site in the FLUXNET2015 dataset

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}, based on code from Chandler Lutz, UCR 2009 (https://www.mathworks.com/matlabcentral/fileexchange/25467-granger-causality-test).

#' @description
#' Given a site, determine if all possible combinations of the variables are Granger cause each other. The Null Hypothesis (H0) is that Time series x does not Granger-cause time series y, and the Alternative Hypothesis (HA): Time series x Granger-causes time series y
#' @param in_data data frame of the site we are studying.  The first column contains the time variable
#' @param alpha the significance level
#' @param max_lag the maximum number of lags to be considered

#'
#'

#' @return A data frame that reports the F statistic, the critical value, and if F > c_v.   (if F > c_v we reject the null hypothesis that x does not Granger Cause y)
#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2021-08-11)
#     original creation
#   Helpful hint from Mohamed Ombadi (personal correspondence)

granger_site <- function(in_data,alpha,max_lag) {


  # Get all pairs of variables:
  var_names <- colnames(in_data[-1])  # Remove the time variable
  var_pairs = map(1:2, ~ var_names) %>%
    cross() %>%
    keep(~ length(unique(.x)) == 2) %>%
    map(unlist) # Combinations of vars, 2 at a time, as a list
  n_pairs <- length(var_pairs)
  out_granger <- vector(mode = "list",length = n_pairs)

  for (i in seq_along(out_granger)) {

    curr_names <- var_pairs[[i]]
    # Select out the data
    small_data <- select(in_data,curr_names)

    # Perform the Granger Test
    out_results <- granger_test(small_data[[1]],small_data[[2]],alpha=alpha,max_lag = max_lag) %>%
      mutate(x = curr_names[1],
             y = curr_names[2])

    out_granger[[i]] <- out_results

  }

  # Collapse these all together
  granger_results <- bind_rows(out_granger)




    return(granger_results)



}











