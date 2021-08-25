#' @title Compute a convergent cross map test at a given site in the FLUXNET2015 dataset

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}, based on code from Chandler Lutz, UCR 2009 (https://www.mathworks.com/matlabcentral/fileexchange/25467-granger-causality-test).

#' @description
#' Given a site, determine if all possible combinations of the variables are Granger cause each other. The Null Hypothesis (H0) is that Time series x does not Granger-cause time series y, and the Alternative Hypothesis (HA): Time series x Granger-causes time series y
#' @param in_data data frame of the site we are studying.  This is a nested data frame that contains
#' @param alpha the significance level
#' @param max_lag the maximum number of lags to be considered

#'
#'

#' @return A data frame that reports the  value of the cross correlation mapping for a given measurement
#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2021-08-11)
#     original creation
#   Helpful hint from Mohamed Ombadi (personal correspondence)

ccm_site <- function(in_data) {


  # Get all pairs of variables:
  var_names <- colnames(in_data$flux_data[[1]][-1])  # Remove the time variable
  # Combinations of vars, 2 at a time, as a list
  var_pairs = var_pairs = combn(var_names, 2,simplify = FALSE)

  n_pairs <- length(var_pairs)
  out_ccm <- vector(mode = "list",length = n_pairs)

  for (i in seq_along(out_ccm)) {

    curr_names <- var_pairs[[i]]
    # Select out the data
    small_data <- select(in_data$flux_data[[1]],curr_names)

    # Perform the CCM test
    out_results <- rEDM::CCM(dataFrame = in_data$flux_data[[1]],
                             columns = curr_names[1],
                             target = curr_names[2],
                             libSizes = in_data$library_size,
                             Tp = 0,
                             E = in_data$E_opt, sample = 1) %>%
      select(-LibSize) %>%
      pivot_longer(cols=c(everything()))

    out_ccm[[i]] <- out_results

  }

  # Collapse these all together
  ccm_results <- bind_rows(out_ccm)




  return(ccm_results)



}











