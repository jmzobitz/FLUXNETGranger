#' @title Compute an autocorrelation coefficient at a given site in the FLUXNET2015 dataset

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}, based on code from rEMD pacakge vignette

#' @description
#' Given a site, determine if all possible combinations of the variables are autocorrelated to a maximum lag. This is a tidy version of the cross correlation matrix.
#' @param in_data data frame of the site we are studying.  The first column contains the time variable
#' @param max_lag the maximum number of lags to be considered

#'
#'

#' @return A data frame that reports the absolute value of the autocorrelation function and the variables associated with it
#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2021-08-13)
#     original creation
#   Helpful hint from Mohamed Ombadi (personal correspondence)

auto_correlation_site <- function(in_data,max_lag) {


  # Get all pairs of variables:
  var_names <- colnames(in_data[-1])  # Remove the time variable
  var_pairs = map(1:2, ~ var_names) %>%
    cross() %>%
    keep(~ length(unique(.x)) == 2) %>%
    map(unlist) # Combinations of vars, 2 at a time, as a list
  n_pairs <- length(var_pairs)
  out_auto_correlation <- vector(mode = "list",length = n_pairs)

  for (i in seq_along(out_auto_correlation)) {

    curr_names <- var_pairs[[i]]
    # Select out the data
    small_data <- select(in_data,curr_names)


    correlation_out <- ccf(small_data[[1]],small_data[[2]], type = "correlation",
                   lag.max = max_lag, plot = FALSE)$acf


    # Perform the Granger Test
    out_results <- tibble(x = curr_names[1],
                          y = curr_names[2],
                          correlation = max(abs(correlation_out))
    )


    out_auto_correlation[[i]] <- out_results

  }

  # Collapse these all together
  correlation_results <- bind_rows(out_auto_correlation)




  return(correlation_results)



}











