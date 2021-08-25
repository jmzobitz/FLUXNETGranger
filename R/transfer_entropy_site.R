#' @title Compute a transfer entropy test at a given site in the FLUXNET2015 dataset

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}, based on code from Chandler Lutz, UCR 2009 (https://www.mathworks.com/matlabcentral/fileexchange/25467-granger-causality-test).

#' @description
#' Given a site, determine if all possible combinations of the variables are Granger cause each other. The Null Hypothesis (H0) is that Time series x does not cause time series y, and the Alternative Hypothesis (HA): Time series x causes time series y
#' @param in_data data frame of the site we are studying.  The first column contains the time variable
#' @param alpha the significance level
#' @param max_lag the maximum number of lags to be considered
#' @param bootstrap_samples Number of bootstrap samples we apply

#'
#'

#' @return A T/F value - this means that the conditional information is less than the significance level alpha for all the considered lags.
#'
#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2021-08-21)
#     modification to RTransferEntropy package
#   John Zobitz (2021-08-11)
#     original creation
#   Helpful hint from Mohamed Ombadi (personal correspondence)

transfer_entropy_site <- function(in_data,alpha,max_lag,bootstrap_samples = 1) {
###

  # Get all pairs of variables:
  var_names <- colnames(in_data)[-1]  # Remove the time variable
  # Combinations of vars, 2 at a time, as a list
  var_pairs = var_pairs = combn(var_names, 2,simplify = FALSE)

  n_pairs <- length(var_pairs)
  out_te <- vector(mode = "list",length = n_pairs)

  # Define the parallel processing
  future::plan(multisession)

  for (i in seq_along(out_te)) {

    curr_names <- var_pairs[[i]]
    # Select out the data
    small_data <- select(in_data,all_of(curr_names))

    # Perform the Transfer Entropy test


    shannon_te <- RTransferEntropy::transfer_entropy(x = small_data[[1]],
                                                     y = small_data[[2]],
                                                     lx = max_lag,
                                                     ly = max_lag,
                                                     nboot = 5,  # We just set this low to avoid the waiting
                                                     quiet = TRUE
    )

    # Start to make a data frame of everything - the coefficiencts from the te method + the rownames for the causality
    shannon_te_coef <- coef(shannon_te)
    shannon_te_rows <- rownames(shannon_te_coef)

    out_results <- shannon_te_coef %>%
      as_tibble() %>%
      mutate(causality = shannon_te_rows) %>%
      separate(col=causality,into = c("x","y"),sep="->") %>%
      mutate(x = if_else(x == "X",curr_names[[1]],curr_names[[2]]),
             y = if_else(y == "X",curr_names[[1]],curr_names[[2]]),
             null_reject = `p-value` < alpha)


    out_te[[i]] <- out_results

  }



  # Collapse these all together
  te_results <- bind_rows(out_te)




  return(te_results)



}











