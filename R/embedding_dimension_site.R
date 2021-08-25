#' @title Compute a the embedding dimension at a given site in the FLUXNET2015 dataset

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}, based on code from Chandler Lutz, UCR 2009 (https://www.mathworks.com/matlabcentral/fileexchange/25467-granger-causality-test).

#' @description
#' Given a site, determine the optimal embedding dimension so we can apply convergent cross mapping
#' @param in_data data frame of the site we are studying.  The first column contains the time variable

#'
#'

#' @return A string (more here)
#'
#' @export
#'
#'


embedding_dimension_site <- function(in_data) {

  n <- dim(in_data)[1]

  # The library size
  l <- ceiling(dim(in_data)[1]*0.7)
  lib_size <- paste("1",l)

  # The prediction size
  pred_size <- paste(l+1,n)

  in_data_small <- in_data %>%
    pivot_longer(cols=c(-"time")) %>%
    group_by(name) %>%
    nest()

  # Compute the embedding dimension for each measurement
  out_results <- in_data_small %>%
    mutate(E = map(.x=data,.f=~rEDM::EmbedDimension(dataFrame = .x, lib = lib_size, pred = pred_size, columns = "value",target = "value",showPlot = FALSE)),
           E_opt = map(.x=E,.f=~which.max(.x$rho)))


  # Average embedding:
  E_opt <- out_results %>%
    select(name,E_opt) %>%
    unnest(cols=c(E_opt)) %>%
    ungroup() %>%
    summarize(ceiling(mean(E_opt))) %>%
    pull() %>%
    ceiling()

  # The new library size
  libSize <- paste(n - E_opt, n - E_opt, E_opt, collapse = " ")

  out_results <- tibble(E_opt,library_size = libSize)

  return(out_results)

}





# Now map across each of these to find the perfect dimension:




