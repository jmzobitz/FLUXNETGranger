#' @title Construct a cross correlation matrix

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}, based on code from https://cran.r-project.org/web/packages/rEDM/vignettes/rEDM-tutorial.pdf

#' @description
#' Given data frame compute the cross correlation between all variables, up to a given lag
#' @param data incoming data frame
#' @param max_lag the maximum number of lags to be considered
#' @param na_action what to do if we have NAs in data (default - omit)
#'
#'

#' @return Cross correlation matrix
#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2021-08-09)
#     original creation


correlation_matrix_construct <- function(in_data,max_lag) {

  vars = colnames(in_data)

  # Remove NAs
  in_data_rev <- in_data %>%
    na.omit()
  ### Correlation matrix
  corr_matrix <- array(NA, dim = c(length(vars), length(vars)), dimnames = list(vars,vars))

  for (ccm_from in vars) {
    for (ccm_to in vars[vars != ccm_from]) {
      ccf_out <- ccf(in_data_rev[, ccm_from], in_data_rev[, ccm_to], type = "correlation",
                     lag.max = max_lag, plot = FALSE)$acf
      corr_matrix[ccm_from, ccm_to] <- max(abs(ccf_out))
    }
  }

  return(corr_matrix)

}
