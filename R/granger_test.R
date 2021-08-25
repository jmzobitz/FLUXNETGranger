#' @title Compute a Granger Causality test

#' @author
#' John Zobitz \email{zobitz@augsburg.edu}, based on code from Chandler Lutz, UCR 2009 (https://www.mathworks.com/matlabcentral/fileexchange/25467-granger-causality-test).

#' @description
#' Given an x (predictor) and y (response) variable, determine if x causes way at the given lag. Uses the F test statistic to compare/evaluate. The Null Hypothesis (H0) is that Time series x does not Granger-cause time series y, and the Alternative Hypothesis (HA): Time series x Granger-causes time series y
#' @param x Predictor variable (can be a vector)
#' @param y Response variable (can be a vector)
#' @param alpha the significance level
#' @param max_lag the maximum number of lags to be considered

#'
#'

#' @return A data frame that reports the F statistic, the critical value, and if F > c_v.   (if F > c_v we reject the null hypothesis that x does not Granger Cause y)
#' @export

# changelog and author contributions / copyrights
#   John Zobitz (2021-08-06)
#     original creation
#   Helpful hint from Mohamed Ombadi (personal correspondence)



granger_test <- function (x, y, alpha, max_lag = 1)
{

  T <- length(x)

  # Specify the BIC for the restricted RSS
  BIC_R <- array(0,max_lag)
  #Specify a matrix for the restricted RSS
  RSS_R <- array(0,max_lag)
  i = 1

  while (i <= max_lag) {
    ystar <- x[(i+1):T]
    xstar <- matrix(0,nrow=T-i,ncol=i)

    j = 1
    while (j <= i) {
      xstar[,j] <- x[(i+1-j):(T-j)]
      j <- j+1
    }

    # Apply the regress function. b = betahat, bint corresponds to the 95%
    # confidence intervals for the regression coefficients and r = residuals
    lin_fit_r <- lm(ystar~xstar)

    #Find the bayesian information criterion
    BIC_R[i] <- BIC(lin_fit_r)

    # Use broom to compute the residual sum of squares
    # sigma = standard error of the residuals
    # df.residual = residual degrees of freedom

    RSS_R[i] <- (broom::glance(lin_fit_r)$sigma^2)*(broom::glance(lin_fit_r)$df.residual)

    i <- i+1
  }

  # Determine which lag has the smallest BIC value
  x_lag <- which.min(BIC_R)

  #First find the proper model specification using the Bayesian Information
  # Criterion for the number of lags of y

   # Specify the BIC for the unrestricted RSS
  BIC_U <- array(0,max_lag)
  #Specify a matrix for the unrestricted RSS

  RSS_U <- array(0,max_lag)

  i = 1
  while (i <= max_lag) {

    ystar <- x[(i+x_lag+1):T]
    xstar <- matrix(0,nrow=(T-(i+x_lag)),ncol=(x_lag+i))

    # Populate the xstar matrix with the corresponding vectors of lags of x
    j = 1;
    while (j <= x_lag) {
      xstar[,j] = x[(i+x_lag+1-j):(T-j)]
      j = j+1;
    }


    # Populate the xstar matrix with the corresponding vectors of lags of y
    j <- 1;
    while (j <= i) {
      xstar[,x_lag+j] = y[(i+x_lag+1-j):(T-j)]
      j <- j+1;
    }


    # Apply the regress function. b = betahat, bint corresponds to the 95%
    # confidence intervals for the regression coefficients and r = residuals
    lin_fit_u <- lm(ystar~xstar)

    #Find the bayesian information criterion
    BIC_U[i] <- BIC(lin_fit_u)

    # Use broom to compute the residual sum of squares
    # sigma = standard error of the residuals
    # df.residual = residual degrees of freedom

    RSS_U[i] <- (broom::glance(lin_fit_u)$sigma^2)*(broom::glance(lin_fit_u)$df.residual)

    i <- i+1



  }


  y_lag <- which.min(BIC_U);

  # The numerator of the F-statistic
  F_num <- ((RSS_R[x_lag] - RSS_U[y_lag])/y_lag)

  # The denominator of the F-statistic
  F_den <- RSS_U[y_lag]/(T-(x_lag+y_lag+1))

  # The F-Statistic
  F <- F_num/F_den;
  c_v <- qf(1-alpha,y_lag,(T-(x_lag+y_lag+1)))
  p_val <- 1-pf(F,y_lag,(T-(x_lag+y_lag+1)))
  out_vector <- tibble( F = F,
                        c_v = c_v,
                        null_reject = F > c_v,
                        p_val = p_val)

  return(out_vector)

}
