
#' @export

#' @title
#' Extract quantiles for use in a frequency distribution plot, e.g. a flow duration curve
#'
#' @description
#' This function calculates quantiles suitable for duration curves of environmental time series data.
#' 
#'
#' @param data either a numeric vector or 
#' an all-numeric data frame (\code{NA}s allowed)
#'
#' 
#' @details
#' \code{ExtractFreq} is a convenience wrapper function, it uses \code{\link{quantile}} to calculate the quantiles 
#' of one or more time series with a density appropriate for duration curves.
#' \code{NA}s are allowed in the input data. For the results to be meaningful, input should represent equally-spaced time series.
#' 
#' @return
#' \code{ExtractFreq} returns a list object with two elements:
#' \itemize{
#'  \item probs.res: a dataframe with probabilities (first column) and quantiles of indata (other columns)
#'  \item no.obs: a numeric vector with no. of observations for each indata column
#' }
#' 
#' 
#' @examples
#' ExtractFreq(rnorm(1000))



ExtractFreq <- function(data){
  # probabilities to be calculated
  prbs <- c(0, 0.0001, 0.001, seq(0.01,0.99,by = .01), 0.999, 0.9999, 1)
  # condition: several variables in data
  if (is.data.frame(data)) {
    # preparation of return variables
    nc <- ncol(data)
    res1 <- matrix(nrow=length(prbs), ncol=nc+1)
    res1 <- as.data.frame(res1)
    res1[,1] <- prbs
    names(res1) <- c("prob", names(data))
    res2 <- NA
    # column-wise calculation of quantiles and number of observations
    for (i in 1:nc) {
      res1[,i+1] <- quantile(data[,i], probs=prbs, na.rm=T, names=F)
      res2[i] <- length(na.omit(data[,i]))
    }
  }
  # condition: vector variable in data
  else {
    res1 <- data.frame(prob=prbs, quantile = quantile(data, probs=prbs, na.rm=T, names=F))
    res2 <- length(na.omit(data))
  }
  # cembination of results in a list to return from function
  outdata <- list(probs.res = res1, no.obs = res2)
  return(outdata)
}
