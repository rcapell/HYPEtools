
#' @export

#' @title
#' Extract quantiles for use in a frequency distribution plot, e.g. a flow duration curve
#'
#' @description
#' This function calculates quantiles suitable for duration curves of environmental time series data.
#' 
#'
#' @param data either a numeric vector or an all-numeric dataframe (\code{NA}s allowed) which holds the variables for which
#' quantiles are computed.
#'
#' 
#' @details
#' \code{ExtractFreq} is a convenience wrapper function, it uses \code{\link{quantile}} to calculate the quantiles 
#' of one or more time series with a density appropriate for duration curves.
#' \code{NA}s are allowed in the input data. For the results to be meaningful, input should represent equally-spaced time series.
#' 
#' @return
#' \code{ExtractFreq} returns a dataframe with probabilities in the first column, and quantiles of data in the following columns. 
#' Number of observations per variable in \code{data} are given in an attribute \code{n.obs} (see \code{\link{attributes}}).
#' 
#' 
#' @examples
#' ExtractFreq(rnorm(1000))



ExtractFreq <- function(data){
  # probabilities to be calculated
  prbs <- c(0, 0.00001, 0.0001, 0.001, seq(0.01,0.99,by = .01), 0.999, 0.9999, 0.99999, 1)
  # condition: several variables in data
  if (is.data.frame(data)) {
    # condition: several variables in data
    # preparation of return variables
    nc <- ncol(data)
    res <- matrix(nrow=length(prbs), ncol=nc+1)
    res <- as.data.frame(res)
    res[,1] <- prbs
    names(res) <- c("prob", names(data))
    n.obs <- NA
    # column-wise calculation of quantiles and number of observations
    for (i in 1:nc) {
      res[,i+1] <- quantile(data[,i], probs=prbs, na.rm=T, names=F)
      n.obs[i] <- length(na.omit(data[,i]))
    }
  } else {
    # condition: vector variable in data
    res <- data.frame(prob=prbs, quantile = quantile(data, probs=prbs, na.rm=T, names=F))
    n.obs <- length(na.omit(data))
  }
  # add number of observations as new attribute to result dataframe
  attr(res, which = "n.obs") <- n.obs
  return(res)
}


## DEBUG
# data <- data.frame(x=rnorm(1000), y=runif(1000), z=rgamma(1000, shape=.5))
# data <- rnorm(1000)
