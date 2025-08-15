#' Goodness of Fit Functions
#'
#' Numerical goodness-of-fit measures between sim and obs, with treatment of missing values.
#'
#' @param sim numeric, vector of simulated values
#' @param obs numeric, vector of observed values
#' @param na.rm a logical value indicating whether 'NA' should be stripped before the computation proceeds. 
#' When an 'NA' value is found at the i-th position in obs OR sim, the i-th value of obs AND sim are removed before the computation.
#' @param do.spearman logical, indicates if the Spearman correlation should be computed. The default is \code{FALSE}.
#' @param s argument passed to the \code{\link{KGE}} function.
#' @param method argument passed to the \code{\link{KGE}} function.
#' @param start.month argument passed to the \code{\link{sKGE}} function.
#' @param out.PerYear logical, argument passed to the \code{\link{sKGE}} function.
#' @param out.type argument passed to the \code{\link{KGE}} function.
#' @param dec argument passed to the \code{\link{pbias}} function.
#' @param digits integer, number of decimal places used for rounding the goodness of fit indexes.
#' @param fun function to be applied to \code{sim} and \code{obs} in order to obtain transformed values thereof before applying any goodness-of-fit function
#' @param epsilon.type argument used to define a numeric value to be added to both \code{sim} and \code{obs} before applying fun. It was designed to allow the use of
#' logarithm and other similar functions that do not work with zero values. It must be one of the following possible values:
#' \itemize{
#' \item{\emph{none}: no value added to \code{sim} or \code{obs}.}
#' \item{\emph{Pushpalatha2012}: one hundredth of the mean observed values is added to both \code{sim} and \code{obs} as described in Pushpalatha et al., 2012.}
#' \item{\emph{otherFactor}: the numeric value defined in \code{epsilon.value} is used to multiply the mean observed values instead of the one hundredth (1/100)
#' described in Pushpalatha et al., (2012). The resulting value is then added to both \code{sim} and \code{obs}.}
#' \item{\emph{otherValue}: the numeric value defined in \code{epsilon.value} is directly added to both \code{sim} and \code{obs}.}
#' }
#' @param epsilon.value numeric, value to be added to both \code{sim} and \code{obs} when \code{epsilon} = "otherValue".
#' @param ... further arguments passed to/from other methods.
#' @details
#' The \code{gof}, \code{mae}, \code{pbias}, \code{NSE}, \code{rPearson}, \code{sKGE}, and \code{KGE} functions are provided to calculate goodness of fit statistics.
#' The functions were adapted from the hydroGOF package \url{https://github.com/hzambran/hydroGOF}.
#'
#' @return
#' \code{gof} Returns a matrix of goodness of fit statistics. \code{mae}, \code{pbias}, \code{NSE}, \code{rPearson}, \code{sKGE}, and \code{KGE} return a numeric of the goodness of fit statistic.
#' 
#'
#' @examples
#' gof(sim = sample(1:100), obs = sample(1:100))
#' 
#' @name GOF
NULL

# _____________________________________________________________________________________________________________________________________
# hydroGOF::gof #####
# _____________________________________________________________________________________________________________________________________

# Functions for goodness of fit
# - Adapted from hydroGOF package which was archived by CRAN on 2023-10-17 due to not updating to remove dependencies on retired r-spatial packages
# - hydroGOF package: https://github.com/hzambran/hydroGOF

# It computes:
# 'me'        : Mean Error
# 'mae'       : Mean Absolute Error
# 'rms'       : Root Mean Square Error
# 'nrms'      : Normalized Root Mean Square Error
# 'r'         : Pearson Correlation coefficient ( -1 <= r <= 1 )
# 'r.Spearman': Spearman Correlation coefficient ( -1 <= r <= 1 ) 
# 'R2'        : Coefficient of Determination ( 0 <= r2 <= 1 )
#               Gives the proportion of the variance of one variable that
#               that is predictable from the other variable
# 'rSD'       : Ratio of Standard Deviations, rSD = SD(sim) / SD(obs)
# 'RSR'       : Ratio of the RMSE to the standard deviation of the observations
# 'NSE'       : Nash-Sutcliffe Efficiency ( -Inf <= NSE <= 1 )
# 'mNSE'      : Modified Nash-Sutcliffe Efficiency
# 'rNSE'      : Relative Nash-Sutcliffe Efficiency
# 'd'         : Index of Agreement( 0 <= d <= 1 )
# 'dr'        : Refined Index of Agreement( -1 <= dr <= 1 )
# 'md'        : Modified Index of Agreement( 0 <= md <= 1 )
# 'rd'        : Relative Index of Agreement( 0 <= rd <= 1 )
# 'cp'        : Coefficient of Persistence ( 0 <= cp <= 1 ) 
# 'PBIAS'     : Percent Bias ( -1 <= PBIAS <= 1 )
# 'bR2'       : weighted coefficient of determination
# 'KGE'       : Kling-Gupta efficiency (-Inf < KGE <= 1)
# 'sKGE'      : Split Kling-Gupta efficiency (-Inf < sKGE <= 1)
# 'KGElf'     : Kling-Gupta efficiency with focus on low values (-Inf < KGElf <= 1)
# 'KGEnp'     : Non-parametric Kling-Gupta efficiency (-Inf < KGEnp <= 1)
# 'VE'        : Volumetric efficiency

#' @rdname GOF
#' @export
gof <-function(sim, obs, ...) UseMethod("gof")

#' @rdname GOF
#' @export
gof.default <- function(sim, obs, na.rm=TRUE, do.spearman=FALSE, #do.pbfdc=FALSE, 
                        #j=1, norm="sd",
                        s=c(1,1,1), method=c("2009", "2012"), 
                        # lQ.thr=0.7, hQ.thr=0.2, 
                        start.month=1, 
                        digits=2, fun=NULL, ...,
                        epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                        epsilon.value=NA){
  
  method        <- match.arg(method)
  epsilon.type  <- match.arg(epsilon.type)
  
  # Set non-calculated values to NA
  ME <- NULL
  MSE <- NULL
  RMSE <- NULL
  NRMSE <- NULL
  RSR <- NULL
  rSD <- NULL
  mNSE <- NULL
  rNSE <- NULL
  d <- NULL
  dr <- NULL
  md <- NULL
  rd <- NULL
  cp <- NULL
  bR2 <- NULL
  KGElf <- NULL
  KGEnp <- NULL
  
  # ME     <- me(sim, obs, na.rm=na.rm, fun=fun, ..., 
  #              epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  MAE    <- mae(sim, obs, na.rm=na.rm, fun=fun, ..., 
                epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  # MSE    <- mse(sim, obs, na.rm=na.rm, fun=fun, ..., 
  #               epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  # RMSE   <- rmse(sim, obs, na.rm=na.rm, fun=fun, ..., 
  #                epsilon.type=epsilon.type, epsilon.value=epsilon.value) 
  # NRMSE  <- nrmse(sim, obs, na.rm=na.rm, norm=norm, fun=fun, ..., 
  #                 epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  # RSR    <- rsr(sim, obs, na.rm=na.rm, fun=fun, ..., 
  #               epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  # rSD    <- rSD(sim, obs, na.rm=na.rm, fun=fun, ..., 
  #               epsilon.type=epsilon.type, epsilon.value=epsilon.value)     
  PBIAS  <- pbias(sim, obs, na.rm=na.rm, fun=fun, ..., 
                  epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  NSE    <- NSE(sim, obs, na.rm=na.rm, fun=fun, ..., 
                epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  # mNSE   <- mNSE(sim, obs, na.rm=na.rm, j=j, fun=fun, ..., 
  #                epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  # rNSE   <- rNSE(sim, obs, na.rm=na.rm, fun=fun, ..., 
  #                epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  # d      <- d(sim, obs, na.rm=na.rm, fun=fun, ..., 
  #             epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  # dr     <- dr(sim, obs, na.rm=na.rm, fun=fun, ..., 
  #              epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  # md     <- md(sim, obs, na.rm=na.rm, fun=fun, ..., 
  #              epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  # rd     <- rd(sim, obs, na.rm=na.rm, fun=fun, ..., 
  #              epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  # cp     <- cp(sim, obs, na.rm=na.rm, fun=fun, ..., 
  #              epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  r      <- rPearson(sim, obs, fun=fun, ..., 
                     epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  # bR2    <- br2(sim, obs, na.rm=na.rm, fun=fun, ..., 
  #               epsilon.type=epsilon.type, epsilon.value=epsilon.value)     
  KGE    <- KGE(sim, obs, na.rm=na.rm, s=s, method=method, out.type="single", 
                fun=fun, ..., epsilon.type=epsilon.type, epsilon.value=epsilon.value) 
  # KGElf  <- KGElf(sim, obs, na.rm=na.rm, s=s, method=method, 
  #                 epsilon.type=epsilon.type, epsilon.value=epsilon.value) 
  if ( inherits(sim, "zoo") & inherits(obs, "zoo") ) {
    do.sKGE <- TRUE
    sKGE   <- sKGE(sim, obs, na.rm=na.rm, s=s, method=method, out.type="single", 
                   start.month=start.month, out.PerYear=FALSE, fun=fun, ..., 
                   epsilon.type=epsilon.type, epsilon.value=epsilon.value) 
  } else {
    do.sKGE <- FALSE
    sKGE <- NA
  } # ELSE end
  
  
  # KGEnp  <- KGEnp(sim, obs, na.rm=na.rm, out.type="single", fun=fun, ..., 
  #                 epsilon.type=epsilon.type, epsilon.value=epsilon.value) 
  VE     <- VE(sim, obs, na.rm=na.rm, fun=fun, ..., 
               epsilon.type=epsilon.type, epsilon.value=epsilon.value)     
  
  # 'R2' is the Coefficient of Determination
  # The coefficient of determination, R2, is useful because it gives the proportion of
  # the variance (fluctuation) of one variable that is predictable from the other variable.
  # It is a measure that allows us to determine how certain one can be in making
  # predictions from a certain model/graph.
  # The coefficient of determination is the ratio of the explained variation to the total
  # variation.
  # The coefficient of determination is such that 0 <  R2 < 1,  and denotes the strength
  # of the linear association between x and y. 
  R2 <- r^2
  
  if (do.spearman) {
    # index of those elements that are present both in 'sim' and 'obs' (NON- NA values)
    vi <- valindex(sim, obs)
    
    if (length(vi) > 0) {	 
      # Filtering 'obs' and 'sim', selecting only those pairs of elements 
      # that are present both in 'x' and 'y' (NON- NA values)
      obs <- obs[vi]
      sim <- sim[vi]
      
      if (!is.null(fun)) {
        fun1 <- match.fun(fun)
        new  <- preproc(sim=sim, obs=obs, fun=fun1, ..., 
                        epsilon.type=epsilon.type, epsilon.value=epsilon.value)
        sim  <- new[["sim"]]
        obs  <- new[["obs"]]
      } # IF end     
      
      r.Spearman <- cor(sim, obs, method="spearman", use="pairwise.complete.obs") 
      
      # if 'sim' and 'obs' were matrixs or data.frame, then the correlation
      # between observed and simulated values for each variable is given by the diagonal of 'r.Pearson' 
      if ( is.matrix(r.Spearman) | is.data.frame(r.Spearman) )
        r.Spearman <- diag(r.Spearman)
      
    } else {
      r.Spearman <- NA
      warning("There are no pairs of 'sim' and 'obs' without missing values !")
    } # ELSE end
    
  } # IF 'do.spearman' end
  
  # if (do.pbfdc) 
  #   pbfdc  <- pbiasfdc(sim, obs, na.rm=na.rm, lQ.thr=lQ.thr, hQ.thr=hQ.thr, plot=FALSE, 
  #                      fun=fun, ..., epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  
  gof <- rbind(ME, MAE, MSE, RMSE, NRMSE, PBIAS, RSR, rSD, NSE, mNSE, rNSE, d, dr, md, rd, cp, r, R2, bR2, KGE, KGElf, KGEnp, VE)     
  
  if("NRMSE" %in% rownames(gof)){
    rownames(gof)[which(rownames(gof) == "NRMSE")] <- "NRMSE %"
  }
  
  if("PBIAS" %in% rownames(gof)){
    rownames(gof)[which(rownames(gof) == "PBIAS")] <- "PBIAS %"
  }
  
  if (do.spearman)
    gof <- rbind(gof, r.Spearman)
  
  # if (do.pbfdc) { 
  #   gof <- rbind(gof, pbfdc) 
  #   rownames(gof)[length(rownames(gof))] <- "pbiasFDC %"
  # } # IF end
  
  if (do.sKGE) { 
    gof <- c( gof[1:21], sKGE, gof[22:length(gof)] )
    rownames(gof)[22] <- "sKGE"
  } # IF end
  
  # Rounding the final results, ofr avoiding scientific notation
  gof <- round(gof, digits)
  
  return(gof)
  
}

# valindex --------------------------------------------------------------------------------------------------------------------


# 'valindex': index of the elements that belongs to both vectors
# 'x'     : vector (numeric, xts, zoo)
# 'y'     : vector (numeric, xts, zoo)
# 'Result': index containing the position in 'x' and 'y' where both vectors have valid elements (NON- NA)

#' @rdname GOF
#' @export
valindex <- function(sim, obs, ...) UseMethod("valindex")

#' @rdname GOF
#' @export
valindex.default <- function(sim, obs, ...) {  
  
  if ( length(obs) != length(sim) ) {
    stop( "Invalid argument: 'length(sim) != length(obs)' !! (", length(sim), "!=", length(obs), ") !!" )
  } else { 
    index <- which(!is.na(sim) & !is.na(obs))
    if (length(index)==0) warning("'sim' and 'obs' are empty or they do not have any common pair of elements with data !!")
    return( index  )
  } # ELSE end
  
} # 'valindex' END

# preproc --------------------------------------------------------------------------------------------------------------------

# 'preproc': It applies a user-defined function to simulated and observed
#            values before computing any goodness-of-fit function, probably
#            adding a user-defined (and small) 'epsilon' value in order to
#            allow the use of logarithm and other similar functions that do
#            not work with zero values
# Reference: Pushpalatha, R., Perrin, C., Le Moine, N., & Andreassian, V.
#            (2012). A review of efficiency criteria suitable for evaluating
#            low-flow simulations. Journal of Hydrology, 420, 171-182.
#            DOI: 10.1016/j.jhydrol.2011.11.055                                
# 'sim'     : numeric, with simulated values
# 'obs'     : numeric, with observed values
# 'fun'     : function to be applied to 'sim' and 'obs' in order to obtain 
#             transformed values thereof before applying any goodness-of-fit 
#             function included in the hydroGOF package
# '...'     : additional argument to be passed to fun
# 'epsilon.type' : argument used to define a numeric value to be added to both 'sim' 
#                  and 'obs' before applying fun. It is was  designed to allow the 
#                  use of logarithm and other similar functions that do not work with 
#                  zero values. It must be one of the following three possible values:
#             -) "Pushpalatha2012": one hundredth of the mean observed values is 
#                                   added to both 'sim' and 'obs', as described  
#                                   in Pushpalatha et al., (2012). 
#             -) "otherFactor"    : the numeric value defined in the \code{epsilon.value} 
#                                   argument is used to multiply the the mean 
#                                   observed values, instead of the 
#                                   one hundredth (1/100) described in Pushpalatha et al. (2012). 
#                                   The resulting value is then added to both 
#                                   \code{sim} and \code{obs}.
#             -) "otherValue"     : the numeric value defined in the 'epsilon.value'
#                                   argument is directly added to both 'sim' and 'obs'

# 'epsilon.value': numeric value to be added to both 'sim' and 'obs' when 
#                  'epsilon="other"'

# 'Output': a list with two numeric vectors:
#           1) 'sim': simulated values after adding 'epsilon.value' and 
#                     applying 'fun' 
#           2) 'obs': observed values after adding 'epsilon.value' and 
#                     applying 'fun' 
preproc <- function (sim, obs, na.rm=TRUE, fun,  ..., 
                     epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                     epsilon.value=0) { 
  
  # fun ?
  fun.exists <- FALSE
  if (!missing(fun)) {
    fun.exists <- TRUE
    fun        <- match.fun(fun)
  } # IF end
  
  # epsilon.type ?
  epsilon.type <- match.arg(epsilon.type)
  
  if (epsilon.type %in% c("otherFactor", "otherValue") )  {
    if (is.na(epsilon.value))
      stop("Missing argument: you need to provide 'epsilon.value' !")
    
    if ( !is.numeric(epsilon.value) )
      stop("Invalid argument: 'epsilon.value' must be numeric !")
  } # IF end
  
  # epsilon.value 
  if (epsilon.type != "none") {
    if (epsilon.type=="Pushpalatha2012") {    
      epsilon.value <- (1/100)*mean(obs, na.rm=na.rm)
    } else if (epsilon.type=="otherFactor") {
      epsilon.value <- epsilon.value*mean(obs, na.rm=na.rm)
    } # ELSE (epsilon="otherValue"): epsilon.value=epsilon.value
  } else epsilon.value <- 0
  
  # Adding epsilon, before applying fun
  obs <- obs + epsilon.value
  sim <- sim + epsilon.value
  
  # using fun (and 'epsilon.value')
  if (fun.exists) {
    obs.bak <- obs
    sim.bak <- sim
    
    obs <- fun( obs, ...)     
    sim <- fun( sim, ...)
    
    if (length(obs) != length(obs.bak))
      stop("Invalid argument: 'fun' returns an object with a length different from 'obs' or 'sim' !")
  } # IF 'fun.exists' end
  
  out <- list(sim=sim, obs=obs)
  
  return(out)
  
} # 'preproc' END

# rPearson --------------------------------------------------------------------------------------------------------------------

# The 'r.Pearson' coefficient ranges from -1 to 1. 
# A value of 1 shows that a linear equation describes the relationship 
# perfectly and positively, with all data points lying on the same line 
# and with Y increasing with X. 
# A score of -1 shows that all data points lie on a single line but 
# that Y increases as X decreases. 
# A value of 0 shows that a linear model is not needed, i.e., that there 
# is no linear relationship between the variables.

#' @rdname GOF
#' @export
rPearson <-function(sim, obs, ...) UseMethod("rPearson")

#' @rdname GOF
#' @export
rPearson.default <- function(sim, obs, fun=NULL, ..., 
                             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                             epsilon.value=NA) {
  
  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
  ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")
  
  vi <- valindex(sim, obs)
  
  if (length(vi) > 0) {
    
    obs <- as.numeric(obs[vi])
    sim <- as.numeric(sim[vi])
    
    if (!is.null(fun)) {
      fun1 <- match.fun(fun)
      new  <- preproc(sim=sim, obs=obs, fun=fun1, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
      sim  <- new[["sim"]]
      obs  <- new[["obs"]]
    } # IF end
    
    rPearson <- cor(sim, obs, method="pearson", use="pairwise.complete.obs")      
    # if 'sim' and 'obs' were matrixs or data.frame, then the correlation
    # between observed and simulated values for each variable is given by the diagonal of 'r.Pearson' 
    
    #if ( is.matrix(r.Pearson) | is.data.frame(r.Pearson) ) {
    #r.Pearson        <- diag(r.Pearson)
    #}
    
  } else {
    rPearson <- NA
    warning("There are no pairs of 'sim' and 'obs' without missing values !")
  } # ELSE end
  
  return(rPearson)
  
} # 'rPearson.default' end

# sKGE --------------------------------------------------------------------------------------------------------------------

# 'sKGE': Kling-Gupta Efficiency with focus on low flows                       
# The optimal value of sKGE is 1

# Ref:
# Fowler, K., Coxon, G., Freer, J., Peel, M., Wagener, T., 
# Western, A., Woods, R. and Zhang, L. (2018). Simulating runoff under 
# changing climatic conditions: A framework for model improvement.
# Water Resources Research, 54(12), pp.9812-9832. doi:https://doi.org/10.1029/2018WR023989

#' @rdname GOF
#' @importFrom stats time
#' @importFrom zoo coredata
#' @export
sKGE <- function(sim, obs, ...) UseMethod("sKGE")

# epsilon: By default it is set at one hundredth of the mean flow. See Pushpalatha et al. (2012)
#' @rdname GOF
#' @export
sKGE.default <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                         method=c("2009", "2012"),
                         start.month=1, out.PerYear=FALSE,
                         fun=NULL,
                         ...,
                         epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                         epsilon.value=NA
) { 
  
  lKGE <- function(i, lsim, lobs, s=c(1,1,1), na.rm=TRUE, 
                   method=c("2009", "2012"), out.type="single",
                   fun1=NULL,
                   ...,
                   epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                   epsilon.value=NA) {
    llsim <- lsim[[i]]
    llobs <- lobs[[i]]
    
    out <- KGE(sim=llsim, obs=llobs, s=s, na.rm=na.rm, method=method, out.type=out.type,
               fun=fun1, ..., epsilon.type=epsilon.type, epsilon.value=epsilon.value)
    return(out)
  } #'lKGE' END
  
  
  # Function for shifting a time vector by 'nmonths' number of months.
  .shiftyears <- function(ltime,       # Date/POSIX* object. It MUST contat MONTH and YEAR
                          lstart.month # numeric in [2,..,12], representing the months. 2:Feb, 12:Dec
  ) {
    syears.bak        <- as.numeric(format( ltime, "%Y" ))
    syears            <- syears
    smonths           <- as.numeric(format( ltime, "%m"))
    months2moveback   <- 1:(lstart.month-1)
    N                 <- length(months2moveback)
    for (i in 1:N) {
      m.index         <- which(smonths == months2moveback[i])
      m.year          <- unique(na.omit(syears.bak[m.index]))
      m.year          <- m.year - 1
      syears[m.index] <- m.year
    } # FOR end
    return(syears)
  } # '.shift' END
  
  
  # Checking 'method' and 'epsilon.type'
  method       <- match.arg(method)
  epsilon.type <- match.arg(epsilon.type)
  
  if ( !inherits(sim, "zoo") | !inherits(obs, "zoo"))
    stop("Invalid argument: 'sim' and 'obs' must be 'zoo' objects !")
  
  # Selecting only valid paris of values
  vi <- valindex(sim, obs)     
  if (length(vi) > 0) {	 
    obs <- obs[vi]
    sim <- sim[vi]
    
    if (!is.null(fun)) {
      fun <- match.fun(fun)
      new <- preproc(sim=sim, obs=obs, fun=fun, ..., 
                     epsilon.type=epsilon.type, epsilon.value=epsilon.value)
      sim <- new[["sim"]]
      obs <- new[["obs"]]
    } # IF end
  } else stop("There are no points with simultaneous values of 'sim' and 'obs' !!")
  
  # Annual index for 'x'
  dates.sim  <- time(sim)
  dates.obs  <- time(obs)
  years.sim  <- format( dates.sim, "%Y")
  years.obs  <- format( dates.obs, "%Y")
  if (!all.equal(years.sim, years.obs)) {
    stop("Invalid argument: 'sim' and 'obs' must have the same dates !")
  } else {
    
    if (start.month !=1) 
      years.obs <- .shiftyears(dates.obs, start.month)
    
    years.unique <- unique(years.obs)
    nyears       <- length(years.unique)
  } # ELSE end
  
  
  # Getting a list of 'sim' and 'obs' values for each year
  sim.PerYear <- split(coredata(sim), years.obs)
  obs.PerYear <- split(coredata(obs), years.obs) # years.sim == years.obs
  
  
  # Computing Annual KGE values
  #if (!is.null(fun)) {
  KGE.yr <- sapply(1:nyears, FUN=lKGE, lsim=sim.PerYear, lobs=obs.PerYear, s=s, 
                   na.rm= na.rm, method=method, out.type="single", 
                   fun1=fun, ..., epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  
  names(KGE.yr) <- as.character(years.unique)
  
  sKGE <- mean(KGE.yr, na.rm=na.rm)
  
  if (out.PerYear) {
    out <- list(sKGE.value=sKGE, KGE.PerYear=KGE.yr)
  } else out <- sKGE
  
  return(out)
} # 'sKGE.default' END

# KGE --------------------------------------------------------------------------------------------------------------------

# The optimal value of KGE is 1

# Ref1:
# Hoshin V. Gupta, Harald Kling, Koray K. Yilmaz, Guillermo F. Martinez, 
# Decomposition of the mean squared error and NSE performance criteria: 
# Implications for improving hydrological modelling, 
# Journal of Hydrology, Volume 377, Issues 1-2, 20 October 2009, Pages 80-91, 
# DOI: 10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694, 

# Ref2:
# Kling, H., M. Fuchs, and M. Paulin (2012), Runoff conditions in the upper
# Danube basin under an ensemble of climate change scenarios, 
# Journal of Hydrology, Volumes 424-425, 6 March 2012, Pages 264-277, 
# DOI:10.1016/j.jhydrol.2012.01.011.

# Ref3: Tang, G., Clark, M. P., & Papalexiou, S. M. (2021).  
# SC-earth: a station-based serially complete earth dataset from 1950 to 2019. 
# Journal of Climate, 34(16), 6493-6511.
# DOI: 10.1175/JCLI-D-21-0067.1.


# 'obs' : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim' : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 's'   : scaling factors.

# 'Result': Kling-Gupta Efficiency between 'sim' and 'obs'

#' @rdname GOF
#' @export
KGE <- function(sim, obs, ...) UseMethod("KGE")

#' @rdname GOF
#' @importFrom stats sd
#' @export
KGE.default <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                        method=c("2009", "2012", "2021"), out.type=c("single", "full"), 
                        fun=NULL, ...,
                        epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                        epsilon.value=NA) { 
  
  # If the user provided a value for 's'
  if (!identical(s, c(1,1,1)) )  {
    if ( length(s) != 3 ) stop("Invalid argument: lenght(s) must be equal to 3 !")
    if ( sum(s) != 1 )    stop("Invalid argument: sum(s) must be equal to 1.0 !")
  } # IF end
  
  method   <- match.arg(method)
  out.type <- match.arg(out.type)  
  
  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
  ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")      
  
  vi <- valindex(sim, obs)
  
  if (length(vi) > 0) {
    
    obs <- as.numeric(obs[vi])
    sim <- as.numeric(sim[vi])
    
    if (!is.null(fun)) {
      fun1 <- match.fun(fun)
      new  <- preproc(sim=sim, obs=obs, fun=fun1, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
      sim  <- new[["sim"]]
      obs  <- new[["obs"]]
    } # IF end
    
    # Mean values
    mean.sim <- mean(sim, na.rm=na.rm)
    mean.obs <- mean(obs, na.rm=na.rm)
    
    # Standard deviations
    sigma.sim <- sd(sim, na.rm=na.rm)
    sigma.obs <- sd(obs, na.rm=na.rm)
    
    # Pearson product-moment correlation coefficient
    r     <- rPearson(sim, obs)
    
    # Alpha is a measure of relative variability between simulated and observed values (See Ref1)
    Alpha <- sigma.sim / sigma.obs
    
    # Beta is the ratio between the mean of the simulated values to the mean of observations
    Beta <- mean.sim / mean.obs
    
    # Beta.2021 is the bias term proposed by Tang et al. (2021) to avoid the 
    # anomalously negative KE or KGE' values when the mean value is close to zero 
    Beta.2021 <- (mean.sim - mean.obs) / sigma.obs
    
    # CV.sim is the coefficient of variation of the simulated values [dimensionless]
    # CV.obs is the coefficient of variation of the observations [dimensionless]
    CV.sim <- sigma.sim / mean.sim
    CV.obs <- sigma.obs / mean.obs
    
    # Gamma is the variability ratio, which is used instead of Alpha (See Ref2)
    Gamma <- CV.sim / CV.obs
    
    # Variability ratio depending on 'method'
    if (method=="2012") {
      br     <- Beta
      br.stg <- "Beta"
      vr     <- Gamma
      vr.stg <- "Gamma"
    } else if (method=="2009") {
      br     <- Beta
      br.stg <- "Beta"
      vr     <- Alpha
      vr.stg <- "Alpha"
    } else if (method=="2021") {
      br     <- Beta.2021
      br.stg <- "Beta.2021"
      vr     <- Alpha
      vr.stg <- "Alpha"
    } # ELSE end
    
    # KGE Computation
    if ( (mean.obs != 0) | (sigma.obs != 0) ) {
      if ( (method=="2009") | (method=="2012") ) {
        KGE <- 1 - sqrt( (s[1]*(r-1))^2 + (s[2]*(vr-1))^2 + (s[3]*(Beta-1))^2 )
      } else KGE <- 1 - sqrt( (s[1]*(r-1))^2 + (s[2]*(vr-1))^2 + (s[3]*(Beta.2021))^2 )
    } else {
      if ( mean.obs != 0)  warning("Warning: 'mean(obs)==0'. Beta = Inf")
      if ( sigma.obs != 0) warning("Warning: 'sd(obs)==0'. ", vr.stg, " = Inf")
      KGE <- NA
    } # ELSE end  
    
  } else {
    r    <- NA
    Beta <- NA
    vr   <- NA
    br   <- NA
    if (method=="2012") {
      br.stg <- "Beta"
      vr.stg <- "Gamma"
    } else if (method=="2009") {
      br.stg <- "Beta"
      vr.stg <- "Alpha" 
    } else {
      br.stg <- "Beta.2021"
      vr.stg <- "Alpha" 
    } # ELSE end
    KGE <- NA
    warning("There are no pairs of 'sim' and 'obs' without missing values !")
  } # ELSE end
  
  if (out.type=="single") {
    out <- KGE
  } else {
    out <- list(KGE.value=KGE, KGE.elements=c(r, br, vr))
    names(out[[2]]) <- c("r", br.stg, vr.stg)
  } # ELSE end    
  
  return(out)
  
} # 'KGE.default' end

# NSE --------------------------------------------------------------------------------------------------------------------

# Nash-Sutcliffe efficiencies (Nash and Sutcliffe, 1970) range from -Inf to 1. 
# An efficiency of 1 (NSE = 1) corresponds to a perfect match of modeled to the observed data. 
# An efficiency of 0 (NSE = 0) indicates that the model predictions are as accurate
# as the mean of the observed data, whereas 
# an efficiency less than zero (-Inf < NSE < 0) occurs when the observed mean is a better predictor than the model.
# Essentially, the closer the model efficiency is to 1, the more accurate the model is.  

# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Nash-sutcliffe Efficiency between 'sim' and 'obs'

#' @rdname GOF
#' @export
NSE <-function(sim, obs, ...) UseMethod("NSE")

#' @rdname GOF
#' @export
NSE.default <- function (sim, obs, na.rm=TRUE, fun=NULL, ..., 
                         epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                         epsilon.value=NA){ 
  
  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo", "xts"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo", "xts")))
  ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo', 'xts')")      
  
  epsilon.type <- match.arg(epsilon.type)  
  
  # index of those elements that are present both in 'sim' and 'obs' (NON- NA values)
  vi <- valindex(sim, obs)
  
  if (length(vi) > 0) {	 
    # Filtering 'obs' and 'sim', selecting only those pairs of elements 
    # that are present both in 'x' and 'y' (NON- NA values)
    obs <- obs[vi]
    sim <- sim[vi]
    
    if (!is.null(fun)) {
      fun1 <- match.fun(fun)
      new  <- preproc(sim=sim, obs=obs, fun=fun1, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
      sim  <- new[["sim"]]
      obs  <- new[["obs"]]
    } # IF end     
    
    denominator <- sum( (obs - mean(obs))^2 )
    
    if (denominator != 0) {      
      NS <- 1 - ( sum( (obs - sim)^2 ) / denominator )     
    } else {
      NS <- NA
      warning("'sum((obs - mean(obs))^2)=0' => it is not possible to compute 'NSE'")  
    } 
  } else {
    NS <- NA
    warning("There are no pairs of 'sim' and 'obs' without missing values !")
  } # ELSE end
  
  return(NS)
  
} # 'NSE' end

# PBIAS (Percent Bias) --------------------------------------------------------------------------------------------------------------------

# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Percent Bias between 'sim' and 'obs', 
#           when multiplied by 100, its units is percentage
# Ref: Yapo P. O., Gupta H. V., Sorooshian S., 1996. 
#      Automatic calibration of conceptual rainfall-runoff models: 
#      sensitivity to calibration data. Journal of Hydrology. v181 i1-4. 23-48.

#' @rdname GOF
#' @export
pbias <-function(sim, obs, ...) UseMethod("pbias")

#' @rdname GOF
#' @export
pbias.default <- function(sim, obs, na.rm=TRUE, dec=1, fun=NULL, ...,
                          epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                          epsilon.value=NA){
  
  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
  ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")
  
  epsilon.type <- match.arg(epsilon.type)  
  
  # index of those elements that are present both in 'sim' and 'obs' (NON- NA values)
  vi <- valindex(sim, obs)
  
  if (length(vi) > 0) {	 
    # Filtering 'obs' and 'sim', selecting only those pairs of elements 
    # that are present both in 'x' and 'y' (NON- NA values)
    obs <- obs[vi]
    sim <- sim[vi]
    
    if (!is.null(fun)) {
      fun1 <- match.fun(fun)
      new  <- preproc(sim=sim, obs=obs, fun=fun1, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
      sim  <- new[["sim"]]
      obs  <- new[["obs"]]
    } # IF end     
    
    # lenght of the data sets that will be ocnsidered for the ocmputations
    n <- length(obs)
    
    denominator <- sum( obs )
    
    if (denominator != 0) {      
      pbias <- 100 * ( sum( sim - obs ) / denominator )
      pbias <- round(pbias, dec)     
    } else {
      pbias <- NA
      warning("'sum((obs)=0' -> it is not possible to compute 'pbias' !")  
    } 
  } else {
    pbias <- NA
    warning("There are no pairs of 'sim' and 'obs' without missing values !")
  } # ELSE end
  
  return( pbias )
  
} # 'pbias.default' end

# MAE (Mean Absolute Error) --------------------------------------------------------------------------------------------------------------------

# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Mean Absolute Error between 'sim' and 'obs', in the same units of 'sim' and 'obs' 

#' @rdname GOF
#' @export
mae <-function(sim, obs, ...) UseMethod("mae")

#' @rdname GOF
#' @export
mae.default <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                        epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                        epsilon.value=NA){
  
  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
  ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")    
  
  if ( length(obs) != length(sim) ) 
    stop("Invalid argument: 'sim' & 'obs' doesn't have the same length !")
  
  epsilon.type <- match.arg(epsilon.type)  
  
  # index of those elements that are present both in 'sim' and 'obs' (NON- NA values)
  vi <- valindex(sim, obs)
  
  if (length(vi) > 0) {	 
    # Filtering 'obs' and 'sim', selecting only those pairs of elements 
    # that are present both in 'x' and 'y' (NON- NA values)
    obs <- obs[vi]
    sim <- sim[vi]
    
    if (!is.null(fun)) {
      fun1 <- match.fun(fun)
      new  <- preproc(sim=sim, obs=obs, fun=fun1, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
      sim  <- new[["sim"]]
      obs  <- new[["obs"]]
    } # IF end        
    
    mae <- mean( abs(sim - obs), na.rm = TRUE) 
    
  } else {
    mae <- NA
    warning("There are no pairs of 'sim' and 'obs' without missing values !")
  } # ELSE end 
  
  return(mae)
  
} # 'mae.default' end

# VE (Volumetric Efficiency) --------------------------------------------------------------------------------------------------------------------

# Reference: Criss, R. E. and Winston, W. E. (2008),
#            Do Nash values have value? Discussion and alternate proposals.
#            Hydrological Processes, 22: 2723-2725. doi: 10.1002/hyp.7072
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Mean Absolute Error between 'sim' and 'obs', in the same units of 'sim' and 'obs' 

#' @rdname GOF
#' @export
VE <-function(sim, obs, ...) UseMethod("VE")

#' @rdname GOF
#' @export
VE.default <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                       epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                       epsilon.value=NA){
  
  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo", "xts"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo", "xts")))
  ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo', 'xts')")    
  
  if ( length(obs) != length(sim) ) 
    stop("Invalid argument: 'sim' & 'obs' doesn't have the same length !")
  
  epsilon.type <- match.arg(epsilon.type)  
  
  # index of those elements that are present both in 'sim' and 'obs' (NON- NA values)
  vi <- valindex(sim, obs)
  
  if (length(vi) > 0) {	 
    # Filtering 'obs' and 'sim', selecting only those pairs of elements 
    # that are present both in 'x' and 'y' (NON- NA values)
    obs <- obs[vi]
    sim <- sim[vi]
    
    if (!is.null(fun)) {
      fun1 <- match.fun(fun)
      new  <- preproc(sim=sim, obs=obs, fun=fun1, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
      sim  <- new[["sim"]]
      obs  <- new[["obs"]]
    } # IF end  
    
    denominator <- sum(obs, na.rm=na.rm)
    
    if (denominator != 0) {      
      ve <- 1 - ( sum( abs(sim-obs) ) / denominator )     
    } else {
      ve <- NA
      warning("'sum((obs)=0' => it is not possible to compute 'VE' !")  
    } 
  } else {
    ve <- NA
    warning("There are no pairs of 'sim' and 'obs' without missing values !")
  } # ELSE end
  
  return(ve)      
  
} # 'VE.default' end


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   Methods extending generics in package hydroGOF
#
#     - NSE method
#     - pbias method
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



#' Nash-Sutcliffe Efficiency
#'
#' Nash-Sutcliffe Efficiency calculation for imported HYPE outputs with single variables for several catchments, i.e. time and
#' map files, optionally multiple model run iterations combined.
#'
#' @param sim \code{\link{HypeSingleVar}} array with simulated variable (one or several iterations).
#' @param obs \code{\link{HypeSingleVar}} array with observed variable, (one iteration). If several iterations are present
#' in the array, only the first will be used.
#' @param na.rm Logical. If \code{TRUE}, incomplete sim-obs pairs will be removed prior to NSE computation.
#' @param progbar Logical, if \code{TRUE} progress bars will be printed for main computational steps.
#' @param ... ignored
#' 
#' @return 
#' \code{NSE.HypeSingleVar} returns a 2-dimensional array of NSE performances for all SUBIDs and model iterations provided in 
#' argument \code{sim}, with values in the same order 
#' as the second and third dimension in \code{sim}, i.e. \code{[subid, iteration]}.
#' 
#' @examples 
#' # Create dummy data, discharge observations with added white noise as model simulations
#' te1 <- ReadObs(filename = system.file("demo_model", "Qobs.txt", package = "HYPEtools"))
#' te1 <- HypeSingleVar(x = array(data = unlist(te1[, -1]) + 
#'                                       runif(n = nrow(te1), min = -.5, max = .5), 
#'                                dim = c(nrow(te1), ncol(te1) - 1, 1), 
#'                                dimnames = list(rownames(te1), colnames(te1)[-1])), 
#'                      datetime = te1$DATE, subid = obsid(te1), hype.var = "cout")
#' te2 <- ReadObs(filename = system.file("demo_model", "Qobs.txt", package = "HYPEtools"))
#' te2 <- HypeSingleVar(x = array(data = unlist(te2[, -1]), 
#'                                dim = c(nrow(te2), ncol(te2) - 1, 1), 
#'                                dimnames = list(rownames(te2), colnames(te2)[-1])), 
#'                      datetime = te2$DATE, subid = obsid(te2), hype.var = "rout")
#' # Nash-Sutcliffe Efficiency
#' NSE(sim = te1, obs = te2, progbar = FALSE)
#' 
#' 
#' 
#' 
#' @importFrom pbapply pblapply
#' @export

NSE.HypeSingleVar <- function(sim, obs, na.rm = TRUE, progbar = TRUE, ...) { 
  
  # Check that 'sim' and 'obs' have the same dimensions
  if (all.equal(dim(sim)[1:2], dim(obs)[1:2]) != TRUE)
    stop(paste0("Invalid argument: dim(sim)[1:2] != dim(obs)[1:2] ( [", paste(dim(sim)[1:2], collapse=", "), 
                "] != [", paste(dim(obs)[1:2], collapse=", "), "] )"))
  
  ## internal variables used in (pb)l/sapply below
  # dimensions of HypeSingleVar array
  dm <- dim(sim)
  # sequence along number of time series in simulation array, to apply over
  dim.seq <- seq(dm[2] * dm[3])
  # 2nd dim indices in correct order, corresponding to time series sequence above
  dim.y <- rep(1:dm[2], times = dm[3])
  # 3rd dim indices in correct order, corresponding to time series sequence above
  dim.z <- rep(1:dm[3], each = dm[2])
  
  # internal function to split HypeSingleVar array into a list of time series, used in (pb)lapply below
  array2list <- function(x, y, z, a) {as.numeric(a[, y[x], z[x]])}
  
  # calculate NSEs, with conditional verbosity
  if (progbar) {
    cat("Preparing 'sim'.\n")
    s <- pblapply(dim.seq, array2list, y = dim.y, z = dim.z, a = sim)
    cat("Preparing 'obs'.\n")
    o <- pblapply(dim.seq[1:dm[2]], array2list, y = dim.y, z = dim.z, a = obs)
    cat("Calculating NSE.\n")
    nse <- array(pbsapply(dim.seq, 
                          FUN = function(x, y, s, o, nr) {NSE.default(sim = s[[x]], obs = o[[y[x]]], nr = na.rm)}, 
                          y = dim.y,
                          s = s, 
                          o = o, 
                          nr = na.rm), 
                 dim = dm[2:3])  
  } else {
    nse <- array(sapply(dim.seq, 
                        FUN = function(x, y, s, o, nr) {NSE.default(sim = s[[x]], obs = o[[y[x]]], nr = na.rm)}, 
                        y = dim.y,
                        s = lapply(dim.seq, array2list, y = dim.y, z = dim.z, a = sim), 
                        o = lapply(dim.seq, array2list, y = dim.y, z = dim.z, a = obs), 
                        nr = na.rm), 
                 dim = dm[2:3])  
  }
  
  # return NSEs, array with 2nd and 3rd dimension extent of input array
  return(nse)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' Percent bias
#' 
#' Percent bias (PBIAS) calculation for imported HYPE outputs with single variables for several catchments, i.e. time and 
#' map files, optionally multiple model runs combined.
#' 
#' @param sim \code{\link{HypeSingleVar}} array with simulated variable (one or several iterations).
#' @param obs \code{\link{HypeSingleVar}} array with observed variable, (one iteration). If several iterations are present
#' in the array, only the first will be used.
#' @param na.rm Logical. If \code{TRUE}, incomplete sim-obs pairs will be removed prior to PBIAS computation.
#' @param progbar Logical. If \code{TRUE}, progress bars will be printed for main computational steps.
#' @param ... ignored
#' 
#' @return 
#' \code{pbias.HypeSingleVar} returns a 2-dimensional array of NSE performances for all SUBIDs and model iterations provided in 
#' argument \code{sim}, with values in the same order 
#' as the second and third dimension in \code{sim}, i.e. \code{[subid, iteration]}.
#' 
#' @examples 
#' # Create dummy data, discharge observations with added white noise as model simulations
#' te1 <- ReadObs(filename = system.file("demo_model", "Qobs.txt", package = "HYPEtools"))
#' te1 <- HypeSingleVar(x = array(data = unlist(te1[, -1]) + 
#'                                runif(n = nrow(te1), min = -.5, max = .5), 
#'                                dim = c(nrow(te1), ncol(te1) - 1, 1), 
#'                                dimnames = list(rownames(te1), colnames(te1)[-1])), 
#'                      datetime = te1$DATE, subid = obsid(te1), hype.var = "cout")
#' te2 <- ReadObs(filename = system.file("demo_model", "Qobs.txt", package = "HYPEtools"))
#' te2 <- HypeSingleVar(x = array(data = unlist(te2[, -1]), 
#'                                dim = c(nrow(te2), ncol(te2) - 1, 1), 
#'                                dimnames = list(rownames(te2), colnames(te2)[-1])), 
#'                      datetime = te2$DATE, subid = obsid(te2), hype.var = "rout")
#' # Percentage bias
#' pbias(sim = te1, obs = te2, progbar = FALSE)
#' 
#' 
#' 
#' @importFrom pbapply pblapply
#' @export


pbias.HypeSingleVar <- function(sim, obs, na.rm = TRUE, progbar = TRUE, ...){ 
  
  # Check that 'sim' and 'obs' have the same dimensions
  if (all.equal(dim(sim)[1:2], dim(obs)[1:2]) != TRUE)
    stop(paste0("Invalid argument: dim(sim)[1:2] != dim(obs)[1:2] ( [", paste(dim(sim)[1:2], collapse=", "), 
                "] != [", paste(dim(obs)[1:2], collapse=", "), "] )"))
  
  ## internal variables used in (pb)l/sapply below
  # dimensions of HypeSingleVar array
  dm <- dim(sim)
  # sequence along number of time series in array, to apply over
  dim.seq <- seq(dm[2] * dm[3])
  # 2nd dim indices in correct order, corresponding to time series sequence above
  dim.y <- rep(1:dm[2], times = dm[3])
  # 3rd dim indices in correct order, corresponding to time series sequence above
  dim.z <- rep(1:dm[3], each = dm[2])
  
  # internal function to split HypeSingleVar array into a list of time series, used in (pb)lapply below
  array2list <- function(x, y, z, a) {as.numeric(a[, y[x], z[x]])}
  
  # calculate PBIAS, with conditional verbosity
  if (progbar) {
    cat("Preparing 'sim'.\n")
    s <- pblapply(dim.seq, array2list, y = dim.y, z = dim.z, a = sim)
    cat("Preparing 'obs'.\n")
    o <- pblapply(dim.seq[1:dm[2]], array2list, y = dim.y, z = dim.z, a = obs)
    cat("Calculating PBIAS.\n")
    pb <- array(pbsapply(dim.seq, 
                         FUN = function(x, y, s, o, nr) {pbias.default(sim = s[[x]], obs = o[[y[x]]], nr = na.rm)}, 
                         y = dim.y,
                         s = s, 
                         o = o, 
                         nr = na.rm), 
                dim = dm[2:3])  
  } else {
    pb <- array(sapply(dim.seq, 
                       FUN = function(x, y, s, o, nr) {pbias.default(sim = s[[x]], obs = o[[y[x]]], nr = na.rm)}, 
                       y = dim.y,
                       s = lapply(dim.seq, array2list, y = dim.y, z = dim.z, a = sim), 
                       o = lapply(dim.seq, array2list, y = dim.y, z = dim.z, a = obs), 
                       nr = na.rm), 
                dim = dm[2:3])  
  }
  
  # return PBIASs, array with 2nd and 3rd dimension extent of input array
  return(pb)
}
