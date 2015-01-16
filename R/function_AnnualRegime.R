
#' @export
#' @title
#' Calculate annual regimes
#'
#' @description
#' Calculate annual regimes based on long-term time series, typically imported HYPE basin output and time output result files.
#'
#' @param x A dataframe with column-wise time series and date-times in \code{\link{POSIXct}} format, typically an imported basin 
#' or time output file from HYPE.
#'
#' @details
#' \code{AnnualRegime} .
#' 
#' @return 
#' \code{AnnualRegime} returns a list. 
#' 
#' @examples
#' \dontrun{AnnualRegime(x = mybasinoutput)}

AnnualRegime <- function(x ) {
  # 
  # order results according to a user-requested starting month to reflect the hydrological year rather than the calender year
  # calculate average year daily statistics, for echam and all time slices in ggplottable format
  te1 <- tapply(tTNflux_e1[,2], format(tTNflux_e1[,1], format="%m-%d"), mean, na.rm=T)
  te2 <- tapply(tTNflux_e1[,2], format(tTNflux_e1[,1], format="%m-%d"), min, na.rm=T)
  te3 <- tapply(tTNflux_e1[,2], format(tTNflux_e1[,1], format="%m-%d"), max, na.rm=T)
  ggdata_e1 <- data.frame(julian_day=c(1:59,NA,60:365), mean=as.numeric(te1), min=as.numeric(te2), max=as.numeric(te3), time_slice = "1981-2010")
  ggdata_e1 <- ggdata_e1[-60,]    # remove leap year day
  
}
  




## DEBUG
#x <- ReadBasinOutput("//winfs-proj/data/proj/Fouh/Sweden/S-HYPE/Projekt/cleo/WP_3/2014-04_SHYPE_combined_scenarios/hadley/BUS/period1/res_ts/0013478.txt")
#gd <- ReadGeoData("//winfs-proj/data/proj/Fouh/Sweden/S-HYPE/Projekt/cleo/WP_3/2014-04_SHYPE_combined_scenarios/hadley/BUS/period1/GeoData.txt")
#summary(x)
