
#' @export
#' 
#' @import sp
#' 
#' @title
#' Map regional irrigation source connection as spatial lines
#'
#' @description
#' This function creates a SpatialLinesDataFrame object which visualizes regional irrigation source connections between HYPE subcatchments.
#'
#' @param data Dataframe with two columns, first column containing SUBIDs and 
#' second column containing model results. See details.
#' 
#' @details
#' none yet
#' 
#' @note
#' none yet
#' 
#' @return 
#' \code{} returns 
#' 
#' @examples
#' \dontrun{}

MapRegionalSources <- function (data, map, map.subid.column = 1) {
  
  # input argument checks
  stopifnot(is.data.frame(data), class(map)=="SpatialPointsDataFrame")
  
  
  # column with target and regional source SUBIDs
  col.subid <- which(toupper(names(data)) == "SUBID")
  col.regsrcid <- which(toupper(names(data)) == "REGSRCID")
  
  # get row indices of regionally connected basins
  row.rcb <- which(data[, col.regsrcid] > 0)
  
  # select data for mapping
  rcb <- data[row.rcb, c(col.subid, col.regsrcid)]
  
  # update row names, necessary for connection to map data below
  rownames(rcb) <- 1:nrow(rcb)
  
  # add a column to hold connection lengths
  rcb <- data.frame(rcb, "Length_km" = 0)
  
  # 
  
  SpatialLines(, proj4string = map@proj4string)
  
  all.lines<-list()
  for(i in 1:nrow(rcb)) { #i<-2
    sm<-match(regs.data[i,"SUBID"],sca[,"SUBID"])
    rm<-match(regs.data[i,"regsrcid"],sca[,"SUBID"])
    this.line<-Line(rbind(coordinates(subcent)[sm,],coordinates(subcent)[rm,]))
    this.lines<-Lines(list(this.line),ID=as.character(i))
    all.lines[length(all.lines)+1]<-this.lines
    regs.data[i,3]<-round(LineLength(this.line,longlat=T),3)  # longlat gives true GreatCircle distance in km, rounded to nearest meter
    
  }
  regs<-SpatialLines(all.lines,proj4string=CRS(proj4))
  regsdf<-SpatialLinesDataFrame(regs,regs.data)
  
}

# DEBUG
library(rgdal)
data <- ReadMgmtData(filename="//winfs-proj/data/proj/Fouh/Europe/E-HYPE/EHYPEv3.0/Data/RepurposedData/Irrigation/MgmtData_2014-12-03+GhA+Dniepr.txt")
map <- readOGR(dsn = "//winfs-proj/data/proj/Fouh/Europe/E-HYPE/EHYPEv3.0/Data/RepurposedData/WHIST/Current_shapefiles", layer = "SUBID_CenterPoints_TotalDomain_WGS84_20140428_degrees")
plot(map)
names(map@data)
