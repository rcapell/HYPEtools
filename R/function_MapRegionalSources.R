
#' @export
#' 
#' @import sp
#' 
#' @title
#' 
#'
#' @description
#'
#' @param data Dataframe with two columns, first column containing SUBIDs and 
#' second column containing model results. See details.
#' 
#' @details
#' 
#' 
#' @note
#' 
#' 
#' @return 
#' \code{} returns 
#' 
#' @examples
#' \dontrun{}

MapRegionalSources <- function () {
  md.regs<-which(md[,"regsrcid"]>0)
  regs.data<-md[md.regs,c("SUBID","regsrcid")]
  rownames(regs.data)<-1:nrow(regs.data)
  regs.data[,3]<-0; colnames(regs.data)[3]<-"Length_km"
  tail(regs.data)
  
  all.lines<-list()
  for(i in 1:nrow(regs.data)) { #i<-2
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
