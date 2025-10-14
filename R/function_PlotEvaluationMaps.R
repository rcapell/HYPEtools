#'
#' Function for plotting up to two HYPE simulation results in maps.
#'
#' Draws maps for selected HYPE variables and performance metrics with pretty scale discretizations and colours.
#'
#' @param tempOutDirectory Temporary directory for intermediate data e.g., geo-spatial objects. It is mandatory character string that is also used for figures if the figures directory is not specified. To save time, intermediate data is saved the first time functions are called and loaded from disk during subsequent calls (e.g., to visualize different performance metrics with the same variable). 
#' @param figuresDirectory The figures output directory, an optional character string. If not set, figures are saved to the temporary directory with the intermediate objects.
#' @param refSubass A mandatory reference (default) HYPE simulation subass object imported with the \code{ReadSubass()} function.  
#' @param simSubass An optional simulation subass object imported with the \code{ReadSubass()} function. It is used for comparison to the reference simulation or for computing relative differences between simulation performances.
#' @param subBasins A mandatory sf object containing the sub-basin geometries. 
#' @param geoData A mandatory geodata object imported by reading the GeoData.txt file with the \code{ReadGeoData()} function. The outlets of the gauged sub-basins in the subass files are also read from this file and converted into geo-spatial objects for visualization. 
#' @param simInfo A mandatory object imported from the info.txt with the function \code{ReadInfo()}. It provides e.g., start/end dates, aggregation periods, etc. 
#' @param var.info The information pertaining to the simulation variable, extracted from the header line of the subass file. It is a mandatory character string of length 2 containing the information \code{c('obs vs sim', 'units')}
#' @param streamShapeFile An optional character string for the path to the shape file containing the stream network of the model domain, which  should be projected to the WGS84 system for consistency. If the stream network is desired without specifying the shape file, the 50m-resolution map from Natural Earth  is used (requires network connection). The layer may not provide the desired level of detail however. 
#' @param criterion An optional valid name of a HYPE subass evaluation criterion. 'NSE', 'KGE', 'CC', 'MAE', 'RE(%)', and 'RMSE' are currently implemented. NSE is plotted by default.
#' @param visualization The mandatory type of visualization from "relative.difference", "best.simulation" and "comparison". The first two options produce a single map while the third produces two side-by-side maps. "Comparison" and "relative difference" require two simulations.
#' @param data.presentation The mandatory data presentation mode on maps, one of "polygons", "outlets" or "centroids". "Polygons" are useful for spatially distributed data e.g., evaporation. "Outlets" and "centroids" are useful for point observations, e.g., discharge and sediment plotted at the sub-basin outlets and centroids, respectively. 
#' @param evaluation.variable A mandatory character string for the variable name being analysed, e.g., "Discharge", "Actual ET", "Snow Water Equivalent", etc.
#' @param simulation.names A vector of two (at least one) character strings with the "names" of the simulations e.g., model versions, used as titles on the maps. The first item mandatory. 
#' @param marker.size A numerical value for the marker size, optional.
#' @param show.borders Logical choice to show political borders (default is FALSE). If requested, the 110m-resolution map will be downloaded from the Natural Earth portal, which requires an internet connection.
#' @param show.streams Logical choice to show river network on the map (default is FALSE). 
#' @param histogram.fill A character string of length two for the histogram fill colours. In case of a single map, the first colour (mandatory) is used.
#' @param used.colours An optional vector of colours for the maps. 
#' @param domain.name The name of the domain. This character string is mandatory. The global model is called "wwhype".
#' @param nsign.figures An optional digit specifying the number of significant digits used to display simulation summary statistics. Default is 3.
#' @param file.format An optional string specifying the figure output format. Options are "pdf" (default) and "png".
#' @param gauge.list A subset of gauges to be plotted (must exist in the subass file), if some gauges in the subass file are to be excluded.
#' 
#' @details
#' \code{PlotEvaluationMaps} visualises model performances from one or two subass files. The user should provide the files as well as the subbasin polygons, the geodata and info.txt files. Relative difference in performance is computed as the (new - ref)/ref simulation *100%. The best simulation is the new in case two are provided or the reference if only one is provided.
#' 
#' @return
#' \code{PlotEvaluationMaps} does not return any objects.
#' @examples 
#' \dontrun{
#' if (interactive()) {
#'   PlotEvaluationMaps(
#'     tempOutDirectory = system.file("demo_model", "inst", "figures",
#'                                    package = "HYPEtools"
#'     ),
#'     refSubass = ReadSubass(system.file("demo_model", "results", "subass1.txt",
#'                                        package = "HYPEtools"
#'     )),
#'     subBasins = system.file("demo_model", "gis", "Nytorp_map.gpkg",
#'                             package = "HYPEtools"
#'     ),
#'     geoData = system.file("demo_model", "GeoData.txt",
#'                           package = "HYPEtools"
#'     ),
#'     simInfo = system.file("demo_model", "info.txt",
#'                           package = "HYPEtools"
#'    ),
#'     criterion = c("KGE"),
#'     visualization = c("best.simulation"),
#'     data.presentation = c("outlets"),
#'     evaluation.variable = c("discharge"),
#'     simulation.names = c("Nytorp"),
#'     show.borders = FALSE,
#'     show.streams = FALSE,
#'     domain.name = "Nytorp",
#'     nsign.figures = 3,
#'     file.format = "pdf"
#'   )
#' }
#' }
#' 
#' @importFrom sf st_as_sf st_crs st_transform as_Spatial st_geometry st_read st_is_valid st_make_valid st_union st_crop st_intersection st_is_empty st_geometry st_centroid 
#' @importFrom graphics par legend strwidth text mtext axis barplot layout
#' @importFrom grDevices pdf
#' @importFrom terra fillHoles vect is.valid makeValid 
#' @importFrom rnaturalearth ne_download ne_countries
#' 
#' @export
# Exported function
PlotEvaluationMaps <- function(figuresDirectory=NULL, tempOutDirectory, refSubass, 
                               simSubass, subBasins, 
                               geoData, 
                               simInfo,
                               var.info,
                               #politicalBorderShapeFile=NULL, 
                               streamShapeFile=NULL, 
                               criterion=c('NSE', 'KGE', 'CC', 'MAE', 'RE(%)', 'RMSE','KGESD','KGEM'),
                               visualization=c("best.simulation", "relative.difference", "comparison"),
                               data.presentation=c("outlets", "polygons", "centroids"), 
                               evaluation.variable, 
                               simulation.names=NULL, 
                               marker.size=NULL, 
                               show.borders=FALSE, 
                               show.streams=FALSE,
                               histogram.fill=NULL,
                               used.colours=NULL,
                               domain.name,
                               gauge.list=NULL,
                               nsign.figures=3,
                               file.format="pdf"
)
{
  # Check/Load Dependencies - do this here so that these packages are not required for the base HYPEtools installation
  if (!all(
    requireNamespace("sf",            quietly=TRUE),
    requireNamespace("terra",         quietly=TRUE),
    requireNamespace("rnaturalearth", quietly=TRUE)
  )) {
    # Warn that a dependency is not installed
    stop('To use the PlotEvaluationMaps features, please ensure that the following packages are installed: c("sf", "terra", "rnaturalearth")', call.=FALSE)
  }
  ### LOCAL FUNCTIONS ###
  MatrixToSf <- function(m., epsg_code=4326, lon_name, lat_name)
  {
    M=sf::st_as_sf(m., coords = c(lon_name, lat_name))
    sf::st_crs(M) = epsg_code
    M=sf::st_transform(M, crs=sf::st_crs(paste0("EPSG:", epsg_code)))
    return(M)
  }
  #
  PrepareGeospatialData <- function(stream_shape=NULL, border_bool=FALSE, 
                                    stream_bool=FALSE, gdata, 
                                    spoly, odir, domain.,  
                                    gauges_vector, vcode.)
  {
    #browser()
    if(!dir.exists(odir)){
      dir.create(odir, recursive=FALSE, mode="0777", showWarnings = F)
    }
    
    #
    fg.=file.path(odir, paste(paste(domain., collapse="-"), vcode., "geospatial_data.RDATA", sep="_"))
    
    if(file.exists(fg.)){
      load(fg.)
    } else {
      #SUB BASINS 
      f.sub=file.path(odir, paste(paste(domain., collapse="-"), vcode., "subbasin_data.RDATA", sep="_"))
      if(file.exists(f.sub)){
        load(f.sub)
      } else {
        sub.x=terra::vect(spoly)
        #retain just the SUBID and geometry columns
        sub.x=sub.x["SUBID"]
        if(domain.=="wwhype"){
          sub.x = st_as_sf(sub.x)
          sub.o =  NA
        } else {
          if(any(!terra::is.valid(sub.x))) sub.x=terra::makeValid(sub.x)
          sub.o=terra::aggregate(sub.x)
          sub.o=terra::fillHoles(sub.o, inverse=FALSE)
          sub.x=st_as_sf(sub.x)
          sub.o=st_as_sf(sub.o)
          
        }
        save(sub.x, sub.o, file=f.sub)
      }
      
      
      #COUNTRY BORDERS
      #browser()
      f.bod=file.path(odir, paste(paste(domain., collapse="-"), vcode., "border_data.RDATA", sep="_"))
      if(file.exists(f.bod)){
        load(f.bod)
      } else {
        #download global borders from Natural Earth portal; crop it to the domain
        s.=terra::vect(ne_countries(scale=50L, type="countries"))
        #check for invalid geometries
        if(any(!terra::is.valid(s.))){
          s.=terra::makeValid(s.)
        }
        #dissolve internal borders
        x.oln=terra::aggregate(s.)
        x.oln=terra::fillHoles(x.oln, inverse=FALSE)
        #
        if(domain. == "wwhype"){
          outline=st_as_sf(x.oln)
          borders=NA
        } else {
          #browser()
          borders=st_as_sf(terra::intersect(vect(sub.o), s.))
          outline=sub.o
        }
        save(borders, outline, file=f.bod)
      }
      #STREAM NETWORK
      f.str=file.path(odir, paste(paste(domain., collapse="-"), "stream_data.RDATA", sep="_"))
      if(file.exists(f.str)){
        load(f.str)
      } else {
        str. <- ne_download(scale = 50, type = 'rivers_lake_centerlines', category = 'physical')
        if(domain. != "wwhype"){
          str. = st_as_sf(terra::intersect(vect(str.), vect(outline)))
        }
        save(str., file=f.str)
      }
      #GAUGING STATIONS geodata pour points
      f.gag = file.path(odir, paste(paste(domain., collapse="-"), vcode., "gauge_data.RDATA", sep="_"))
      if(file.exists(f.gag)){
        load(f.gag)
      } else {
        v. = gdata[match(gauges_vector, gdata$SUBID), c("SUBID", "POURX", "POURY")]
        gag. = MatrixToSf(m=v., lon_name = "POURX", lat_name = "POURY")
        save(gag., file=f.gag)
      }
      
      
      #
      #browser()
      y.=list("borders" = borders,
              "outline" = outline,
              "subids"  = sub.x,
              #"domain"  = sub.o,
              "rivers"  = str.,
              "gauges"  = gag.)
      save(y., file=fg.)
    } 
    return(y.)
  } 
  #
  CondenseNumeric    <- function(x)
  {
    ifelse(nchar(x)>7, format(x, scientific=TRUE, digits = 2), format(x, scientific=FALSE))
  }
  #
  GetSimulationDetails   <- function(info., dt., ov.)
  {
    #obtain details about the simulation: begin/end dates, aggregation period and temporal resolution from the info file
    #browser()
    txt=paste(trimws(unlist(strsplit(x=dt.[1], split=",", fixed=TRUE))), collapse=" vs ")
    units=trimws(dt.[2])
    
    if(grepl(x=txt, pattern="cout", fixed=FALSE)){
      z.="QOB"
      dt="point"
    } else if (grepl(x=txt, pattern="snow", fixed=FALSE)){
      z.="SWE"
      dt="spatial"
    } else if (grepl(x=txt, pattern="fsc", fixed=FALSE)){
      z.="FSC"
      dt="spatial"
    } else if (grepl(x=txt, pattern="evap", fixed=FALSE)){
      z.="AET"
      dt="spatial"
    } else if (grepl(x=txt, pattern="repo", fixed=FALSE)){
      z.="PET"
      dt="spatial"
    } else if (grepl(x=txt, pattern="ctsl", fixed=FALSE)){
      z.="tot susp load"
      dt="point"
    } else if (grepl(x=txt, pattern="ccss", fixed=FALSE)){
      z.="susp load"
      dt="point"
    } else if (grepl(x=txt, pattern="ccts", fixed=FALSE)){
      z.="tot susp sed"
      dt="point"
    } 
    #   
    x.=list()
    x.$cdate = as.Date(info.$cdate, format="%Y-%m-%d")
    x.$edate = as.Date(info.$edate, format="%Y-%m-%d")
    aa=info.$timeoutput$meanperiod
    if(aa==1){
      ts.="Daily"
    }else if (aa==2){
      ts.="Weekly"
    }else if (aa==3){
      ts.="Monthly"
    }else if (aa==4){
      ts.="Annual"
    }else if (aa==5){
      ts.="Sim Period Average"
    } else {
      ts.="Undefined"
    }
    x.$resolution = ts.
    x.$variable   = paste(toupper(ov.), paste0("[", units, "]"))
    x.$varcodes   = txt
    x.$varshort   = z.
    x.$data.type  = dt
    return(x.)
  }
  #
  GetScore <- function(sub., sco)
  {
    subsco=sub.[, c(1, which(colnames(sub.) %in% sco))]
    nas.idx=which(subsco[,2]=="NA" | subsco[,2]==-9999)
    if(length(nas.idx) >0) {
      subsco[nas.idx, 2] = NA
      
    }
    subsco[,2] = as.numeric(subsco[,2])
    return(subsco)
  }
  #
  RoundNumber   <- function(x, n)
  {
    if(x>1000 | x < -1000){
      return(formatC(x, digits=pmax(0, (n-2)), format="g"))
    } else {
      return(formatC(x, digits=n, format="f"))
    }
  }
  #
  ComputeDiff   <- function(sco1, sco2, type., opt.val)
  {
    # Compute relative difference
    #browser()
    rel.diff = (abs(sco2[,2]-opt.val)-abs(sco1[,2]-opt.val))*100/abs(sco1[,2]-opt.val)
    diff. = data.frame(sco1[,1], -rel.diff)
    colnames(diff.) = c("SUBID", "refdiff")
    return(diff.)
  }
  #
  GetColour      <- function(vals, mtype, obj.=NULL, palcols=NULL, val.sca)
  {
    color = list()
    vals=vals[,2]
    vals[vals < val.sca[1]] = val.sca[1]
    vals[vals < val.sca[length(vals)]] = val.sca[length(vals)]
    if(grepl(x=mtype, pattern='relative.difference')) {
      # Difference scale
      pal_col = colorRampPalette(palcols)(length(val.sca)-1)
      pal_val = val.sca
      color. = as.character(cut(vals, include.lowest = TRUE, breaks = pal_val, labels = pal_col))
    } else { 
      pal_col = colorRampPalette(palcols)(length(val.sca)-1)
      pal_val = val.sca
      color. = as.character(cut(c(vals), include.lowest = TRUE, breaks = pal_val, labels = pal_col))
    }
    #
    color[[1]] = pal_val
    color[[2]] = pal_col
    color[[3]] = color.
    return(color)
  }
  # 
  DrawColourBar <- function(x, z, col.border=NA, horiz=FALSE, plot.type, tit., scl.=NULL)
  {
    # This function plots a ramp palette
    LCOL=length(x)
    SEQ=seq(1,10,length.out=(LCOL+1))
    plot(1:10,1:10,type='n',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    if(!horiz) {for(i in 1:LCOL) rect(1,SEQ[i],10,SEQ[i+1],col=x[i],border=col.border)}
    if( horiz) {for(i in 1:LCOL) rect(SEQ[i],1,SEQ[i+1],10,col=x[i],border=col.border)}
    
    if(grepl(x=plot.type, pattern="relative.difference")){
      leg.txt="\nRel. Difference"
      tit. = toupper(paste(tit., "rel. diff."))
    }else if (grepl(x=plot.type, pattern="best.run")){
      leg.txt="\nBest sim"
    }else if (grepl(x=plot.type, pattern="performance")){
      leg.txt="\nPerformance"
    }
    
    text. = sapply(round(z,2), CondenseNumeric)
    labs. = formatC(as.numeric(text.[seq(1, length(text.), by=2)]), digits=1, format="f")
    locs. = seq(1, 10, length.out=length(z))
    locs. = locs.[seq(1, length(locs.), by=2)]
    
    if(horiz){
      mtext(2, at=5, text=sub('-','\n',tit.), cex=1.05, line=-0.5, font=2)
      mtext(3, at=locs., text=labs., line = 0)
    }else {
      mtext(3, at=5, text=sub('-','\n',tit.), cex=1.05, line=-0.5, font=2)
      mtext(4, at=locs., text=labs., line = 0)
    }
  }
  #
  GetScale      <- function(sco.1, sco.2, cut.)
  {
    if(names(sco.1) %in% c('NSE','KGE', 'CC')) {
      scale = c((cut.-0.0001), seq(cut., 1, by=0.2))
    } else if(names(sco.1) %in% c('RMSE','MAE')) {
      scale = c(0,1,5,10,15,25,35,50,75,cut.)
    } else if(names(sco.1) %in% c('KGESD','KGEM')) {
      scale = c(seq(0, cut.,by=0.2))
    } else if(names(sco.1) %in% c('RE(%)','RSDE(%)', "rel.diff")) {
      scale = c((-cut.-0.0001), seq(-100,-20,20), -10, 0, 10, seq(20, 100,20),(cut.+0.0001))
    }
    return(scale)
  }
  #
  PlotHistogram <- function(x1, x2, colbar, obj., coff., val.sca)
  {
    par(las=2, mai=c(0.0, 0.75, 0.2, 0.25), cex=1.1)
    categ1 = table( cut(x1[,1], include.lowest = TRUE, breaks = val.sca) )
    if(is.null(x2)){
      categ2=NULL
    } else {
      categ2 = table( cut(x2[,1], include.lowest = TRUE, breaks = val.sca) )
    }
    xx=rbind(categ1,categ2)
    ax=unlist(lapply(colnames(xx), function(str.){
      ch1=substr(str., 1, 1)
      ch2=substr(str., nchar(str.), nchar(str.))
      chr=sapply(unlist(strsplit(x=substr(str., 2, nchar(str.)-1), split=",", fixed=TRUE)),
                 function(f.){
                   if(as.numeric(f.) < -1000){
                     return(formatC(as.numeric(f.), digits=1, format="g"))
                   } else {
                     return(formatC(as.numeric(f.), digits=1, format="f"))
                   }
                 })  
      paste0(ch1, paste(chr, collapse=","), ch2)
    }))
    #
    if(grepl(x=names(x1), pattern="diff")){
      ax[1]=paste0("< ", formatC(-coff., format="f", digits=1))
      ax[length(ax)]=paste0("> ", formatC(coff., format="f", digits=1))
      txt.=paste(obj., "Rel. Diff. Histogram")
    } else {
      ax[1]=paste0("< ", formatC(coff., format="f", digits=1))
      txt.=paste(obj., "Histogram")
    }
    colnames(xx)=ax
    rm(ax)
    if(is.null(x2)){
      barplot(xx, cex.axis=0.75, cex.names=0.75, beside=TRUE, ylab='', col="gray50", border=NA, ylim=range(pretty(c(0, xx))))
    } else {
      barplot(xx, cex.axis=0.75, cex.names=0.75, beside=TRUE, ylab='', col=colbar, border=NA, ylim=range(pretty(c(0, xx))))
    }
    #
    #mtext(side = 3, text = txt., line=0, las=0, cex=1.1, font=2)
    mtext(side = 2, text = 'Number of catchments', line=2.75, las=0, cex=0.9)
    par(xpd=TRUE)
    par(xpd=FALSE)
  }
  #
  WriteStats    <- function(stats1, stats2, coltxt, colbar, critname, ctyp., sig.)
  {
    #if(grepl(x=ctyp., pattern="relative.difference")){
    #  txt.=paste(critname, "Rel. Diff. Statistics")
    #} else {
    txt.=paste("Summary Statistics")
    #}
    text(0.2, 0.9450, txt., adj = c(0,0.5), cex=1.1, font=2)
    
    #header text
    if(is.null(stats2)) colbar="gray30"
    text(0.05, 0.750, paste('Median :')       , adj = c(0,0.5), col=coltxt)
    text(0.05, 0.625, paste('Max :   ')       , adj = c(0,0.5), col=coltxt)
    text(0.05, 0.500, paste('Q75 :   ')       , adj = c(0,0.5), col=coltxt)
    text(0.05, 0.375, paste('Q25 :   ')       , adj = c(0,0.5), col=coltxt)
    text(0.05, 0.250, paste('Min :   ')       , adj = c(0,0.5), col=coltxt)
    #first column
    text(0.33, 0.750, paste(RoundNumber(stats1[3], n=sig.)) , adj = c(0,0.5), col=colbar[1])
    text(0.33, 0.625, paste(RoundNumber(stats1[5], n=sig.)) , adj = c(0,0.5), col=colbar[1])
    text(0.33, 0.500, paste(RoundNumber(stats1[4], n=sig.)) , adj = c(0,0.5), col=colbar[1])
    text(0.33, 0.375, paste(RoundNumber(stats1[2], n=sig.)) , adj = c(0,0.5), col=colbar[1])
    text(0.33, 0.250, paste(RoundNumber(stats1[1], n=sig.)) , adj = c(0,0.5), col=colbar[1])
    #second column if provided
    if(!is.null(stats2)){
      text(0.65, 0.75, paste(RoundNumber(stats2[3], n=sig.))      , adj = c(0,0.5), col=colbar[2])
      text(0.65, 0.625, paste(RoundNumber(stats2[5], n=sig.)) , adj = c(0,0.5), col=colbar[2])
      text(0.65, 0.500, paste(RoundNumber(stats2[4], n=sig.)) , adj = c(0,0.5), col=colbar[2])
      text(0.65, 0.375, paste(RoundNumber(stats2[2], n=sig.)) , adj = c(0,0.5), col=colbar[2])
      text(0.65, 0.250, paste(RoundNumber(stats2[1], n=sig.)) , adj = c(0,0.5), col=colbar[2])
    } 
    par(mar=c(1, 1, 1.5, 1)); box(); #par(mar=rep(1,4)) ; box()
  }
  #
  WriteSimInfo <- function(siminfo, coltxt)
  {
    text(0.20, 0.975, 'Simulation Details', adj = c(0,0.5), cex=1.1, font=2)
    text(0.05,0.75, paste("Start (cdate):",siminfo$cdate),          adj=c(0,0.5), col=coltxt) 
    text(0.05,0.60, paste("End (edate):  ", siminfo$edate),         adj=c(0,0.5), col=coltxt)
    # text(0.05,0.55, paste("Variable:        ", siminfo$varshort),   adj=c(0,0.5), col=coltxt)
    text(0.05,0.465, paste("Comparison: ", siminfo$varcodes),        adj=c(0,0.5), col=coltxt)
    text(0.05,0.345, paste("               ", "       (obs vs sim) "), adj=c(0,0.5), col=coltxt, font=3)
    text(0.05,0.20, paste("Time step:    ", siminfo$resolution),    adj=c(0,0.5), col=coltxt)
    par(mar=c(1, 1, 1.5, 1)) ; box() 
  }
  #
  DrawMaps  <- function(sim.info, diff., title, ctype., cex.=0.1, tcol, river.network, 
                        landcol, country.borders, show.data.as, geo.data, wcol, coff,
                        sco1=NULL, sco2=NULL, crit., rnames, cbar, stat1=NULL, 
                        stat2=NULL, pal.cols, dom., num.plots, scale.values,
                        stat.sign)
  {
    #browser()
    subids = diff.$SUBID
    gag.shp = geo.data$gauges
    # if(river.network){
    #   riv.shp = terra::intersect(terra::makeValid(vect(geo.data$outline)), 
    #                              terra::makeValid(vect(geo.data$rivers)))
    # } else {
    #   riv.shp=NULL
    # }
    riv.shp = terra::vect(geo.data$rivers)
    if(length(riv.shp)==0 || !river.network) riv.shp=NULL
    #geo.data$rivers
    sub.shp = geo.data$subids
    if(dom.=="wwhype"){
      map.bground=geo.data$outline
    } else if (!country.borders){
      map.bground=geo.data$domain
    } else {
      map.bground=geo.data$borders
    }
    
    idx_gag.shp <- match(subids, gag.shp$SUBID)
    # Color vectors
    color = GetColour(vals=diff.[, c(1,2)], mtype=ctype., obj.=crit., palcols=pal.cols, val.sca=scale.values)
    if(num.plots==2){
      color2 = GetColour(vals=diff.[, c(1,3)], mtype=ctype., obj.=crit., palcols=pal.cols, val.sca=scale.values)
    } 
    # Map colors
    if (landcol == "Dark") {
      ColMap <- c("gray78", NA, "gray90", wcol)
    } else if (landcol == "Light") {
      Li_blue <- rgb(red=206, green=224, blue=238,maxColorValue=255)
      Choco_trans <- rgb(red=139, green=69, blue=19, maxColorValue=255, alpha=102)
      ColMap <- c(Choco_trans, Li_blue, "black", Li_blue)
    }
    #
    #browser()
    par(mar=rep(0,4))
    plot.new() #left margin
    #map heading1
    plot.new() 
    ab=paste(unlist(strsplit(x=ctype., split=".", fixed=TRUE)), collapse=" ")
    aa=sim.info$variable
    mtext(paste(toupper(dom.) , aa, toupper(crit.), toupper(ab), collapse=" "), side=1, font=2, cex=1.75)
    #right margin
    plot.new() 
    #map heading2
    if(num.plots==1){ #leave blank
      plot.new()
    } else if (num.plots == 2){
      #left column
      plot.new()
      mtext(toupper(rnames[1]), col=cbar[1], font=2, side=1, cex=1.25)
      #right column
      plot.new()
      mtext(toupper(rnames[2]), col=cbar[2], font=2, side=1, cex=1.25)
    }
    # if(num.plots ==1){
    #   
    #   mtext(toupper(paste(dom., aa, collapse=" ")), side=1, font=2)  
    # } else {
    #   
    # }
    # #
    # plot.new()
    # if(num.plots==2){
    #   
    # }
    
    #maps
    par(mar=c(0.50, 0.1, 0.5, 0.1), cex=1.1)
    # Plot the country border if requested
    plot(st_geometry(map.bground), col=ColMap[3], border=ColMap[3], axes=FALSE) # political borders
    #Add stream network if requested
    if (river.network) {
      if(!is.null(riv.shp)){
        terra::plot(riv.shp, lwd=1.1, col=ColMap[4], add=TRUE)
      }
    }
    #browser()
    # Plot outlets (gauge locations), centoids or filled polygons
    if(show.data.as == "outlets"){
      plot((gag.shp), col=color[[3]], pch=16, cex=cex., add=TRUE)
      if(num.plots ==2){
        #plot the domain outline
        plot(st_geometry(map.bground), col=ColMap[3], border=ColMap[3], lwd=1.5, axes=FALSE)
        # Plot stream network if requested
        if (river.network) {
          if(!is.null(riv.shp)){
            terra::plot(riv.shp, lwd=1.1, col=ColMap[4], add=TRUE)
          }
        }
        plot((gag.shp), col=color2[[3]], pch=16, cex=cex., add=TRUE)
      }
      
    } else if (show.data.as == "polygons"){ 
      plot(st_geometry(sub.shp), col=color[[3]], border=color[[3]], add=TRUE)
      if(num.plots ==2){
        #plot the domain outline
        # plot(st_geometry(map.bground), col=ColMap[3], border=ColMap[3], axes=FALSE)
        # Plot stream network if requested
        # if (river.network) {
        #   plot((riv.shp), lwd=1.1, col=ColMap[4], add=TRUE)
        # }
        plot(st_geometry(sub.shp), col=color2[[3]], border=color2[[3]])
      }
      
    } else if (show.data.as == "centroids"){
      #browser()
      if(dom.=="wwhype"){
        plot(st_geometry(gag.shp), col=color[[3]], pch=16, cex=cex., add=TRUE)
      } else {
        idx.gag = which(geo.data$subids$SUBID %in% geo.data$gauges$SUBID)
        gag.ctr = st_centroid(geo.data$subids[idx.gag, ])
        plot(st_geometry(gag.ctr), col=color[[3]], pch=16, cex=cex., add=TRUE)
      }
      #
      if(num.plots ==2){
        plot(st_geometry(map.bground), col=ColMap[3], border=ColMap[3], axes=FALSE) # background map
        #Add stream network if requested
        if (river.network) {
          if(!is.null(riv.shp)){
            terra::plot(riv.shp, lwd=1.1, col=ColMap[4], add=TRUE)
          }
        }
        #centroid points
        if(dom. == "wwhype"){
          plot(st_geometry(gag.shp), col=color2[[3]], pch=16, cex=cex., add=TRUE)
        } else {
          plot(st_geometry(gag.ctr), col=color2[[3]], pch=16, cex=cex., add=TRUE)  
        }
        
      }
    }
    #
    plot.new()
    #The colour bar under the map(s)
    par(las=1, mai=c(0.025, 0.025, 0.1, 0.25), cex=1.1  ) #
    DrawColourBar(x=color[[2]], z=color[[1]], plot.type=ctype., tit.=crit., horiz=TRUE, scl.=scale.values)
    plot.new()
    #plot summary information under the colour bar
    #1. Write simulation information
    par(mar=c(0,0,0.4,0), cex=1)
    plot.new()
    WriteSimInfo(siminfo=sim.info, coltxt=tcol)
    #2. Plot histogram
    PlotHistogram(x1=sco1, x2=sco2, obj.=crit., colbar=cbar, coff.=coff, val.sca=scale.values)
    #3. Write score statistics
    par(mar=c(0,0,0,0), cex=1)
    plot.new()
    WriteStats(coltxt=tcol, colbar=cbar, stats1=stat1, stats2=stat2, 
               critname=crit., ctyp.=ctype., sig.=stat.sign)
  }
  ###############
  #browser()
  ###############
  criterion=match.arg(criterion, several.ok = FALSE)
  #if not specified, set figures directory to the temporary output directory
  if(is.null(figuresDirectory)) figuresDirectory = tempOutDirectory
  #
  optima. = list("NSE"=1, "KGE"=1, "CC"=1, "RE(%)"=0, "MAE"=0, "RMSE"=0, "KGESD"=0, "KGEM"=0)
  cutoff. = list("NSE"=-1, "KGE"=-1, "CC"=-1, "RE(%)"=100, "MAE"=100, "RMSE"=100, "KGESD"=2, "KGEM"=2)
  wat.col = "skyblue"
  col.txt = "gray20"
  if(is.null(histogram.fill)) col.bar = c('#1e90ff', "#fe6f5e")
  landcol.="Dark"
  #if(is.null(marker.size)) marker.size=1.0
  #
  tmp.col=c("#00FFFF", "#00BFFF", "#1F75FE", "#3457D5", "#3F00FF", "#0000CD", "#00008B",  "#070738")
  col.pal  = c("#8b0000", "#ff7518","#CD9F07", "#9C990F", "#6B9316", "#3A8D1E", "#1E7B2D", "#165C44", "#0F3D5C", "#071E73", "#00008B")
  col.sym = c(col.pal, rev(col.pal))
  col.div = c("#8b0000", "#ff7518", "#CD9F07", "#9C990F", "#6B9316", "#50C878", "#008000", "#013220",
              rev(tmp.col))
  col.sch = list("NSE"=col.pal, "KGE"=col.pal, "CC"=col.pal, "MAE"=rev(col.pal), 
                 "RMSE"=rev(col.pal), "KGESD"=col.div, "KGEM"=col.div,"RE(%)"=col.div)
  #assign colour pallette if not specified at call time
  if(is.null(used.colours)){
    if(grepl(x=visualization, pattern="relative.difference")){
      used.colours=col.div
    } else {
      used.colours=col.sch[[criterion]]
    }
  }
  #
  if(visualization %in% c("best.simulation", "relative.difference")){
    plot.number=1
  } else if (visualization %in% c("comparison")) {
    plot.number=2
  }
  # obtain the subbasin list for the reference simulation (or common to both in case of two)
  subass1=refSubass
  subvec1=subass1[, "SUBID"]
  subass2=NULL
  score2 = NULL
  sco2. = NULL
  quantiles2 = NULL
  if(plot.number==2 || visualization=="relative.difference"){
    subass2=simSubass
    subvec2=subass2[, "SUBID"]
    #
    subvec1=intersect(subvec1, subvec2)
    subass2=subass2[match(subvec1, subvec2), ]
  } 
  #
  subass1=subass1[match(subvec1, subass1$SUBID), ]
  #Get information about simulation
  #z.=GetVariableDetails(ov.=evaluation.variable, f.=refSubass)
  run.info=GetSimulationDetails(dt.=var.info, ov.=evaluation.variable, info.=simInfo)
  
  if(!is.null(gauge.list)){
    idx.=match(gauge.list, subvec1)
    subass1=subass1[idx., ]
    subass2=subass2[idx., ]
  }
  #
  optimum. = optima.[[criterion]]
  name. = criterion
  cut.val=cutoff.[[names(cutoff.)[match(criterion, names(cutoff.))]]]
  #
  score1          = GetScore(sub.=subass1, sco=criterion)
  if(!is.null(subass2)){
    score2 = GetScore(sub.=subass2, sco=criterion)
  } 
  
  if(grepl(x=criterion, pattern="RE(%)", fixed=TRUE)){
    name. = 'RE'
  } else if(grepl(x=criterion, pattern="RSDE(%)", fixed=TRUE)){
    name. = 'RSDE'
  }
  # rescale the scores to the cutoff values
  if(criterion %in% c("NSE","CC","KGE")){
    score1[, criterion][score1[, criterion] < cut.val] = cut.val
    score2[, criterion][score2[, criterion] < cut.val] = cut.val
  } else if(criterion %in% "RE(%)"){
    score1[, criterion][score1[, criterion] < -cut.val] = -cut.val
    score2[, criterion][score2[, criterion] < -cut.val] = -cut.val
  } 
  if(criterion %in% c("RE(%)","MAE","KGESD","KGESD")){
    score1[, criterion][score1[, criterion] > cut.val] = cut.val
    score2[, criterion][score2[, criterion] > cut.val] = cut.val
  }
  # Compute differences if requested
  
  if(visualization %in% "best.simulation"){
    met.diff = score1
  } else if (visualization %in% "relative.difference"){
    met.diff = ComputeDiff(sco1=score1, sco2=score2, type.=visualization, opt.val=optimum.) 
    colnames(met.diff)[2] = "rel.diff"
    cut.val  = 100
  } else {
    met.diff = cbind(score1, score2[,2])
    colnames(met.diff)[2:3] = c(colnames(score1)[2], paste0(colnames(score1)[2], (2)))
  }
  #
  sco1. = data.frame(met.diff[,2])
  colnames(sco1.)=colnames(met.diff)[2]
  if(!is.null(score2)){
    if(visualization == "relative.difference"){
      sco2. = NULL
    }else {
      sco2. = data.frame(met.diff[,3])
      colnames(sco2.)=colnames(met.diff)[3]
    }
  } 
  #
  scale.vec = GetScale(sco.1=sco1., sco.2=sco2., cut.=cut.val)
  vals. = sco1.
  bad.idx=which(vals.[,1] < scale.vec[1])
  if(length(bad.idx)>0) vals.[bad.idx,1] = scale.vec[1]
  rm(bad.idx)
  #
  bad.idx=which(vals.[,1] > scale.vec[length(scale.vec)])
  if(length(bad.idx)>0) vals.[bad.idx,1] = scale.vec[length(scale.vec)]
  rm(bad.idx)
  #
  quantiles1   = boxplot(as.numeric(vals.[,1]), range=0, plot = F)$stats
  if(!is.null(sco2.)){
    vals. = sco2.
    bad.idx=which(vals.[,1] < scale.vec[1])
    if(length(bad.idx)>0) vals.[bad.idx,1] = scale.vec[1]
    rm(bad.idx)
    bad.idx=which(vals.[,1] > scale.vec[length(scale.vec)])
    if(length(bad.idx)>0) vals.[bad.idx,1] = scale.vec[length(scale.vec)]
    rm(bad.idx)
    quantiles2   = boxplot(as.numeric(vals.[,1]), range=0, plot = F)$stats
  }
  #
  
  vcode=tolower(x=gsub(run.info$varshort, pattern=" ", replacement="-", fixed=TRUE))
  
  if(is.null(marker.size)){
    if(tolower(run.info$data.type) == "point"){
      marker.size=0.6
    } else if (tolower(run.info$data.type) == "spatial"){
      marker.size=0.275
    }
  }
  # get the geospatial information
  geo.summary=PrepareGeospatialData(#stream_shape=streamShapeFile, 
    border_bool=show.borders, 
    stream_bool=show.streams,
    gdata=geoData,
    vcode.=vcode,
    odir=tempOutDirectory, 
    domain.=domain.name, 
    gauges_vector=subvec1, 
    #file_string=NULL, 
    spoly=subBasins) 
  #
  #browser()
  if(!dir.exists(figuresDirectory)){
    dir.create(figuresDirectory, recursive=FALSE, showWarnings=FALSE)
  } 
  if(!is.null(domain.name)){
    txt.=paste("summary", gsub(x=domain.name, pattern=" ", replacement="_", fixed=TRUE), sep="_")
  } else {
    txt.="summary"
  }
  evar=gsub(x=evaluation.variable, pattern=" ", replacement="-", fixed=TRUE)
  
  #
  if(grepl(x=visualization, pattern="comparison")){
    str.="comparison"
  } else if (grepl(x=visualization, pattern="^best")){
    str.="bestsim"
  }else if (grepl(x=visualization, pattern="^relative")){
    str.="reldiff"
  }
  #
  fig.tit=ifelse(is.null(domain.name),
                 paste(tolower(name.), vcode, str., sep="."),
                 paste(domain.name, tolower(name.), vcode, str., sep="."))
  fname=tolower(paste(paste(txt., evar, name., gsub(x=visualization, pattern=".", replacement="-", fixed=TRUE), sep="_"),file.format, sep="."))
  cat("plotting", file.path(figuresDirectory, fname), "...")
  if(file.format == "pdf"){
    pdf(file=file.path(figuresDirectory, fname), paper="a4r", width=45, height=45/1.75, title=fig.tit)  
  } else if (file.format == "png"){
    png(filename=file.path(figuresDirectory, fname), width=45, height=45/1.75, units="cm", res=300)  
  }
  
  
  rm(fig.tit)
  if(plot.number==1){#a single map of relative.difference/best.runs/single.runs
    nf=layout(matrix(c(1, rep(2, 12), 3,      #map title
                       1, rep(4, 12), 3,                         #map title 
                       1, rep(5, 12), 3,                         # map
                       1, rep(6,2), rep(7, 8), rep(8,2), 3,      #colour bar
                       1, rep(9,3), rep(10,6), rep(11,3), 3,     #histpgram, statistics, etc
                       1, rep(12,12), 3 ), ncol=14, byrow=TRUE),
              widths=c(0.1, rep(c(rep(3,2), rep(1,2), rep(3,2)), 2), 0.1), #rep(2.5, 2)
              heights=c(0.75, 0.75,10, 0.75, 4, 2.75), respect=FALSE)
  } else if(plot.number==2) {                                                      #two side-by-side plots
    nf=layout(matrix(c(1, rep(2, 12), 3,                        #map titles
                       1, rep(4,6), rep(5,6), 3,                #map titles
                       1, rep(6,6), rep(7,6), 3,                # maps
                       1, rep(8,2), rep(9,8), rep(10, 2), 3,    #colour bar
                       
                       1, rep(11,3), rep(12,6), rep(13,3), 3,   #histpgram, statistics, etc
                       1, rep(14,12), 3), ncol=14, byrow=TRUE),
              widths=c(0.1, rep(c(rep(3,2), rep(1,2), rep(3,2)), 2), 0.1), #rep(2.5, 2)
              heights=c(0.75, 0.75, 10, 0.75, 4, 2.5), respect=FALSE)
  }
  #
  DrawMaps(diff.=met.diff, title=name., ctype.=visualization, cex.=marker.size, 
           show.data.as=data.presentation, sim.info=run.info, coff=cut.val, 
           landcol=landcol., river.network=show.streams, 
           country.borders=show.borders, cbar=col.bar, geo.data=geo.summary, 
           crit.=criterion, rnames=simulation.names, wcol=wat.col, tcol=col.txt, 
           pal.cols=used.colours, dom.=domain.name, num.plots=plot.number,
           scale.values = scale.vec, stat1=quantiles1, stat2=quantiles2, 
           sco1=sco1., sco2=sco2., stat.sign=nsign.figures)
  dev.off()
  cat("done.\n")
 
}

