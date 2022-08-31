
#' Custom color ramp palettes
#' 
#' Pre-defined color ramp palettes which are used in other \code{HYPEtools} functions.
#' 
#' @param n Integer, number of colors to generate.
#' 
#' @details 
#' These functions build on calls to \code{\link{colorRampPalette}}.
#' 
#' @return 
#' All functions return vectors of length \code{n} with interpolated RGB color values in hexadecimal 
#' notation (see \code{\link{rgb}}).
#' 
#' @examples 
#' ColNitr(10)
#' ColGreens(6)
#' barplot(rep(1, 11), col = ColTemp(11))
#' 
#' @name CustomColors

NULL

# color ramp palettes with strong saturation and value gradients, for quantitative differences

#' @rdname CustomColors
#' @export
ColNitr <- colorRampPalette(c("#fff5a8", "#6b0601"))
#' @rdname CustomColors
#' @export
ColPhos <- colorRampPalette(c("#dcf5e9", "#226633"))
#' @rdname CustomColors
#' @export
ColPrec <- colorRampPalette(c("#e0e7e8", "#00508c"))
#' @rdname CustomColors
#' @export
ColTemp <- colorRampPalette(c("#0000ff", "#0080ff", "#80ffff", "#f0f0f0", "#ffff00", "#ff8000", "#ff0000"))
#' @rdname CustomColors
#' @export
ColQ <- colorRampPalette(c("#ede7ff", "#2300ff"))
#' @rdname CustomColors
#' @export
ColDiffTemp <- colorRampPalette(c("#2892c7", "#e9f0e8", "#e81515"))
#' @rdname CustomColors
#' @export
ColDiffGeneric <- colorRampPalette(c("#e81515", "#e9f0e8", "#2892c7"))

# color ramp palettes with dominant color and saturation gradient, for qualitative differences
#' @rdname CustomColors
#' @export
ColBlues <- colorRampPalette(c("#0a0a96", "#a3a3db"))
#' @rdname CustomColors
#' @export
ColReds <- colorRampPalette(c("#f77497", "#670101"))
#' @rdname CustomColors
#' @export
ColGreens <- colorRampPalette(c("#04eb04", "#004400"))
#' @rdname CustomColors
#' @export
ColYOB <- colorRampPalette(c("#ffe851", "#da531d", "#5b1e00"))
#' @rdname CustomColors
#' @export
ColPurples <- colorRampPalette(c("#da62ed", "#300275"))
