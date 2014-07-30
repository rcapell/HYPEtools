


PlotDurationCurve <- function(freq, unit = "m3s", ylim = NULL) {
  
  # yaxis label
  if (unit == "m3s") {
    ylabel <- expression(paste("Q (m"^3, " s"^{-1}, ")"))
  } else if (unit == "mmd") {
    ylabel <- expression(paste("Q (mmm d"^{-1}, ")"))
  } else {
    ylabel <- unit
    warning("Argument 'unit' unknown, printing as provided by user.")
  }
  
  par(mar = c(3,3,1,1)+.1, tcl = -0.2, mgp = c(1.8, 0.3, 0))
  plot(1 - freq[, 1], freq[, 2], log = "y", type = "n", ylab = ylabel, xlab = "Flow exceedance percentile", ylim = ylim)
  grid(equilogs = F)
  lines(1 - freq[, 1], freq[, 2], type = "l", pch = 16, cex = 0.4, col = "blue")
}
