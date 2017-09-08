#-------------------------------------------------------------------------------
#
#           addBreakColorLegend
#
#-------------------------------------------------------------------------------

#' Adds a Legend to sp Plots with Colors and Breaks
#'
#' Adds a Legend to sp Plots with Colors and Breaks
#'
#' @param xleft a scalar of the left x position.
#' @param ybottom a scalar of bottom y position.
#' @param xright a scalar of the right x position.
#' @param ytop a scalar of top y position.
#' @param brks the breaks between color classes.  Needs a lower bound, and upper bound, so there should be one more break than number of classes.
#' @param colors a vector of colors, should have one less element than the number of breaks.
#' @param printFormat: a character variable in '4.2' format where the 4 control the number of digits before the decimal, and 2 controls the number of digits after the decimal.
#'
#' @seealso \code{\link{plotPointsRGB}}, \code{\link{rect}}, 

#' @return add a color ramp legend as a rectangle to the currently active plot
#'
#' @author Jay Ver Hoef
#' @rdname addBreakColorLegend
#' @export addBreakColorLegend 


addBreakColorLegend <- function(xleft, ybottom, xright, ytop, breaks, 
	colors, printFormat = "4.2", ...) 
{
  nshades <- length(colors)
  for(i in 1:nshades)
    rect(xleft, ybottom + (i-1)/nshades*(ytop - ybottom), xright, 
	ybottom + i/nshades*(ytop - ybottom), col = colors[i], border = NA)
  tickInc <- (ytop - ybottom)/nshades
  breakLabels <- sprintf(paste("%",as.character(printFormat),"f", 
    sep = ""), breaks)
  for(i in 1:nshades) 
  text(xright, ybottom + tickInc*(i-1), breakLabels[i], 
    pos = 4, ...)
  text(xright, ytop, breakLabels[nshades + 1], 
    pos = 4, ...)
  return(invisible())	
}


