
#' draw letter
#' 
#' draws a single letter as a polygon within a given bounding box
#' 
#' @param letter a single character string of the letter to draw
#' @param x0 the leftmost x-coordinate of the bounding box
#' @param y0 the bottom y-coordinate of the bounding box
#' @param x1 the rightmost x-coordinate of the bounding box
#' @param y1 the top y-coordinate of the bounding box
#' @param col the color in which to draw the letter, defaults to the foreground color
#' @param bgcol the background color used to fill cut-out parts of letters
#' @export
#' 
drawLetter <- function(letter,x0,y0,x1,y1,col=par("fg"),bgcol="white") {
	polycoords <- alphapolys[[letter]]
	w <- x1-x0
	h <- y1-y0
	#draw filled part
	with(polycoords[[1]],polygon(x*w+x0,y*h+y0,col=col,border=NA))
	#draw empty parts in fill 
	if (length(polycoords) > 1) {
		invisible(lapply(polycoords[-1],function(coords) {
			with(coords,polygon(x*w+x0,y*h+y0,col=bgcol,border=NA))
		}))
	}
}
