
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
	if (!(letter %in% names(alphapolys))) {
		stop("Letter \"",letter,"\" is not supported!")
	}
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

#' single letter amino acid codes
#' @export
aaLetters <- c("A","V","L","I","M","F","Y","W","H","K","R","D","E","N","Q","S","T","G","C","P")

#' single letter nucleotide codes
#' @export
ncLetters <- c("A","C","G","T")

#' A color scheme for nucleotides
#' @export
ncColors <- setNames(
	c("steelblue3","darkolivegreen3","gold","firebrick3"),
	ncLetters
)

#' A color scheme for amino acids
#' @export
aaColors <- c(
	A="steelblue1",V="steelblue2",L="steelblue3",I="steelblue",M="steelblue4",
	F="turquoise1",Y="turquoise2",W="turquoise3",R="firebrick1",H="firebrick2",K="firebrick3",
	D="violetred1",E="violetred2",S="darkolivegreen1",T="darkolivegreen2",
	N="darkolivegreen3",Q="darkolivegreen4",G="darkgoldenrod1",C="darkorange",P="gold"
)

#' draw a simple weblogo
#' 
#' draws a simple web logo for a matrix of values.
#' 
#' @param data a numerical matrix with rownames corresponding to the letters to be drawn.
#' @param col a named vector of colors to be used for each letter.
#' @param lttrs the letters corresponding to the rows in the matrix, defaults to the matrix's colnames.
#' @param bgcol the background color used to fill cut-out parts of letters
#' @param ... Other parameters to be passed to the \code{plot()} function.
#' @export
#'
drawSimpleLogo <- function(data,col,lttrs=rownames(data),bgcol="white",...) {
	plot(0,type="n",xlim=c(0,ncol(data)+1),ylim=c(0,max(colSums(data,na.rm=TRUE),na.rm=TRUE)),...)
	for (i in 1:ncol(data)) {
		top <- sum(data[,i])
		for (j in 1:nrow(data)) {
			drawLetter(lttrs[[j]],i-.5,top-data[j,i],i+.5,top,col=col[[lttrs[[j]]]],bgcol=bgcol)
			top <- top-data[j,i]
		}
	}
}
