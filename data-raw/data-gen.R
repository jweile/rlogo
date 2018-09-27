alphafile <- "data-raw/alphabet.csv"

alphabet <- read.csv(alphafile,stringsAsFactors=FALSE)
alphapolys <- tapply(1:nrow(alphabet),alphabet$group,function(is){
	lettergroup <- alphabet[is,]
	tapply(1:nrow(lettergroup),lettergroup$pathGroup,function(js) {
		lettergroup[js,]
	})
})

aaColors <- c(
	A="steelblue1",V="steelblue2",L="steelblue3",I="steelblue",M="steelblue4",
	F="turquoise1",Y="turquoise2",W="turquoise3",R="firebrick1",H="firebrick2",K="firebrick3",
	D="violetred1",E="violetred2",S="darkolivegreen1",T="darkolivegreen2",
	N="darkolivegreen3",Q="darkolivegreen4",G="darkgoldenrod1",C="darkorange",P="gold"
)

devtools::use_data(alphapolys,aaColors,internal=TRUE)
