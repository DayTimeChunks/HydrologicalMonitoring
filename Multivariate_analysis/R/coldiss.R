# coldiss, F. Gillet, 18.10.2005
# Color plots of a dissimilarity matrix, without and with ordering

"coldiss" <- function(D, X, nc = 4, byrank = TRUE, diag = FALSE)
{
	require(gclus)

	if (max(D)>1) D <- D/max(D)

	if (byrank) {
		spe.color = dmat.color(1-D, cm.colors(nc))
	}
	else {
		spe.color = dmat.color(1-D, byrank=FALSE, cm.colors(nc))
	}

	spe.o = order.single(1-D)
	speo.color = spe.color[spe.o,spe.o]
	
	op = par(mfrow=c(1,2), pty="s")

	if (diag) {
		plotcolors(spe.color, rlabels=row.names(X), 
			main="Dissimilarity Matrix", 
			dlabels=row.names(X))
		plotcolors(speo.color, rlabels=row.names(X)[spe.o], 
			main="Ordered Dissimilarity Matrix", 
			dlabels=row.names(X)[spe.o])
	}
	else {
		plotcolors(spe.color, rlabels=row.names(X), 
			main="Dissimilarity Matrix")
		plotcolors(speo.color, rlabels=row.names(X)[spe.o], 
			main="Ordered Dissimilarity Matrix")
	}

	par(op)
}

# Usage:
# coldiss(D = dissimilarity.matrix, X = data.frame, nc = 4, diag = FALSE)
# If D is a distance matrix, D is divided by max(D)

# Example:
# coldiss(spe.dj, spe, nc=9, byrank=F, diag=T)

# byrank= TRUE		equal-sized categories
# byrank= FALSE		equal-length intervals
