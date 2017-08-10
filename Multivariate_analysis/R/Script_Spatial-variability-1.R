########################################################################################
# Analysis of T-RFLP data sets in environmental microbiology
# Non exhaustive list of useful tools
# 
# Modified from: D. Borcard & F. Gillet
# Multivariate Analysis in Community Ecology: Constrained ordination and other analysis
#
# Pierre Rossi, LBE-EPFL, 2007-2008
# Analysis of bacterial community structures from CEs contaminated aquifers
# T-RFLP analysis
########################################################################################

# Preparation of the workspace
# Remove all R objects in the workspace
rm(list = ls())

# Load required libraries
require(vegan)
require(cluster)
require(gclus)
require(mvpart)

# Additional functions required for the following operations
source("coldiss.R")
source("panelutils.R")
# Function to compute a binary distance matrix from groups
source("grpdist.R")
# Function to draw ordered dendrograms with groups
source("hcoplot.R")
# Variance partitioning
source("vartest.R")

# Import datasets
spe = read.csv2("spet_6.csv", dec=".", row.names=1)
env = read.csv2("env6.csv", dec=".", row.names=1)

# Transpose the species dataset
# Operation required only when the numbers of columns exceeded the capacity of an Excel worksheet
spe = data.frame(t(spe))

# Transform the species dataset
spe.pa = decostand(spe, "pa")
spe.nor = decostand(spe, "norm")
spe.hell = decostand(spe, "hellinger")

##########################################################################
# 1. Clustering techniques and dendrograms
##########################################################################

# Compute dissimilarity and distance matrices (Q mode)
	spe.dh = vegdist(spe.hell, "euclidean") # Hellinger

# Clusterings based on the species dataset
	spe.hw = hclust(spe.dh, "ward")	    # Minimum variance clustering

# Plot dendrograms of Hellinger distance based clusterings
	windows(8,6)
	par(mfrow=c(2,2))
	plot(spe.hw, main="Ward method", xlab="", sub="")

# Hellinger distance based clustering
	windows(8,8)
	par(mfrow=c(2,2))
	plot(spe.hw$height, nrow(spe):2, type="S", main="Ward/Hellinger",
	ylab="k (number of clusters)", xlab="h (node height)", col="grey")
	text(spe.hw$height, nrow(spe):2, nrow(spe):2, col="red", cex=0.6)

# Average silhouette width (Rousseeuw internal quality index) 
	# Hellinger/Ward
	windows(16,8)
	par(mfrow=c(1,2))
	Si = numeric(nrow(spe))
	for (k in 2:(nrow(spe)-1)) {                              
	sil = silhouette(cutree(spe.hw, k=k), spe.dh)
	Si[k] = summary(sil)$avg.width
	}                       
	k.best = which.max(Si)
	plot(1:nrow(spe), Si, type="h", main="Silhouette-optimal number of clusters - Hellinger/Ward",
	xlab="k (number of groups)", ylab="Average silhouette width")
	axis(1, k.best, paste("optimum",k.best,sep="\n"), col="red", col.axis="red")

# Spearman's rank correlations
	# Ward/Hellinger
	windows(16,8)
	par(mfrow=c(1,2))
	kt = data.frame(k=1:nrow(spe), r=0)
	for (i in 2:(nrow(spe)-1)) {
	gr = cutree(spe.hw, i)
	distgr = grpdist(gr)
	kt$r[i] = cor(spe.dh, distgr, method="spearman")
	}
	kt
	k.best = which.max(kt$r)
	plot(kt$k, kt$r, type="h", main="Spearman-optimal number of clusters - Ward/Hellinger",
	xlab="k (number of groups)", ylab="Spearman's rank correlation")
	axis(1, k.best, paste("optimum",k.best,sep="\n"), col="red",
	col.axis="red")

# Silhouette plot of Ward/Hellinger
	k = 3
	spehw.g = cutree(spe.hw, k)
	sil = silhouette(spehw.g, spe.dh)
	silo = sortSilhouette(sil)
	rownames(silo) = row.names(spe)[attr(silo,"iOrd")]
	windows()
	plot(silo, main="Silhouette plot - Ward/Hellinger", cex.names=0.5,
	col=spehw.g+1, nmax.lab=100)

# Dendrogram with rectangles delimiting the X clusters
	# Ward/Hellinger, Re-order the dendrogram
	windows(30,20)
	spe.hwo = reorder.hclust(spe.hw, spe.dh)
	plot(spe.hwo, hang=-1, xlab="73 samples", sub="7 groups",
	ylab="Height (Hellinger distance)", 
	main="Minimum variance clustering (reordered)")
	spehw.g = cutree(spe.hw, k)
	so = spehw.g[spe.hwo$order]
	gro = numeric(k)
	for (i in 1:k) {
	gro[i] = so[1]
	if (i<k) so = so[so!=gro[i]]
	}
	rect.hclust(spe.hwo, k=k, border=gro+1, cluster=spehw.g)
	legend("topright", paste("Group", 1:k),
	pch=22, col=2:(k+1), bty="n")
	hcoplot(spe.hw, spe.dh, k)

# Heat map Ward/Hellinger    
	# Convert hclust object to dendrogram object
	dend = as.dendrogram(spe.hwo)
	plot(dend, horiz=T)
# Heat map (dendrogram + reordered distance matrix)
	heatmap(as.matrix(spe.dh), Rowv=dend, symm=TRUE, margin=c(3,3))

########################################################################
# 2. Species PCA on covariance matrix
##########################################################################

# PCA on a covariance matrix (default scale=FALSE)
	spe.pca = rda(spe.hell)
	spe.pca
	plot (spe.pca)
	summary(spe.pca)

# With clustering
	k = 3
	gr = cutree(spe.hw, k)
	# Plot the sites with cluster symbols
	k = length(levels(factor(gr)))
	sit.sc = scores(spe.pca, choices = c(1,2), display="wa", scaling=1)
	pl = plot(spe.pca, display="sites", type="n", scaling=1,
 	main="PCA cov/Hell + clusters Ward/Hellinger")
	# Plot the points with different symbols and colors
	points(sit.sc, cex=2, col=1+c(1:k)[gr], pch=14+c(1:k)[gr])
	text(sit.sc, rownames(spe), pos=4, cex=.7)
	# Add clustering dendrogram if required
	# ordicluster(pl, spe.hw, col="dark grey")
	# Add a legend for groups
	# (click at the place on the plot you wish to draw the legend)
	legend(locator(1), paste("Group",c(1:k)), pch=14+c(1:k), col=1+c(1:k), pt.cex=2)

# A posteriori interpretation of the species by significative environmental variables
	## Selection of the significant variables      
	# windows(8,8)                                    
	par(mfrow=c(1,1))                                                                          
	fit = envfit(spe.pca, env, perm=1000)                                                           
	fit                                                                                                
	plot(spe.pca, type="t", main=paste("PCA/Hellinger"))       
	plot(fit, axis=T)                                                                                  
 
	## Plot of the selected variables one by one, with contour  
	fitsite = envfit(spe.pca~ COVtot, env, scaling=1, perm=1000)
	fitsite
	windows(9,5)                                                   
	par(mfrow=c(1,2))                                                
	plot(spe.pca, display="sites", type="t", main=paste("PCA - COV"))
	plot(fitsite, axis=T)

# Eigenvalues
	ev = spe.pca$CA$eig
	ev
# Percentage of variance for each axis
	100*ev/sum(ev)
# Apply Kaiser's rule to select axes
	ev[ev > mean(ev)] 
# Broken stick model (MacArthur 1957)
	n = length(ev)
	bsm = data.frame(j=seq(1:n), p=0)
	bsm$p[1] = 1/n
	for (i in 2:n) {
	bsm$p[i] = bsm$p[i-1] + (1/(n + 1 - i))
	}
	bsm$p = 100*bsm$p/n
	bsm
# Plot eigenvalues and % of variance for each axis
	par(mfrow=c(2,1))
	barplot(ev, main="Eigenvalues", col="bisque", las=2)
	abline(h=mean(ev), col="red") # average eigenvalue
	legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
	barplot(t(cbind(100*ev/sum(ev),bsm$p[n:1])), beside=T,
	main="% variance", col=c("bisque",2), las=2)
	legend("topright", c("% eigenvalue", "Broken stick model"),
	pch=15, col=c("bisque",2), bty="n")

# Biplot scaling 1
	par(mfrow=c(1,1))
	plot1 = plot(spe.pca, scaling=1, main="PCA covariance - Distance biplot")
	x = scores(spe.pca, scaling=1)$species[,1]
	y = scores(spe.pca, scaling=1)$species[,2]
	arrows(0, 0, x, y, code=0, col=2, lty=2)

# So as to run a PCA using MVSP, re-transpose the data and export them in a .csv format
	write.table(spe.hell, file = "spe.csv", sep = ";", col.names = NA)
	spe.hell_final = data.frame(t(spe.hell))
	write.table(spe.hell_final, file = "spe.csv", sep = ";", col.names = NA)

##########################################################################
# 2. Species NMDS on normalized datasets
##########################################################################

spe.nor = decostand(spe, "norm")
spe.dc = vegdist(spe.nor, "euc")
spe.dcm= hclust(spe.dc, "ward")

# Improved method by Jari Oksanen (with projection of species)   
	spe.nmds = metaMDS(spe.nor, distance="euc")
	spe.nmds
	spe.nmds$stress
	coord = scores(spe.nmds)[,c(1,2)]
	coord
	windows(6,6)
	plot(spe.nmds, type="t", main=paste("NMDS/chord - Stress =", round(spe.nmds$stress,2), "%"))
	summary (spe.nmds)
# Shepard plot and goodness of fit
	windows(8,4)
	par(mfrow=c(1,2))
	stressplot(spe.nmds, main="Shepard plot")
	gof = goodness(spe.nmds)
	plot(spe.nmds, type="t", main="Goodness of fit")
	points(spe.nmds, display="sites", cex=gof)
# Plot the sites with cluster symbols
	windows(30,30)
	k = length(levels(factor(gr)))
	sit.sc = scores(spe.nmds)
	pl = ordiplot(spe.nmds, type="n", display="sites", main="NMDS/chord + clusters Ward/chord")
	abline(h=0, lty=3)
	abline(v=0, lty=3)
# Plot the points with different symbols and colors
	points(sit.sc, cex=2, col=1+c(1:k)[gr], pch=14+c(1:k)[gr])
	text(sit.sc, rownames(spe), pos=4, cex=.7)
# Add a legend for groups
	legend(locator(1), paste("Group",c(1:k)), pch=14+c(1:k), col=1+c(1:k), pt.cex=2)

##########################################################################
# 3. Redundancy analysis - RDA
##########################################################################

# Import datasets when required
	env = read.csv2("env2.csv", dec=".", row.names=1)
# RDA
	spe.rda = rda(spe.hell, env)		# for numerical variables !
	spe.rda = rda(spe.hell~., env)	# for nominal variables !
	plot(spe.rda)
	spe.rda
	anova(spe.rda)
	summary (spe.rda)
# Import datasets
	U = read.csv2("U.csv", dec=".", row.names=1)
	env = read.csv2("env4.csv", dec=".", row.names=1)
	Depth = read.csv2("Depth.csv", dec=".", row.names=1)
# pRDA
	spe.rda = rda(spe.hell, env2, metals)
	plot(spe.rda)
	spe.rda
	anova(spe.rda)

# Permutation tests
# *********************

# Permutation test of the overall analysis
	anova(spe.rda, step=10000)
# Permutation tests of each explanatory variable with separate RDAs
	testvar = data.frame(Variable=names(env), Variation=0, Perm=0, 
	Prob=1, Sig="ns")
	levels(testvar$Sig) = c("ns", ".", "*", "**", "***")
	for (i in 1:ncol(env)) {
		aa = rda(spe.hell ~ ., as.data.frame(env[,i]))
		testvar$Variation[i] = 100 * sum(aa$CCA$eig) / aa$tot.chi
		pt = anova(aa, step=1000)
		testvar$Perm[i] = pt$N.Perm[1]
		testvar$Prob[i] = pt$Pr[1]
		if (pt$Pr[1] <= 0.1) testvar$Sig[i] = "."
		if (pt$Pr[1] <= 0.05) testvar$Sig[i] = "*"
		if (pt$Pr[1] <= 0.01) testvar$Sig[i] = "**"
		if (pt$Pr[1] <= 0.001) testvar$Sig[i] = "***"
		}
		testvar$Variation = round(testvar$Variation, 2)
		testvar$Prob = round(testvar$Prob, 3)
		testvar$Prob[testvar$Prob<0.001] = "< 0.001"
		testvar

# Stepwise selection of explanatory variables
# ***********************************************

# null model (PCA!)
	mod0 = rda(spe.hell ~ 1, env)
	mod0
# full model (all variables included)
	mod1 = rda(spe.hell ~ ., env)
	mod1
# Show changes in site position from PCA (null model) to RDA (full model)
	plot(procrustes(mod0, mod1))
# Forward selection of explanatory variables
	mod = step(mod0, scope = formula(mod1))
	mod$anova
	mod
	plot(mod, scaling=3, 
	main="Triplot RDA best model (forward) - scaling 3")
# Show changes from full to best RDA model
	plot(procrustes(mod1, mod))

# Variation Partitioning
# **********************

# Import function from F. Kohler (after Oksanen & Legendre)
	source("anaveg.R")
# Import datasets
	geol = read.csv2("geol.csv", dec=".", row.names=1)
	COV = read.csv2("COV.csv", dec=".", row.names=1)
	env = read.csv2("env5.csv", dec=".", row.names=1)
# Variation partitioning with RDA
	#result = parrda3(spe.hell, env, as.matrix(U), as.matrix(Depth), permu=999)
	#result
	result = parrda3(spe.hell, env, geol, COV, permu=999)
	result
var = result[c(4,6,5,7),2]
barplot(as.matrix(var),legend=c(row.names(result[4,]),row.names(result[6,]),
	row.names(result[5,]),row.names(result[7,])), main="pRDA variances", 
	ylab="% of variation explained (adjusted R-squares)", col=terrain.colors(4))




