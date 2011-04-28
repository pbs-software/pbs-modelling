#plotBubbles----------------------------2010-08-04
# Function to construct a bubble plot for a matrix z
#  z:     input matrix or data frame
#  xval:  x-values for the columns of z (if length xval != # colums in z, xval is ignored)
#         if xval=TRUE, first row contains x-values for the columns
#  yval:  y-values for the rows of z (if length yval != # rows in z, yval is ignored)
#         if yval=TRUE, first column contains y-values for the rows
#  dnam:  if TRUE, use dimnames as xval and yval
#         (overwrites previously specified values)
#  rpro:  if rpro=TRUE, convert rows to proportions
#  cpro:  if cpro=TRUE, convert columns to proportions
#  rres:  if rres=TRUE, use row residuals (subtract row means)
#  cres:  if cres=TRUE, use column residuals (subtract column means)
#  powr:  power tranform; radii proportional to z^powr
#         powr=0.5 gives bubble areas proportional to z
#  clrs:  colours used for positive, negative, and zero values
#  size:  size (inches) of the largest & smallest bubble
#  lwd:   line width for drawing circles
#  hide0: if TRUE, hide zero-value bubbles
#  frange: fraction by which the range of the axes should be extended
#  prettyaxis: logical: if TRUE, apply the pretty function to both axes
#  ...:   further parameters for the plotting functions 
#-------------------------------------------JTS/RH
plotBubbles <- function(z, xval=FALSE, yval=FALSE, dnam=FALSE, 
    rpro=FALSE, cpro=FALSE, rres=FALSE, cres=FALSE, 
    powr=0.5, size=0.2, lwd=1, clrs=c("black","red","blue"), 
    hide0=FALSE, frange=0.1, prettyaxis=FALSE, ...) {

	if (is.data.frame(z)) {
		use = !sapply(z,is.factor) & sapply(z,is.numeric)
		z=z[,use,drop=FALSE]; if (ncol(z)==0) {showAlert("data frame not useable"); return()}
		#names(z)=gsub("\\.","_",names(z))
		z=as.matrix(z) }
	dz <- dim(z);  ny=ny1=dz[1];  nx=nx1=dz[2]
	if (length(dz)>2) {showAlert("Input matrix must have only 2 dimensions"); return() }
	xval1 <- 1:nx;  yval1 <- 1:ny
	# If first row contains x-values for columns
	if (mode(xval) == "logical") {
		if (xval[1]) {
			xval1 <- z[1,]; ny1 <- ny - 1; } }
	# If first column contains y-values for rows 
	if (mode(yval) == "logical") {
		if (yval[1]) {
			yval1 <- z[,1]; nx1 <- nx - 1; } }
	xind <- (nx - nx1 + 1):nx
	x2=xlabel=xval1[xind]
	yind <- (ny - ny1 + 1):ny
	y2=ylabel=yval1[yind]
	if ((mode(xval) != "logical") & (length(xval) == nx1)) {
		if (mode(xval)=="numeric") x2=xval
		xlabel=xval }
	if ((mode(yval) != "logical") & (length(yval) == ny1)) {
		if (mode(yval)=="numeric") y2=yval
		ylabel=yval }

	zz <- array(z[yind,xind],dim=c(length(yind),length(xind)),dimnames=dimnames(z[yind,xind,drop=FALSE]))
	dots=list(...)
	xlab=dots$xlab; if (is.null(xlab)) xlab=""  # x-axis label
	ylab=dots$ylab; if (is.null(ylab)) ylab=""  # y-axis label

	# dimnames are to be used to over-ride xval and yval
	if (dnam & !is.null(dimnames(zz))) { 
		warn=options()$warn; options(warn=-1)
		if (!is.null(dimnames(zz)[[2]])) {
			xpos = try(as.numeric(dimnames(zz)[[2]]),silent=TRUE)
			if (all(is.na(xpos))) xlabel=dimnames(zz)[[2]]
			else if (!any(is.na(xpos)) && all(diff(xpos)>0 | all(diff(xpos)<0))) {
				xlabel=as.character(xpos); x2=xpos } # strictly increasing / decreasing
		}
		if (!is.null(dimnames(zz)[[1]])) { 
			ypos = try(as.numeric(dimnames(zz)[[1]]),silent=TRUE)
			if (all(is.na(ypos))) ylabel=dimnames(zz)[[2]]
			else if (!any(is.na(ypos)) && all(diff(ypos)>0 | all(diff(ypos)<0))) {
				ylabel=as.character(ypos); y2=ypos } # strictly increasing / decreasing
		}
		options(warn=warn)
	}
#browser()
	xx <- rep(x2, each = length(y2))
	yy <- rep(y2, length(x2))
	minz <- min(zz,na.rm=TRUE);  maxz <- max(zz,na.rm=TRUE);
	if (rpro | cpro) {
		if (minz < 0) {
			zz <- zz - minz
			minz <- 0
			maxz <- max(zz,na.rm=TRUE) } }
	if (rpro) {
		zs <- apply(zz, 1, sum, na.rm=TRUE)
		zz <- sweep(zz, 1, zs, "/") }
	if (cpro) {
		zs <- apply(zz, 2, sum, na.rm=TRUE)
		zz <- sweep(zz, 2, zs, "/") }
	if (rres) {
		zm <- apply(zz, 1, mean, na.rm=TRUE)
		zz <- sweep(zz, 1, zm, "-") }
	if (cres) {
		zm <- apply(zz, 2, mean, na.rm=TRUE)
		zz <- sweep(zz, 2, zm, "-") }
	zNA <- is.na(zz) | is.nan(zz) | is.infinite(zz); zz[zNA] <- 0;
	z0 <- sign(zz) * abs(zz)^abs(powr)
	z1 <- z3 <- z0;  z1[z0 <= 0] <- NA; z3[z0<0 | z0>0] <- NA;
	z2 <- -z0; z2[z0 >= 0] <- NA;
	za <- max(z0,na.rm=TRUE);  zb <- min(z0,na.rm=TRUE)
	zM <- max(abs(z0))
	sz1 <- max(za * size/zM, 0.001)
	sz2 <- max(-zb * size/zM, 0.001)
	#plot(0,0,xlim=extendrange(x2),ylim=extendrange(y2),type="n",xaxt="n",...)
	#axis(1,at=x2,labels=xlabel,...)
	#symbols(xx,yy,circles=as.vector(abs(z0)),inches=size,fg=0,...)
	evalCall(plot,argu=list(x=0,y=0,xlim=extendrange(x2,f=frange),
		ylim=extendrange(y2,f=frange),type="n",axes=FALSE,xlab=xlab,ylab=ylab),...,checkdef=TRUE,checkpar=TRUE)
	if (prettyaxis) {
		if (length(min(x2):max(x2))<=5) xshow = is.element(x2,x2)
		else                            xshow = is.element(x2,pretty(x2,n=10))
		yshow = is.element(y2,pretty(y2,n=10))
	}
	else {
		xshow = rep(TRUE,length(x2)); yshow = rep(TRUE,length(y2)) }
	if (!all(xshow))
		axis(1,at=x2[!xshow],labels=FALSE,tcl=ifelse(is.null(dots$tcl),par()$tcl,dots$tcl)/3)
	if (!all(yshow))
		axis(2,at=y2[!yshow],labels=FALSE,tcl=ifelse(is.null(dots$tcl),par()$tcl,dots$tcl)/3)
	evalCall(axis,argu=list(side=1,at=x2[xshow],labels=xlabel[xshow]),...,checkpar=TRUE)
	evalCall(axis,argu=list(side=2,at=y2[yshow],labels=ylabel[yshow]),...,checkpar=TRUE)
	if (!hide0 && !all(is.na(z3))) {
		evalCall(symbols,argu=list(x=xx,y=yy,circles=as.vector(z3),inches=0.001,fg=clrs[3],lwd=lwd,add=TRUE),...,checkpar=TRUE) }
		#symbols(xx, yy, circles = as.vector(z3), inches = 0.001, fg = clrs[3], lwd = lwd, add = TRUE, ...) }
	if (!all(is.na(z2))) {
		evalCall(symbols,argu=list(x=xx,y=yy,circles=as.vector(z2),inches=sz2,fg=clrs[2],lwd=lwd,add=TRUE),...,checkpar=TRUE) }
		#symbols(xx, yy, circles = as.vector(z2), inches = sz2, fg = clrs[2], lwd = lwd, add = TRUE, ...) }
	if (!all(is.na(z1))) {
		evalCall(symbols,argu=list(x=xx,y=yy,circles=as.vector(z1),inches=sz1,fg=clrs[1],lwd=lwd,add=TRUE),...,checkpar=TRUE) }
		#symbols(xx, yy, circles = as.vector(z1), inches = sz1, fg = clrs[1], lwd = lwd, add = TRUE, ...) }
	box(); invisible(z0) }
#--------------------------------------plotBubbles

#plotBubbles(test,dnam=T,hide0=T,size=0.15,clrs="blue",las=3,cex.axis=.8,ylab="",tcl=.25,powr=0.5,prettyaxis=T,xlim=c(1998.5,1999.5),rpro=T)


