#================================================|
#                 PBS Modelling                  |
#------------------------------------------------|
# Authors:                                       |
#  Jon T. Schnute <Jon.SchnuteJ@dfo-mpo.gc.ca>   |
#  Alex Couture-Beil <alex@mofo.ca>              |
#  Rowan Haigh <Rowan.HaighR@dfo-mpo.gc.ca>      |
#  Anisa Egeli <>                                |
#================================================|

#compileDescription---------------------2006-08-28
#  Convert a GUI description file into a complete GUI desc List
#  which can be passed directly to createWin
# Arguments:
#  descFile - filename of GUI description file
#  outFile  - filename to save list to. 
#             WARNING: this will overwrite the file, if it currently exists
#----------------------------------------------ACB
compileDescription <- function(descFile, outFile="") {
	if (outFile!="")
		sink(outFile)
	x<-parseWinFile(descFile)
	cat(paste(
	"#This file was automaticaly generated from window description file \"",
	descFile, "\"\n#with the compileDescription function.\n\n", sep=""))
	cat("#This list can then be passed directly to createWin()\n\n")
	cat(paste("#To assign this list to a variable: GUIdesc<-eval(parse(\"", outFile, "\"))\n", sep=""))
	writeList(x)
	cat("\n")
	if (outFile!="")
		sink()
}


#.addslashes----------------------------2006-08-28
# Escapes special characters from a string, which can then be used in the "P" format
# if x has more than one element, then it will returned a nested characterVector
# ie: c("it's", "O K") becomes => "'it\'s' 'O K'"
# Arguments:
#  x - string to escape
#----------------------------------------------ACB
.addslashes <- function(x) {
	#escape backslashes
	x <- gsub("\\\\", "\\\\\\\\", x)
	#escase doublequotes
	x <- gsub("\"", "\\\\\"", x)
	#escase singlequotes
	x <- gsub("'", "\\\\'", x)
	#convert into substrings if applicable
	if (length(x)>1) {
		i<-append(grep("[ \t\\\\]+", x), grep("^$", x)) #indicies needing quotes
		x[i]<-paste("'", x[i], "'", sep="")
		x<-paste(x, collapse=" ")
	}
	else {
		#special case where it is a single word with no special chars
		if (!any(grep("[ \t\\\\]+", x)) && x!="")
			return(x)
	}
	return(paste("\"", x, "\"", sep="")) }

#.mapArrayToVec-------------------------2006-09-16
# Determines which index to use for a vector, when given an 
# N-dim index of an array.
# Arguments:
#  x       - array index (numeric vector)
#  d       - dimensions of the array
#  byright - if true, vary most right indices first, 
#            if false, vary by left (R default)
#----------------------------------------------ACB
.mapArrayToVec <- function(x,d, byright=FALSE) {
	x <- x - 1 #start counting at 0 instead of 1
	m <- length(x)
	if (m!=length(d))
		stop("given points (x), does not match lenght of given dimensions (d)")
	if (byright) {
		ind <- x[m]
		for(i in (m-1):1) {
			ind <- ind+x[i]*prod(d[(i+1):m]) }
	}
	else {
		ind <- x[1]
		for(i in 2:length(d)) {
			ind <- ind+x[i]*prod(d[(i-1):1]) 
#print(ind+1)
#browser();return()
			}
	}
	return(ind+1)

	#return(x[1] + d[1]*x[2])
}

#.getArrayPts---------------------------2009-02-10
#  Returns all possible indices of an array
# Arguments:
#  d is a vector of integers specifing the dimensions
# output: a list of vectors of all possible indices
#-------------------------------------------ACB/RH
.getArrayPts <- function(d, byright=FALSE, byrow=TRUE) {
	x<-list(); m=length(d);
	if (m>2) mbyrow=c(2,1,3:m) else mbyrow=c(2,1)
	for(i in 1:length(d)) {
		if (byright) x[[i]] = d[i]:1
		else x[[i]] = 1:d[i] }
	if (byrow) {
		xx=expand.grid(c(x[2],x[-2])); xx=xx[,mbyrow] }
	else xx=expand.grid(x)
#browser();return()
	y<-list()
	for(i in 1:length(xx[[1]])) {
		z<-unlist(xx[i,])
		attributes(z)<-NULL
		y[[i]]<-z }
	return(y) }

#.convertVecToArray---------------------2006-09-16
# Converts a vector to an Array
# Arguments:
#  x       - a vector of data to use to create array
#  d       - dimensions of the array
#  byright - if TRUE, vary indices by the most right number first
#                 ex) 1,1 - 1,2 - 1,3 - 2,1 - 2,2 - 2,3
#            if FALSE, varry by most left (R default)
#                 ex) 1,1 - 2,1 - 1,2 - 2,2 - 1,3 - 2,3  
#----------------------------------------------ACB
.convertVecToArray <- function(x,d, byright=FALSE, byrow=TRUE) {
	if (length(x)!=prod(d))
		stop("given vector x length does not match product of dimensions")
	#create an empty array with correct dimensionality
	y=vector(mode(x),prod(d));  dim(y)=d
	#iterate over every possible index
	pts<-.getArrayPts(d,byright=byright,byrow=byrow)
	for(i in 1:length(pts)) {
		arrIndex <- paste(pts[[i]],collapse=",")
		if (byrow) vecIndex=i
		else       vecIndex=.mapArrayToVec(pts[[i]], d, byright)
#print(paste(vecIndex,arrIndex,sep=" : "))
#browser();return()
		#map it to the appropriate place in the given X vector
		code = paste("y[", arrIndex, "] <- ", x[vecIndex], sep="")
		eval(parse(text=code))
	}
	return(y)
}

#createVector---------------------------2006-09-16
# Create a GUI with a vector widget and button
# Arguments:
#  vec:          a vector of widget variable names
#                if vec is named, then the names are used as widget variable 
#                names and the values are used as the default value
#
#  vectorLabels: if supplied, this vector of labels are printed above each entry box
#                There should be one label for every variable defined in vec
#                i.e. length(vectorLabels)==length(vec)
#
#  func:         function name as a string
#                If given, this function will be called whenever data is entered
#                i.e. Enter pressed, or submit button clicked. This user function
#                would then most likely use getWinVal()
#
#  windowname:  windowname to use for this GUI
#
# Output: If no user defined function is given (see func paramater), then global variables 
#         matching the variable name is set with the value of the widget
#         whenever text focus is in a widget and enter is pressed, or when submit is pushed.
#         Otherwise, func will be called and it is the user's responsibility to  make use of getWinVal
#----------------------------------------------ACB
createVector <- function (vec, vectorLabels=NULL, func="", windowname="vectorwindow") {
	if (is.null(names(vec))) {
		namesVal <- vec
		valuesVal <- ""
	}
	else {
		namesVal <- names(vec)
		valuesVal <- vec
	}
	if (!is.character(func))
		stop("func must be a character string")
	namesVal <- as.character(namesVal)
	if (is.null(vectorLabels)) 
		vecLabels <- names(vec)
	else {
		if (length(vectorLabels) != length(vec)) 
			stop("length of paramaters vec and vectorLabels should be the same length")
		vectorLabels <- as.character(vectorLabels)
		vecLabels <- vectorLabels
	}
	winList <- list(list(title = "Vector", windowname = windowname, vertical = TRUE, 
		onclose = "", .widgets = list(list(type = "vector", names = namesVal, 
		length = length(vec), labels = vecLabels, values = valuesVal, 
		font = "", vertical = FALSE, "function" = func, enter = TRUE, 
		action = "", mode = "numeric", width = 6, sticky = "", 
		padx = 0, pady = 0), list(type = "button", "function" = func, 
		text = "Go", padx = 0, pady = 0)), .menus = list()))
	createWin(winList)
}

#promptOpenFile-------------------------2006-09-16
# Opens a prompt and asks a user to select a file.
# Arguments:
#  initialfile - filename to pre-select
#  filetype - list of vectors specifying allowed filetypes
# Returns:
#  selected filename
# Example:
#  promptOpenFile("intial_file.txt", filetype=list(c(".txt", "text files"), 
#                 c(".r", "R files"), c("*", "All Files")))
#----------------------------------------------ACB
promptOpenFile <- function(initialfile="", filetype=list(c("*", "All Files")), open=TRUE) {
	filetypes <- ""
	for(i in 1:length(filetype)) {
		filetype[[i]]
		if (is.na(filetype[[i]][2]))
			filetype[[i]][2] <- filetype[[i]][1]
		if (filetype[[i]][1] != "*" && substr(filetype[[i]][1],1,1)!=".")
			filetype[[i]][1] <- paste(".", filetype[[i]][1], sep="")
		filetypes <- paste(filetypes, " {{", filetype[[i]][2], "} {", filetype[[i]][1], "}}", sep="")
	}

	filetypes <- .trimWhiteSpace(filetypes)
	if (open)
		return(tclvalue(tkgetOpenFile(initialfile=initialfile, filetypes=filetypes)))
	else
		return(tclvalue(tkgetSaveFile(initialfile=initialfile, filetypes=filetypes)))
}

#promptSaveFile-------------------------2006-09-16
# Exactly the same as promptOpenFile except displays a 
# save button instead of an open button
#----------------------------------------------ACB
promptSaveFile <- function(initialfile="", filetype=list(c("*", "All Files")), save=TRUE)
{
	return(promptOpenFile(initialfile, filetype, !save))
}

#showArgs-------------------------------2009-02-23
#  show arguments of a widget definition
# Arguments:
#  widget - only show information about supplied widget
#-------------------------------------------ACB/RH
showArgs <- function(widget, width=70, showargs=FALSE) {
	x <- .widgetDefs
	if (missing(widget)) widget=sort(names(x))
	x=x[widget]; xnam=names(x)
	out=character(0) # output file

	for(i in 1:length(x)) {
		#print widget name, and underline it
		cat(xnam[i])
		cat(paste("\n",paste(rep("-",nchar(xnam[i])),collapse=""),"\n",sep=""))
		
		expr=xnam[i]
		for(j in 2:length(x[[i]])) { 
			argu=x[[i]][[j]]$param
			if (x[[i]][[j]]$required==TRUE) { }
			else if ( any( names( x[[i]][[j]] ) == "default" ) ) {
				if (x[[i]][[j]]$class=="character" || x[[i]][[j]]$class=="characterVector")
					delim="\""
				else
					delim=""				
				default <- ifelse( is.null( x[[i]][[j]]$default ), "NULL", paste(delim,x[[i]][[j]]$default,delim,sep="") )
				argu=c(argu,"=")
				argu=c(argu,default)
			}
			else {
				cat("\n\n")
				stop(paste(xnam[i],"::",x[[i]][[j]]$param, "is not required, but has no default."))
			}
			expr=paste(expr,paste(argu,collapse=""),sep=" ")
		}
		wexp=strwrap(expr,width=width,exdent=3)
		cat(paste(wexp,collapse="\n")); cat("\n\n")
		out=c(out,expr) # save for output

		if (showargs) {
			for(j in 1:length(x[[i]])) { 
				cat(x[[i]][[j]]$param)
				if (x[[i]][[j]]$required==TRUE) {
					cat("\t"); cat("(required)") }
				cat("\n") }
			cat("\n\n") }
	}
	invisible(out) }
#-----------------------------------------showArgs

#genMatrix------------------------------2006-08-28
#  Generate a test matrix for use in plotBubbles
# Arguments:
#  m     - number of rows
#  n     - number of columns
#  mu    - mean value of distribution
#  sigma - std deviation of distribution
#----------------------------------------------JTS
genMatrix <- function (m,n,mu=0,sigma=1) {
	matrix(rnorm(m*n,mean=mu,sd=sigma), m, n)
}

#pad0-----------------------------------2006-08-28
# Takes numbers, converts them to integers then text,
#   and pads them with leading zeroes.
# Arguments:
#    x - Vector of numbers
#    n - Length of padded integer
#    f - Factor of 10 to expand x by
# Note: For meaningful results, n should be at least as
#       large as the order of factored x (x * 10^f).
#-----------------------------------------------RH
pad0 <- function (x, n, f = 0) {
	xin <- x; xord <- max(ceiling(log10(abs(x * 10^f))), na.rm = TRUE);
	if (any(max(abs(x * 10^f)) == 10^(-10:10))) xord <- xord + 1;
	if (n < xord) n <- xord; # No padding occurs if n<=xord
	x <- round(x, f) * 10^f; xneg <- x < 0;
	x <- abs(x);  x <- format(x, scientific=FALSE);
	x <- gsub(" ", "", x); nx <- length(x);
	base0 <- rep(paste(rep(0, n), collapse = ""), nx);
	nchr <- nchar(x); ndiff <- n - nchr;
	add0 <- substring(base0, 1, ndiff);
	xnew <- paste(add0, x, sep = "");
	xnew[xneg] <- paste("-", xnew[xneg], sep = "");
	attr(xnew, "input") <- xin; return(xnew); };


#show0----------------------------------2006-08-28
# Shows decimal places including zeroes (string)
# Return character representation of number with
# specified decimal places.
# Arguments:
#  x  - Number as scalar or vector
#  n  - Number of decimal places to show, include zeroes
#  add2int - If TRUE, add zeroes on the end of integers
#-----------------------------------------------RH
show0 <- function (x, n, add2int = FALSE) {
	x <- as.character(x)
	oldx <- x
	pnt <- regexpr("\\.", x)
	z <- grep(-1, pnt)
	x[z] <- paste(x[z], ".", sep = "")
	pnt[z] <- nchar(x)[z]
	int <- substring(x, 1, pnt)
	end <- substring(x, pnt + 1)
	nx <- length(end)
	base0 <- rep(paste(rep(0, n), collapse = ""), nx)
	nchr <- nchar(end)
	ndiff <- n - nchr
	add0 <- substring(base0, 1, ndiff)
	newx <- paste(int, end, add0, sep = "")
	if (!add2int) 
		newx[z] <- oldx[z]
	return(newx)
}

#view-----------------------------------2008-05-22
# View first/last/random n element/rows of an object.
#-----------------------------------------------RH
view <- function (obj, n=5, last=FALSE, random=FALSE, ...) {
	getn=function(n,N,last=FALSE,random=FALSE,...) {
		n=min(n,N)
		if (random) return(sample(1:N,n,...)) 
		n1=ifelse(last,N-n+1,1); n2=ifelse(last,N,n)
		return(n1:n2) }
	showVec=function(obj,n,last=FALSE,random=FALSE,...){
		N=length(obj); if (N==0) return("empty vector")
		v.vec=obj[getn(n,N,last,random,...)]
		return(v.vec) }
	showTab=function(obj,n,last=FALSE,random=FALSE,...){
		N=nrow(obj); if (N==0) return("empty table")
		v.tab=obj[getn(n,N,last,random,...),]
		return(v.tab) }
	showLis=function(obj,n,last=FALSE,random=FALSE,...){
		nL=length(obj); if (nL==0) return("empty list")
		v.lis=list()
		if (is.null(names(obj))) ii=1:nL else ii=names(obj)
		for (i in 1:nL) {
			iobj=obj[[i]]
			if (is.data.frame(iobj) || is.matrix(iobj))
				v.lis=c(v.lis,list(showTab(iobj,n,last,random,...)))
			else if (is.list(iobj))
				v.lis=c(v.lis,list(showLis(iobj,n,last,random,...)))
			else if (is.vector(iobj) || is.integer(obj) || is.numeric(obj) || is.character(obj))
				v.lis=c(v.lis,list(showVec(iobj,n,last,random,...)))
			else  v.lis=c(v.lis,list(showAll(iobj))) 
		}
		names(v.lis)=ii; return(v.lis) }
	showAll=function(obj){
		return(obj) }
	if (n==0) return("nada")
	n=abs(n) # coerce to positive
	if (is.data.frame(obj) || is.matrix(obj)) 
		viewed=showTab(obj,n,last,random,...)
	else if (is.list(obj)) 
		viewed=showLis(obj,n,last,random,...)
	else if (is.vector(obj) || is.integer(obj) || is.numeric(obj) || is.character(obj)) 
		viewed=showVec(obj,n,last,random,...)
	else viewed=showAll(obj)
	print(viewed); invisible(viewed)
}
#---------------------------------------------view

#calcGM---------------------------------2006-08-28
# Calculates the geometric mean of a vector of numbers
# Return the geometric mean of a vector of numbers
# Arguments:
#  x      - Vector of numbers
#  offset - Added value to validate zeroes
#  exzero - If TRUE, exclude zeroes
#----------------------------------------------JTS
calcGM <- function (x, offset = 0, exzero = TRUE) {
	x <- x[!is.na(x)]
	if (exzero) 
		x <- x[x > 0 & !is.na(x)]
	n <- length(x)
	if (n == 0) 
		return(0)
	x <- x + offset
	g <- exp(mean(log(x)))
	return(g)
}

#pause----------------------------------2006-08-28
# Pause, typically between graphics displays
# Arguments:
#  s  - string to display to user
#----------------------------------------------JTS
pause <- function (s = "Press <Enter> to continue") {
	cat(s)
	readline()
	invisible()
}


#calcFib--------------------------------2006-08-28
# Calculate a vector containing fibonacci numbers
# Arguments:
#  len    - return the last "len" calculated numbers 
#  n      - calculate the nth number
#  method - use .C, .Call, R code, or closed form
#----------------------------------------------JTS
calcFib <- function(n, len=1, method="C") {
	if (n<0)
		return(NA)
	if (len>(n+1))
		len <- (n+1)

	switch(casefold(method),
	       c=.fibC(n,len),
	       call=.fibCall(n,len),
	       r=.fibR(n,len),
	       closed=.fibClosedForm(n,len)
	       )
}

.fibCall <- function(n, len=1) {
	retArr <- numeric(len)
	out <- .Call("fibonacci2", as.integer(n), as.integer(len), PACKAGE="PBSmodelling")
	return(out) }

.fibC <- function(n, len=1) {
	retArr <- numeric(len)
	out <- .C("fibonacci", as.integer(n), as.integer(len), as.numeric(retArr), PACKAGE="PBSmodelling")
	x <- out[[3]]
	return(x) }

.fibR <- function(n, len=1) {
	retArr <- numeric(len)
	xa <- 0; xb <- 1;
	for(i in 0:n) {
		#init conds: fib(0)=0, fib(1)=1
		if (i <= 1) { xn <- i }
		#fib(n)=fib(n-1)+fib(n-2)
		else {
			xn <- xa+xb; xa <- xb; xb <- xn }
		## save results if iteration i is within the 
		## range from n-len to n
		j <- i - n + len;
		if (j>0) retArr[j] <- xn
	}
	return(retArr) }

.fibClosedForm <- function(n, len=1) {
	n <- (n-(len-1)):n
	phi <- (1+sqrt(5))/2
	return(round((phi^n - (1-phi)^n)/sqrt(5))) }
#---------------------------FIB functions finished

#findPat--------------------------------2006-08-28
# Searches all patterns in pat from vec, and returns the
# matched elements in vec.
# Arguments:
#  pat - character vector of patterns to match in vec.
#  vec - character vector where matches are sought.
#-----------------------------------------------RH
findPat <- function (pat, vec) {
	n <- length(vec)
	z <- NULL
	for (xstr in pat) {
		a <- regexpr(xstr, vec)
		b <- (1:n)[a > 0]
		z <- union(z, b) }
	found <- vec[z]
	return(found)
}

#testWidgets----------------------------2006-08-28
# Display a "master" GUI that displays other sample GUIs
#-----------------------------------------------RH
testWidgets <- function () {
	.testWidHelper <- function() {
		getWinVal(scope="L");
		if (getWinAct()[1]=="__USE_EDIT__") {
			if (wtxt=="\n" || wtxt=="No widgets displayed\n")
				return()
			winDesc <- strsplit(wtxt, "\n")[[1]]
			createWin(winDesc, astext=TRUE)
			return()
		}
		if (wN==0) {
			wtxt <- "No widgets displayed";
			closeWin(name="widWin");
		}
		else {
			pckg <- "PBSmodelling"; dnam <- "testWidgets";
			act  <- getWinAct()[1];
			wtmp <- paste(dnam,"/",act,".txt",sep="");
			wnam <- system.file(wtmp,package=pckg)
			wtxt <- paste(readLines(wnam),collapse="\n");
			createWin(wnam);
		}
		setWinVal(list(wtxt=wtxt), winName="testW");
	}
	assign(".testWidHelper",.testWidHelper,envir=.GlobalEnv)
	pckg <- "PBSmodelling"; dnam <- "testWidgets";
	wtmp <- paste(dnam,"/","testWidgetWin.txt",sep="");
	wnam <- system.file(wtmp,package=pckg)
	createWin(wnam);
}

#.viewPkgDemo---------------------------2009-03-04
# Display a GUI to display something equivalent to R's demo()
#-------------------------------------------ACB/RH
.viewPkgDemo <- function() {
	act <- getWinAct()[1]
	if (act=="pkg") {
		package=getWinVal("pkg")$pkg
		eval(parse(text=paste("OK=require(",package,",quietly=TRUE)",sep="")))
		if (!OK) {
			mess=paste(package,"package is not available")
			showAlert(mess); stop(mess) }
		return(runDemos(package)) }
	if (act=="demo") {
		demo <- getWinVal("demo")$demo
		source(demo, echo=TRUE, max.deparse.length=100)
		return(invisible(NULL))
	}
	if (act=="source") {
		demo <- getWinVal("demo")$demo
		openFile(demo)
		return(invisible(NULL))
	}
}

.dUpdateDesc <- function() {
	demo.id <- getWinVal()$demo.id
	package <- .trimWhiteSpace( getWinVal("package")$package )
	x <- demo(package = .packages(all.available = TRUE))
	x <- x$results[x$results[,"Package"]==package,]
	if (is.null(dim(x))) {
		tmp<-names(x)
		dim(x)<-c(1,4)
		colnames(x)<-tmp
	}

	setWinVal( list( demo_desc=x[demo.id,"Title"] ) )
}
#.dClose--------------------------------2006-08-28
# Function to execute on closing runDemos().
#----------------------------------------------ACB
.dClose <- function() {
	act <- getWinAct()[1];
	closeWin();
	setwd(.dwd)
	if (is.null(act) || act=="demo") {
		remove(list = setdiff(ls(pos=1, all=TRUE), .dls), pos = 1);
		remove(list = c(".dwd", ".dls"), pos = 1); }; # final good-bye
	return(); };

#runDemos-------------------------------2009-03-04
# Display a GUI to display something equivalent to R's demo()
#-------------------------------------------ACB/RH
runDemos <- function (package) {
	if (!exists(".dwd",where=1)) assign(".dwd",getwd(),envir=.GlobalEnv)
	if (!exists(".dls",where=1)) assign(".dls",c(".dls",ls(pos = 1, all = TRUE)),envir=.GlobalEnv)
	try(closeWin(),silent=TRUE)
	x <- demo(package = .packages(all.available = TRUE))
	if (missing(package)) {
		#display a list of packages to choose from
		pkgDemo <- unique(x$results[,"Package"])
		radios <- list(list(list(type="label", text="Select a package to view available demos:",
			sticky="W",padx=12,font="bold 10")))
		i <- 3
		#create droplist labels with counts
		pkg_labels <- c()
		for(pkg in pkgDemo) {
			len <- length(x$results[,"Package"][x$results[,"Package"]==pkg])
			if (len==1)
				items <- "(1 demo)"
			else
				items <- paste("(",len," demos)", sep="")
			pkg_labels <- c( pkg_labels, paste( pkg, items ) )
		}


		radios[[ 2 ]] <- list(list(type = "droplist",
		                           name = "pkg",
		                           values = pkgDemo,
								   labels = pkg_labels,
		                           add = FALSE,
		                           mode = "character"
		                           ) )
		win <- list(title = "R Demos", windowname = "pbs.demo", onclose=".dClose", 
			.widgets = list(list(type="grid", .widgets=c( #mixing the c() and lists() become really akward - watch out!
			#the c() requires an extra level of list() which are later stripped out
			#the reason is because of the way the radios list is merged into this list
			list(list(
			list(type="label",text=paste("R Demos",paste(rep(" ", times=50),collapse="")),font="bold underline",fg="blue",padx=10,sticky="w")
			)),
			radios,
			list(list(
			list(type="button", "function"=".viewPkgDemo", action="pkg", text="View Demos", sticky="w", padx=12, bg="aliceblue")
			))
			))))
		assign("xxy",win,envir=.GlobalEnv)
		createWin(list(win))
		return(invisible(NULL))
	}
	#display demos from a certain package
	eval(parse(text=paste("OK=require(",package,",quietly=TRUE)",sep="")))
	if (!OK) {
		mess=paste(package,"package is not available")
		showAlert(mess); stop(mess) }
	x <- x$results[x$results[,"Package"]==package,]
	radios <- list(list(list(type="label", text="Select a Demo to view:", sticky="W", padx=12,font="bold 10")))

	#i <- 3
	if (is.null(dim(x))) {
		tmp<-names(x)
		dim(x)<-c(1,4)
		colnames(x)<-tmp
	}

	droplist_data <- c()
	for(j in 1:length(x[,1])) {
		demoDir <- file.path(x[j,"LibPath"], package, "demo")
		path <- tools::list_files_with_type(demoDir, "demo")
		path <- path[x[j,"Item"]==tools::file_path_sans_ext(basename(path))]
		droplist_data[ j ] <- path
	}
	titles <- x[,"Title"]
	title_cut_off <- 50 #cut off titles longer than this
	labels <- paste( x[,"Item"], " ::: ", substring( titles, 1, title_cut_off ), ifelse( nchar(titles) > title_cut_off, "...", "" ), sep="" )

	radios[[ 2 ]] <- list(list(type = "droplist",
	                           name = "demo",
	                           values = droplist_data,
	                           labels = labels,
	                           add = FALSE,
	                           mode = "character",
							   sticky = "W",
							   padx = 20,
							   width = 55,
							   "function" = ".dUpdateDesc"
	                           ) )
	radios[[ 3 ]] <- list(list(type="label",
	                           name="demo_desc",
	                           text=x[1,"Title"],
	                           sticky="w",
	                           wraplength=500,
	                           padx=20,
							   pady=c(20,0)
	                           ) )

	win <- list(title = paste("R Demos:", package), windowname = "pbs.demo", onclose=".dClose",
		.widgets = list(list(type="grid", .widgets=c(
			list(list(
				list(type="label",name="package",text=paste(package,paste(rep(" ",times=100),collapse="")),
					font="bold underline",fg="red3",sticky="W")
			)),
			radios,
			list(list(list(type="null", pady=4))),
				list(list(
					list(type="grid", sticky="w", pady=3, .widgets=
						list(
							list(
								list(type="button", "function"=".viewPkgDemo", action="demo", text="Run Demo", sticky="w", padx=12, bg="greenyellow"),
								list(type="button", "function"=".viewPkgDemo", action="source", text="View Source", sticky="w", padx=12),
								list(type="button", "function"="runDemos", action="", text="All Packages", sticky="w", padx=12)
							)
						)
					)
				))
			)
		)))
	assign("xx",win,envir=.GlobalEnv)
	createWin(list(win))
	return(invisible(NULL))
}
#-----------------------------------------runDemos

#showHelp-------------------------------2008-05-29
# Show help files for package contents as HTML in browser window
#-----------------------------------------------RH
showHelp <- function(pat="methods") {
	warn <- options()$warn
	options(warn = -1)
	Apacks = .packages(all.available = TRUE) # all packages
	Spacks = findPat(pat,Apacks)             # show packages that match the pattern
	npacks = length(Spacks)
	if (npacks==0) { print("No such package"); return() }
	getURL = function(x) {
		path=system.file(package=x)
		url=paste(path,"/html/00Index.html",sep="")
		return(url) }
	URLs=sapply(Spacks,getURL)
	openFile(URLs)
	options(warn = warn)
	invisible(list(Apacks=Apacks,Spacks=Spacks,URLs=URLs))
}

#showVignettes--------------------------2008-07-10
# Display a GUI to display something equivalent to R's vignette()
# Arguments: package = string specifying a package name.
#-----------------------------------------------AE
showVignettes <- function (package) {
	if (!exists(".dwd",where=1)) assign(".dwd",getwd(),envir=.GlobalEnv)
	if (!exists(".dls",where=1)) assign(".dls",c(".dls",ls(pos = 1, all = TRUE)),envir=.GlobalEnv)
	closeWin();
	x <- vignette()
	if (missing(package)) {
		#display a list of packages to choose from
		pkgVignette <- unique(x$results[,"Package"])
		radios <- list(list(list(type="label", text="Select a package to view available vignettes.", sticky="w", padx=12)))
		i <- 2
		for(pkg in pkgVignette) {
			len <- length(x$results[,"Package"][x$results[,"Package"]==pkg])
			if (len==1)
				items <- "(1 vignette)"
			else
				items <- paste("(",len," vignettes)", sep="")
			radios[[i]] <- list(list(type="radio",
			                    name="pkg",
			                    value=pkg,
			                    text=paste(pkg, items),
			                    mode="character",
			                    sticky="w",
			                    padx=12))
			i <- i+1
		}
		win <- list(title = "R Vignettes", windowname = "pbs.vignette", onclose=".dClose",
			.widgets = list(list(type="grid", .widgets=c(

			list(list(
			list(type="label", text=paste("R Vignettes", paste(rep(" ", times=100), collapse="")), font="bold underline", fg="red3", padx=10, sticky="w")
			)),
			radios,
			list(list(
			list(type="button", "function"=".viewPkgVignette", action="pkg", text="View Vignettes", sticky="w", padx=12)
			))
			))))
		assign("xxy",win,envir=.GlobalEnv)
		createWin(list(win))
		return(invisible(NULL))
	}
	#display vignettes from a certain package
	x <- x$results[x$results[,"Package"]==package,]
	radios <- list(list(list(type="label", text="Select a Vignette to view.", sticky="w", padx=12)))
	i <- 2
	if (is.null(dim(x))) {
		tmp<-names(x)
		dim(x)<-c(1,4)
		colnames(x)<-tmp
	}
	for(j in 1:length(x[,1])) {
		vignetteDir <- file.path(x[j,"LibPath"], package, "doc")
		path <- tools::list_files_with_type(vignetteDir, "vignette")
		path <- path[x[j,"Item"]==tools::file_path_sans_ext(basename(path))]

		if (length(path)==0)
			stop("error - could not find the path for vignette - this is most likely a bug!")
		radios[[i]] <- list(list(type="radio",
		                    name="vignette",
		                    value=path,
		                    text=x[j,"Item"],
		                    mode="character",
		                    font="underline",
		                    sticky="w",
		                    padx=12))
		i <- i+1
		radios[[i]] <- list(list(type="label",
		                    text=x[j,"Title"],
		                    sticky="w",
		                    wraplength=500,
		                    padx=20
		                    ))
		i <- i+1
	}
	win <- list(title = paste("R Vignettes:", package), windowname = "pbs.vignette", onclose=".dClose",
		.widgets = list(list(type="grid", .widgets=c(
			list(list(
				list(type="label", text=paste(package, paste(rep(" ", times=100), collapse="")), font="bold underline", fg="red3", sticky="w")
			)),
			radios,
			list(list(list(type="null", pady=4))),
				list(list(
					list(type="grid", sticky="w", pady=3, .widgets=
						list(
							list(
								list(type="button", "function"=".viewPkgVignette", action="vignette", text="View Vignette", sticky="w", padx=12),
								list(type="button", "function"=".viewPkgVignette", action="source", text="View Source", sticky="w", padx=12),
								list(type="button", "function"="showVignettes", action="", text="All Packages", sticky="w", padx=12)
							)
						)
					)
				))
			)
		)))
	assign("xx",win,envir=.GlobalEnv)
	createWin(list(win))
	return(invisible(NULL))
}
#-------------------------------------showVignettes

#.viewPkgVignettes----------------------2008-07-10
# Display a GUI to display something equivalent to R's vignette()
#-----------------------------------------------AE
.viewPkgVignette <- function() {
	act <- getWinAct()[1]
	if (act=="pkg")
		return(showVignettes(getWinVal("pkg")$pkg))
	vignette <- getWinVal("vignette")$vignette
	if (act=="vignette")
		 vignette <- paste(tools::file_path_sans_ext(vignette), ".pdf", sep="")
	openFile(vignette)
	return(invisible(NULL))
}


#showRes--------------------------------2008-04-29
# Show results of the calculation in string x
#----------------------------------------------JTS
showRes <- function(x, cr=TRUE, pau=TRUE) {
  cat(">",x," "); if(cr==TRUE) cat("\n");
  if(pau) pause("...") else cat("...\n");
  xres <- eval(parse(text=x));
  print(xres); 
  cat("************** Results shown above **************\n\n");
  if(pau) pause();
  invisible(xres); };

#clearAll-------------------------------2008-08-28
# Remove all data in the global environment
# Arguments:
#  hidden  - if T remove all variables including dot variables
#  verbose - list all removed variables
#  PBSsave - if TRUE, do not remove .PBSmod
#-------------------------------------------ACB/AE
clearAll <- function(hidden=TRUE, verbose=TRUE, PBSsave=TRUE) {
	objs <- ls(all.names = TRUE, pos = ".GlobalEnv")
	if (verbose && length(objs))
		print(objs)
	rmlist <- ls(all.names = hidden, pos = ".GlobalEnv")
	if(PBSsave)
		rmlist=rmlist[rmlist!=".PBSmod"]
	rm(list = rmlist, pos = ".GlobalEnv")
	if (verbose) {
		cat("Removed:\n")
		if (length(rmlist))
			print(rmlist)
		else
			cat("\n")

		cat("Remaining:\n")
		if (length(ls(all.names = TRUE, pos = ".GlobalEnv")))
			print(ls(all.names = TRUE, pos = ".GlobalEnv"))
		else
			cat("\n")
	}
	invisible() }
#-----------------------------------------clearAll

#runExamples----------------------------2008-09-08
# Display a master GUI to display examples
#-------------------------------------------RH/ACB
runExamples <- function () {
	allWin=c("runE","window","widWin","testW","choisir","Swiss") # all potential windows open
	.runExHelper <- function() {
		getWinVal(scope = "L")
		act <- getWinAct()[1]
		if (!exists("act") || !exists("eN")) return()
		if (act == "quit") { 
			.runExHelperQuit()
		} else if (act=="clear") {
			wtxt <- "No examples chosen"
			closeWin(name=setdiff(allWin,"runE"))
		} else if (act == "__USE_EDIT__") {
			if (wtxt == "\n" || wtxt == "No examples chosen\n") return()
			winDesc <- strsplit(wtxt, "\n")[[1]]
			createWin(winDesc, astext = TRUE)
			return()
		} else if (act=="swissTalk") {
			closeWin(name=setdiff(allWin,"runE"))
			tnam=paste(act,".txt",sep="") # talk description file
			wtxt <- paste(readLines(tnam), collapse = "\n")
			presentTalk(tnam)
		} else {
			if (act!="TestFuns")
				closeWin(name=setdiff(allWin,c("runE","window")))
			source(paste(act, ".r", sep = ""))
			wnam <- paste(act, "Win.txt", sep = "") # window description file
			wtxt <- paste(readLines(wnam), collapse = "\n")
		}
		setWinVal(list(wtxt=wtxt), winName="runE")
	}
	.runExHelperQuit <- function() {
		closeWin(name=allWin)
		setwd(.cwd)
		remove(list = setdiff(ls(pos = 1), .cls), pos = 1)
		return()
	}
	assign(".runExHelper",.runExHelper,envir=.GlobalEnv)
	assign(".runExHelperQuit",.runExHelperQuit,envir=.GlobalEnv)
	assign(".cls",ls(pos = 1, all = TRUE),envir=.GlobalEnv)
	assign(".cwd",getwd(),envir=.GlobalEnv)
	pckg <- "PBSmodelling"
	dnam <- "examples"
	rdir <- system.file(package = pckg)
	wdir <- paste(rdir, "/", dnam, sep = "")
	fnam <- paste(wdir, list.files(wdir), sep = "/")
	rtmp <- tempdir()
	file.copy(fnam, rtmp, overwrite = TRUE)
	setwd(rtmp)
	createWin("runExamplesWin.txt")
	msg <- paste("Examples are running in ", rtmp, sep = "")
	setWinVal(list(wtxt = msg), winName = "runE") 
	invisible() }
#--------------------------------------runExamples

#isWhat---------------------------------2008-09-08
# Prints the class, mode, type, and attributes
# of the given object x.
#----------------------------------------------JTS
isWhat <- function(x) {
  cat("class: "); print(class(x));
  cat("mode:  "); print(mode(x));
  cat("type:  "); print(typeof(x));
  att=attributes(x)
  cat(paste("attributes:",ifelse(is.null(att)," NULL\n","\n"),sep=""))
  if (!is.null(att)) print(att)
  invisible() }

#openFile-------------------------------2008-09-22
# Opens a file for viewing based on System file
# extension association or .PBSmod$.options$openfile
#----------------------------------------------ACB
openFile <- function(fname="") {
	.openFile=function(fname) {
		if (!exists(".PBSmod"))  .initPBSoptions()
		if (fname=="")  fname=getWinAct()[1]
		if (any(grep("^~", fname)))
			fname <- path.expand(fname)
		else if (!any(grep("^([a-z]:(\\\\|/)|\\\\\\\\|/)", fname, ignore.case = TRUE)))
			fname <- paste(getwd(), "/", fname, sep="")
		if (!file.exists(fname))
			stop(paste("File \"", fname, "\" does not exist", sep=""))

		ext <- sub("^.*\\.", "", fname)
		if ( is.null( .PBSmod$.options$openfile[[ ext ]] ) ) {
			if (!exists("shell.exec", mode="function")) 
				stop(paste("There is no program associated with the extension '", ext, "'\n",
				           "Please set an association with the setPBSext command\n"))
			shell.exec(fname); return(fname)
		} else {
			cmd <- getPBSext(ext)
			cmd <- gsub("%f", fname, cmd)
			if (.Platform$OS.type=="windows")
				shell(cmd,wait=FALSE)
			else
				system(cmd,wait=FALSE)
			return(cmd)
		}
	}
	ops=sapply(fname,.openFile)
	invisible(ops) }
#-----------------------------------------openFile

#setPBSoptions--------------------------2008-10-02
# Change user options. Arguments:
#   option - name of option to change
#   value  - new value of option
# AE: Now a value '.PBSmod$.options$.optionsChanged' is set to TRUE when an option is changed,
#   so that the user doesn't always have to be prompted to save the options file.
#   By default, '.PBSmod$.options$.optionsChanged' is not set or NULL.
#   Also, if an option is set to "" or NULL then it is removed.
#   '.initPBSoptions()' is now called first (options starting with a dot "." do not set '.optionsChanged').
# RH: if the value is a sublist of an option, it can be changed individually using 'sublist=TRUE'.
#----------------------------------------ACB/AE/RH
setPBSoptions <- function(option, value, sublist=FALSE) {
	.initPBSoptions()
	if(!is.null(value) && length(value)==1 && value=="") value=NULL
 	if(substr(option, 1, 1)!="." && !identical(.PBSmod$.options[[option]], value))
		packList(".optionsChanged",".PBSmod$.options",TRUE) #.PBSmod$.options$.optionsChanged<<-TRUE
	if(is.null(value) && !sublist)
		.PBSmod$.options <<- .removeFromList(.PBSmod$.options, option)
	else{
		if(is.list(value) && sublist){
			for (i in 1:length(value)){
				ii=names(value[i]); if (ii=="") next
				ival=value[[i]]
				txt=paste(".PBSmod$.options$",option,ifelse(ii=="","","$"),ii," <<- ival",sep="")
				eval(parse(text=txt))
			}
		}
		else .PBSmod$.options[[option]] <<- value
	}
}
#------------------------------------setPBSoptions

#.removeFromList------------------------2008-10-06
# Remove items from a list.
#--------------------------------------------AE/RH
.removeFromList = function (l, items) {
	if (!length(l) || !length(items))  return(l)
	keep = l[!is.element(names(l),items)]
	return(keep) }

#.initPBSoptions------------------------2009-07-20
# Called from zzz.R's .First.lib() intialization function
#-----------------------------------------------AE
.initPBSoptions <- function() {
	if (!exists(".PBSmod"))
		assign(".PBSmod",list(),envir=.GlobalEnv) #.PBSmod <<- list()
	if (is.null(.PBSmod$.options))
		packList(".options",".PBSmod",list()) #.PBSmod$.options <<- list()
	if (is.null(.PBSmod$.options$openfile))
		packList("openfile",".PBSmod$.options",list()) #.PBSmod$.options$openfile <<- list()
	grDevices::dev.new(); par.default=graphics::par(no.readonly=TRUE); grDevices::dev.off()
	packList("par.default",".PBSmod$.options") # for resetGraph
}

.checkParDefault <- function() {
	if (!exists(".PBSmod") || is.null(.PBSmod$.options$par.default))
		.initPBSoptions()
}

#getPBSoptions--------------------------2006-09-16
# Retrieve a user option.  Argument:
#   option - name of option to retrieve
#----------------------------------------------ACB
getPBSoptions <- function(option) {
	if (missing(option))
		return(.PBSmod$.options)
	return(.PBSmod$.options[[option]])
}

#getPBSext------------------------------2006-09-16
# Retrieve previously saved command.  Argument:
#  ext - file extension
#----------------------------------------------ACB
getPBSext <- function(ext) {
	if (!exists(".PBSmod"))
		stop(".PBSmod was not found")
	if (missing(ext))
		return(.PBSmod$.options$openfile)
	if ( is.null( .PBSmod$.options$openfile[[ ext ]] ) )
		return(NULL)
	return(.PBSmod$.options$openfile[[ext]])
}

#setPBSext------------------------------2008-07-31
# Associate a new command with file types;
#  use "%f" in cmd to designate where the filename will be placed.
# AE: Added the setting of 'optionsChanged'
# Arguments:
#  ext - file extension
#  cmd - cmd to open these types of files
#-------------------------------------------ACB/AE
setPBSext <- function(ext, cmd) {
	if (!exists(".PBSmod")) 
		stop(".PBSmod was not found")
	if (!any(grep("%f", cmd)))
		stop(paste("No %f was found in supplied command \"", cmd, 
		           "\".\n%f must be used to indicate where the filename will ",
		           "be inserted by openfile().\n",
		           "Did you mean \"", cmd, " %f\"?", sep=""))
		           
	if(is.null(.PBSmod$.options$openfile[[ext]]) ||
			.PBSmod$.options$openfile[[ext]]!=cmd)
		packList(".optionsChanged",".PBSmod$.options",TRUE) #.PBSmod$.options.optionsChanged<<-TRUE
		
	.PBSmod$.options$openfile[[ext]] <<- cmd
}
#----------------------------------------setPBSext

#clearPBSext----------------------------2008-07-21
# Disassociate any number of file extensions from commands
#  previously save with setPBSext.
# Argument:
#  ext - optional character vector of file extensions to
#        clear; if unspecified, all associations are removed
#-----------------------------------------------AE
clearPBSext=function(ext){
  .initPBSoptions()
  if(missing(ext))
    packList("openfile",".PBSmod$.options",list()) #.PBSmod$.options$openfile<<-list()
  else{
    oldLen=length(.PBSmod$.options$openfile)
    .PBSmod$.options$openfile<<-.removeFromList(.PBSmod$.options$openfile, ext)
    if(oldLen!=length(.PBSmod$.options$openfile))
      packList(".optionsChanged",".PBSmod$.options",TRUE) #.PBSmod$.options$.optionsChanged<<-TRUE
  }
}

#writePBSoptions------------------------2006-09-16
# Save PBS options to a text file
#  fname - name of options file (or path to this file)
#----------------------------------------------ACB
writePBSoptions=function(fname="PBSoptions.txt") {
  .initPBSoptions()
  if(fname!="PBSoptions.txt")
    packList(".optionsFile",".PBSmod$.options",fname) #.PBSmod$.options$.optionsFile<<-fname
    
	packList(".optionsChanged",".PBSmod$.options",NULL) #.PBSmod$.options$.optionsChanged<<-NULL
	
	saveOpt=.PBSmod$.options[-grep("^[.]", names(.PBSmod$.options))]
  writeList(saveOpt, fname)
}

#readPBSoptions-------------------------2006-09-16
# Load PBS options from a text file. The loaded options will
# overwrite existing ones in memory; however, an existing
# option in memory will not be cleared if this option does
# not exist in the options file.
# Input:
#  fname - name of options file (or path to this file)
# Output:
#  returns FALSE if file did not exist or if read failed
#  otherwise returns TRUE
#----------------------------------------------ACB
readPBSoptions=function(fname="PBSoptions.txt"){
  .initPBSoptions()
     
  optList=try(readList(fname), silent=TRUE)
  if(class(optList)=="try-error")
    return(FALSE)
    
  .PBSmod$.options<<-.mergeLists(.PBSmod$.options, optList)
  if(fname!="PBSoptions.txt")
    packList(".optionsFile",".PBSmod$.options",fname) #.PBSmod$.options$.optionsFile<<-fname
  packList(".optionsChanged",".PBSmod$.options",NULL) #.PBSmod$.options$.optionsChanged<<-NULL
}

#.forceMode-----------------------------2009-02-11
# Forces a variable into a mode without showing any warnings
# Arguments:
#   x    - variable to convert
#   mode - mode to convert to
#-----------------------------------------------RH
.forceMode <- function(x, mode) {
	ex1=paste("xcon=as.",mode,"(x)",sep="")          # explicit conversion
	ex2=paste("xcon=",paste(x,collapse=""),sep="")   # character representation of object
	warn=options()$warn; options(warn=-1)
	try(eval(parse(text=ex1)),silent=TRUE)
	if (!exists("xcon",envir=environment()) || all(is.na(xcon)))
		try(eval(parse(text=ex2)),silent=TRUE)
	if (!exists("xcon",envir=environment()) || all(is.na(xcon))) xcon=x
	options(warn=warn)
	return(xcon) }

#convSlashes---------------------------2009-02-16
# Convert unix "/" to R's "\\" if OS is windows.
#-----------------------------------------------RH
convSlashes=function(expr, os=.Platform$OS.type, addQuotes=FALSE){
	if (os=="windows") 
		expr=gsub("/","\\\\",expr)
	else 
		expr=gsub("\\\\","/",expr)
	if (addQuotes) expr=paste("\"",expr,"\"",sep="")
	return(expr) }

#showPacks------------------------------2009-02-17
# Show packages that need to be installed.
#-----------------------------------------------RH
showPacks = function(packs=c("PBSmodelling","PBSmapping","PBSddesolve",
            "rgl","deSolve","akima","deldir","sp","maptools","KernSmooth")) {
	warn <- options()$warn
	options(warn = -1)
	Apacks = .packages(all.available = TRUE)   # all packages
	Ipacks = sort(findPat(packs,Apacks))       # installed packages that match those required
	Mpacks = sort(setdiff(packs,Ipacks))       # missing packages
	if (length(Mpacks)==0)
		showAlert("Congratulations! All specified pakages installed.","Package Check","info")
	else {
		mess=paste("You need to install these packages:\n     ",paste(Mpacks,collapse="\n     "),sep="")
		showAlert(mess,"Package Check","warning")
	}
	options(warn = warn)
	invisible(list(Apacks=Apacks,Ipacks=Ipacks,Mpacks=Mpacks)) }
#----------------------------------------showPacks

#evalCall-------------------------------2009-03-03
# Evaluates a function call, resolving conflicting arguments.
#-----------------------------------------------RH
evalCall=function(fn,argu,...,envir=parent.frame(),checkdef=FALSE,checkpar=FALSE){
	fnam=as.character(substitute(fn))
	fnam.def=paste(fnam,"default",sep=".")
	base=formals(fnam)
	if (checkdef && exists(fnam.def)) {
		defs=formals(fnam.def)
		udefs=defs[setdiff(names(defs),names(base))]
		base=c(base,udefs) }
	base=base[setdiff(names(base),"...")]
	if (checkpar) {
		pars=par(); pars=pars[setdiff(names(pars),names(base))] # use only pars not in base
		forms=c(base,pars) } # all possible formal arguments
	else forms=base
#if (fnam=="axis") {browser();return()}
	#forms=NULL
	#for (i in names(base)) forms[[i]]=get(i,envir=envir)
	dots=list(...)
	argus=argu[setdiff(names(argu),names(dots))]
	given=c(argus,dots)
	allow=given[intersect(names(given),names(forms))]
	strargs=sapply(allow,deparse,width.cutoff=500,simplify=FALSE)
	strargs=sapply(strargs,paste,collapse="",simplify=FALSE) # collapse multiple strings
	argspec=character(0)
	for (i in names(strargs)) argspec=c(argspec,paste(i,"=",strargs[[i]]))
	expr=paste(fnam,"(",paste(argspec,collapse=","),")",sep="")
	eval(parse(text=expr)) 
	invisible(expr) }
#-----------------------------------------evalCall

#getPrefix------------------------------2009-02-23
# Search for and gather all prefixes with the specified suffix.
#-----------------------------------------------RH
getPrefix=function(suffix,path="."){
	spat=gsub("\\.","\\\\\\.",suffix)
	sfiles=list.files(path,pattern=paste(spat,"$",sep=""),ignore.case=TRUE)
	pref=substring(sfiles,1,nchar(sfiles)-nchar(suffix))
	return(pref) }

#getSuffix------------------------------2009-02-23
# Search for and gather all suffixes with the specified prefix.
#-----------------------------------------------RH
getSuffix=function(prefix,path="."){
	ppat=gsub("\\.","\\\\\\.",prefix)
	pfiles=list.files(path,pattern=paste("^",ppat,sep=""),ignore.case=TRUE)
	pref=substring(pfiles,nchar(prefix)+1)
	return(pref) }

.findSquare=function(nc) {
	sqn=sqrt(nc); m=ceiling(sqn); n=ceiling(nc/m)
	return(c(m,n)) }

#viewCode-------------------------------2009-04-21
# View package R code on the fly.
#-----------------------------------------------RH
viewCode=function(pkg="PBSmodelling", funs){
	eval(parse(text=paste("if(!require(",pkg,",quietly=TRUE)) stop(\"",pkg," package is required\")",sep="")))
	tdir <- tempdir(); tdir <- gsub("\\\\","/",tdir)                    # temporary directory for R
	if (is.element(pkg,loadedNamespaces())){
		penv=asNamespace(pkg); pkgOb=ls(penv,all.names=TRUE)             # objects in package including those in namespace
		bad=regexpr(".__",pkgOb); pkgOb=pkgOb[bad<0]                     # get rid of names starting with ".__"
		delim=":::" }
	else {
		pkgOb=ls(paste("package:",pkg,sep=""),all.names=TRUE)            # package objects
		delim="::" }
	bad=regexpr("[>=~@:/&%!\\|()[{^$*+?<-]",pkgOb); pkgOb=pkgOb[bad<0]  # get rid of weird names
	z=sapply(pkgOb,function(x){
		if (is.element(x,c("break","for","function","if","next","repeat","while"))) return(FALSE) # special words in pkg 'base'
		eval(parse(text=paste("is.function(",paste(pkg,x,sep=delim),")",sep=""))) } )
	pkgF=names(z)[z]                                                    # package functions
	if (length(pkgF)==0) {showAlert(paste(pkg,"has no recognizable functions")); return()}
	dots=regexpr("^\\.",pkgF); pkgF0=pkgF[dots==1]; pkgF1=pkgF[dots!=1]
	code=c(paste("#",pkg,"Functions"),paste("#",paste(rep("-",nchar(pkg)+10),collapse="")))
	pkgFuns=c(pkgF1,pkgF0)
	if (missing(funs)) funs=pkgFuns
	if (is.null(funs) || is.na(funs) || !is.character(funs) || all(funs=="")) {
		showAlert("Your choice for 'funs' is badly specified")
		return(invisible("Error: 'funs' badly specified")) }
	seeFuns=pkgFuns[is.element(pkgFuns,funs)]
	if (length(seeFuns)==0) {
		showAlert("Your choices yield no functions")
		return(invisible("Error: choices for 'funs' yield no functions")) }
	for (i in c(seeFuns)) {
		expr=paste("fun=deparse(",pkg,delim,i,"); fun[1]=paste(\"",i,"\",fun[1],sep=\" = \",collapse=\"\"); code=c(code,fun)",sep="")
		eval(parse(text=expr)) }
	fname=paste(tdir,"/",pkg,".r",sep="")
	writeLines(code, fname); openFile(convSlashes(fname))
	invisible(code) }
#----------------------------------------viewCode

#clipVector-----------------------------2007-06-09
# Clip a vector at one or both ends of a 
# specified value or string
#-----------------------------------------------RH
clipVector <- function (vec,clip,end=0) {
	#clip=as.character(substitute(clip))
	if (is.null(names(vec))) names(vec) <- 1:length(vec)
	if (any(end==c(0,1))) { # clip vector from the front
		znot = !vec%in%clip
		zzz  = cumsum(znot)
		z1   = match(1,zzz)
		vec  = vec[z1:length(vec)] }
	if (any(end==c(0,2))) { # clip vector from the end
		vrev = rev(vec)
		znot = !vrev%in%clip
		zzz  = cumsum(znot)
		z1   = match(1,zzz)
		vrev = vrev[z1:length(vrev)]
		vec  = rev(vrev) }
	return(vec)
}

#focusRcon------------------------------2009-05-14
# Give the R console focus.
#-----------------------------------------------NO
focusRcon <- function(os=.Platform$OS.type) {
	if (os!="windows") {
		err="'focusRcon' needs Windows OS to use Windows Scripting"
		cat(err,"\n"); return(invisible(err)) }
	tdir <- tempdir()
	fname <- paste(tdir, "\\focusRcon.vbs", sep="")
   cmd <- 'Set w = CreateObject("WScript.Shell"): w.AppActivate("R Console")'
   cat(cmd, file=fname)
	system(paste("cscript //NoLogo", fname), minimized=TRUE)
	invisible(fname) }
	
#clearRcon------------------------------2009-05-14
# Clear the R console display.
#-----------------------------------------------NO
clearRcon <- function(os=.Platform$OS.type) {
	if (os!="windows") {
		err="'clearRcon' needs Windows OS to use Windows Scripting"
		cat(err,"\n"); return(invisible(err)) }
	tdir <- tempdir()
	fname <- paste(tdir, "\\clearRcon.vbs", sep="")
	cat('Dim pShell\n', file=fname)
	cat('Set pShell = CreateObject("WScript.Shell")\n', file=fname, append=TRUE)
	cat('pShell.AppActivate "R Console"\n', file=fname, append=TRUE)
	cat('pShell.SendKeys "^L"\n', file=fname, append=TRUE)
	cat('Set pShell = Nothing\n', file=fname, append=TRUE)
	system(paste("cscript //NoLogo", fname), minimized=TRUE)
	invisible(fname) }


