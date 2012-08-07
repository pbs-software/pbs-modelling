#writeList------------------------------2009-02-05
# Writes a list to a file in "D" or "P" format.
# Arguments:
#  x        - list to save
#  fname    - file to write list to
#  format   - write list in "D" or "P" format
#  comments - include string as comment at the top of file
#-------------------------------------------ACB/RH
writeList <- function(x, fname="", format="D", comments="") {
	NoComments<-missing(comments)
	comments <- sub("^", "#", comments)
	if (format=="D") {
		dput(x, fname)
		if (file.exists(fname) && !NoComments) {
			output <- scan(fname, what=character(0), sep="\n")
			output <- paste(output, collapse="\n")
			sink(fname)
				#add comments
				if (all(comments!="#"))
					cat(paste(comments, collapse="\n")); cat("\n");
				#spit out original output from dput
				cat(output); cat("\n")
			sink()
		}
		return(fname)
	}
	if (format=="P") {
		if (!is.list(x) || !length(x))
			stop("x must be a non-empty list.")
		.writeList.P(x, fname, comments)
		return(fname)
	}
	stop(paste("format \"",format,"\" not recognized."))
}

#.writeList.P---------------------------2009-02-10
# Saves list x to disk using "P" format
#-------------------------------------------ACB/RH
.writeList.P <- function( x, fname="", comments) {
	if (fname!="") sink(fname)
	if (!missing(comments))
		cat(paste(comments,collapse="\n")); cat("\n")
	xNames=names(x); z=is.element(xNames,"")
	if (any(z)) xNames[z]=names(x)[z]=paste("X",1:sum(z),sep="")
	#check for errors
	for(i in 1:length(x)) {
		ii=xNames[i]; iii=x[[i]]
		if (!is.vector(iii) && !is.matrix(iii) && class(iii)!="data.frame") next
			#stop("writelist can only support modes of vector, matrix, and dataframes.")
		#prepare character strings with quotes if spaces exist
		if (is.character(iii) && length(iii)>0) {
			for(j in 1:length(iii)) {
				#only strings with spaces need quotes
				if (typeof(iii[j])=="character") {
					x[[i]][j] <- .addslashes(iii[j])
				}
			}
		}
	}
	#start cat-ing keys and values
	for(i in 1:length(x)) {
		ii=xNames[i]; iii=x[[i]]
		if (!is.vector(iii) && !is.matrix(iii) && class(iii)!="data.frame" && !is.array(iii)) next
		#print varName
		cat(paste("$", ii, "\n", sep=""))
		if (is.vector(iii)) {
			#print names
			vecNames<-names(iii)
			if (is.null(vecNames)) vecNames=""
			vecNames <- .addslashes(vecNames)
			cat(paste("$$vector mode=\"", typeof(iii), "\" names=", vecNames, "\n", sep=""))
			cat(iii); cat("\n")
		}
		else if( is.matrix( iii ) ) {
			#print colnames
			matColNames <- colnames( iii )
			matRowNames <- rownames( iii )
			if (is.null(matColNames))
				matColNames <- ""
			if (is.null(matRowNames))
				matRowNames <- ""
			matColNames <- .addslashes(matColNames)
			matRowNames <- .addslashes(matRowNames)
			cat(paste("$$matrix mode=\"", typeof( iii ), "\" rownames=", matRowNames, " colnames=", matColNames, " ncol=", ncol( iii ), "\n", sep=""))

			for(j in 1:dim(x[[i]])[1]) {
				cat(iii[j,]); cat("\n")
			}
			
		}
		else if ( is.array(iii) ) {


			d=dim(iii); nr=d[1]; nc=d[2]; nd=length(d) # dimension info

			#get the dimensional names
			dim_names_flat <- c() #single dimension vector
			dim_names <- dimnames( iii ) #note: dimnames may also have a names attribute
			if( is.null( dim_names ) ) {
				dim_names_flat <- ""
			} else {
				#ensure a name exists, assign an index value otherwise
				if( is.null( names( dim_names ) ) )
					names( dim_names ) <- 1:length(dim_names)
				#flatten into a vector c( 1st_dimname, 1st_dim_element_1, 2, ... n, 2nd_dimname, ... )
				for( i in 1:length(dim_names) ) 
					dim_names_flat <- c( dim_names_flat, names(dim_names)[i], dim_names[[i]] )
			}
			dim_names_flat <- .addslashes( dim_names_flat )

			cat(paste("$$array mode=\"", typeof(iii), "\" dim=",.addslashes(d)," byright=FALSE",
				" byrow=TRUE dimnames=", dim_names_flat, "\n", sep="")) 
			dhi=d[3:nd]; nhi=length(dhi) # extra dimensions above 2
			idx=1:nhi; index=letters[10+(idx)]
			ex1=paste(paste("for(",rev(index)," in 1:",dhi[rev(idx)],"){",sep=""),collapse=" ")
			ex2="for(j in 1:nr){"
			ex3=paste("cat(iii[j,,",paste(index,collapse=","),"]); cat(\"\\n\")",sep="")
			ex4=paste(rep("}",nhi+1),collapse="")
			expr=paste(ex1,ex2,ex3,ex4,collapse=" ")
			eval(parse(text=expr)) 
		}
		else if (class(iii)=="data.frame") {
			cat("$$data "); 
			#ncol
			cat("ncol="); cat(dim(iii)[2]); cat(" ");
			#modes
			cat("modes=\"")
			for (j in 1:length(iii)) {
				if (j>1) cat(" ")
					cat(typeof(iii[[j]])) }
			cat("\" ")
			#rownames
			cat("rownames="); cat(.addslashes(rownames(iii))); cat(" ")
			#colnames
			cat("colnames="); cat(.addslashes(colnames(iii))); cat(" ")
			#byrow
			cat("byrow=TRUE"); cat("\n")
			for(j in 1:dim(iii)[1]) {
				for(k in 1:dim(iii)[2]) {
					cat(iii[j,k]); cat(" ") }
				cat("\n") }
		}
	}
	if (fname!="") sink()
}
#-------------------------------------.writeList.P

#readList-------------------------------2008-07-14
# Returns a list in either "D" or "P" format read from disk.
# Arguments:
#  fname - file to read
#  Change (Anisa Egeli): There is a check to see if the file exists. 
#  This allows try(readList(...), silent=TRUE) to catch the error.
#-------------------------------------------ACB/AE
readList <- function(fname) {
	if(!file.exists(fname))
		stop(paste("File", fname, "does not exist."))
	#detect file type
	f <- scan(fname, what=character(), sep="\n", quiet=TRUE)
	for(i in 1:length(f)) {
		if (!any(grep("^[ \t]*#", f[i]))) {
			if (any(grep("^[ \t]*structure", f[i]))) fileformat <- "D"
			else if (any(grep("^[ \t]*list", f[i]))) fileformat <- "R"
			else if (any(grep("^[ \t]*\\$", f[i])))  fileformat <- "P"
			else stop("unknown fileformat detected.")
			break;
		}
	}
	if (fileformat == "R" || fileformat == "D") 
		return(eval(parse(fname)))
	if (fileformat == "P") 
		return(.readList.P(fname))
}

#.readList.P----------------------------2009-02-05
# Read list in "P" format.
#----------------------------------------ACB/AE/RH
.readList.P <- function(fname) {
	#srcfile will be modified, orgfile is untouched and only used for user debug error messages
	srcfile=orgfile=scan(fname, what=character(), sep="\n", quiet=TRUE, blank.lines.skip=FALSE)
	data=list(); j=0; halt=FALSE; str=""
	extendLine <- FALSE    #used for extending a single line into lines with \
	extendLineNumber <- 0  #where a new widget starts - used for error messages

	if (!length(srcfile)) {stop("Input file is empty\n")}
	#print("loop start"); print(date());
	#if comments were striped out earlier, we would lose the line count.
	for(i in 1:length(srcfile)) {
		if (!any(grep("^[[:space:]]*(#.*)?$", srcfile[i]))) {
			srcfile[i] <- .stripComments(srcfile[i])
			#append last string onto new string if applicable
			if (extendLine == TRUE)
				str <- paste(str, srcfile[i], sep=" ")
			else {
				str <- srcfile[i]
				extendLineNumber <- i
			}
			#determine if this string is extended by a \ at the end.
			tmp <- sub('\\\\$', '', str)
			if (tmp==str) #no sub took place
				extendLine = FALSE
			else
				extendLine = TRUE
			str <- tmp
			#parse the line once it is complete (no \)
			if (extendLine == FALSE) {
				j <- j + 1
				data[[j]]<-list(str=str, line.start=extendLineNumber, line.end=i)
			}
		}
	}
	#convert the "data" list into a real list
	varName=varOptions=NULL
	varData=retData=list()    #list to return
	for(i in 1:length(data)) {
		str <- data[[i]]$str
		#varOptions (optional)
		if (substr(str,1,2)=="$$") {
			if (!is.null(varOptions))
				stop("extra $$ line found")
			if (is.null(varName))
				stop("$$ line found before $ line")
			varOptions <-data[[i]]
			varOptions$str = substr(varOptions$str, 3, nchar(varOptions$str)) #remove $$
		}
		#varName
		else if (substr(str,1,1)=="$") {
			if (!is.null(varName)) {
				#save data into the retData list
				retData[[varName]] <- .readList.P.convertData(varOptions, varData, fname, orgfile)
				if (is.null(retData[[varName]])) halt<-TRUE
				varName <- varOptions <- NULL
				varData <- list()
			}
			varName <- .trimWhiteSpace(substr(str, 2, nchar(str)))
			if (!any(grep("^[a-zA-Z0-9_.]+$", varName))) {
				.catError(
					paste("Variable name \"", varName,"\" is not valid", sep=""), fname, 
					data[[i]]$line.start, data[[i]]$line.end, 
					orgfile, "readList error"
					)
				halt<-TRUE
			}
			line.start <- data[[i]]$line.start
		}
		else {
			varData[[length(varData)+1]] <-data[[i]]
		}
	}
	#save anything from after
	if (!is.null(varName)) {
		#print(".readList.P.convertData start"); print(date());
#browser();return()
		retData[[varName]] <- .readList.P.convertData(varOptions, varData, fname, orgfile)
		#print(".readList.P.convertData end"); print(date());
		if (is.null(retData[[varName]])) halt=TRUE
	}
	if (halt) {stop("Errors were found in the file. Unable to continue\n")}
	return(retData)
}
#--------------------------------------.readList.P

#.readList.P.convertData----------------2008-03-05
# Helper function to convert data into proper mode.
#-------------------------------------------ACB/RH
.readList.P.convertData <- function(varOptions, varData, fname="", sourcefile=list()) {
	if (is.null(varOptions)) {
		#simple format with no options
		if (length(varData)==0) return(varData)
		else if (length(varData)==1) {
			#just a vector
			return(.autoConvertMode(.convertParamStrToVector(varData[[1]]$str, fname, varData[[1]]$line.start)))
		}
		else {
			#some sort of matrix to parse
			dimSize <- c(length(varData),0) #num of rows
			matData <- c()                  #vector to hold values byrow
			for(i in 1:length(varData)) {
				tmp <- .convertParamStrToVector(varData[[i]]$str, fname, varData[[i]]$line.start)
				if (dimSize[2]==0)
					dimSize[2] <- length(tmp)
				else if (length(tmp)!=dimSize[2]) {
					.catError(paste("Matrix row (line ",varData[[i]]$line.start,") lenght should match first row (line ",varData[[1]]$line.start,") length of ", dimSize[2], sep=""), fname, 
					varData[[i]]$line.start, varData[[i]]$line.end, 
					sourcefile, "readList error")
					return(NULL)
				}
				matData <- append(matData, tmp)
			}
			matData <- .autoConvertMode(matData)
			return(matrix(matData, dimSize[1], dimSize[2], byrow=TRUE))
		}
	}
	#otherwise varOptions was given (in string format)
	#convert it into a list first
	opts <-.getParamFromStr(varOptions$str, fname, varOptions$line.start, varOptions$line.end, sourcefile, .pFormatDefs)
	if (is.null(opts)) stop("Errors were detected")
	if (length(varData)==0) return(eval(parse(text=paste(opts$mode,"()",sep="")))) # return empty typeof

	#flatten all data into a vector (of characters)
	x <- c()
	for(i in 1:length(varData)) {
		#weird things happen if its x[i] <- as.vector(.convert...)
		x <- c(x, .convertParamStrToVector(varData[[i]]$str, fname, varData[[i]]$line.start))
	}
	if(opts$type=="vector") {
		x <- .forceMode(x, opts$mode)
		if (any(opts$names!="")) {
			names(x)<-opts$names
		}
		return(x)
	}
	else if(opts$type=="matrix") {
		x <- .forceMode(x, opts$mode)
		#calculate dims
		nrow <- length(x)/opts$ncol
		if (as.integer(nrow)!=nrow) {
			.catError(paste("Matrix data length [", length(x), "] is not a sub-multiple of ncol [", opts$ncol, "]", sep=""), fname, 
			varOptions$line.start, varData[[length(varData)]]$line.end, 
			sourcefile, "readList error")
			return(NULL)
		}
		#convert to matrix
		mat <- matrix(x, nrow, opts$ncol, byrow=opts$byrow)
		#add colnames
		if (any(opts$colnames!="")) {
			if (length(opts$colnames)!=opts$ncol) {
				.catError(paste("Matrix colnames length [", length(opts$colnames), "] is not equal to ncol [", opts$ncol, "]", sep=""), fname, 
				varOptions$line.start, varData[[length(varData)]]$line.end, 
				sourcefile, "readList error")
				return(NULL)
			}
			colnames(mat)<-opts$colnames
		}
		#add rownames
		if (any(opts$rownames!="")) {
			if (length(opts$rownames)!=nrow) {
				.catError(paste("Matrix rownames length [", length(opts$rownames), "] is not equal to nrow [", nrow, "]", sep=""), fname, 
				varOptions$line.start, varData[[length(varData)]]$line.end, 
				sourcefile, "readList error")
				return(NULL)
			}
			rownames(mat)<-opts$rownames
		}
		return(mat)
	}
	else if(opts$type=="array") {
		x <- .forceMode(x, opts$mode)
		opts$dim <- .convertMode(opts$dim, "numeric")
		if (any(is.na(opts$dim))) {
			.catError("dim values must be numeric", fname, 
			          varOptions$line.start, varData[[length(varData)]]$line.end, 
			          sourcefile, "readList error")
			return(NULL)
		}
		#check dims works
		if (length(x)!=prod(opts$dim)) {
			.catError(paste("dim [product ",prod(opts$dim),"] do not match the length of object [",length(x),"]", sep=""), fname, 
			          varOptions$line.start, varData[[length(varData)]]$line.end, 
			          sourcefile, "readList error")
			return(NULL)
		}
		x=.convertVecToArray(x,opts$dim,byright=opts$byright,byrow=opts$byrow)

		if( all( opts$dimnames == "" ) )
			return( x )

		#restore dimnames
		# example: dimnames(Titanic) -> there are 4 dimensions (class, sex, age, survived)
		# these dimensions have different number of names: i.e. Sex has two: male, female
		# dimnames contains first the name of the element "sex", followed by the labels for the each dimension
		# "male", "female". dim(Titanic)[2] tells us sex only has two labels, so the next label is a dimension name
		# ex: dimnames="Class 1st 2nd 3rd Crew Sex Male Female Age Child Adult Survived No Yes" dim="4 2 2 2"
		dim_name_dimensions <- length( opts$dim )
		dim_names <- list()

		for( i in 1:dim_name_dimensions ) {
			#j points to name of the dimension
			if( i == 1 )
				j <- 1
			else
				j <- sum( opts$dim[ 1:(i-1) ] + 1 ) + 1
			#dim_name_elements are the element names for a particular dimension
			dim_name_elements <- ( j + 1 ) : ( j + opts$dim[ i ] )
			dim_names[[ i ]] <- opts$dimnames[ dim_name_elements ]
			names( dim_names )[ i ] <- opts$dimnames[ j ]
		}
		dimnames( x ) <- dim_names
		return(x)
	}
	else if(opts$type=="data") {
		#check ncol works
		if (length(x)%%opts$ncol>0) {
			.catError(paste("dataframe data length [", length(x), "] is not a sub-multiple of ncol [", opts$ncol, "]", sep=""), fname, 
			varOptions$line.start, varData[[length(varData)]]$line.end, 
			sourcefile, "readList error")
			return(NULL)
		}
		if (opts$ncol != length(opts$colnames)) {
			.catError(paste("Data colnames length [", length(opts$colnames), "] is not equal to ncol [", opts$ncol, "]", sep=""), fname, 
			varOptions$line.start, varData[[length(varData)]]$line.end, 
			sourcefile, "readList error")
			return(NULL)
		}
		if (opts$ncol != length(opts$modes)) {
			.catError(paste("Data modes length [", length(opts$modes), "] is not equal to ncol [", opts$ncol, "]", sep=""), fname, 
			varOptions$line.start, varData[[length(varData)]]$line.end, 
			sourcefile, "readList error")
			return(NULL)
		}
		#calculate nrow
		nrow <- length(x)/opts$ncol
		#break up data into a vector of a list, such that each element represents a column
		dataCols <- list()
		if (opts$byrow) {
			for(i in 1:length(x)) {
				j <- i%%opts$ncol
				if (j==0)
					j <- opts$ncol
				if (length(dataCols)<j)
					dataCols[[j]] <- x[i]
				else
					dataCols[[j]] <- c(dataCols[[j]], x[i])
			}
		}
		else {
			for(i in 1:length(x)) {
				j <- as.integer((i-1)/(length(x)/opts$ncol))+1

				if (length(dataCols)<j)
					dataCols[[j]] <- x[i]
				else
					dataCols[[j]] <- c(dataCols[[j]], x[i])
			}
		}
		#create data.frame and use colnames to refer to each colum
		#the data.frame will be stored as 'ret'
		txt <- "ret <- data.frame("
		for(i in 1:opts$ncol) { #for each column
			#convert into propper mode
			dataCols[[i]] <- .convertMode(dataCols[[i]], opts$modes[i])
			if (i>1)
				txt <- paste(txt, ", ", sep="")
			name <- opts$colnames[i]
			txt <- paste(txt, name, "=dataCols[[", i, "]]", sep="")
		}
		txt <- paste(txt, ")", sep="")
		eval(parse(text=txt))
		#add rownames if any exist
		if (any(opts$rownames!="")) {
			if (length(opts$rownames)!=nrow) {
				.catError(paste("Data rownames length [", length(opts$rownames), "] is not equal to nrow [", nrow, "]", sep=""), fname, 
				varOptions$line.start, varData[[length(varData)]]$line.end, 
				sourcefile, "readList error")
				return(NULL)
			}
			rownames(ret)<-opts$rownames
		}
		return(ret)
	}
}
#--------------------------.readList.P.convertData

#unpackList-----------------------------2006-06-19
# Make local/global variables from the components of a named list.
#----------------------------------------------ACB
unpackList <- function(x, scope="L") {
	namx <- names(x); nx <- length(namx);
	if (nx > 0) for (i in 1:nx) {
		if (namx[i] != "") {
			if (scope=="L")
				assign(namx[i], x[[i]], pos=parent.frame(1))
			else if (scope=="G")
				assign(namx[i], x[[i]], envir = .GlobalEnv)
		}
	}
	namx[namx != ""] }

#packList-------------------------------2009-08-04
# Pack a list with (i) existing objects using their 
# names or (ii) one explicit value.
#-----------------------------------------------RH
packList=function(stuff, target="PBSlist", value,
                  lenv=parent.frame(), tenv=.GlobalEnv) {
	# Deparse bad objects: those that break code (see function 'deparse')
	deparseBO = function(x){
		if (is.list(x) && !is.data.frame(x)) {
			sapply(x,function(x){deparseBO(x)},simplify=FALSE) # recursion through lists within lists
		} else {
			xclass=class(x)
			if (mode(x) %in% c("call","expression","(","function","NULL"))
				x=paste(deparse(x),collapse="")
			class(x)=xclass
			return(x)
		}
	}
	if (!is.vector(stuff) || !is.character(stuff))
		showAlert("Provide a vector of names denoting objects")
	target=deparse(substitute(target))
	target=gsub("^\"","",gsub("\"$","",target)) # strip leading and ending escaped quotes
	endpos=regexpr("[\\[$]",target)-1; if (endpos<=0) endpos=nchar(target)
	base=substring(target,1,endpos)
	if (!exists(base,envir=tenv)) 
		assign(base,list(),envir=tenv)
	if (!missing(value)) { # use explicit value instead of objects
		objet=paste(deparse(value),collapse="\n")
		eval(parse(text=paste(target,"[[\"",stuff,"\"]]=",objet,sep="")),envir=tenv) } #pack explicit value into the list
	else {
		for (s in stuff) {
			if (!exists(s,envir=lenv)) next
			eval(parse(text=paste("objet=get(\"",s,"\",envir=lenv)",sep=""))) #grab the local object
			if (is.list(objet) && !is.data.frame(objet)) {
				atts=attributes(objet)
				objet=deparseBO(objet)
				natts=setdiff(names(atts),names(attributes(objet)))
				# retain additional attributes of the original list
				if (length(natts)>0) { 
					for (i in natts) attr(objet,i)=atts[[i]] }
				# Reminder: applying original class can cause a display error if 
				# underlying objects (e.g., functions, calls) have been converted to strings.
				#lclass=sapply(objet,class,simplify=FALSE)
				#for(i in 1:length(objet)) attr(objet[[i]],"class")=lclass[[i]]
			}
			objet=paste(deparse(objet),collapse="\n")
			eval(parse(text=paste(target,"[[\"",s,"\"]]=",objet,sep="")),envir=tenv) } #pack into the list
	}
	invisible() }
#-----------------------------------------packList



