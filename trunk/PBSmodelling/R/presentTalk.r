############### # Note: The code in this file is preliminary, and 
############### #  the functionality of 'presentTalk' has limitations.
############### #  A future version will likely be built using S4 class structures.
############### 
############### #2008-10-09----------------------------------AE/RH
############### # Launch a GUI to control the talks described by
############### # the given file and run the first talk.
############### #   x = filename of Talk Description File
############### # Note: 'chunk' in code is equivalent to 'segment' in documentation.
############### #Start presentTalk--------------------------------
############### presentTalk=function(x,debug=FALSE){
############### 	dbg=debug
############### 	pTalk=getPBSoptions(".presentTalk")
############### 	if(!is.null(pTalk))
############### 		unpackList(pTalk,scope="L")
############### 	#if a talk already exist, kill its GUI
############### 	if(exists("tn",frame=sys.nframe()))
############### 		closeWin(names(talks)[tn])
############### 	setPBSoptions(".presentTalk",NULL) # clear entire talk
############### 	setPBSoptions(".presentTalk",.parseTalk(x))
############### 	setPBSoptions(".presentTalk",list(tn=1,sn=1,cn=1,back=0,atEnd=FALSE,debug=dbg),sublist=TRUE)
############### 	.runTalk(1)
############### 	invisible() }
############### #End presentTalk----------------------------------
############### 
############### #2008-10-09----------------------------------AE/RH
############### # Returns talk description file parsed into list.
############### #Start .parseTalk---------------------------------
############### .parseTalk=function(x){
############### 	#Subfunctions------------------------
############### 	#regular expression functions with different defaults
############### 	myGrep=function(pattern, x, value=FALSE){
############### 		return(grep(pattern, x, value=value, ignore.case=TRUE, perl=TRUE))
############### 	}
############### 	mySub=function(subpattern, replacement, x){
############### 		return(sub(subpattern, replacement, x, ignore.case=TRUE, perl=TRUE))
############### 	}
############### 	myGsub=function(pattern, replacement, x){
############### 		return(gsub(pattern, replacement, x, ignore.case=TRUE, perl=TRUE))
############### 	}
############### 	myRegexpr=function(pattern, text){
############### 		return(regexpr(pattern, text, ignore.case=TRUE, perl=TRUE))
###############  	}
###############  	myGregexpr=function(pattern, text){
############### 		return(gregexpr(pattern, text, ignore.case=TRUE, perl=TRUE))
###############  	}
###############  	myStrsplit=function(x, split){
############### 		return(strsplit(x, split, perl=TRUE))
############### 	}
############### 	# Assumes tagType and optionType are valid
############### 	# stop()s if optVal is invalid
############### 	validateOption=function(tagType, optType, opt, tag){
############### 		for (i in .tagDefs[[tagType]][[optType]]$checks){
############### 			if(!length(myGrep(.tagOptionChecks[[i]]$regexp, opt)))
############### 				stop(paste("Bad value for ", optType, " option in ", tagType,
############### 						" tag:\n", tag, "\n", .tagOptionChecks[[i]]$error, sep=""))
############### 		}
############### 	}
############### 	# Returns list:
############### 	# $tagType=type of tag
############### 	# $opts=list of valid options
############### 	# stops if there is an error.
############### 	validateTag=function(tag){
############### 		validTagTypes=names(.tagDefs)
############### 		matchTagType=paste("((", paste(validTagTypes, collapse=")|("), "))", sep="")
############### 		matchTag=paste("^<", matchTagType, "((\\s+.*>)|(>))$", sep="")
############### 
############### 		if(!length(myGrep(matchTag, tag)))
############### 			stop(paste("Bad tag name\n", tag, "\n",
############### 					"Valid tag names are ", paste(validTagTypes, collapse=", "), sep=""))
############### 
############### 		tagType=tolower(mySub(matchTag, "\\1", tag))
############### 
############### 		validOptTypes=names(.tagDefs[[tagType]])
############### 		matchOptTypes=paste("((", paste(validOptTypes, collapse=")|("), "))",
############### 				sep="")
############### 
############### 		#isolate options from rest of tag
############### 		opts=mySub(paste("^<", tagType, "\\s*(.*)\\s*>$", sep=""), "\\1", tag)
############### 
############### 		validOptions=list()
############### 		while(opts!=""){ #while there are unparsed options
############### 			matchOptType=paste("^\\s*", matchOptTypes, ".*", sep="")
############### 			if(!length(myGrep(matchOptType, opts)))
############### 				stop(paste("Bad option name for ", tagType, " tag\n", tag, "\n",
############### 					"Valid option names are ", paste(validOptTypes, collapse=", "),
############### 							sep=""))
############### 			optType=mySub(matchOptType, "\\1", opts)
############### 
############### 			#handle quoted strings separately
############### 			if(.tagDefs[[tagType]][[optType]][["class"]]=="character" &&
############### 				length(myGrep(paste("^\\s*", optType, "\\s*=\\s*\"", sep=""), opts))){
############### 				matchQuoted=paste("^\\s*", optType, "\\s*=\\s*\"([^\"]*)\".*", sep="")
############### 				if(!length(myGrep(matchQuoted, opts)))
############### 					stop(paste("Value for ", optType, " option in ", tagType, " does ",
############### 						"have closing quote.\n", tag, sep=""))
############### 				string=mySub(matchQuoted, "\\1", opts)
############### 
############### 	####continuing in while loop
############### 				opts=mySub(paste("^\\s*", optType, "\\s*=\\s*\"[^\"]*\"\\s*(.*)",
############### 						sep=""), "\\1", opts)
############### 				validateOption(tagType, optType, string, tag)
############### 				validOptions[[optType]]=string
############### 				next
############### 			}
############### 
############### 			matchNonQuoted=paste("^\\s*", optType, "\\s*=\\s*([^\\s]+)\\s*",
############### 					sep="")
############### 			if(!length(myGrep(matchNonQuoted, opts)))
############### 					stop(paste("Value for", optType, "option in", tagType, "tag must be",
############### 						"specified.\n", tag))
############### 			opt=mySub(paste(matchNonQuoted, "(.*)", sep=""), "\\1", opts)
############### 
############### 			if(.tagDefs[[tagType]][[optType]][["class"]]=="integer"){
############### 				if(!length(myGrep("^((-?[1-9][0-9]*)|0)$", opt)))
############### 				stop(paste("Value for", optType, "option in", tagType, "tag must be",
############### 						"valid integer.\n", tag))
############### 				opt=as.integer(opt)
############### 			}
############### 
############### 			if(.tagDefs[[tagType]][[optType]][["class"]]=="logical"){
############### 				if(!length(myGrep("^(T|TRUE|F|FALSE)$", opt)))
############### 				stop(paste("Value for", optType, "option in", tagType, "tag must be",
############### 						"valid boolean value: T/TRUE or F/FALSE.\n", tag))
############### 				opt=as.logical(opt)
############### 			}
############### 			validateOption(tagType, optType, opt, tag)
############### 			validOptions[[optType]]=opt
############### 			opts=mySub(matchNonQuoted, "", opts)
############### 		}
############### 		for(i in validOptTypes){
############### 			if(!is.null(validOptions[[i]]))
############### 				next
############### 			if(is.null(.tagDefs[[tagType]][[i]]$default))
############### 				stop(paste("Missing required option ", i, " from ", tagType, " tag.\n",
############### 						tag, sep=""))
############### 			validOptions[[i]]=.tagDefs[[tagType]][[i]]$default
############### 		}
############### 		return(list(tagType=tagType, opts=validOptions))
############### 	}
############### 	# Returns character position of first tag in a given line for 0 if no tag in the line
############### 	firstTagPos=function(x){
############### 		validTagTypes=names(.tagDefs)
############### 		matchTagType=paste("((", paste(validTagTypes, collapse=")|("), "))", sep="")
############### 		tagPosInfo=myGregexpr(paste("(\\\\)?<", matchTagType, sep=""), x)
############### 		if(tagPosInfo[[1]][1]==-1) #no tags found
############### 			return(0)
############### 		for(i in tagPosInfo[[1]]){
############### 			if(substr(x,i,i)!="\\") #tag wasn't preceded by backslash
############### 				return(i)
############### 		}
############### 		return(0)
############### 	}
############### 	parseFileChunk=function(chunk){
############### 		chunk=paste(chunk, collapse=" ")
############### 		files=character(0)
############### 		chunk=mySub("^\\s*(.*)", "\\1", chunk) #remove inital whitespace
############### 		while(chunk!=""){
############### 			firstChar=substr(chunk, 1, 1)
############### 			if(firstChar=="\""){
############### 				if(!length(myGrep("\".*\"", chunk)))
############### 					stop(paste("Quoted file name does not have closing quote ('\"')",
############### 							chunk, sep="\n"))
############### 				files=c(files, mySub("^\"([^\"]*)\".*", "\\1", chunk))
############### ##while increments here
############### 				chunk=mySub("^\"[^\"]*\"\\s*(.*)", "\\1", chunk)
############### 			}else{
############### 					files=c(files, mySub("^([^\\s]+).*", "\\1", chunk))
############### 					chunk=mySub("^[^\\s]+\\s*(.*)", "\\1", chunk)
############### 			}
############### 		}
############### 		return(files)
############### 	}
############### 	#End subfunctions------------------------
############### 
############### 	validTagTypes=names(.tagDefs)
############### 	matchTagType=paste("((", paste(validTagTypes, collapse=")|("), "))", sep="")
############### 
############### 	if(!file.exists(x))
############### 		stop(paste("Cannot find file", x))
############### 
############### 	desc=readLines(x, warn=FALSE, encoding=getOption("encoding"))
############### 	if(!length(desc))
############### 		error(paste("Empty file", x))
############### 	desc=.stripComments(desc)
############### 
############### 	talks=list()
############### 	fileList=list()
############### 	buttonList=list()
############### 	nTalks=0
############### 	nSects=0
############### 	inSect=FALSE
############### 	chunk=NULL
############### 	talktab=NULL
############### 	i=1
############### 	while(i<=length(desc)){#while there are lines to parse
############### 		firstTag=firstTagPos(desc[i])
############### 		#if there is a tag but not at the first position
############### 		#and there is only whitespace before the tag, remove it
############### 		if(firstTag>1 && length(myGrep("^\\s+$", substr(desc[i], 1, firstTag-1)))){
############### 				desc[i]=mySub("^\\s+", "", desc[i])
############### 				firstTag=1
############### 		}
############### 		if(firstTag!=1){ #if there is a text segment before the tag, or no tag
############### 			if(is.null(chunk)){ #if not within text/code chunk
############### 				#if there's non-tag, non-whitespace text, error
############### 				if(!length(myGrep("^\\s*$", desc[i])))
############### 					stop(paste("Text outside of valid segment:\n",
############### 							desc[i]))
############### 				#skip this line if it was all whitespace
############### #WHILE INCREMENTS HERE
############### 				i=i+1
############### 				next
############### 			}
############### 			if(firstTag){ #if there is a tag later in the line
############### 				#move it to the beginning of a new next line
############### 				desc=append(desc, substr(desc[i], firstTag, length(desc[i])), after=i)
############### 				desc[i]=substr(desc[i], 1, firstTag-1)
############### 				}
############### #WHILE INCREMENTS HERE
############### 				i=i+1
############### 				next
############### 		}
############### 		#make sure the tag is closed on this line
############### 		if(!length(myGrep(paste("^<", matchTagType, "[^>]*>", sep=""), desc[i])))
############### 			stop(paste("Tags must be completed on the line they are started\n",
############### 					desc[i]))
############### 		tag=mySub(paste("(^<", matchTagType, "[^>]*>).*", sep=""), "\\1", desc[i])
############### 
############### 		#if there is something after this tag, move it to its own line,
############### 		#trimming initial whitespace
############### 		afterTag=mySub(paste("^<", matchTagType, "[^>]*>", sep=""), "", desc[i])
############### 		afterTag=mySub("^\\s*", "", afterTag)
############### 		if(afterTag!=""){
############### 			desc=append(desc, afterTag, after=i)
############### 			desc[i]=tag
############### 		}
############### 		if(!is.null(chunk)){ #if in a text/code chunk
############### 			if((i-1)>=chunk){
############### 				chunk=desc[chunk:(i-1)]
############### 				if(tagType=="file"){
############### 					chunk=parseFileChunk(chunk)
############### 					fileList[[nTalks]][[opts$name]]=chunk
############### 					attr(fileList[[nTalks]][[opts$name]], "options")=opts
############### 				}
############### 				if(inSect){
############### 					talks[[nTalks]][[nSects]]=c(talks[[nTalks]][[nSects]], list(chunk))
############### 					talktab=rbind(talktab,c(tn=nTalks,sn=nSects,cn=length(talks[[nTalks]][[nSects]])))
############### 					attr(talks[[nTalks]][[nSects]][[length(talks[[nTalks]][[nSects]])]],
############### 						"options")=opts
############### 				}
############### 			}
############### 			chunk=NULL
############### 		}
############### 		#unpacks variables: tagType and opts
############### 		unpackList(validateTag(tag), scope="L")
############### 
############### 		#do talk/segment increments, logical location flags, & make sure tags are in the right place
############### 		if(tagType=="talk"){
############### 			nTalks=nTalks+1
############### 			nFiles=0
############### 			inSect=FALSE
############### 		}
############### 		if(tagType=="section"){
############### 			if(!nTalks)
############### 				stop(paste("Section must be within a talk block\n", desc[i]))
############### 			inSect=TRUE
############### 			nSects=nSects+1
############### 		}
############### 		if(any(c("code", "text")==tagType)){
############### 				if(!inSect)
############### 					stop(paste("Code must be within a section block\n",desc[i]))
############### 		}
############### 		if(tagType=="file"){
############### 			nFiles=nFiles+1
############### 			if(!nTalks)
############### 				stop(paste("File tag must be within a talk block\n",desc[i]))
############### 		}
############### 		#add information to lists
############### 		if(tagType=="talk"){
############### 			talks[[nTalks]]=list()
############### 			fileList[[nTalks]]=list()
############### 			names(talks)[[nTalks]]=opts$name
############### 			attr(talks[[nTalks]], "options")=.removeFromList(opts,"name")
############### 			buttonList[[nTalks]]=list()
############### 			if(opts$button){ #note button must be added to all talks
############### 				for(j in 1:nTalks){
############### 					if(length(buttonList[[j]])<opts$col ||
############### 							is.null(buttonList[[j]][[opts$col]]))
############### 						buttonList[[j]][[opts$col]]=list()
############### 					row=length(buttonList[[j]][[opts$col]])+1
############### 					buttonList[[j]][[opts$col]][[row]]=nTalks
############### 					attr(buttonList[[j]][[opts$col]][[row]], "type")="talk"
############### 				}
############### 			}
############### 		}else if(tagType=="section"){
############### 			index=length(talks[[nTalks]])+1
############### 			talks[[nTalks]][[index]]=list()
############### 			names(talks[[nTalks]])[[index]]=opts$name
############### 			attr(talks[[nTalks]][[index]], "options")=
############### 					.removeFromList(opts, "name")
############### 			if(opts$button){ #button added for this talk
############### 				if(length(buttonList[[nTalks]])<opts$col ||
############### 						is.null(buttonList[[nTalks]][[opts$col]]))
############### 					buttonList[[nTalks]][[opts$col]]=list()
############### 				row=length(buttonList[[nTalks]][[opts$col]])+1
############### 				buttonList[[nTalks]][[opts$col]][[row]]=nSects
############### 				attr(buttonList[[nTalks]][[opts$col]][[row]], "type")="section"
############### 			}
############### 		} else if (tagType=="file"){
############### 			chunk=i+1
############### 			if(opts$button){ #button added for this file
############### 				if(length(buttonList[[nTalks]])<opts$col ||
############### 						is.null(buttonList[[nTalks]][[opts$col]]))
############### 					buttonList[[nTalks]][[opts$col]]=list()
############### 				row=length(buttonList[[nTalks]][[opts$col]])+1
############### 				buttonList[[nTalks]][[opts$col]][[row]]=opts$name
############### 				attr(buttonList[[nTalks]][[opts$col]][[row]], "type")="file"
############### 			}
############### 		} else
############### 			chunk=i+1
############### 		i=i+1
############### #WHILE INCREMENTS HERE
############### 	}
############### 	talktab=as.data.frame(talktab)
############### 	talktab$seg=1:nrow(talktab)
############### 	z0=talktab$cn==1 # start chunk
############### 	z1=c(z0[2:length(z0)],TRUE) # end chunk
############### 	talktab$chunkEnds = (z0|z1)
############### 	return(list(talks=talks, fileList=fileList, buttonList=buttonList, talktab=talktab)) }
############### #End .parseTalk-----------------------------------
############### 
############### #2008-10-03----------------------------------AE/RH
############### # Run given talk, creating a window to control it 
############### # and setting the talk number globally.
############### #Start .runTalk-----------------------------------
############### .runTalk=function(x){
############### 	if(missing(x))
############### 		x=as.integer(getWinAct()[1])
############### 	unpackList(getPBSoptions(".presentTalk"),scope="L")
############### 	guiExists=exists("tn",frame=sys.nframe())
############### 	if (guiExists && tn!=x)
############### 		closeWin(names(talks)[tn])
############### 	if(!guiExists || tn!=x){
############### 		setPBSoptions(".presentTalk",list(tn=x),sublist=TRUE) }
############### 	.makeTCGUI()
############### 	.runSection(1) }
############### #End .runTalk-------------------------------------
############### 
############### #2008-10-09----------------------------------AE/RH
############### # Run given section number, setting section number globally 
############### # and resetting midCode boolean to FALSE.
############### #Start .runSection--------------------------------
############### .runSection=function(x){
############### 	if(missing(x))
############### 		x=as.integer(getWinAct()[1])
############### 	setPBSoptions(".presentTalk",list(sn=x,midCode=FALSE),sublist=TRUE)
############### 	cat("\nSection",x,paste(rep("=",40),collapse=""),"\n\n")
############### 	.runChunk(1) }
############### #End .runSection----------------------------------
############### 
############### #2008-10-09----------------------------------AE/RH
############### # Run a text/code chunk, setting the chunk number globally, 
############### # incrementing chunk, and continuing to next chunk if no breaks occurred.
############### #Start .runChunk----------------------------------
############### .runChunk=function(cn){
############### 	setPBSoptions(".presentTalk",list(cn=cn),sublist=TRUE)
############### 	unpackList(getPBSoptions(".presentTalk"),scope="L")
############### 	if (debug)
############### 		cat(paste("\nFrame ",sys.nframe(),paste(rep("~",40),collapse=""),">",paste(c(tn,sn,cn),collapse=","),"\n"))
############### 
############### 	chunk=talks[[tn]][[sn]][[cn]]
############### 	opts=attr(talks[[tn]][[sn]][[cn]], "options")
############### 	# Special case <file>: opening a file
############### 	if(!is.null(opts$button)){
############### 		if (!midCode) {
############### 		for(i in chunk){
############### 			if(file.exists(i)){
############### 				openFile(i)
############### 				cat(paste(paste(rep(".",40),collapse=""),"Opened :", i,"\n"))
############### 			}else
############### 				cat(paste(paste(rep("!",40),collapse=""),"No file:", i,"\n"))
############### 		}}
############### 		#if no break, go to next chunk
############### 		if(midCode || !opts$`break`){
############### 			if((cn+1)>length(talks[[tn]][[sn]]) && sn<length(talks[[tn]])) {
############### 				sn=sn+1; setPBSoptions(".presentTalk",list(sn=sn,midCode=FALSE),sublist=TRUE)
############### 				.runSection(sn) }
############### 			else if ((cn+1)<=length(talks[[tn]][[sn]])) {
############### 				cn=cn + 1; 
############### 				setPBSoptions(".presentTalk",list(cn=cn,midCode=FALSE),sublist=TRUE) 
############### 				.runChunk(cn) }
############### 		}else {
############### 			#cat("--------------- Opened files, Press GO ---------------\n")
############### 			cat(paste(paste(rep("-",40),collapse=""),"<<Press GO>>\n"))
############### 			setPBSoptions(".presentTalk",list(midCode=TRUE),sublist=TRUE) }
############### 		return()
############### 	}
############### 	isCode=!is.null(opts$show)
############### 	mustCat=!midCode && (!isCode || opts$show)
############### 	mustBreakAfterCat=!midCode && ((!isCode && opts$`break`)||(isCode && any(c("show", "all")==opts$`break`)))
############### 	mustEval = !mustBreakAfterCat && isCode
############### 	mustBreakAfterEval = mustEval && isCode && any(c("print","all")==opts$`break`)
############### 	midCode = mustBreakAfterCat && isCode
############### 	atEnd = !midCode && !(cn<length(talks[[tn]][[sn]]) || sn<length(talks[[tn]]))
############### 	setPBSoptions(".presentTalk",list(midCode=midCode,atEnd=atEnd),sublist=TRUE)
############### 	if (debug)
############### 		print(t(data.frame(isCode,midCode,mustCat,mustBreakAfterCat,mustEval,mustBreakAfterEval)))
############### 	if(mustCat){
############### 		if(isCode) cat("> ")
############### 		cat(paste(chunk, collapse=ifelse(isCode,"\n> ","\n")),"\n")
############### 	}
############### 	if(mustBreakAfterCat)
############### 		if(!atEnd)
############### 			cat(paste(paste(rep("-",40),collapse=""),"<<Press GO>>\n"))
############### 			#cat("--------------- Finished display, Press GO ---------------\n")
############### 	if(mustEval){
############### 		for(i in chunk){
############### 			res=eval(parse(text=i), envir=globalenv())
############### 			if(opts$print) print(res)
############### 		}
############### 	}
############### 	if(mustBreakAfterEval && !atEnd)
############### 		cat(paste(paste(rep("-",40),collapse=""),"<<Press GO>>\n"))
############### 
############### 	#if not midCode, increment chunk number
############### 	if(!midCode && !atEnd){
############### 		#if no break, go to next chunk
############### 		if(!mustBreakAfterCat || !mustBreakAfterEval){
############### 			if((cn+1) > length(talks[[tn]][[sn]]) && sn < length(talks[[tn]])) {
############### 				sn=sn+1; setPBSoptions(".presentTalk",list(sn=sn,cn=1,midCode=FALSE),sublist=TRUE) 
############### 				cat("\nSection",sn,paste(rep("=",30),collapse=""),"<<Press GO>>\n\n") }
############### 			else if ((cn+1) <= length(talks[[tn]][[sn]])) {
############### 				cn=cn+1; setPBSoptions(".presentTalk",list(cn=cn,midCode=FALSE),sublist=TRUE) 
############### 			if ((!isCode&&!mustBreakAfterCat) || (isCode&&!mustBreakAfterEval)) .runChunk(cn) }
############### 		}
############### 	} 
############### }
############### #End .runChunk------------------------------------
############### 
############### 
############### #2008-10-03----------------------------------AE/RH
############### # Create window description file from talk data and launch it.
############### #Start .makeTCGUI---------------------------------
############### .makeTCGUI=function(){
############### 	unpackList(getPBSoptions(".presentTalk"),scope="L")
############### 	if (!exists("tn",frame=sys.nframe())) tn=1
############### 	setPBSoptions(".presentTalk",list(tn=tn),sublist=TRUE)
############### 	winDesc=c(
############### 		paste("window", paste(paste(c("name", "title"),
############### 				names(talks)[tn], sep="="), collapse=" ")),
############### 		paste("menu nitems=", length(talks)," label=Talks", sep=""),
############### 		paste("menuitem label=", names(talks)," function=.runTalk action=",
############### 			1:length(talks), sep="")
############### 	)
############### 	opts=attr(talks[[tn]], "options")
############### 
############### 	#menu
############### 	winDesc=c(winDesc,
############### 		paste("menu nitems=", length(talks[[tn]])," label=Sections", sep=""),
############### 		paste("menuitem label=", names(talks[[tn]]), " function=.runSection action=",
############### 			1:length(talks[[tn]]), sep="")
############### 	)
############### 	if(length(fileList[[tn]])){
############### 		winDesc=c(winDesc,
############### 			paste("menu nitems=",length(fileList[[tn]])," label=Files", sep=""),
############### 			paste("menuitem label=\"", names(fileList[[tn]]),
############### 				"\" function=.openFilesFromGUI action=\"",names(fileList[[tn]]), "\"", sep="")
############### 		)
############### 	}
############### 	#control widgets
############### 	winDesc=c(winDesc,
############### 		#"label text=Sections: font=\"bold 8\" sticky=W fg=darkblue",
############### 		"grid 1 7",
############### 		paste("button function=.CGUIchooseSection action=.first text=\"<<<",
############### 			"\\nStart\" bg=lightblue1 sticky=S"),
############### 		paste("button function=.CGUIchooseSection action=.prev",
############### 			"text=\"<<\\nPrev\" bg=lightskyblue1 sticky=S"),
############### 		paste("button function=.CGUIchooseSection action=.back text=\" < ",
############### 			"\\nCurr\" bg=skyblue sticky=S"),
############### 		paste("button function=.CGUIchooseSection action=.skip",
############### 			"text=\">\\nNext\" bg=skyblue sticky=S"),
############### 		"null",
############### 		paste("button function=.CGUIback action=.back text=\" < ",
############### 			"\\nBack\" bg=greenyellow sticky=S font=\"bold 10\" width=4"),
############### 		paste("button function=.CGUIgo text=\">\\nGO\" bg=greenyellow",
############### 			"font=\"bold 10\" width=4")
############### 	)
############### 	if(length(buttonList[[tn]])){
############### 		buttonCols=list()
############### 		maxRows=0
############### 		for(col in 1:length(buttonList[[tn]])){
############### 			if(is.null(buttonList[[tn]][[col]])){
############### 				buttonCols[[col]]="null padx=4"
############### 				next
############### 			}
############### 			maxRows=max(maxRows, length(buttonList[[tn]][[col]]))
############### 			buttonCols[[col]]=character(0)
############### 			for(button in buttonList[[tn]][[col]]){
############### 				buttonType=attr(button, "type")
############### 				if(buttonType=="talk"){
############### 					buttonCols[[col]]=c(buttonCols[[col]], paste("button text=",names(talks)[button],
############### 						" function=.runTalk action=",button," padx=4 pady=4 fg=darkgreen bg=whitesmoke",sep=""))
############### 				}
############### 				if(buttonType=="section"){
############### 					buttonCols[[col]]=c(buttonCols[[col]], paste("button text=",names(talks[[tn]])[button],
############### 						" function=.runSection action=",button," padx=4 pady=4 fg=red3 bg=whitesmoke",sep=""))
############### 				}
############### 				if(buttonType=="file"){
############### 					buttonCols[[col]]=c(buttonCols[[col]], paste("button text=\"",
############### 						button, "\" function=.openFilesFromGUI action=", button,
############### 						" padx=4 pady=4 fg=blue bg=whitesmoke", sep=""))
############### 				}
############### 			}
############### 		}
############### 		winDesc=c(winDesc, paste("grid ",length(buttonList[[tn]]), " ", maxRows,
############### 				" byrow=FALSE relief=groove padx=4 pady=4", sep=""))
############### 		for(buttons in buttonCols){
############### 			winDesc=c(winDesc, c(buttons, rep("null padx=4",maxRows-length(buttons))))
############### 		}
############### 	}
############### 	createWin(winDesc, astext=TRUE) }
############### #End .makeTCGUI-----------------------------------
############### 
############### #2008-09-22----------------------------------AE/RH
############### # Function for changing section from GUI controls.
############### #Start .CGUIchooseSection-------------------------
############### .CGUIchooseSection=function(){
############### 	unpackList(getPBSoptions(".presentTalk"),scope="L")
############### 	action=getWinAct()[1]
############### 	if(action==".first") sn=1
############### 	if(action==".prev" && sn!=1) sn=sn-1
############### 	if(action==".skip"){
############### 		if(sn!=length(talks[[tn]]))
############### 			sn=sn+1
############### 		else{
############### 			showAlert("End of talk.  Press 'Start' button to restart.")
############### 			return()
############### 		}
############### 	}
############### 	.runSection(sn) }
############### #End .CGUIchooseSection---------------------------
############### 
############### #2008-09-22----------------------------------AE/RH
############### # Open file specified by action.
############### #Start .openFilesFromGUI--------------------------
############### .openFilesFromGUI=function(){
############### 	unpackList(getPBSoptions(".presentTalk"),scope="L")
############### 	action=getWinAct()[1]
############### 	files=fileList[[tn]][[action]]
############### 	badFiles=character(0)
############### 	for(i in files){
############### 		tryRes=try(openFile(i), silent=TRUE)
############### 		if(class(tryRes)=="try-error")
############### 			badFiles=c(badFiles, i)
############### 	}
############### 	if(length(badFiles))
############### 		showAlert(paste("Could not open files:", paste(badFiles, collapse=", ")))
############### }
############### #End .openFilesFromGUI----------------------------
############### 
############### #2008-10-09----------------------------------AE/RH
############### .CGUIgo=function(){
############### 	unpackList(getPBSoptions(".presentTalk"),scope="L")
############### 	if (sn==length(talks[[tn]]) && cn==length(talks[[tn]][[sn]])) atEnd=TRUE
############### 	if (atEnd) z=nrow(talktab) else
############### 	z=apply(talktab[,1:3],1,function(x,p){all(x==p)},p=c(tn,sn,cn))
############### 	go=max(1,(1:nrow(talktab))[z]) # find current position in 'talktab' ('go' not used currently)
############### 	setPBSoptions(".presentTalk",list(back=0,go=go,atEnd=atEnd),sublist=TRUE) # reset backwards counter to 0
############### 	#cat(paste(paste(rep("~",52),collapse=""),">",paste(c(tn,sn,cn),collapse=","),"\n")) # activate for debugging
############### 	if (atEnd) {
############### 		.runChunk(cn)
############### 		showAlert("End of talk.  Press 'Start' button to restart.") }
############### 	else if (cn>length(talks[[tn]][[sn]]) && sn<length(talks[[tn]])) {
############### 		.runSection(sn) }
############### 	else if (cn<length(talks[[tn]][[sn]])) {
############### 		.runChunk(cn) }
############### }
############### 
############### #2008-10-09----------------------------------AE/RH
############### .CGUIback=function(){
############### 	unpackList(getPBSoptions(".presentTalk"),scope="L")
############### 	if (back==0){
############### 		if (atEnd) z=nrow(talktab) else
############### 		z=apply(talktab[,1:3],1,function(x,p){all(x==p)},p=c(tn,sn,cn)) # find current position in 'talktab'
############### 		back=max(1,(1:nrow(talktab))[z]-ifelse(atEnd,1,2)) }
############### 	else back=max(1,back-1)
############### 	#cat(paste(paste(rep("~",52),collapse=""),">",paste(c(tn,sn,cn),collapse=","),"\n")) # activate for debugging
############### 	last=talktab[back,] # vector of previous (tn,sn,cn) in 'talktab'
############### 	tn=as.numeric(last[1]);sn=as.numeric(last[2]);cn=as.numeric(last[3])
############### 	setPBSoptions(".presentTalk",list(back=back,last=last,tn=tn,sn=sn,cn=cn,midCode=FALSE),sublist=TRUE)
############### 	.runChunk(cn) 
############### }

#set classes used by presenttalk
setClass( "text", representation( text = "character", "break" = "logical" ) )
setClass( "file", representation( name = "character", filename = "character", "break" = "logical", button = "logical", col = "integer" ) )
setClass( "code", representation( show = "logical", print = "logical", code = "character", "break" = "character", eval = "logical" ) )
setClass( "break", representation( "NULL" ) ) #this prints a message - wtf R?!!! give me an empty class that isn't virtual

setClass( "section", representation( name = "character", items = "list", button = "logical", col = "integer", section_id = "integer" ) ) #items should be a list of the above 4 s4 classes
setClass( "talk", representation( name = "character", sections = "list", files = "list" ) )

	





.parseTalkFile <- function( talk_fname )
{
	.processSection <- function( node )
	{
		x <- new( "section", 
				name = node$attributes[ "name" ],
				button = as.logical( xmlGetAttr( node, "button", FALSE ) ),
				col = as.integer( xmlGetAttr( node, "col", 2 ) )
				 )
		for( i in xmlChildren( node ) ) {
			
			#TODO refactor with a do.call
			if( xmlName( i ) == "text" ) item <- .processText( i )
			else if( xmlName( i ) == "file" ) item <- .processFile( i )
			else if( xmlName( i ) == "code" ) item <- .processCode( i )
			#else if( xmlName( i ) == "break" ) item <- processBreak( i )
			else if( xmlName( i ) == "comment" ) { next } #do nothing
			else stop( paste( "not implmented:", xmlName(i) ) )
			
			x@items[[ length( x@items ) + 1 ]] <- item
		}
	
		#scan through items, and place breaks accordingly (it would be easier to just have a <break/>)
		items <- x@items
		x@items <- list()
		i <- 1
		for( item in items ) {
			if( inherits( item, "text" ) || inherits( item, "file" ) ) {
				#insert item
				x@items[[ i ]] <- item
				i <- i + 1
	
				#insert break
				if( item@"break" == TRUE ) {
					x@items[[ i ]] <- new( "break" )
					i <- i + 1
				}
			} else if( inherits( item, "code" ) ) {
				#just print code first
				item@eval = FALSE
				x@items[[ i ]] <- item
				i <- i + 1
	
				#break
				if( any( item@"break" == c( "show", "all" ) ) ) {
					x@items[[ i ]] <- new( "break" )
					i <- i + 1
				}
	
				#just eval code
				item@eval = TRUE
				x@items[[ i ]] <- item
				i <- i + 1
	
				#break
				if( any( item@"break" == c( "print", "all" ) ) ) {
					x@items[[ i ]] <- new( "break" )
					i <- i + 1
				}
	
			}
		}
	
		return( x )
	}
	
	.processText <- function( node )
	{
		return( new( "text", text = xmlValue( node ), "break" = as.logical( xmlGetAttr( node, "break", FALSE ) ) ) )
	}
	
	.processFile <- function( node )
	{
		return( new( "file", 
			name = xmlGetAttr( node, "name", "" ), 
			"break" = as.logical( xmlGetAttr( node, "break", TRUE ) ),
			filename = xmlValue( node ),
			button = as.logical( xmlGetAttr( node, "button", FALSE ) ),
			col = as.integer( xmlGetAttr( node, "col", 3 ) )
			 ) )
	}
	
	.processCode <- function( node )
	{
		return( new( "code", 
				show = as.logical( xmlGetAttr( node, "show", TRUE ) ), 
				print = as.logical( xmlGetAttr( node, "print", TRUE ) ), 
				code = xmlValue( node ), 
				"break" = xmlGetAttr( node, "break", "print" )
				) )
	}

	#start parsing below

	talk_xml <- xmlTreeParse( talk_fname )
	stopifnot( !is.null( talk_xml$doc$children$talk ) )

	#create root element
	talk_node <- talk_xml$doc$children$talk
	name <- xmlGetAttr( talk_node, "name" )
	stopifnot( !is.null( name ) )
	talk <- new( "talk", name = name )

	#parse child objects
	for( i in xmlChildren( talk_node ) ) {
		if( xmlName( i ) == "comment" ) {
			next
		} else if( xmlName( i ) == "file" ) {
			talk@files[[ length( talk@files ) + 1 ]] <- .processFile( i )
		} else if( xmlName( i ) == "section" ) {
			section <- .processSection( i )
			talk@sections[[ length( talk@sections ) + 1 ]] <- section
		} else {
			stop( paste( "unhandled xml tag:", xmlName( i ) ) )
		}
	}

	return( talk )
}


#given a talk, return a vector of all section names
.getSectionNames <- function( talk )
{
	stopifnot( any( class( talk ) == "talk" ) )
	section_names <- character( length( talk@sections ) )
	i <- 1
	for( s in talk@sections ) {
		section_names[ i ] <- s@name
		i <- i + 1
	}
	return( section_names )
}
	
	

#retuns a list of 2 element vectors (i,j) where i is the section index, and j is the items index
#each element of the list corresponds to a break point
.getTalkIndexes <- function( talk )
{
	stopifnot( !missing( talk ) )
	stopifnot( inherits( talk, "talk" ) )
	i <- 1
	breaks <- list()
	for( section in talk@sections ) {
		j <- 1
		start <- TRUE
		for( item in section@items ) {
			is_break <- inherits( item, "break" )
			if( start && is_break == FALSE )
				breaks[[ length( breaks ) + 1 ]] <- c( i, j )
			start <- is_break
			j <- j + 1
		}
		i <- i + 1
	}
	return( breaks )
}

#get the slide index which corresponds to the first slide in a given section
.getIndexForSection <- function( talk, section_id )
{
	indices <- .getTalkIndexes( talk )
	i <- 1
	for( index in indices ) {
		if( section_id == index[1] )
			break
		i <- i + 1
	}
	return( i )
}

#returns win description for a button for a file or section
.getButton <- function( talk_name, obj )
{
	if( inherits( obj, "section" ) ) {
		b <- paste( "button text=\"", obj@name, "\" function=.setsection action=\"", talk_name, ":", obj@section_id,"\" width=10 padx=4 pady=4 fg=red3 bg=whitesmoke",sep="")
		return( b )
	}
	if( inherits( obj, "file" ) ) {
		b <- paste( "button text=\"", obj@name, "\" function=.presentTalkOpenFile action=\"", obj@filename,"\" width=10 padx=4 pady=4 fg=blue bg=whitesmoke",sep="")
		return( b )
	}
}

#gets widget descriptions for file and section buttons
.getButtons <- function( talk )
{
	#create a list of buttons
	section_but <- list()
	file_but <- list()

	#process top level buttons (under talk)
	for( f in talk@files ) {
		i <- length( file_but ) + 1
		file_but[[ i ]] <- f
	}

	#process buttons under sections
	sect_id <- 1
	for( s in talk@sections ) {
		if( s@button == TRUE ) {
			i <- length( section_but ) + 1
			s@section_id <- as.integer( sect_id )
			section_but[[ i ]] <- s
		}
		for( item in s@items ) {
			if( inherits( item, "file" ) && item@button == TRUE ) {
				i <- length( file_but ) + 1
				file_but[[ i ]] <- item
			}
		}
		sect_id <- sect_id + 1
	}

	#create win desc corresponding to list of buttons

	n_files <- length( file_but ) #files on the left
	n_sections <- length( section_but ) #sections on the right
	cols <- 4 #number of cols to use
	min_rows <- 9999
	min_row_i <- 0
	for( i in 1:cols ) {
		#determine optimal column assignment
		rows <- ceiling( max( n_files / i, n_sections / ( cols - i ) ) )
		if( i == 0 || rows <= min_rows ) {
			min_rows <- rows
			min_row_i <- i
		}
	}
	
	stopifnot( min_rows > 0 )
	w <- paste( "grid ", min_rows, " ", cols + 1, " relief=groove sticky=NEW", sep="" )
	file_i <- 1
	section_i <- 1
	#iterate over each (i,j) position in button grid, insert file buttons on the left,
	#then a center null widget, then section buttons on the right
	for( j in 1:min_rows ) {
		for( i in 1:(cols+1) ) {
			if( i <= min_row_i ) {
				if( file_i > length( file_but ) ) {
					w <- append( w, "null padx=34" )
				} else {
					w <- append( w, .getButton( talk@name, file_but[[ file_i ]] ) )
					file_i <- file_i + 1
				}
			} else if( i == ( min_row_i + 1 ) ) {
				w <- append( w, "null padx=5" )
			} else {
				if( section_i > length( section_but ) ) {
					w <- append( w, "null padx=20" )
				} else {
					w <- append( w, .getButton( talk@name, section_but[[ section_i ]] ) )
					section_i <- section_i + 1
				}
			}
		}
	}
	return( w )
}

#get widget description for menus
.getMenus <- function( talk )
{
	stopifnot( inherits( talk, "talk" ) )
	sections <- c()
	files <- c()
	sect_id <- 1
	for( s in talk@sections ) {
		#save section under menu
		i <- length( sections ) + 1
		sections[ i ] <- paste( "menuitem label=", s@name, " function=.setsection action=", talk@name, ":", sect_id, sep="" )

		#look for files
		for( item in s@items ) {
			if( inherits( item, "file" ) ) {
				i <- length( files ) + 1
				files[ i ] <- paste( "menuitem label=", item@name, " function=.presentTalkOpenFile action=", item@filename, sep="" )
			}
		}
		sect_id <- sect_id + 1
	}

	w <- paste( "menu nitems=", length( sections ), " label=Sections", sep="" )
	w <- append( w, sections )

	w <- append( w, paste( "menu nitems=", length( files ), " label=Files", sep="" ) )
	w <- append( w, files )

	return( w )
}

#open files from the win act, and supports multiple files
.presentTalkOpenFile <- function()
{
	f <- getWinAct()[ 1 ]
	f <- strsplit( f, "\\s+" )
	print( f )
	sapply( f, openFile )
}

.updateSlide <- function( talk )
{
	index <- .PBSmod[[ ".presentTalk" ]][[ talk@name ]]$index 
	indicies <- .getTalkIndexes( talk )
	section_id <- indicies[[ index ]][ 1 ]
	item_id <- indicies[[ index ]][ 2 ]
	items <- talk@sections[[ section_id ]]@items
	num_sections <- length( talk@sections )

	#make sure the correct section is visible
	section_names <- .getSectionNames( talk )
	setWinVal( list( section = section_names[ section_id ] ) )

	#set slide label
	#setWinVal( list( slide_num = paste( "slide: ", index, "/", length( indicies ) ) ) )
	setWinVal( list( slides = index ) )

	if( index > length( indicies ) ) {
		cat( "end of talk" )
		return();
	}

	#setWidgetState( "next", ifelse( index >= length( indicies ), "disabled", "normal" ) );
	#setWidgetState( "prev", ifelse( index <= 1, "disabled", "normal" ) );
	#setWidgetState( "start", ifelse( indicies[[ index ]][ 2 ] == 1, "disabled", "normal" ) );

	setWidgetState( "prev", ifelse( section_id == 1, "disabled", "normal" ) );
	setWidgetState( "next", ifelse( section_id == num_sections, "disabled", "normal" ) );
	#setWidgetState( "start", ifelse( section_id == 1 && item_id == 1, "disabled", "normal" ) );

	setWidgetState( "go", ifelse( index >= length( indicies ), "disabled", "normal" ) );
	setWidgetState( "back", ifelse( index <= 1, "disabled", "normal" ) );


	
	#get next item to iterate to, if next item is in the next section, stop at last in this section
	if( index < length( indicies ) )
		m <- max( length( items ), indicies[[ index + 1 ]][ 2 ] )
	else
		m <- length( items ) #last slide case

	text <- c()
	while( item_id <= m ) {
		item <- items[[ item_id ]]
		item_id <- item_id + 1
		if( inherits( item, "text" ) ) {
			#text items
			cat( item@text, "\n" )
			text <- append( text, item@text )
		} else if( inherits( item, "file" ) ) {
			#file items
			openFile( item@filename )
		} else if( inherits( item, "code" ) ) {
			#code items
			code <- strsplit( item@code, "\n" )[[ 1 ]]
			if( item@print == TRUE && item@eval == FALSE ) {
				#print to consol
				code_cat <- paste( "> ", code, "\n", sep="" )
				cat( code_cat, sep="" )
			}
			if( item@eval == FALSE ) next
			res <- capture.output( eval( parse( text = code ), envir = globalenv() ) )
			#print results
			if( item@show == TRUE ) {
				res <- paste( c( res, ""), collapse = "\n" )
				cat( res )
			}
		} else if( inherits( item, "break" ) ) {
			break
		}
	}
	cat( "-----------------------------------< Press next slide >---------\n" )
}


.startSlide <- function( talk ) {
	.PBSmod[[ ".presentTalk" ]][[ talk@name ]]$index <<- 1
	talk <- .PBSmod[[ ".presentTalk" ]][[ talk@name ]]$talk
	.updateSlide( talk )
}
.prevSlide <- function() {
	talk_name <- getWinAct()[1]
	index <- .PBSmod[[ ".presentTalk" ]][[ talk_name ]]$index
	talk <- .PBSmod[[ ".presentTalk" ]][[ talk_name ]]$talk
	.PBSmod[[ ".presentTalk" ]][[ talk_name ]]$index <<- index - 1
	.updateSlide( talk )
}
.nextSlide <- function() {
	talk_name <- getWinAct()[1]
	index <- .PBSmod[[ ".presentTalk" ]][[ talk_name ]]$index
	talk <- .PBSmod[[ ".presentTalk" ]][[ talk_name ]]$talk
	.PBSmod[[ ".presentTalk" ]][[ talk_name ]]$index <<- index + 1
	.updateSlide( talk )
}
.slidedrop <- function() {
	#get talk
	talk_name <- getWinAct()[1]
	talk <- .PBSmod[[ ".presentTalk" ]][[ talk_name ]]$talk
	index <- .PBSmod[[ ".presentTalk" ]][[ talk_name ]]$index

	section_id <- .getTalkIndexes( talk )[[ index ]][ 1 ]
	new_index <- getWinVal()$slides.id

	#do nothing if current section is re-selected
	if( index == new_index )
		return()

	.PBSmod[[ ".presentTalk" ]][[ talk_name ]]$index <<- new_index
	.updateSlide( talk )
}
.sectiondrop <- function() {
	#get talk
	talk_name <- getWinAct()[1]
	talk <- .PBSmod[[ ".presentTalk" ]][[ talk_name ]]$talk
	index <- .PBSmod[[ ".presentTalk" ]][[ talk_name ]]$index

	section_id <- .getTalkIndexes( talk )[[ index ]][ 1 ]
	new_sect_id <- getWinVal()$section.id

	#do nothing if current section is re-selected
	if( section_id == new_sect_id )
		return()

	.PBSmod[[ ".presentTalk" ]][[ talk_name ]]$index <<- .getIndexForSection( talk, new_sect_id )
	.updateSlide( talk )
}

.setsection <- function()
{
	#TODO encode talk name and section id together
	act = getWinAct()[ 1 ]
	act = unlist( strsplit( act, ":" ) )
	talk_name <- act[ 1 ]
	act <- act[ 2 ]
	talk <- .PBSmod[[ ".presentTalk" ]][[ talk_name ]]$talk
	index <- .PBSmod[[ ".presentTalk" ]][[ talk_name ]]$index
	indicies <- .getTalkIndexes( talk )
	section_id <- indicies[[ index ]][ 1 ]

	if( act == "+1" )
		index <- .getIndexForSection( talk, section_id + 1 )
	else if( act == "-1" )
		index <- .getIndexForSection( talk, section_id - 1 )
	else if( act == "0" )
		index <- .getIndexForSection( talk, section_id )
	else
		index <- .getIndexForSection( talk, as.integer( act ) )

	.PBSmod[[ ".presentTalk" ]][[ talk_name ]]$index <<- index
	.updateSlide( talk )
}


presentTalk <- function( fname )
{
	PBSmodelling:::.initPBSoptions()
	#setup .PBSmod$.talk (should be seperate package)
	if( !is.null( .PBSmod[[ ".presentTalk" ]] ) )
		.PBSmod[[ ".presentTalk" ]] <<- list()
	
	#parse XML into a DOM
	talk <- .parseTalkFile( fname )

	#save parsed talk
	name <- talk@name
	.PBSmod[[ ".presentTalk" ]][[ name ]] <<- list( index = 0, talk = talk )

	#create a GUI for it
	createWin( c(
	"window name=presentwin",
	.getMenus( talk ),
	"grid 1 2 pady=\"0 5\"",
		"grid 1 1 relief=groove",
		"grid 2 1 padx=3 pady=3",
		"grid 1 2 sticky=E",
		"label \"section:\" font=8 sticky=W",
		paste( "droplist name=section values=\"\" function=.sectiondrop bg=skyblue width=15 action=\"", name, "\"", sep="" ),
		"grid 1 3",
		paste( "button name=prev text=\"< Prev\" bg=skyblue sticky=S function=.setsection action=\"",name,":-1\" width=7", sep="" ),
		paste( "button name=curr text=\"Restart\" bg=skyblue sticky=S function=.setsection action=\"",name,":0\" width=7", sep="" ),
		paste( "button name=next text=\"Next >\" bg=skyblue sticky=S function=.setsection action=\"",name,":+1\" width=7", sep="" ),
	
		"grid 1 1 relief=groove padx=\"5 0\"",
		"grid 2 1 padx=3 pady=3",
		"grid 1 3 sticky=E",
		"label \"slide:\" font=\"8\" sticky=W",
	 	paste( "droplist name=slides values=\"\" function=.slidedrop bg=greenyellow width=9 action=\"",name,"\"", sep="" ),
		"label \"/ n\" name=slidecount font=\"8\" sticky=W",
		"grid 1 2",
		paste( "button name=back text=\"< Back\" bg=greenyellow sticky=S function=.prevSlide action=\"",name,"\" width=10", sep="" ),
		paste( "button name=go text=\"> Go\" bg=greenyellow sticky=S function=.nextSlide action=\"",name,"\" width=10", sep="" ),
	
	.getButtons( talk ),
	""
	), astext = TRUE )
	
	#initialize section droplist
	section_names <- .getSectionNames( talk )
	setWinVal( list( section.values = section_names ) )
	setWinVal( list( section = section_names[1] ) )
	
	#initialize slide droplist
	indicies <- .getTalkIndexes( talk )
	vals <- 1:(length( indicies ) )
	setWinVal( list( slides.values = vals ) )
	setWinVal( list( slides = vals[1] ) )
	setWinVal( list( slidecount = paste( "/", length( indicies ) ) ) )

	#move to first slide
	.startSlide( talk )
}
