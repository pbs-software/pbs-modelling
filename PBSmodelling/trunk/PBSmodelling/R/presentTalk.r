# Note: The code in this file is preliminary, and 
#  the functionality of 'presentTalk' has limitations.
#  A future version will likely be built using S4 class structures.

#2008-10-09----------------------------------AE/RH
# Launch a GUI to control the talks described by
# the given file and run the first talk.
#   x = filename of Talk Description File
# Note: 'chunk' in code is equivalent to 'segment' in documentation.
#Start presentTalk--------------------------------
presentTalk=function(x,debug=FALSE){
	dbg=debug
	pTalk=getPBSoptions(".presentTalk")
	if(!is.null(pTalk))
		unpackList(pTalk,scope="L")
	#if a talk already exist, kill its GUI
	if(exists("tn",frame=sys.nframe()))
		closeWin(names(talks)[tn])
	setPBSoptions(".presentTalk",NULL) # clear entire talk
	setPBSoptions(".presentTalk",.parseTalk(x))
	setPBSoptions(".presentTalk",list(tn=1,sn=1,cn=1,back=0,atEnd=FALSE,debug=dbg),sublist=TRUE)
	.runTalk(1)
	invisible() }
#End presentTalk----------------------------------

#2008-10-09----------------------------------AE/RH
# Returns talk description file parsed into list.
#Start .parseTalk---------------------------------
.parseTalk=function(x){
	#Subfunctions------------------------
	#regular expression functions with different defaults
	myGrep=function(pattern, x, value=FALSE){
		return(grep(pattern, x, value=value, ignore.case=TRUE, perl=TRUE))
	}
	mySub=function(subpattern, replacement, x){
		return(sub(subpattern, replacement, x, ignore.case=TRUE, perl=TRUE))
	}
	myGsub=function(pattern, replacement, x){
		return(gsub(pattern, replacement, x, ignore.case=TRUE, perl=TRUE))
	}
	myRegexpr=function(pattern, text){
		return(regexpr(pattern, text, ignore.case=TRUE, perl=TRUE))
 	}
 	myGregexpr=function(pattern, text){
		return(gregexpr(pattern, text, ignore.case=TRUE, perl=TRUE))
 	}
 	myStrsplit=function(x, split){
		return(strsplit(x, split, perl=TRUE))
	}
	# Assumes tagType and optionType are valid
	# stop()s if optVal is invalid
	validateOption=function(tagType, optType, opt, tag){
		for (i in .tagDefs[[tagType]][[optType]]$checks){
			if(!length(myGrep(.tagOptionChecks[[i]]$regexp, opt)))
				stop(paste("Bad value for ", optType, " option in ", tagType,
						" tag:\n", tag, "\n", .tagOptionChecks[[i]]$error, sep=""))
		}
	}
	# Returns list:
	# $tagType=type of tag
	# $opts=list of valid options
	# stops if there is an error.
	validateTag=function(tag){
		validTagTypes=names(.tagDefs)
		matchTagType=paste("((", paste(validTagTypes, collapse=")|("), "))", sep="")
		matchTag=paste("^<", matchTagType, "((\\s+.*>)|(>))$", sep="")

		if(!length(myGrep(matchTag, tag)))
			stop(paste("Bad tag name\n", tag, "\n",
					"Valid tag names are ", paste(validTagTypes, collapse=", "), sep=""))

		tagType=tolower(mySub(matchTag, "\\1", tag))

		validOptTypes=names(.tagDefs[[tagType]])
		matchOptTypes=paste("((", paste(validOptTypes, collapse=")|("), "))",
				sep="")

		#isolate options from rest of tag
		opts=mySub(paste("^<", tagType, "\\s*(.*)\\s*>$", sep=""), "\\1", tag)

		validOptions=list()
		while(opts!=""){ #while there are unparsed options
			matchOptType=paste("^\\s*", matchOptTypes, ".*", sep="")
			if(!length(myGrep(matchOptType, opts)))
				stop(paste("Bad option name for ", tagType, " tag\n", tag, "\n",
					"Valid option names are ", paste(validOptTypes, collapse=", "),
							sep=""))
			optType=mySub(matchOptType, "\\1", opts)

			#handle quoted strings separately
			if(.tagDefs[[tagType]][[optType]][["class"]]=="character" &&
				length(myGrep(paste("^\\s*", optType, "\\s*=\\s*\"", sep=""), opts))){
				matchQuoted=paste("^\\s*", optType, "\\s*=\\s*\"([^\"]*)\".*", sep="")
				if(!length(myGrep(matchQuoted, opts)))
					stop(paste("Value for ", optType, " option in ", tagType, " does ",
						"have closing quote.\n", tag, sep=""))
				string=mySub(matchQuoted, "\\1", opts)

	####continuing in while loop
				opts=mySub(paste("^\\s*", optType, "\\s*=\\s*\"[^\"]*\"\\s*(.*)",
						sep=""), "\\1", opts)
				validateOption(tagType, optType, string, tag)
				validOptions[[optType]]=string
				next
			}

			matchNonQuoted=paste("^\\s*", optType, "\\s*=\\s*([^\\s]+)\\s*",
					sep="")
			if(!length(myGrep(matchNonQuoted, opts)))
					stop(paste("Value for", optType, "option in", tagType, "tag must be",
						"specified.\n", tag))
			opt=mySub(paste(matchNonQuoted, "(.*)", sep=""), "\\1", opts)

			if(.tagDefs[[tagType]][[optType]][["class"]]=="integer"){
				if(!length(myGrep("^((-?[1-9][0-9]*)|0)$", opt)))
				stop(paste("Value for", optType, "option in", tagType, "tag must be",
						"valid integer.\n", tag))
				opt=as.integer(opt)
			}

			if(.tagDefs[[tagType]][[optType]][["class"]]=="logical"){
				if(!length(myGrep("^(T|TRUE|F|FALSE)$", opt)))
				stop(paste("Value for", optType, "option in", tagType, "tag must be",
						"valid boolean value: T/TRUE or F/FALSE.\n", tag))
				opt=as.logical(opt)
			}
			validateOption(tagType, optType, opt, tag)
			validOptions[[optType]]=opt
			opts=mySub(matchNonQuoted, "", opts)
		}
		for(i in validOptTypes){
			if(!is.null(validOptions[[i]]))
				next
			if(is.null(.tagDefs[[tagType]][[i]]$default))
				stop(paste("Missing required option ", i, " from ", tagType, " tag.\n",
						tag, sep=""))
			validOptions[[i]]=.tagDefs[[tagType]][[i]]$default
		}
		return(list(tagType=tagType, opts=validOptions))
	}
	# Returns character position of first tag in a given line for 0 if no tag in the line
	firstTagPos=function(x){
		validTagTypes=names(.tagDefs)
		matchTagType=paste("((", paste(validTagTypes, collapse=")|("), "))", sep="")
		tagPosInfo=myGregexpr(paste("(\\\\)?<", matchTagType, sep=""), x)
		if(tagPosInfo[[1]][1]==-1) #no tags found
			return(0)
		for(i in tagPosInfo[[1]]){
			if(substr(x,i,i)!="\\") #tag wasn't preceded by backslash
				return(i)
		}
		return(0)
	}
	parseFileChunk=function(chunk){
		chunk=paste(chunk, collapse=" ")
		files=character(0)
		chunk=mySub("^\\s*(.*)", "\\1", chunk) #remove inital whitespace
		while(chunk!=""){
			firstChar=substr(chunk, 1, 1)
			if(firstChar=="\""){
				if(!length(myGrep("\".*\"", chunk)))
					stop(paste("Quoted file name does not have closing quote ('\"')",
							chunk, sep="\n"))
				files=c(files, mySub("^\"([^\"]*)\".*", "\\1", chunk))
##while increments here
				chunk=mySub("^\"[^\"]*\"\\s*(.*)", "\\1", chunk)
			}else{
					files=c(files, mySub("^([^\\s]+).*", "\\1", chunk))
					chunk=mySub("^[^\\s]+\\s*(.*)", "\\1", chunk)
			}
		}
		return(files)
	}
	#End subfunctions------------------------

	validTagTypes=names(.tagDefs)
	matchTagType=paste("((", paste(validTagTypes, collapse=")|("), "))", sep="")

	if(!file.exists(x))
		stop(paste("Cannot find file", x))

	desc=readLines(x, warn=FALSE, encoding=getOption("encoding"))
	if(!length(desc))
		error(paste("Empty file", x))
	desc=.stripComments(desc)

	talks=list()
	fileList=list()
	buttonList=list()
	nTalks=0
	nSects=0
	inSect=FALSE
	chunk=NULL
	talktab=NULL
	i=1
	while(i<=length(desc)){#while there are lines to parse
		firstTag=firstTagPos(desc[i])
		#if there is a tag but not at the first position
		#and there is only whitespace before the tag, remove it
		if(firstTag>1 && length(myGrep("^\\s+$", substr(desc[i], 1, firstTag-1)))){
				desc[i]=mySub("^\\s+", "", desc[i])
				firstTag=1
		}
		if(firstTag!=1){ #if there is a text segment before the tag, or no tag
			if(is.null(chunk)){ #if not within text/code chunk
				#if there's non-tag, non-whitespace text, error
				if(!length(myGrep("^\\s*$", desc[i])))
					stop(paste("Text outside of valid segment:\n",
							desc[i]))
				#skip this line if it was all whitespace
#WHILE INCREMENTS HERE
				i=i+1
				next
			}
			if(firstTag){ #if there is a tag later in the line
				#move it to the beginning of a new next line
				desc=append(desc, substr(desc[i], firstTag, length(desc[i])), after=i)
				desc[i]=substr(desc[i], 1, firstTag-1)
				}
#WHILE INCREMENTS HERE
				i=i+1
				next
		}
		#make sure the tag is closed on this line
		if(!length(myGrep(paste("^<", matchTagType, "[^>]*>", sep=""), desc[i])))
			stop(paste("Tags must be completed on the line they are started\n",
					desc[i]))
		tag=mySub(paste("(^<", matchTagType, "[^>]*>).*", sep=""), "\\1", desc[i])

		#if there is something after this tag, move it to its own line,
		#trimming initial whitespace
		afterTag=mySub(paste("^<", matchTagType, "[^>]*>", sep=""), "", desc[i])
		afterTag=mySub("^\\s*", "", afterTag)
		if(afterTag!=""){
			desc=append(desc, afterTag, after=i)
			desc[i]=tag
		}
		if(!is.null(chunk)){ #if in a text/code chunk
			if((i-1)>=chunk){
				chunk=desc[chunk:(i-1)]
				if(tagType=="file"){
					chunk=parseFileChunk(chunk)
					fileList[[nTalks]][[opts$name]]=chunk
					attr(fileList[[nTalks]][[opts$name]], "options")=opts
				}
				if(inSect){
					talks[[nTalks]][[nSects]]=c(talks[[nTalks]][[nSects]], list(chunk))
					talktab=rbind(talktab,c(tn=nTalks,sn=nSects,cn=length(talks[[nTalks]][[nSects]])))
					attr(talks[[nTalks]][[nSects]][[length(talks[[nTalks]][[nSects]])]],
						"options")=opts
				}
			}
			chunk=NULL
		}
		#unpacks variables: tagType and opts
		unpackList(validateTag(tag), scope="L")

		#do talk/segment increments, logical location flags, & make sure tags are in the right place
		if(tagType=="talk"){
			nTalks=nTalks+1
			nFiles=0
			inSect=FALSE
		}
		if(tagType=="section"){
			if(!nTalks)
				stop(paste("Section must be within a talk block\n", desc[i]))
			inSect=TRUE
			nSects=nSects+1
		}
		if(any(c("code", "text")==tagType)){
				if(!inSect)
					stop(paste("Code must be within a section block\n",desc[i]))
		}
		if(tagType=="file"){
			nFiles=nFiles+1
			if(!nTalks)
				stop(paste("File tag must be within a talk block\n",desc[i]))
		}
		#add information to lists
		if(tagType=="talk"){
			talks[[nTalks]]=list()
			fileList[[nTalks]]=list()
			names(talks)[[nTalks]]=opts$name
			attr(talks[[nTalks]], "options")=.removeFromList(opts,"name")
			buttonList[[nTalks]]=list()
			if(opts$button){ #note button must be added to all talks
				for(j in 1:nTalks){
					if(length(buttonList[[j]])<opts$col ||
							is.null(buttonList[[j]][[opts$col]]))
						buttonList[[j]][[opts$col]]=list()
					row=length(buttonList[[j]][[opts$col]])+1
					buttonList[[j]][[opts$col]][[row]]=nTalks
					attr(buttonList[[j]][[opts$col]][[row]], "type")="talk"
				}
			}
		}else if(tagType=="section"){
			index=length(talks[[nTalks]])+1
			talks[[nTalks]][[index]]=list()
			names(talks[[nTalks]])[[index]]=opts$name
			attr(talks[[nTalks]][[index]], "options")=
					.removeFromList(opts, "name")
			if(opts$button){ #button added for this talk
				if(length(buttonList[[nTalks]])<opts$col ||
						is.null(buttonList[[nTalks]][[opts$col]]))
					buttonList[[nTalks]][[opts$col]]=list()
				row=length(buttonList[[nTalks]][[opts$col]])+1
				buttonList[[nTalks]][[opts$col]][[row]]=nSects
				attr(buttonList[[nTalks]][[opts$col]][[row]], "type")="section"
			}
		} else if (tagType=="file"){
			chunk=i+1
			if(opts$button){ #button added for this file
				if(length(buttonList[[nTalks]])<opts$col ||
						is.null(buttonList[[nTalks]][[opts$col]]))
					buttonList[[nTalks]][[opts$col]]=list()
				row=length(buttonList[[nTalks]][[opts$col]])+1
				buttonList[[nTalks]][[opts$col]][[row]]=opts$name
				attr(buttonList[[nTalks]][[opts$col]][[row]], "type")="file"
			}
		} else
			chunk=i+1
		i=i+1
#WHILE INCREMENTS HERE
	}
	talktab=as.data.frame(talktab)
	talktab$seg=1:nrow(talktab)
	z0=talktab$cn==1 # start chunk
	z1=c(z0[2:length(z0)],TRUE) # end chunk
	talktab$chunkEnds = (z0|z1)
	return(list(talks=talks, fileList=fileList, buttonList=buttonList, talktab=talktab)) }
#End .parseTalk-----------------------------------

#2008-10-03----------------------------------AE/RH
# Run given talk, creating a window to control it 
# and setting the talk number globally.
#Start .runTalk-----------------------------------
.runTalk=function(x){
	if(missing(x))
		x=as.integer(getWinAct()[1])
	unpackList(getPBSoptions(".presentTalk"),scope="L")
	guiExists=exists("tn",frame=sys.nframe())
	if (guiExists && tn!=x)
		closeWin(names(talks)[tn])
	if(!guiExists || tn!=x){
		setPBSoptions(".presentTalk",list(tn=x),sublist=TRUE) }
	.makeTCGUI()
	.runSection(1) }
#End .runTalk-------------------------------------

#2008-10-09----------------------------------AE/RH
# Run given section number, setting section number globally 
# and resetting midCode boolean to FALSE.
#Start .runSection--------------------------------
.runSection=function(x){
	if(missing(x))
		x=as.integer(getWinAct()[1])
	setPBSoptions(".presentTalk",list(sn=x,midCode=FALSE),sublist=TRUE)
	cat("\nSection",x,paste(rep("=",40),collapse=""),"\n\n")
	.runChunk(1) }
#End .runSection----------------------------------

#2008-10-09----------------------------------AE/RH
# Run a text/code chunk, setting the chunk number globally, 
# incrementing chunk, and continuing to next chunk if no breaks occurred.
#Start .runChunk----------------------------------
.runChunk=function(cn){
	setPBSoptions(".presentTalk",list(cn=cn),sublist=TRUE)
	unpackList(getPBSoptions(".presentTalk"),scope="L")
	if (debug)
		cat(paste("\nFrame ",sys.nframe(),paste(rep("~",40),collapse=""),">",paste(c(tn,sn,cn),collapse=","),"\n"))

	chunk=talks[[tn]][[sn]][[cn]]
	opts=attr(talks[[tn]][[sn]][[cn]], "options")
	# Special case <file>: opening a file
	if(!is.null(opts$button)){
		if (!midCode) {
		for(i in chunk){
			if(file.exists(i)){
				openFile(i)
				cat(paste(paste(rep(".",40),collapse=""),"Opened :", i,"\n"))
			}else
				cat(paste(paste(rep("!",40),collapse=""),"No file:", i,"\n"))
		}}
		#if no break, go to next chunk
		if(midCode || !opts$`break`){
			if((cn+1)>length(talks[[tn]][[sn]]) && sn<length(talks[[tn]])) {
				sn=sn+1; setPBSoptions(".presentTalk",list(sn=sn,midCode=FALSE),sublist=TRUE)
				.runSection(sn) }
			else if ((cn+1)<=length(talks[[tn]][[sn]])) {
				cn=cn + 1; 
				setPBSoptions(".presentTalk",list(cn=cn,midCode=FALSE),sublist=TRUE) 
				.runChunk(cn) }
		}else {
			#cat("--------------- Opened files, Press GO ---------------\n")
			cat(paste(paste(rep("-",40),collapse=""),"<<Press GO>>\n"))
			setPBSoptions(".presentTalk",list(midCode=TRUE),sublist=TRUE) }
		return()
	}
	isCode=!is.null(opts$show)
	mustCat=!midCode && (!isCode || opts$show)
	mustBreakAfterCat=!midCode && ((!isCode && opts$`break`)||(isCode && any(c("show", "all")==opts$`break`)))
	mustEval = !mustBreakAfterCat && isCode
	mustBreakAfterEval = mustEval && isCode && any(c("print","all")==opts$`break`)
	midCode = mustBreakAfterCat && isCode
	atEnd = !midCode && !(cn<length(talks[[tn]][[sn]]) || sn<length(talks[[tn]]))
	setPBSoptions(".presentTalk",list(midCode=midCode,atEnd=atEnd),sublist=TRUE)
	if (debug)
		print(t(data.frame(isCode,midCode,mustCat,mustBreakAfterCat,mustEval,mustBreakAfterEval)))
	if(mustCat){
		if(isCode) cat("> ")
		cat(paste(chunk, collapse=ifelse(isCode,"\n> ","\n")),"\n")
	}
	if(mustBreakAfterCat)
		if(!atEnd)
			cat(paste(paste(rep("-",40),collapse=""),"<<Press GO>>\n"))
			#cat("--------------- Finished display, Press GO ---------------\n")
	if(mustEval){
		for(i in chunk){
			res=eval(parse(text=i), envir=globalenv())
			if(opts$print) print(res)
		}
	}
	if(mustBreakAfterEval && !atEnd)
		cat(paste(paste(rep("-",40),collapse=""),"<<Press GO>>\n"))

	#if not midCode, increment chunk number
	if(!midCode && !atEnd){
		#if no break, go to next chunk
		if(!mustBreakAfterCat || !mustBreakAfterEval){
			if((cn+1) > length(talks[[tn]][[sn]]) && sn < length(talks[[tn]])) {
				sn=sn+1; setPBSoptions(".presentTalk",list(sn=sn,cn=1,midCode=FALSE),sublist=TRUE) 
				cat("\nSection",sn,paste(rep("=",30),collapse=""),"<<Press GO>>\n\n") }
			else if ((cn+1) <= length(talks[[tn]][[sn]])) {
				cn=cn+1; setPBSoptions(".presentTalk",list(cn=cn,midCode=FALSE),sublist=TRUE) 
			if ((!isCode&&!mustBreakAfterCat) || (isCode&&!mustBreakAfterEval)) .runChunk(cn) }
		}
	} 
}
#End .runChunk------------------------------------


#2008-10-03----------------------------------AE/RH
# Create window description file from talk data and launch it.
#Start .makeTCGUI---------------------------------
.makeTCGUI=function(){
	unpackList(getPBSoptions(".presentTalk"),scope="L")
	if (!exists("tn",frame=sys.nframe())) tn=1
	setPBSoptions(".presentTalk",list(tn=tn),sublist=TRUE)
	winDesc=c(
		paste("window", paste(paste(c("name", "title"),
				names(talks)[tn], sep="="), collapse=" ")),
		paste("menu nitems=", length(talks)," label=Talks", sep=""),
		paste("menuitem label=", names(talks)," function=.runTalk action=",
			1:length(talks), sep="")
	)
	opts=attr(talks[[tn]], "options")

	#menu
	winDesc=c(winDesc,
		paste("menu nitems=", length(talks[[tn]])," label=Sections", sep=""),
		paste("menuitem label=", names(talks[[tn]]), " function=.runSection action=",
			1:length(talks[[tn]]), sep="")
	)
	if(length(fileList[[tn]])){
		winDesc=c(winDesc,
			paste("menu nitems=",length(fileList[[tn]])," label=Files", sep=""),
			paste("menuitem label=\"", names(fileList[[tn]]),
				"\" function=.openFilesFromGUI action=\"",names(fileList[[tn]]), "\"", sep="")
		)
	}
	#control widgets
	winDesc=c(winDesc,
		#"label text=Sections: font=\"bold 8\" sticky=W fg=darkblue",
		"grid 1 7",
		paste("button function=.CGUIchooseSection action=.first text=\"<<<",
			"\\nStart\" bg=lightblue1 sticky=S"),
		paste("button function=.CGUIchooseSection action=.prev",
			"text=\"<<\\nPrev\" bg=lightskyblue1 sticky=S"),
		paste("button function=.CGUIchooseSection action=.back text=\" < ",
			"\\nCurr\" bg=skyblue sticky=S"),
		paste("button function=.CGUIchooseSection action=.skip",
			"text=\">\\nNext\" bg=skyblue sticky=S"),
		"null",
		paste("button function=.CGUIback action=.back text=\" < ",
			"\\nBack\" bg=greenyellow sticky=S font=\"bold 10\" width=4"),
		paste("button function=.CGUIgo text=\">\\nGO\" bg=greenyellow",
			"font=\"bold 10\" width=4")
	)
	if(length(buttonList[[tn]])){
		buttonCols=list()
		maxRows=0
		for(col in 1:length(buttonList[[tn]])){
			if(is.null(buttonList[[tn]][[col]])){
				buttonCols[[col]]="null padx=4"
				next
			}
			maxRows=max(maxRows, length(buttonList[[tn]][[col]]))
			buttonCols[[col]]=character(0)
			for(button in buttonList[[tn]][[col]]){
				buttonType=attr(button, "type")
				if(buttonType=="talk"){
					buttonCols[[col]]=c(buttonCols[[col]], paste("button text=",names(talks)[button],
						" function=.runTalk action=",button," padx=4 pady=4 fg=darkgreen bg=whitesmoke",sep=""))
				}
				if(buttonType=="section"){
					buttonCols[[col]]=c(buttonCols[[col]], paste("button text=",names(talks[[tn]])[button],
						" function=.runSection action=",button," padx=4 pady=4 fg=red3 bg=whitesmoke",sep=""))
				}
				if(buttonType=="file"){
					buttonCols[[col]]=c(buttonCols[[col]], paste("button text=\"",
						button, "\" function=.openFilesFromGUI action=", button,
						" padx=4 pady=4 fg=blue bg=whitesmoke", sep=""))
				}
			}
		}
		winDesc=c(winDesc, paste("grid ",length(buttonList[[tn]]), " ", maxRows,
				" byrow=FALSE relief=groove padx=4 pady=4", sep=""))
		for(buttons in buttonCols){
			winDesc=c(winDesc, c(buttons, rep("null padx=4",maxRows-length(buttons))))
		}
	}
	createWin(winDesc, astext=TRUE) }
#End .makeTCGUI-----------------------------------

#2008-09-22----------------------------------AE/RH
# Function for changing section from GUI controls.
#Start .CGUIchooseSection-------------------------
.CGUIchooseSection=function(){
	unpackList(getPBSoptions(".presentTalk"),scope="L")
	action=getWinAct()[1]
	if(action==".first") sn=1
	if(action==".prev" && sn!=1) sn=sn-1
	if(action==".skip"){
		if(sn!=length(talks[[tn]]))
			sn=sn+1
		else{
			showAlert("End of talk.  Press 'Start' button to restart.")
			return()
		}
	}
	.runSection(sn) }
#End .CGUIchooseSection---------------------------

#2008-09-22----------------------------------AE/RH
# Open file specified by action.
#Start .openFilesFromGUI--------------------------
.openFilesFromGUI=function(){
	unpackList(getPBSoptions(".presentTalk"),scope="L")
	action=getWinAct()[1]
	files=fileList[[tn]][[action]]
	badFiles=character(0)
	for(i in files){
		tryRes=try(openFile(i), silent=TRUE)
		if(class(tryRes)=="try-error")
			badFiles=c(badFiles, i)
	}
	if(length(badFiles))
		showAlert(paste("Could not open files:", paste(badFiles, collapse=", ")))
}
#End .openFilesFromGUI----------------------------

#2008-10-09----------------------------------AE/RH
.CGUIgo=function(){
	unpackList(getPBSoptions(".presentTalk"),scope="L")
	if (sn==length(talks[[tn]]) && cn==length(talks[[tn]][[sn]])) atEnd=TRUE
	if (atEnd) z=nrow(talktab) else
	z=apply(talktab[,1:3],1,function(x,p){all(x==p)},p=c(tn,sn,cn))
	go=max(1,(1:nrow(talktab))[z]) # find current position in 'talktab' ('go' not used currently)
	setPBSoptions(".presentTalk",list(back=0,go=go,atEnd=atEnd),sublist=TRUE) # reset backwards counter to 0
	#cat(paste(paste(rep("~",52),collapse=""),">",paste(c(tn,sn,cn),collapse=","),"\n")) # activate for debugging
	if (atEnd) {
		.runChunk(cn)
		showAlert("End of talk.  Press 'Start' button to restart.") }
	else if (cn>length(talks[[tn]][[sn]]) && sn<length(talks[[tn]])) {
		.runSection(sn) }
	else if (cn<length(talks[[tn]][[sn]])) {
		.runChunk(cn) }
}

#2008-10-09----------------------------------AE/RH
.CGUIback=function(){
	unpackList(getPBSoptions(".presentTalk"),scope="L")
	if (back==0){
		if (atEnd) z=nrow(talktab) else
		z=apply(talktab[,1:3],1,function(x,p){all(x==p)},p=c(tn,sn,cn)) # find current position in 'talktab'
		back=max(1,(1:nrow(talktab))[z]-ifelse(atEnd,1,2)) }
	else back=max(1,back-1)
	#cat(paste(paste(rep("~",52),collapse=""),">",paste(c(tn,sn,cn),collapse=","),"\n")) # activate for debugging
	last=talktab[back,] # vector of previous (tn,sn,cn) in 'talktab'
	tn=as.numeric(last[1]);sn=as.numeric(last[2]);cn=as.numeric(last[3])
	setPBSoptions(".presentTalk",list(back=back,last=last,tn=tn,sn=sn,cn=cn,midCode=FALSE),sublist=TRUE)
	.runChunk(cn) 
}
