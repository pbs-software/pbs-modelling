# To start present talk demo:
# 1) setwd( "current directory which contains swisstalk.xml" )
# 2) source( "present_talk.r" )

# Note: this requires the following packages:
require( XML )
require( PBSmodelling )

setClass( "text", representation( text = "character" ) )
setClass( "file", representation( filename = "character" ) )
setClass( "code", representation( show = "logical", print = "logical", code = "character" ) )
setClass( "break", representation( "NULL" ) ) #this prints a message - wtf R?!!! give me an empty class that isn't virtual

setClass( "section", representation( name = "character", items = "list" ) ) #items should be a list of the above 4 s4 classes
setClass( "talk", representation( name = "character", sections = "list" ) )


#given a talk, return a vector of all section names
getSectionNames <- function( talk )
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


processSection <- function( node )
{
	x <- new( "section", name = node$attributes[ "name" ] )
	for( i in xmlChildren( node ) ) {
		
		#TODO refactor with a do.call
		if( xmlName( i ) == "text" ) item <- processText( i )
		else if( xmlName( i ) == "file" ) item <- processFile( i )
		else if( xmlName( i ) == "code" ) item <- processCode( i )
		else if( xmlName( i ) == "break" ) item <- processBreak( i )
		else if( xmlName( i ) == "comment" ) { next } #do nothing
		else stop( paste( "not implmented:", xmlName(i) ) )
		
		x@items[[ length( x@items ) + 1 ]] <- item
	}
	return( x )
}

processText <- function( node )
{
	return( new( "text", text = xmlValue( node ) ) )
}

processFile <- function( node )
{
	return( new( "file", filename = xmlGetAttr( node, "name", "" ) ) )
}

processCode <- function( node )
{
	return( new( "code", 
			show = as.logical( xmlGetAttr( node, "show", TRUE ) ), 
			print = as.logical( xmlGetAttr( node, "print", TRUE ) ), 
			code = xmlValue( node )
			) )
}

processBreak <- function( node )
{
	return( new( "break" ) )
}

talk_xml <- xmlTreeParse( "swisstalk.xml" )

stopifnot( !is.null( talk_xml$doc$children$talk ) )

talk_node <- talk_xml$doc$children$talk
name <- xmlGetAttr( talk_node, "name" )
stopifnot( !is.null( name ) )
talk <- new( "talk", name = name )

for( i in xmlChildren( talk_node ) ) {
	stopifnot( xmlName( i ) == "section" )
	section <- processSection( i )
	talk@sections[[ length( talk@sections ) + 1 ]] <- section
}

#retuns a list of 2 element vectors (i,j) where i is the section index, and j is the items index
#each element of the list corresponds to a break point
getTalkIndexes <- function( talk )
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

getIndexForSection <- function( section_id )
{
	indices <- getTalkIndexes( talk )
	i <- 1
	for( index in indices ) {
		if( section_id == index[1] )
			break
		i <- i + 1
	}
	return( i )
}





#create a GUI for it
createWin( c(
"window name=presentwin",
"droplist name=section values=\"\" function=sectiondrop",
  "grid 1 4 sticky=w",
    "button name=start text=\"section start\" function=startSlide",
    "button name=prev text=\"prev slide\" function=prevSlide",
    "button name=replay text=\"replay current slide\" function=replaySlide",
    "button name=next text=\"next slide\" function=nextSlide",
"text name=text width=80 height=32",
"label name=slide_num text=\"slide: 1/n\" sticky=E"
), astext = TRUE )

#initialize droplist
section_names <- getSectionNames( talk )
setWinVal( list( section.values = section_names ) )
setWinVal( list( section = section_names[1] ) )

updateSlide <- function()
{
	indicies <- getTalkIndexes( talk )
	section_id <- indicies[[ index ]][ 1 ]
	item_id <- indicies[[ index ]][ 2 ]
	items <- talk@sections[[ section_id ]]@items

	#make sure the correct section is visible
	setWinVal( list( section = section_names[ section_id ] ) )

	#set slide label
	setWinVal( list( slide_num = paste( "slide: ", index, "/", length( indicies ) ) ) )

	if( index > length( indicies ) ) {
		cat( "end of talk" )
		return();
	}

	setWidgetState( "next", ifelse( index >= length( indicies ), "disabled", "normal" ) );
	setWidgetState( "prev", ifelse( index <= 1, "disabled", "normal" ) );
	setWidgetState( "start", ifelse( indicies[[ index ]][ 2 ] == 1, "disabled", "normal" ) );
	
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
			if( item@print == TRUE ) {
				#print to consol
				code_cat <- paste( "> ", code, "\n", sep="" )
				cat( code_cat, sep="" )
				#print to text in GUI
				tmp <- paste( code_cat, collapse="" )
				#remove trailing \n
				tmp <- substring( tmp, 1, nchar( tmp ) - 1 )
				text <- append( text, tmp )
			}
			res <- capture.output( eval( parse( text = code ), envir = globalenv() ) )
			#print results
			if( item@show == TRUE ) {
				res <- paste( c( res, ""), collapse = "\n" )
				cat( res )
				text <- append( text, res )
			}
		} else if( inherits( item, "break" ) ) {
			break
		}
	}
	text <- paste( text, collapse = "\n" )
	setWinVal( c( "text" = text ) )
	cat( "-----------------------------------< Press next slide >---------\n" )
	#text <- str( items )
	#print( text )
	#setWinVal( list( text = "eooo" ) )

	##reached end of section - move to next section
	#if( index >= length( items ) ) {
	#	if( section_id < length( talk@sections ) ) {
	#		setWinVal( list( section = section_names[ section_id + 1 ] ) )
	#		index <<- 1
	#	} else
	#		setWidgetState( "next", "disabled" );
	#}
}

index <- 0

startSlide <- function() {
	index <<- 1
	index <<- getIndexForSection( getWinVal()$section.id )
	updateSlide()
}
prevSlide <- function() {
	index <<- index - 1
	updateSlide()
}
replaySlide <- function() {
	#nothing	
	updateSlide()
}
nextSlide <- function() {
	index <<- index + 1
	updateSlide()
}
sectiondrop <- function() {
	section_id <- getTalkIndexes( talk )[[ index ]][ 1 ]
	new_sect_id <- getWinVal()$section.id

	#do nothing if current section is re-selected
	if( section_id == new_sect_id )
		return()

	index <<- getIndexForSection( new_sect_id )
	updateSlide()
}

startSlide()

