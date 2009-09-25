require(PBSmodelling)
#returns name of this package -> inside a function to make it const

#taken from R Curl - merge x and y, keeping x elements if it exists in y too
.mergeLists <- function( x, y ) 
{
    if (length(x) == 0) 
        return(y)
    if (length(y) == 0) 
        return(x)
    i = match(names(y), names(x))
    i = is.na(i)
    if (any(i)) 
        x[names(y)[which(i)]] = y[which(i)]
    x
}


createOptionManager <- function( filename, initial_options = list(), gui_prefix = "option" )
{
	options <- initial_options

	load <- function( fname )
	{
		if( missing( fname ) == FALSE )
			setFileName( fname )
		if( file.exists( filename ) == FALSE )
			return()
		tmp <- readList( filename )
		options <<- .mergeLists( tmp, options )
	}

	setFileName <- function( name )
	{
		filename <<- name
	}

	getFileName <- function()
	{
		return( filename )
	}

	saveas <- function() #prompts user to select a file name
	{
		selected <- selectFile( mode="save")
		if( length( selected ) == 0 || selected == "" )
			return()

		#TODO alert if file already exists - do we really want to overwrite?

		filename <<- selected
		save()
	}

	#silent save
	save <- function( fname )
	{
		if( missing( fname ) == FALSE )
			setFileName( fname )
		writeList( options, fname = filename )
	}

	#get( some_key ) or get() for all key/values
	get <- function( key )
	{
		if( missing( key ) )
			return( options )
		return( options[[ key ]] )
	}

	#set( key = value )
	set <- function( ... )
	{
		v <- list( ... )
		if( length( v ) == 0 ) return()
		if( is.null( names( v ) ) || any( names( v ) == "" ) )
			stop( "values must be named" )

		for( i in 1:length( v ) ) {
			options[ names(v)[ i ] ] <<- list( v[[ i ]] )
		}
		
	}

	#save GUI values to this option list (R)
	savegui <- function()
	{
		values <- getWinVal()
		widgets <- names( values )
		opts <- paste( gui_prefix, names( get() ), sep="" )
		m <- match( widgets, opts )
		to_update = widgets[ !is.na( m ) ] #names of widgets which correspond to a value

		for( w in to_update )
			options[[ w ]] <<- values[[ w ]]
			
	}

	#load GUI values from this option list (R)
	loadgui <- function()
	{
		values <- getWinVal()
		widgets <- names( values )
		opts <- paste( gui_prefix, names( get() ), sep="" )
		m <- match( widgets, opts )
		to_update = widgets[ !is.na( m ) ] #names of widgets which correspond to a value

		opts <- list()
		for( w in to_update )
			opts[[ w ]] <- get( w )

		setWinVal( opts )
	}
	
	getenv <- function()
	{
		return( parent.env( environment() ) )
	}

	#load all functions into the instance list to return
	instance <- list()
	items <- ls() 
	for( i in items ) {
		v <- base::get( i )
		if( is.function( v ) == FALSE ) next
		instance[[ i ]] <- v
	}

	instance$load()
	return( instance )
}


