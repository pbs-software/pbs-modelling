sortWidget <- function( d )
{
	if( is.data.frame( d ) == FALSE ) stop( "d must be a data.frame" )
	r <- tclArray()
	for( i in 1:nrow(d) ) {
		#display initial row position as first column
		tcl( "set", paste(r,"(",i,",",1,")", sep=""), i )
		for( j in 1:ncol(d) )
			tcl( "set", paste(r,"(",i,",",j+1,")", sep=""), d[i,j] )
	}

	tt <- tktoplevel()
	p <- tcl( "PBSmodelling::create", .Tk.subwin( tt ), r, paste( nrow(d), ncol(d)+1 ) )
	tkpack( p, expand=1, fill="both" )
	tkfocus(tt)
	return( r )
}

sortWidgetGetVal <- function( tcl_var )
{
	tmp <- tclvalue( tcl("array", "get", tcl_var) )
	tmp <- .tclArrayToVector( tmp )
	
	row_max <- 0
	col_max <- 0
	for( i in seq(from=1, to=length(tmp), by=2 ) ) {
		key <- as.integer( strsplit(tmp[i], ",")[[1]] )
		row_max <- max( row_max, key[1] )
		col_max <- max( col_max, key[2] )
	}
	ret <- matrix( nrow=row_max, ncol=col_max )
	for( i in seq(from=1, to=length(tmp), by=2 ) ) {
		key <- as.integer( strsplit(tmp[i], ",")[[1]] )
		ret[ key[1], key[2] ] <- tmp[i+1]
	}
	return( ret )
}

#r <- sortWidget( beaver1 )
#print( sortWidgetGetVal( r ) )

