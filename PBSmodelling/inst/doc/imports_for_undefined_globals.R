## Routine originally came from tools:::imports_for_undefined_globals
## Last modified: RH 250618

## Set up a test object called 'txt'
## ---------------------------------
txt <- "
  abline acf approx arrows axis
  box browseURL
  capture.output col2rgb colorRamp colors contour contourLines cor
  demo density dev.cur dev.new dev.off dummkopf
  extendrange
  fix frame
  grey
  hist
  legend lines
  make.packages.html median mtext
  nlm nlminb narnia nairobi
  optim
  pairs par plot plot.new points polygon
  qnorm
  rect rgb rnorm runif
  segments str strwidth symbols sumtingwong
  text
  update
  vignette
"
txt = gsub("\\n"," ",txt)

imports_for_undefined_globals <- 
	function(txt, lst, selective=TRUE, pkg="PBSmodelling")
{
	if(!missing(txt))
		lst <- scan(what = character(), text = txt, quiet = TRUE)
	nms   <- lapply(lst, find)
	ind   <- sapply(nms, length) > 0L
	vnms  <- sapply(nms[ind],function(x){x[1]})  ## get first find (e.g., 'plot' appears under graphics and base) (RH 250618)
	imp   <- split(lst[ind], substring(vnms, 9L))
	out = "## Put the following into 'NAMESPACE':"
	if(selective) {
		out = c(out, sprintf("importFrom(%s)",
			vapply(Map(c, names(imp), imp),
			function(e)
			paste0("\"", e, "\"", collapse = ", "),
			"")))
	} else {
		out = c(out, sprintf("import(\"%s\")", names(imp)))
	}
	## Collect txt items not found in package (RH 250618)
	if (!all(ind)) {
		lost = sort(lst[!ind])
		lose = split(lost, substring(lost,1,1))
		loss = lapply(lose, function(x) { paste0("\"", paste0(x, collapse="\", \""), "\"") } )
		xout = "## Put the following into 'zzz.r':"
		xout = c(xout, "if(getRversion() >= \"2.15.1\") utils::globalVariables(names=c(")
		for (i in 1:length(loss))
			xout = c(xout, loss[[i]])
		xout = c(xout, paste0("), package=\"", pkg, "\" )"))
		out =  c(out, "", xout)
	}
	return(out)
}

#writeLines(imports_for_undefined_globals(txt))

