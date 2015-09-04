txt <- "
  abline acf approx arrows axis
  box browseURL
  capture.output col2rgb colorRamp colors contour contourLines cor
  demo density dev.cur dev.new dev.off
  extendrange
  fix frame
  grey
  hist
  legend lines
  make.packages.html median mtext
  nlm nlminb
  optim
  pairs par plot plot.new points polygon
  qnorm
  rect rgb rnorm runif
  segments str strwidth symbols
  text
  update
  vignette
"
txt = gsub("\\n"," ",txt)

imports_for_undefined_globals <-
  function(txt, lst, selective = TRUE)
  {
    if(!missing(txt))
      lst <- scan(what = character(), text = txt, quiet = TRUE)
    nms <- lapply(lst, find)
    ind <- sapply(nms, length) > 0L
    imp <- split(lst[ind], substring(unlist(nms[ind]), 9L))
    if(selective) {
      sprintf("importFrom(%s)",
              vapply(Map(c, names(imp), imp),
                     function(e)
                       paste0("\"", e, "\"", collapse = ", "),
                     ""))
    } else {
      sprintf("import(\"%s\")", names(imp))
    }
  }

# writeLines(imports_for_undefined_globals(txt))

