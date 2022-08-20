

#' On Load function
.onLoad <- function(libname,pkgname){
  bftheme.init()
  packageStartupMessage("BFTheme loaded successfully")
}
