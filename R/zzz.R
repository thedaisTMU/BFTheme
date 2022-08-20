

#' On Load function
.onLoad <- function(libname,pkgname){
  bftheme.init()
  packageStartupMessage("BFTheme loaded successfully \nEnsure you set the RStudio graphical device to Ragg instead of default for correct font rendering")
}
