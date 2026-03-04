

# @importFrom parallel detectCores
.onLoad <- function(libname, pkgname) {

  options(
    
    bitmapType = 'cairo' # unicode support # MUST as of macOS, R 4.5.1
    
  )  
  
}

