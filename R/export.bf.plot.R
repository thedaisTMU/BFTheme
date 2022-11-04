
#' Export a graph into PDF
#'
#' @param f.name Character denoting file name, can end with or without .pdf Basically a wrapper for ggsave and then embed font
#' @param p.obj Plot object to export
#' @param p.height height of the plot (in inches)
#' @param p.width width of the plot (in inches)
#' @return Exported .PDF with the file name for the given plot object
#' @examples
#' export.bf.plot("Rplot.PDF",plot)
export.bf.plot <- function(f.name,p.obj, p.height=6,p.width=7.25,type="pdf"){
  if(type=="pdf"){
    if(stringr::str_sub(f.name,nchar(f.name)-3,nchar(f.name))!=".pdf"){
      f.name <- stringr::str_c(f.name,".pdf") #Add the extension into the name if it's not already added
    }
    ggplot2::ggsave(f.name,
                    plot=p.obj,
                    device = "pdf", #Device used is pdf
                    width=p.width,
                    height=p.height,
                    pointsize=12,
                    bg = "transparent",
                    useDingbats=FALSE)
  }
  else if(type == "eps"){
    if(stringr::str_sub(f.name,nchar(f.name)-3,nchar(f.name))!=".eps"){
      f.name <- stringr::str_c(f.name,".eps") #Add the extension into the name if it's not already added
    }
    ggplot2::ggsave(f.name,
                    plot=p.obj,
                    device = "eps", #Device used is pdf
                    width=p.width,
                    height=p.height,
                    pointsize=12,
                    bg = "transparent")
  }
  else{
    stop("File type is not surpported")
  }
}
