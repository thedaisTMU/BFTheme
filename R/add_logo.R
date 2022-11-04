
#' Support function to add logo to a graph
#' @param p Graph object
#' @param logo.type String, takes either "small" or "big" and returns the logo of the appropriate size
#' @return Graph object with the logo added
#' @examples
#' bftheme.init()
#'
add_logo <- function(p,logo.type){
  if(logo.type == "small"){
    brookfield_logo_tag <- str_c("<img src='",.libPaths(),"/BFTheme/extdata/small_icon.png' width='20'/>")
    p <- p + ggplot2::labs(tag = brookfield_logo_tag)
    return(p)
  }
  if(logo.type == "big"){
    brookfield_logo_tag <- str_c("<img src='",.libPaths(),"/BFTheme/extdata/big_icon.png' width='75'/>")
    p <- p + ggplot2::labs(tag = brookfield_logo_tag) + theme(plot.tag.position = c(0.91,0.02))
    return(p)
  }
}
