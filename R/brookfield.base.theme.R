
#' Set Brookfield's base theme
#' @param inverted logical. Whether an inverse colour scheme (on dark background) is needed
brookfield.base.theme <- function(inverted=FALSE){
  if(!inverted){
    return(ggplot2::theme(panel.background = ggplot2::element_rect(fill="transparent", colour=NA), #Make sure plot area background is transparent
                          plot.background = ggplot2::element_rect(fill="transparent", colour=NA), #Make sure render area background is transparent
                          axis.line = ggplot2::element_line(size=0.4, colour = "#626466"), #Set axis line width and set colour to grey
                          axis.ticks = ggplot2::element_line(size=0.4, colour = "#626466"), #Set axis tick width and set colour to grey
                          panel.grid.major = ggplot2::element_blank(), #Remove the panel grid lines
                          panel.grid.minor = ggplot2::element_blank(), #Remove the panel grid lines
                          text = ggplot2::element_text(family="RooneySans-Regular"), #Set the font for every text element (except for geom elements)
                          plot.title = ggplot2::element_text(family="GT-Pressura-Bold", size=13, colour = "#14365D"), #Format figure number
                          plot.subtitle = ggplot2::element_text(family="RooneySans-Regular", size=11), #Format main title
                          plot.caption = ggtext::element_markdown(family="RooneySans-Regular", size=8.2,hjust=0,colour="#707D85"), #Format for caption and other notes
                          plot.tag = ggtext::element_markdown(),
                          plot.tag.position = c(0.98, 0.02),
                          legend.background = ggplot2::element_rect(fill="transparent",colour=NA), #Make sure legend box is transparent (for export)
                          legend.key = ggplot2::element_blank(), #Background on each key is transparent
                          legend.box.margin = ggplot2::margin(b=4,t=6), #Set margin for the box for appropriate distance
                          legend.title = ggplot2::element_text(family="RooneySans-Light",size=10,hjust=0.5), #Legend title text settings, also make centre it. Light so it's not as prominent
                          legend.title.align = 0.5,
                          legend.text = ggplot2::element_text(family="RooneySans-Light",size=9,margin=ggplot2::margin(r=2)), #Legend text settings. Light so it's not as prominent
                          legend.margin = ggplot2::margin(b=1), #Small margin in the bottom
                          legend.position = "top", #Set the legend to top so panel can be full width (for export)
                          legend.box.spacing = ggplot2::unit(c(0,0,0,0),units=c("cm","cm","cm","cm")), #Legend box spacing - maybe not needed?
                          axis.text.x = ggplot2::element_text(size=10, margin=ggplot2::margin(t=2), family = "GT-Pressura-Light"), #Set axis text. Light to make it less prominent - margin is also precise
                          axis.text.y = ggplot2::element_text(size=10, margin=ggplot2::margin(r=2), family = "GT-Pressura-Light"), #Set axis text. Light to make it less prominent - margin is also precise
                          axis.title.x = ggplot2::element_text(size=11, margin=ggplot2::margin(t=4)), #Set axis title. Margin is also precise
                          axis.title.y = ggplot2::element_text(size=11, margin=ggplot2::margin(r=4)))) #Set axis title. Margin is also precise

  }
  else{
    return(ggplot2::theme(panel.background = ggplot2::element_rect(fill="transparent", colour=NA), #Make sure plot area background is transparent
                          plot.background = ggplot2::element_rect(fill="transparent", colour=NA), #Make sure render area background is transparent
                          axis.line = ggplot2::element_line(size=0.4, colour = "#B1B8BC"), #Set axis line width and set colour to grey
                          axis.ticks = ggplot2::element_line(size=0.4, colour = "#B1B8BC"), #Set axis tick width and set colour to grey
                          panel.grid.major = ggplot2::element_blank(), #Remove the panel grid lines
                          panel.grid.minor = ggplot2::element_blank(), #Remove the panel grid lines
                          text = ggplot2::element_text(family="RooneySans-Regular",color="white"), #Set the font for every text element (except for geom elements)
                          plot.title = ggplot2::element_text(family="GT-Pressura-Bold", size=13,color="white"), #Format figure number
                          plot.subtitle = ggplot2::element_text(family="RooneySans-Regular", size=11,color="white"), #Format main title
                          plot.caption = ggplot2::element_text(family="RooneySans-Regular",face="italic", size=8.2, margin=ggplot2::margin(t=10),hjust=0,colour="#B1B8BC"), #Format for caption and other notes
                          plot.tag = ggtext::element_markdown(),
                          plot.tag.position = c(0.98, 0.02),
                          legend.background = ggplot2::element_rect(fill="transparent",colour=NA), #Make sure legend box is transparent (for export)
                          legend.key = ggplot2::element_blank(), #Background on each key is transparent
                          legend.box.margin = ggplot2::margin(b=4,t=6), #Set margin for the box for appropriate distance
                          legend.title = ggplot2::element_text(family="RooneySans-Light",size=10,hjust=0.5,color="white"), #Legend title text settings, also make centre it. Light so it's not as prominent
                          legend.title.align = 0.5,
                          legend.text = ggplot2::element_text(family="RooneySans-Light",size=9,margin=ggplot2::margin(r=2),color="white"), #Legend text settings. Light so it's not as prominent
                          legend.margin = ggplot2::margin(b=1), #Small margin in the bottom
                          legend.position = "top", #Set the legend to top so panel can be full width (for export)
                          legend.box.spacing = ggplot2::unit(c(0,0,0,0),units=c("cm","cm","cm","cm")), #Legend box spacing - maybe not needed?
                          axis.text.x = ggplot2::element_text(size=9, margin=ggplot2::margin(t=2), family = "GT-Pressura-Light",color="white"), #Set axis text. Light to make it less prominent - margin is also precise
                          axis.text.y = ggplot2::element_text(size=9, margin=ggplot2::margin(r=2), family = "GT-Pressura-Light",color="white"), #Set axis text. Light to make it less prominent - margin is also precise
                          axis.title.x = ggplot2::element_text(size=10, margin=ggplot2::margin(t=4),color="white"), #Set axis title. Margin is also precise
                          axis.title.y = ggplot2::element_text(size=10, margin=ggplot2::margin(r=4),color="white"))) #Set axis title. Margin is also precise

  }
}
