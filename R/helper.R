#Helper functions
#Dictionary
#      set.colours
#      export.plot.bf
#      set.ticks.seq


#' Set a colour palette or generate one according to your specification.
#' Refers to the main Style Guide on choosing colours that match your intention and report style.
#'set.colours <- function(n,type = "categorical",gradient.choice = "dark.blue")
#' @param n Integer to select the right set of colours
#' @param type Either "categorical" or "gradient", decides the type of colour you're going to get
#' @param categorical.choice Specify custom colour names as a vector (same name as gradient.choice)One of:
#' c("dark.blue","light.blue","pink","yellow","magenta","orange","green","teal","grey")
#' @param gradient.choice For gradient type, specify the colour. One of: c("dark.blue","light.blue",
#' "pink","yellow","magenta","orange","green","teal","grey")
#' @param special special colours, will be added in the future - opposing colours, opposing gradients
#' @return Character vector containing HEX value for colours
#' @examples
#' set.colours(1,type="categorical",gradient.choice="dark.blue")
set.colours <- function(n,
                        type = "categorical",
                        gradient.choice = "magenta",
                        categorical.choice = NULL,
                        special = NULL){
  #Setting all the base vectors to refer to - precise because I don't trust R's generation of gradients
  base.set <- c("dark.blue"="#14365D","light.blue"="#8AD4DF","magenta"="#DD347A","yellow"="#FAAF19"
                ,"pink"="#FFA5B9","green"="#3C7D78","brown"="#C37546","grey"="#626466","ecru"="#EBE6DE")
  dark.blue <- c("#14365D","#284469","#4F6885","#4c6282","#8A9BAE","#6f829b","#8192a8","#94a3b5")
  light.blue <- c("#8AD4DF","#94D7E1","#9FDBE4","#A9DFE7","#B4E3EA","#BFE7ED","#C9EBF0","#D4EFF3")
  magenta <- c("#DD347A","#E04686","#E35892","#E66B9E","#E97DAA","#EC90B6","#EFA2C2","#F2B5CE")
  yellow <- c("#FFC800","#FFCD17","#FFD22E","#FFD745","#FFDC5C","#FFE173","#FFE68B","#FFEBA2")
  pink <- c("#FFA5B9","#85284F","#913D61","#9D5372","#A96884","#B57E95","#C293A7","#CEA9B8")
  brown <- c("#F7941E","#F79D32","#F8A746","#F9B15B","#F9BA6F","#FAC484","#FBCE98","#FCD8AD")
  green <- c("#3C7D78","#528b86","#6D9E9A","#679894","#9EBEBC","#7ca6a2","#a5c2bf","#CEDFDD")
  grey <- c("#626466","#7D8890","#8A949B","#939598","#A4ACB1","#8e9396","#626466","#B1B8BC")
  #Check if you have way too many categories - 7 is the absolute max!
  if(n > 7){
    stop("You have way too many categories. Reduce it!")
  }
  #Check if the type is categorical
  if(type == "categorical"){
    if(is.null(categorical.choice)){ #Check if a specific colour set was requested
      return(unname(base.set[1:n])) #If not then return sequential from dark blue,light blue to pink
    }
    else{
      if(length(categorical.choice)!=n){ #Check if the length of choice matches requested number of colours
        stop("You didn't have the same number of colours as you specified. Change n or categorical.choice") #This is for sanity check, not because of code
      }
      return(unname(base.set[categorical.choice])) #Return the corresponding set of colours
    }
  }
  if(type == "gradient"){ #On the otherhand, if it's a gradient
    #Set up all the gradient choices
    gra2 <- c(1, 5)
    gra3 <- c(1, 4, 7)
    gra4 <- c(1, 3, 5, 7)
    gra5 <- c(1, 2, 4, 6, 7)
    gra6 <- c(1, 2, 3, 4, 6, 7)
    gra7 <- c(1, 2, 3, 4, 5, 6, 7)
    return(get(gradient.choice)[get(str_c("gra",n))]) #Get the right number of gradients.
  }
}

#' Set appropriate ticks. Wrapper for cbreaks in scale that allows for $ and percent ticks
#'
#' @param max numerical max of the ticked plot
#' @param min numerical min of the ticked plot
#' @param unit Unit that goes wit hthe ticks
#' @num.ticks Currently not in use - to specify number of ticks
#' @return A vector with $breaks which are breaks and $labels which are the labelled values
#' @examples
#' set.ticks.seq(100,0,"%")
set.ticks.seq <- function(max,min,unit,num.ticks=5){
  if(unit==""){ #If there are no unit
    ticks <- scales::cbreaks(c(max,min),labels= unit_format(unit=unit,big.mark = ",",sep=""))
  }
  if(unit=="$"){
    ticks <- scales::cbreaks(c(max,min),labels = dollar_format(largest_with_cents = 100)) #Format money
    return(ticks)
  }
  if(unit=="%" & max >= 75){
    ticks <- scales::cbreaks(c(100,0),labels = unit_format(unit="%",big.mark = ",",sep="")) #Format percentage
    return(ticks)
  }
  else{
    ticks <- scales::cbreaks(c(max,min),labels=unit_format(unit=unit,big.mark = ",",sep=" ")) #Format percentage without the percentage sign
    return(ticks)
  }
}

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
                    bg = "transparent",
                    useDingbats=FALSE)
  }
  else{
    stop("File type is not surpported")
  }
}



#' Set Brookfield's base theme

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


#' Initial set up for the BFTheme package that installs the fonts
#'
#' @return Set up for bftheme
#' @examples
#' bftheme.init()


bftheme.init <- function(){
    ##########################
    #Add fonts - Rooney Pro
    ##########################
    sysfonts::font_add("RooneyPro-Regular",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Pro/RooneyPro-Regular.otf"))
    sysfonts::font_add("RooneyPro-RegularItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Pro/RooneyPro-RegularItalic.otf"))
    sysfonts::font_add("RooneyPro-Bold",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Pro/RooneyPro-Bold.otf"))
    sysfonts::font_add("RooneyPro-BoldItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Pro/RooneyPro-BoldItalic.otf"))
    sysfonts::font_add("RooneyPro-Medium",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Pro/RooneyPro-Medium.otf"))
    sysfonts::font_add("RooneyPro-MediumItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Pro/RooneyPro-MediumItalic.otf"))
    sysfonts::font_add("RooneyPro-Light",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Pro/RooneyPro-Light.otf"))
    sysfonts::font_add("RooneyPro-LightItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Pro/RooneyPro-LightItalic.otf"))
    ##########################
    #Add fonts - Rooney Sans
    ##########################
    sysfonts::font_add("RooneySans-Regular",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Sans/RooneySans-Regular.otf"))
    sysfonts::font_add("RooneySans-RegularItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Sans/RooneySans-RegularItalic.otf"))
    sysfonts::font_add("RooneySans-Bold",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Sans/RooneySans-Bold.otf"))
    sysfonts::font_add("RooneySans-BoldItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Sans/RooneySans-BoldItalic.otf"))
    sysfonts::font_add("RooneySans-Medium",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Sans/RooneySans-Medium.otf"))
    sysfonts::font_add("RooneySans-MediumItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Sans/RooneySans-MediumItalic.otf"))
    sysfonts::font_add("RooneySans-Light",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Sans/RooneySans-Light.otf"))
    sysfonts::font_add("RooneySans-LightItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Rooney_Sans/RooneySans-LightItalic.otf"))
    ##########################
    #Add fonts - Pressura
    ##########################
    sysfonts::font_add("GT-Pressura-Regular",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Pressura/GT-Pressura-Regular.otf"))
    sysfonts::font_add("GT-Pressura-RegularItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Pressura/GT-Pressura-RegularItalic.otf"))
    sysfonts::font_add("GT-Pressura-Bold",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Pressura/GT-Pressura-Bold.otf"))
    sysfonts::font_add("GT-Pressura-BoldItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Pressura/GT-Pressura-BoldItalic.otf"))
    sysfonts::font_add("GT-Pressura-Light",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Pressura/GT-Pressura-Light.otf"))
    sysfonts::font_add("GT-Pressura-LightItalic",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Pressura/GT-Pressura-LightItalic.otf"))
    ##########################
    #Add fonts - Entypo
    ##########################
    sysfonts::font_add("Entypo",stringr::str_c(.libPaths(),"/BFTheme/extdata/font_files/Entypo/Entypo-Social.otf"))
    showtext::showtext_auto()

}

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


#Experimental Function
generate.dot <- function(number,
                         nrow,direction="right",
                         starting.coord=c(0,0),
                         dual.colour.break = 0,
                         colour.title = c("First","Second")){
  clean.col <- floor(number/nrow)
  if(direction == "right"){
    x.nudge <- 1
  }
  if(direction == "left"){
    x.nudge = -1
  }
  end.x <- starting.coord[1] + x.nudge * (clean.col-1)
  end.y <- starting.coord[2] + nrow - 1
  main.data <- data.table::data.table(x = rep(seq(starting.coord[1], end.x), each = nrow),
                                      y = rep(seq(starting.coord[2], end.y), times = clean.col))
  remaining <- number - clean.col*nrow
  if(remaining > 0){
    aux.data <- data.table::data.table(x = rep(end.x + x.nudge,times=remaining),
                                       y = seq(starting.coord[2], starting.coord[2] + remaining - 1))
    main.data <- data.table::rbindlist(list(main.data,aux.data))
  }
  if(dual.colour.break > 0){
    main.data[,colour:=c(rep(colour.title[1],dual.colour.break),rep(colour.title[2],number - dual.colour.break))]
  }
  else{
    main.data[,colour:=colour.title[1]]
  }
  return(main.data)
}

#Calculate label brightness
label.colour <- function(col.input){
  rgb <- col2rbg(col.input)
  brightness <- (rgb[1]*299+rgb[2]*587+rgb[3]*114)/1000
  if(brightness > 123){
    return("black")
  }
  else{
    return("white")
  }
}











