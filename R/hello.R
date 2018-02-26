# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

library(ggplot2)
library(data.table)
library(stringr)
library(extrafont)

#' Set a colour palette
#'
#' @param n Integer to select the right set of colours
#' @return Character vector containing HEX value for colours
#' @examples
#' set.colours(1)
set.colours <- function(n){
  if(n==1){
    return("#14365D")
  }
  if(n==2){
    return(c("#14365D","#DD347A"))
  }
  if(n==3){
    return(c("#14365D","#DD347A","#8AD4DF"))
  }
  if(n==4){
    return(c("#14365D","#DD347A","#8AD4DF","#FFC800"))
  }
  if(n>=5){
    stop("You have way too many categories. Reduce it!")
  }
}

#' Export a graph into EPS
#'
#' @param f.name Character denoting file name, can end with or without .eps Basically a wrapper for ggsave and then embed font
#' @param p.obj Plot object to export
#' @param p.height height of the plot (in inches)
#' @param p.width width of the plot (in inches)
#' @return Exported .EPS with the file name for the given plot object
#' @examples
#' export.bf.plot("Rplot.EPS",plot)
export.bf.plot <- function(f.name,p.obj, p.height=6,p.width=9){
  if(stringr::str_sub(f.name,nchar(f.name)-3,nchar(f.name))!=".eps"){
    f.name <- stringr::str_c(f.name,".eps")
  }
  ggplot2::ggsave(f.name,
         plot=p.obj,
         family = "RooneySans Regular",
         fonts = c("RooneySans Light","RooneySans Medium"),
         device = "eps",
         width=p.width,
         height=p.height,
         pointsize=12,
         bg = "transparent")
  extrafont::embed_fonts(f.name,options="-dEPSCrop")
}


#' Create a scatter plot with Brookfield formatting. Uses Rooney font family - make sure you install that with extrafonts before using this.
#' @usage plot.scatter.bf(data,x,y,group.by=NULL,p.size=NULL,unit.x="",
#' unit.y="",plot.title="",plot.fig.num="",title.x.axis="",
#' title.y.axis="",legend.title = "",deg.45 = FALSE,
#' export = FALSE,export.name = "")
#' @param data Main data table to plot
#' @param x Character corresponding the column name for x-axis
#' @param y Character corresponding to the column name for y-axis
#' @param group.by Character corresponding to the column name that points are grouped by (colour)
#' @param p.size Character corresponding to the column name that sets the size for points
#' @param unit.x Character denoting the unit for x-axis. Special formatting for \% and $
#' @param unit.y Character denoting the unit for y-axis. Special formatting for \% and $
#' @param plot.title Character denoting title of the plot
#' @param plot.fig.num Character denoting plot number (or another plot annotations)
#' @param title.x.axis Character denoting title for the x axis
#' @param title.y.axis Character denoting title for the y axis
#' @param legend.title Character denoting legend titles
#' @param deg.45 TRUE/FALSE denoting whether to add 45 degree line or not
#' @param export TRUE/FALSE whether to export file as EPS under default options (height=6 inches, width=9 inches)
#' @param export.name Name of the exported EPS file
#' @return Scatter plot that conforms to Brookfield style
#' @examples
#' plot.scatter.bf(data,"income","automation.risk")
plot.scatter.bf <- function(data,x,y,
                            group.by=NULL,
                            p.size=NULL,
                            unit.x="",
                            unit.y="",
                            plot.title="",
                            plot.fig.num="",
                            title.x.axis="",
                            title.y.axis="",
                            legend.title = "",
                            deg.45 = FALSE,
                            export = FALSE,
                            export.name = ""){
  #This bit sets up the base Brookfield theme elements
  brookfield_theme <- ggplot2::theme(panel.background = element_rect(fill="transparent", colour=NA),
                            plot.background = element_rect(fill="transparent", colour=NA),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            text = element_text(family="RooneySans-Regular"),
                            axis.line = element_line(size=0.25, colour = "#727D84"),
                            axis.ticks = element_line(size=0.25, colour = "#727D84"),
                            plot.title = element_text(family="RooneySans-LightItalic", size=10),
                            plot.subtitle = element_text(family="RooneySans-Medium", size=13),
                            legend.background = element_rect(fill="transparent",colour=NA),
                            legend.key = element_blank(),
                            axis.text.x = element_text(size=11, margin=margin(t=10)),
                            axis.text.y = element_text(size=11, margin=margin(r=10)),
                            axis.title.x = element_text(size=12, margin=margin(t=20)),
                            axis.title.y = element_text(size=12, margin=margin(r=20)))
  #This bit sets the plot size, as well as the frequency of the ticks
  min.x <- signif(floor(min(data[!is.na(get(x)),get(x)])*0.9),2) #Get it down to 2 significant figures
  max.x <- signif(ceiling(max(data[!is.na(get(x)),get(x)])*1.1),2) #Get it down to 2 significant figures
  min.y <- signif(floor(min(data[!is.na(get(y)),get(y)])*0.9),2) #Get it down to 2 significant figures
  max.y <- signif(ceiling(max(data[!is.na(get(y)),get(y)])*1.1),2) #Get it down to 2 significant figures
  #Set tick requirement if it's %
  ticks.seq.x <- signif(seq(min.x,max.x,floor((max.x-min.x)/5)),2)
  if(unit.x=="%"){
    if(max.x>=75){
      ticks.seq.x <- seq(0,100,25)
      min.x <- 0
      max.x <- 105
    }
  }
  ticks.seq.y <- signif(seq(min.y,max.y,floor((max.y-min.y)/5)),2)
  if(unit.y=="%"){
    if(max.y>=75){
      ticks.seq.y <- seq(0,100,25)
      min.y <- 0
      max.y <- 105
    }
  }
  labels.x <- paste0(ticks.seq.x,unit.x)
  labels.y <- paste0(ticks.seq.y,unit.y)
  if(unit.x=="$"){
    labels.x <- paste0(unit.x,formatC(ticks.seq.x,format="d",big.mark=","))
  }
  if(unit.y=="$"){
    labels.y <- paste0(unit.y,formatC(ticks.seq.y,format="d",big.mark=","))
  }
  #Generate base plot
  p <- ggplot2::ggplot(data=data,aes_string(x,y,size=p.size)) +
    brookfield_theme
  #Dealing for cases of having groupings
  if(is.null(group.by)){
    p <- p + ggplot2::geom_point(colour="#14365D")
  }
  if(!is.null(group.by)){
    colours <- set.colours(length(unique(data[,get(group.by)])))
    p <- p + ggplot2::geom_point(aes_string(colour=group.by)) +
      ggplot2::scale_colour_manual(values=colours)
    if(legend.title!=""){
      p <- p + ggplot2::guides(colour=guide_legend(title=legend.title))
    }
  }
  if(!is.null(p.size)){
    p <- p + ggplot2::scale_size(guide=FALSE)
  }
  #Dealing with tick units
  if(deg.45){
    p <- p + ggplot2::geom_abline(intercept = 0, slope = 1, colour = "#14365D")
  }
  p <- p + ggplot2::scale_x_continuous(limits=c(min.x,max.x),breaks=ticks.seq.x,labels=labels.x) +
    ggplot2::scale_y_continuous(limits=c(min.y,max.y),breaks=ticks.seq.y,labels=labels.y) +
    #Deal with labels
    ggplot2::labs(title=plot.fig.num,subtitle=plot.title,x=title.x.axis,y=title.y.axis)
  #Export into a file if export option is on - with default options
  if(export){
    if(export.name==""){
      export.name <- "Rplot"
    }
    export.bf.plot(export.name,p,p.height=6,p.width=9)
  }
  #Returning the plot
  return(p)
}





