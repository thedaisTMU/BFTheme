
#' Create a scatter plot with Brookfield formatting. Uses Rooney font family - make sure you install that with extrafonts before using this.
#' @usage plot.scatter.bf(data,x,y,group.by=NULL,colours=NULL,p.size=NULL,deg.45=FALSE,trend.line=FALSE
#' plot.limit = NULL,unit.x="",unit.y="",x.axis="",y.axis="",
#' plot.title="",plot.fig.num="",caption= "",logo = FALSE, logo.type = "small",legend.title = "",
#' export = FALSE,export.name = "")
#' @param data Main data table to plot a scatter plot. If not in data.table format, will be converted to it with a warning.
#' @param x Character corresponding the column name for x-axis
#' @param y Character corresponding to the column name for y-axis
#' @param group.by Character corresponding to the column name that points are grouped by (colour)
#' @param colours Vector (or set.colour function) of colours to use. If not, default palette is generated.
#' @param p.size Character corresponding to the column name that sets the size for points
#' @param deg.45 logical TRUE/FALSE denoting whether to add 45 degree line or not
#' @param trend.line logical TRUE/FALSE add a trend line using OLS
#' @param unit.x Character denoting the unit for x-axis. Special formatting for \% and $
#' @param unit.y Character denoting the unit for y-axis. Special formatting for \% and $
#' @param x.axis Character denoting title for the x axis
#' @param y.axis Character denoting title for the y axis
#' @param plot.limit Vector of form c(min.x,max.x,min.y,max.y) or numerics that sets out plot limits
#' @param plot.title Character denoting title of the plot
#' @param plot.fig.num Character that is ordinarily in the form of "Figure X" (or another plot annotations)
#' @param caption Character for caption (sources etc)
#' @param logo logical (TRUE/FALSE) denote whether to add BIIE logo or not
#' @param logo.type Character; either "small" or "big" and activates only when logo is TRUE, decides whether to add full (big) or abridged (small) logo
#' @param legend.title Character denoting legend titles
#' @param export logical TRUE/FALSE whether to export file as PDF under default options (height=6 inches, width=9 inches)
#' @param export.name Name of the exported PDF file
#' @return Scatter plot that conforms to Brookfield style
#' @examples
#' plot.scatter.bf(data,"income","automation.risk")
plot.scatter.bf <- function(data,x,y,
                            group.by=NULL,
                            colours=NULL,
                            p.size=NULL,
                            deg.45 = FALSE,
                            trend.line = FALSE,
                            plot.limit = NULL,
                            unit.x="",
                            unit.y="",
                            x.axis="",
                            y.axis="",
                            plot.title="",
                            plot.fig.num="",
                            caption = "",
                            logo = FALSE,
                            logo.type = "small",
                            legend.title = "",
                            export = FALSE,
                            export.name = ""){
  #This bit sets up the base Brookfield theme elements
  if(!data.table::is.data.table(data)){ #Chek and coerce into data.table
    clone <- data.table::as.data.table(data)
    warning("Data supplied is not data.table - forcing it to be data.table; may not produce desirable results")
  }
  else{
    clone <- cbind(data)

  }
  scatter.theme <- brookfield.base.theme()
  scatter.theme <- scatter.theme + ggplot2::theme(axis.line=ggplot2::element_blank())
  #This bit sets the plot size, as well as the frequency of the ticks
  if(is.null(plot.limit)){
    plot.limit["min.x"] <- signif(floor(min(clone[,get(x)],na.rm=TRUE)*0.9),2) #Get it down to 2 significant figures
    plot.limit["max.x"] <- signif(ceiling(max(clone[,get(x)],na.rm=TRUE)*1.1),2) #Get it down to 2 significant figures
    plot.limit["min.y"] <- signif(floor(min(clone[,get(y)],na.rm=TRUE)*0.9),2) #Get it down to 2 significant figures
    plot.limit["max.y"] <- signif(ceiling(max(clone[,get(y)],na.rm=TRUE)*1.1),2) #Get it down to 2 significant figures
  }
  else{
    names(plot.limit) <- c("min.x","max.x","min.y","max.y")
  }
  #Set ticks
  ticks.seq.x <- set.ticks.seq(plot.limit["max.x"],plot.limit["min.x"],unit=unit.x)
  plot.limit["max.x"] <- max(c(ticks.seq.x$breaks,plot.limit["max.x"]))
  plot.limit["min.x"] <- min(c(ticks.seq.x$breaks,plot.limit["min.x"]))
  ticks.seq.y <- set.ticks.seq(plot.limit["max.y"],plot.limit["min.y"],unit=unit.y)
  plot.limit["max.y"] <- max(c(ticks.seq.y$breaks,plot.limit["max.y"]))
  plot.limit["min.y"] <- min(c(ticks.seq.y$breaks,plot.limit["min.y"]))
  if(is.null(p.size)){ #If no variable name is specified for point size. Can't leave it as NULL since normal point needs size
    p.size <- 1
  }
  #Generate base plot
  p <- ggplot2::ggplot(data=clone,ggplot2::aes_string(x,y)) +
    scatter.theme
  #Dealing for cases of having groupings

  if(is.null(group.by)){
    if(is.null(colours)){ #If a colour is not specified
      colours <- set.colours(1) #Set it to dark blue
    }
    if(trend.line){
      p <- p + ggplot2::geom_smooth(data=clone,ggplot2::aes_string(x,y),colour=colours[1],alpha=1,fill="#D6D6D6",method = "lm") #Add trend line using lm into the plot
    }
    p <- p + ggplot2::geom_point(ggplot2::aes_string(size=p.size),colour=colours) #Only one colour needed
  }
  if(!is.null(group.by)){ #If group by is active
    if(is.null(colours)){ #And colour is not specified
      colours <- set.colours(length(unique(clone[,get(group.by)]))) #Generate default categorical set
    }
    if(trend.line){
      p <- p + ggplot2::geom_smooth(data=clone,ggplot2::aes_string(x,y),colour=colours[1],alpha=0.5,fill="#D6D6D6", method = "lm") #Add trend line using lm into the plot
    }
    p <- p + ggplot2::geom_point(ggplot2::aes_string(colour=group.by,size=p.size)) +
      ggplot2::scale_colour_manual(values=colours)
    if(legend.title!=""){ #If we know the legend title
      num.row <- round(sum(nchar(as.character(unique(clone[,get(group.by)]))))/100)+1 #Set number of row for legend. It's designed for 7.25in exports
      p <- p + ggplot2::guides(colour=ggplot2::guide_legend(title=legend.title,
                                                            title.hjust = 0.5,
                                                            title.position = "top",
                                                            nrow = num.row,
                                                            label.vjust = 0.5))
    }
  }
  if(p.size == 1){ #Scale point size
    p <- p + ggplot2::scale_size(range=c(2.3,2.3),guide="none")
  }
  else{ #Scale point size
    p <- p + ggplot2::scale_size(range=c(2.3,7),guide="none")
  }
  #If 45 degree line is needed
  if(deg.45){
    p <- p + ggplot2::geom_abline(intercept = 0, slope = 1, colour = "#14365D")
  }
  #Dealing with tick units
  p <- p + ggplot2::scale_x_continuous(expand=c(0,0),limits=plot.limit[c("min.x","max.x")],breaks=ticks.seq.x$breaks,labels=ticks.seq.x$labels) +
    ggplot2::scale_y_continuous(expand=c(0,0),limits=plot.limit[c("min.y","max.y")],breaks=ticks.seq.y$breaks,labels=ticks.seq.y$labels) +
    #Deal with labels
    ggplot2::labs(title = plot.fig.num,
                  subtitle = plot.title,
                  x = x.axis,
                  y = y.axis,
                  caption = caption)
  line.limits <- data.table::data.table(x=c(min(ticks.seq.x$breaks),max(ticks.seq.x$breaks)),
                                        y=c(min(ticks.seq.y$breaks),max(ticks.seq.y$breaks)))
  names(line.limits) <- c(x,y)
  p <- p + ggthemes::geom_rangeframe(data= line.limits,size=0.5, colour="#626466")
  #Add logo if needed
  if(logo){
    p <- add_logo(p,logo.type)
  }
  #Export into a file if export option is on - with default options


  if(export){
    if(export.name==""){
      export.name <- "Rplot.pdf"
    }
    export.bf.plot(export.name,p,p.height=6,p.width=7.25)
  }
  #Returning the plot
  return(p)
}
