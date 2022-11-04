
#' Plot a pyramid
#' Make sure diff is 2 groups and x has the aggregated value
#' @param data Data to use
#' @param x The X axis - the values that the bars will represent
#' @param diff The 2 groups the bar will contrast
#' @param group All the different groups that the ar will show (age groups)
#' @param group.order Order in with the group is displayed in
#' @param colours Vector (or set.colour function) of colours to use. If not, default palette is generated.
#' @param plot.title Character denoting title of the plot
#' @param plot.fig.num Character denoting plot number (or another plot annotations)
#' @param unit.x Character denoting the unit for x-axis. Special formatting for \% and $
#' @param x.axis Character denoting the x axis title
#' @param y.axis Character denoting the y axis title
#' @param legend.title Character denoting the title for the colour legend
#' @param caption Character that denotes caption (Sources)
#' @param logo logical (TRUE/FALSE) denote whether to add BIIE logo or not
#' @param logo.type Character; either "small" or "big" and activates only when logo is TRUE, decides whether to add full (big) or abridged (small) logo
#' @param export TRUE/FALSE whether to export file as EPS under default options
#' (height=scaled according to row number and cell number, width=12 inches)
#' @param export.name Name of the exported EPS file
#' @return ggplot2 object with all the right formatting
#' @examples
#' plot.pyramid.bf(data,"Count","Gender","Age")
plot.pyramid.bf <- function(data,
                            x,
                            diff,
                            group,
                            group.order = NULL,
                            colours = NULL,
                            plot.title="",
                            plot.fig.num="",
                            unit.x = "",
                            x.axis = "",
                            y.axis = "",
                            legend.title = "",
                            caption = "",
                            logo = FALSE,
                            logo.type = "small",
                            export = FALSE,
                            export.name = ""){

  #Set up basic theme elements
  levels <- unique(data[,get(diff)])
  if(!data.table::is.data.table(data)){ #Chek and coerce into data.table
    clone <- data.table::as.data.table(data)
    warning("Data supplied is not data.table - forcing it to be data.table; may not produce desirable results")
  }
  else{
    clone <- cbind(data)

  }
  if(length(levels)!=2){ #Pyramid plot only makes sense if you have 2 levels
    stop("There are either more or less than 2 levels. Can't draw a pyramid in that case")
  }
  pyramid.theme <- brookfield.base.theme()
  if(is.null(colours)){
    colours <- set.colours(2)
  }
  if(!is.null(group.order)){ #Just like with line chart, get grouping orders. See comments in plot.line.bf for details
    dum.vec <- clone[,get(group)]
    group.ord.num <- seq(1,length(group.order))
    names(group.ord.num) <- group.order
    dum.vec <- unname(group.ord.num[dum.vec])
    clone[,group] <- factor(clone[,get(group)],levels = group.order)
  }
  max.plot <- signif(max(clone[,get(x)])*1.05,1) #Set max of the plot and add in buffer for x axis
  seq.ticks <- c(seq(-max.plot,0,max.plot/5),seq(max.plot/5,max.plot,max.plot/5)) #Set the ticks for x
  ticks.labels <- paste0(scales::comma(c(seq(max.plot,0,-max.plot/5),seq(max.plot/5,max.plot,max.plot/5))),unit.x) #Set the tick labels
  clone[get(diff)==levels[2],(x):=-get(x)] #Invert the amount for second label to construct a pyramid
  p <- ggplot2::ggplot(data=clone,ggplot2::aes_string(group,x,fill=diff)) + #Set up the base plot
    pyramid.theme +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::scale_y_continuous(breaks = seq.ticks,labels = ticks.labels, limits = c(-max.plot,max.plot)) +
    ggplot2::scale_fill_manual(values=colours) +
    ggplot2::coord_flip() #Flip it - this is most important to make it look like a pyramid.
  #Handle exporting
  p <- p + ggplot2::labs(title=plot.fig.num,subtitle=plot.title,y=x.axis,x=y.axis,caption=caption) +
    ggplot2::guides(fill=guide_legend(title=legend.title,title.position = "top"))
  #Add logo if needed
  if(logo){
    p <- add_logo(p,logo.type)
  }
  if(export){
    if(export.name==""){
      export.name <- "Rplot.pdf"
    }
    export.bf.plot(export.name,p)
  }
  #Return p
  return(p)
}
