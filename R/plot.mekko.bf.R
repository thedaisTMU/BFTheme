
#' Plot a mekko chart
#' @param data Data to use
#' @param x String. The X axis - the values that the width of each bar will represent
#' @param y String. The Y axis - what the height of bars will represent
#' @param group.by String. Variable that the bar should be coloured by
#' @param colours Vector (or set.colour function) of colours to use. If not, default palette is generated.
#' @param plot.title Character denoting title of the plot
#' @param plot.fig.num Character denoting plot number (or another plot annotations)
#' @param unit.x Character denoting the unit for x-axis. Special formatting for \% and $
#' @param unit.y Character denoting the unit for y-axis. Special formatting for \% and $
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
#' plot.mekko.bf(mtcars,"mpg","disp")

plot.mekko.bf <- function(data,
                          x,
                          y,
                          plot.title = "",
                          plot.fig.num = "",
                          caption = "",
                          logo = FALSE,
                          logo.type = "small",
                          group.by = NULL,
                          legend.title = "",
                          colours = NULL,
                          unit.x = "",
                          unit.y = "",
                          x.axis = "",
                          y.axis = "",
                          export = FALSE,
                          export.name = ""){
  if(!data.table::is.data.table(data)){ #Chek and coerce into data.table
    clone <- data.table::as.data.table(data)
    warning("Data supplied is not data.table - forcing it to be data.table; may not produce desirable results")
  }
  else{
    clone <- cbind(data)

  }
  data.table::setkeyv(x=clone,y)
  clone[,w:=cumsum(get(x))]
  clone[,wm:=w-get(x)]
  clone[,width:=w-wm]
  mekko.theme <- brookfield.base.theme()
  if(is.null(group.by)){
    if(is.null(colours)){
      colours <- set.colours(1)
    }
    p <- ggplot2::ggplot(data=clone) +
      mekko.theme +
      ggplot2::geom_rect(ggplot2::aes_string(xmin = "wm", xmax = "w",ymax = y, ymin = 0),
                         colour = "white",
                         size = 0.1,
                         fill=colours)
  }
  else{
    if(is.null(colours)){
      colours <- set.colours(length(unique(clone[,get(group.by)])))
    }
    p <- ggplot(data=clone) +
      mekko.theme +
      ggplot2::geom_rect(aes_string(xmin = "wm", xmax = "w",ymax = y, ymin = 0, fill = group.by),
                         colour = "white",
                         size = 0.1) +
      ggplot2::scale_fill_manual(values = colours)
  }
  max.plot.x <- max(clone[,w]) * 1.05
  max.plot.y <- max(clone[,get(y)]) * 1.05
  ticks.x <- set.ticks.seq(max.plot.x,0,unit.x)
  ticks.y <- set.ticks.seq(max.plot.y,0,unit.y)
  p <- p +
    ggplot2::labs(title = plot.fig.num, subtitle = plot.title, caption = caption, x=x.axis,y=y.axis) +
    ggplot2::scale_x_continuous(expand = c(0,0),limits=c(0,max.plot.x),breaks = ticks.x$breaks, labels = ticks.x$labels) +
    ggplot2::scale_y_continuous(expand=c(0,0),limits=c(0,max.plot.y),breaks = ticks.y$breaks, labels = ticks.y$labels) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = legend.title, title.position = "top"))
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
  return(p)
}
