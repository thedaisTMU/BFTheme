
#' Plot a line chart
#' Make sure you don't confuse between x and y. X is what goes on the bottom (Year...)
#' @param data Data to use
#' @param x The X axis the line will be connected along this point
#' @param y The Y axis points will reside along this
#' @param group.by Line colour will be determined by this
#' @param show.points TRUE/FALSE whether to show the points on the line or not
#' @param cat.order Put the order of categorical variable in here (as a vector) c("cat.1","cat.2","cat.3") and so on.
#' If you don't, a default order will be generated.
#' @param ingraph.labels TRUE/FALSE whether to show a label at the end of the line
#' @param smooth TRUE/FALSE - default as FALSE. If TRUE, use loess to draw line instead of connected lines
#' @param colours Vector (or set.colour function) of colours to use. If not, default palette is generated.
#' @param plot.title Character denoting title of the plot
#' @param plot.fig.num Character denoting plot number (or another plot annotations)
#' @param plot.limit Vector of form c(x.min,x.max,y.min,y.max)
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
#' plot.line.bf(data,"Year","Income",group.by="Gender")
#' plot.line.bf(data,"Education","Count",cat.order = c("High School","Bachelor's","Master's","PhD"))
plot.line.bf <- function(data,x,y,
                         group.by=NULL,
                         show.points = FALSE,
                         cat.order = NULL,
                         ingraph.labels = FALSE,
                         smooth = FALSE,
                         colours = NULL,
                         plot.title="",
                         plot.fig.num="",
                         plot.limit = NULL,
                         unit.x = "",
                         unit.y = "",
                         x.axis = "",
                         y.axis = "",
                         legend.title = "",
                         caption = "",
                         logo = FALSE,
                         logo.type = "small",
                         export = FALSE,
                         export.name = ""){
  if(!data.table::is.data.table(data)){ #Check and coerce into data.table
    clone <- data.table::as.data.table(data)
    warning("Data supplied is not data.table - forcing it to be data.table; may not produce desirable results")
  }
  else{
    clone <- cbind(data)

  }
  #This bit sets up the base Brookfield theme elements
  line.theme <- brookfield.base.theme()
  if(is.null(plot.limit)){
    plot.limit["min.y"] <- min(clone[,get(y)]) #Set the plot limit for minimum
    plot.limit["max.y"] <- max(clone[,get(y)])*1.05 #Set the max plot limit - add in a bit of buffer
  }
  names(plot.limit) <- c("min.y","max.y")
  ticks.seq.y <- set.ticks.seq(plot.limit["max.y"],plot.limit["min.y"],unit.y) #Set tick sequence based on the plot limits
  if(is.numeric(clone[,get(x)])){
    dum <- FALSE #Scale x continuous as opposed to discrete later on.
    ticks.seq.x <- unique(clone[,get(x)]) #Set the x axis ticks, if it's numeric then extract ordered set directly
  }
  else{ #But if it's not
    dum <- TRUE #Scale x discrete as opposed to continuous later on.
    if(!is.null(cat.order)){ #If a categorical order is not provided - alphabetica order will be used
      dum.vec <- clone[,get(x)]
      cat.ord.num <- seq(1,length(cat.order))
      names(cat.ord.num) <- cat.order
      dum.vec <- unname(cat.ord.num[dum.vec])
      clone[,x] <- factor(clone[,get(x)],levels = cat.order) #This makes sure every data has an ordered category
    }
  }
  if(is.null(group.by)){ #If there is only one group
    if(is.null(colours)){
      colours <- set.colours(1) #Only use one colour
    }
    if(smooth==TRUE){
      p <- ggplot2::ggplot(clone,ggplot2::aes_string(x,y),colour=colours) + #Main plot object
        line.theme +
        ggplot2::geom_smooth(colour=colours,size=1.2,se=FALSE)
    }
    else{
      p <- ggplot2::ggplot(clone,ggplot2::aes_string(x,y),colour=colours) + #Main plot object
        line.theme +
        ggplot2::geom_line(colour=colours,size=1.2)
    }

  }
  else{
    num.row <- round(sum(nchar(as.character(unique(clone[,get(group.by)]))))/100)+1 #Set numnber of legend rows
    if(is.null(colours)){
      colours <- set.colours(length(unique(clone[,get(group.by)]))) #Set colours according to the group
    }
    if(smooth==TRUE){
      p <- ggplot2::ggplot(data,ggplot2::aes_string(x,y,colour=group.by,group=group.by)) + #Main plot object
        line.theme +
        ggplot2::geom_smooth(ggplot2::aes_string(colour=group.by),size=1.2,se=FALSE) +
        ggplot2::scale_colour_manual(values=colours)
    }
    else{
      p <- ggplot2::ggplot(data,ggplot2::aes_string(x,y,colour=group.by,group=group.by)) + #Main plot object
        line.theme +
        ggplot2::geom_line(ggplot2::aes_string(colour=group.by),size=1.2) +
        ggplot2::scale_colour_manual(values=colours)
    }

  }
  if(show.points){
    p <- p + ggplot2::geom_point(size=2.3) #Add in points
  }
  if(!dum){ #Scale continuous because underlying categories were numeric
    p <- p + ggplot2::scale_x_continuous(breaks = ticks.seq.x, labels = paste0(ticks.seq.x,unit.x))

  }
  if(length(unique(clone[,get(x)]))>= 10){ #If there are more than 10 groups, make the x axis certicle
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90,size=11, margin=ggplot2::margin(t=0,l=10),hjust=1,vjust=0.5))
  }


  p <- p + ggplot2::labs(title=plot.fig.num,subtitle=plot.title,x=x.axis,y=y.axis,caption = caption) + #Add in all the captions
    ggplot2::scale_y_continuous(breaks = ticks.seq.y$breaks,labels = ticks.seq.y$labels)
  if(!is.null(group.by)){
    p <- p + ggplot2::guides(colour=guide_legend(title=legend.title,nrow=num.row,title.position = "top"))
  }
  if(ingraph.labels){
    p <- p + directlabels::geom_dl(aes_string(label=group.by),method=list("last.points",
                                                                          fontfamily="GT-Pressura-Regular",
                                                                          cex = 0.8))
  }
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
