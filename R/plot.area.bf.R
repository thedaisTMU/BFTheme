
#' Plot an area graph
#' @param data Data to use
#' @param x String. Column name for the X axis
#' @param y String. Column name for the Y axis - main value that is changing
#' @param group.by String. Group the area being plotted by this column
#' @param colours Vector. NULL or Specify colours you want the areas to be filled by
#' @param stacked Default is TRUE, set as FALSE for overlapping but not stacked graph
#' @param alpha Default is 0.5, be careful changing this as it might impact readability
#' @param order.area Vector of String - giving the order you want the areas to be plotted, from bottom to top and from backgroudn to foreground
#' @param order.y Vector of String - giving the order you want the x axis to be plotted - only works if y is non numerical
#' @param unit.x String. Unit for the x axis. Special formatting for \% and $
#' @param unit.y String. Unit for the y axis. Special formatting for \% and $
#' @param x.axis Character denoting the x axis title
#' @param y.axis Character denoting the y axis title
#' @param legend.title Character denoting the title for legend box
#' @param plot.title Character denoting title of the plot
#' @param plot.fig.num Character denoting plot number (or another plot annotations)
#' @param caption Character that denotes caption (Sources)
#' @param logo logical (TRUE/FALSE) denote whether to add BIIE logo or not
#' @param logo.type Character; either "small" or "big" and activates only when logo is TRUE, decides whether to add full (big) or abridged (small) logo
#' @return ggplot2 object with all the right formatting
#' @examples
#' plot.area.bf()
plot.area.bf <- function(data,
                         x,
                         y,
                         group.by,
                         colours = NULL,
                         stacked = TRUE,
                         alpha = 0.5,
                         order.area = NULL,
                         order.y = NULL,
                         unit.x = "",
                         unit.y = "",
                         x.axis = "",
                         y.axis = "",
                         legend.title = "",
                         plot.title = "",
                         plot.fig.num = "",
                         caption = "",
                         logo = FALSE,
                         logo.type = "small"){
  if(!data.table::is.data.table(data)){ #Chek and coerce into data.table
    clone <- data.table::as.data.table(data)
    warning("Data supplied is not data.table - forcing it to be data.table; may not produce desirable results")
  }
  else{
    clone <- cbind(data)

  }
  #At this point, we're dealing with the detailed stuff like how to order the categories around and stuff

  if(is.null(order.area)){ #Check if an ordering has been provided, if not make one
    clone[,avg:=-mean(get(y)),by=get(group.by)] #General principal is that the set with the lowest average goes in the foreground
    clone[,group.by:=reorder(get(group.by),avg)] #Reorder according to negative of the average - assuming all positive values here
  }
  else{
    order.area.num <- seq(1,length(order.area)) #Set the temporary vector for numbers
    names(order.area.num) <- order.area #Force the name of temporary to the values in provided vector
    order.area <- order.area.num #Substitute the temp for final vector
    rm(order.area.num) #Delete the temporary file
    clone[,group.by:=reorder(get(group.by),order.area[clone[,get(group.by)]])] #Using named vector provided to order the bars in stacked case. Names in vector must correspond to levels in group.by
  }
  if(is.numeric(clone[,get(x)])){
    dum <- FALSE #Scale x continuous as opposed to discrete later on.
    ticks.seq.x <- unique(clone[,get(x)]) #Set the x axis ticks, if it's numeric then extract ordered set directly
  }
  else{ #But if it's not
    dum <- TRUE #Scale x discrete as opposed to continuous later on.
    if(!is.null(order.y)){ #If a categorical order is not provided - alphabetica order will be used
      dum.vec <- clone[,get(x)]
      cat.ord.num <- seq(1,length(order.y))
      names(cat.ord.num) <- order.y
      dum.vec <- unname(cat.ord.num[dum.vec])
      clone[,x] <- factor(clone[,get(x)],levels = cat.order) #This makes sure every data has an ordered category
    }
  }

  #Now we're actually onto plotting things
  p <- ggplot2::ggplot(clone,ggplot2::aes_string(x,y,fill="group.by"),colours=NULL) + #Set up base plot with the colour variable at least
    brookfield.base.theme()
  #If position is stacked
  if(stacked){
    max.plot.y <- max(clone[,sum(get(y)),by=get(x)][,V1]) #Find the max plot in y
    min.plot.y <- 0 #Min plot is always 0
    ticks.y <- set.ticks.seq(max.plot.y,min.plot.y,unit = unit.y) #Set tick accordingly
    p <- p + ggplot2::geom_area(position = "stack") #Set up the area so it stacks on top of each other
  }
  else{ #If it's identity - Wish list: specify different levels of opacity
    max.plot.y <- max(clone[,get(y)]) #Get max plot in y
    min.plot.y <- 0 #Min plot is always 0
    ticks.y <- set.ticks.seq(max.plot.y, min.plot.y, unit = unit.y) #Set the ticks accordingly
    p <- p + ggplot2::geom_area(ggplot2::aes(group=group.by), position = "identity", alpha=alpha) #Set up area so it appears one before the other. Alpha is important for opacity and readability
  }
  if(is.null(colours)){ #If colours are not provided, set colours here
    colours <- set.colours(length(unique(clone[,group.by])))
  }
  p <- p + ggplot2::scale_fill_manual(values = colours) #Scale colours accordingly
  p <- p + ggplot2::scale_y_continuous(expand = c(0,0),
                                       limits = c(min.plot.y, max.plot.y*1.05),
                                       breaks = ticks.y$breaks,
                                       labels = ticks.y$labels) #Set all the tick elements for x
  if(!dum){
    p <- p + ggplot2::scale_x_continuous(expand = c(0,0.02)) #
  }
  #Add in all the label components
  p <- p + ggplot2::labs(title    = plot.fig.num,
                         subtitle = plot.title,
                         x        = x.axis,
                         y        = y.axis,
                         caption  = caption) +
    ggplot2::guides(fill=guide_legend(title=legend.title)) #Add legend and guide
  #Add logo if needed
  if(logo){
    p <- add_logo(p,logo.type)
  }
  return(p)
}
