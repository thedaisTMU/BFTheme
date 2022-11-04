
#' Plot a change graph
#' @param data Data to use
#' @param x String. Column name for the X axis - the value the categories plotted are changing
#' @param cat String. Column name for the category of things that are changing
#' @param time.var String. Column name of variable that denotes time - time has to be numeric
#' @param order.by String. One of "change" or "value", way to order the categories. Either by the magnitude of change, or the absolute magnitude
#' @param annotate logical (TRUE/FALSE) on whether to add annotation on beginning period and end period for one of the arrows
#' @param label logical TRUE/FALSE whether to add numbers for each of the arrows denoting starting and ending values
#' @param nudge.beg numeric amount to nudge the text for each arrow, recommended to not change from default
#' @param nudge.end numeric amount of nudget the text for each arrow, recommended to not change from default
#' @param plot.title Character denoting title of the plot
#' @param plot.fig.num Character denoting plot number (or another plot annotations)
#' @param unit.x Character denoting the unit for x-axis. Special formatting for \% and $
#' @param x.axis Character denoting the x axis title
#' @param y.axis Character denoting the y axis title
#' @param caption Character that denotes caption (Sources)
#' @param logo logical (TRUE/FALSE) denote whether to add BIIE logo or not
#' @param logo.type Character; either "small" or "big" and activates only when logo is TRUE, decides whether to add full (big) or abridged (small) logo
#' @return ggplot2 object with all the right formatting
#' @examples
#' plot.change.arrow.bf(change.gdp,"gdp","country","year")
plot.change.arrow.bf <- function(data,
                                 x,
                                 cat,
                                 time.var,
                                 order.by = "change",
                                 annotate = FALSE,
                                 label = FALSE,
                                 nudge.beg = 0.02,
                                 nudge.end = 0.015,
                                 unit.x = "",
                                 x.axis = "",
                                 y.axis = "",
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
  max.plot <- max(clone[, get(x)]) #Find the maximum for the plot
  min.plot <- min(clone[, get(x)]) #Find the minimum for the plot
  ticks    <- set.ticks.seq(max.plot, min.plot, unit.x) #Set nice tick with units
  if(diff(unique(data[, get(time.var)])) < 0){ #If the start year is ordered below end year
    warning("Your data may not be ordered correctly - make sure the year is ordered from earlier to later")
  }
  clone[, diff:=diff(get(x)), by = cat] #Generate differences by category
  if(order.by %in% c("change", "value")){ #Set the order of the bars. Has to be one or either
    if(order.by == "change"){
      clone[, cat] <- reorder(clone[, get(cat)],
                              clone[, diff]) #Reorder bars
      if(label){
        later.x <- clone[diff==max(diff) & get(time.var) == max(get(time.var)), get(x)]
        sooner.x <- clone[diff==max(diff) & get(time.var) == min(get(time.var)),get(x)]
      }
    }
    if(order.by == "value"){
      clone[, cat] <- reorder(clone[, get(cat)],
                              clone[, get(x)]) #Reorder bars
      if(label){
        cat.main <- clone[get(x) == max(get(x)), as.character(get(cat))] #Get the category at top to put the label on
        later.x <- clone[get(cat) == cat.main & get(time.var) == max(get(time.var)), get(x)] #Get the later x
        sooner.x <- clone[get(cat) == cat.main & get(time.var) == min(get(time.var)),get(x)] #Get the sooner x
      }
    }
  }
  else{
    stop("Invalid order by variable. need to be either 'change' or 'value'") #Otherwise it'll stop
  }
  p <- ggplot2::ggplot(clone,ggplot2::aes_string(x, cat, group = cat)) + #Base plot elements. Grouping is done by cat for geom_path
    brookfield.base.theme() + #Set base theme - nothing new for this plot
    ggplot2::geom_path(ggplot2::aes(colour = diff),
                       size      = 3,
                       arrow     = ggplot2::arrow(angle  = 25,
                                                  length = unit(0.15, "inches"),
                                                  type   = "closed"), #All the arrow elements including angle and length, arrow type is closed
                       lineend   = "butt",
                       linejoin  = "mitre", #Lineend and linejoin as well as mitre set to have a "crisp" end
                       linemitre = 4)
  if(min(clone[, diff]) >= 0 & max(clone[, diff]) > 0){ #If we only have increasing vectors
    arrow.colours <- set.colours(2, categorical.choice = c("grey", "light.blue")) #Set colour from grey to light blue
    p <- p + ggplot2::scale_colour_gradient(low   = arrow.colours[1],
                                            high  = arrow.colours[2],
                                            guide = "none")
  }
  else if(max(clone[,diff])<=0 & min(clone[,diff])<0){ #If we only have decreasing vectors
    arrow.colours <- set.colours(2, categorical.choice = c("magenta", "grey"))
    p <- p + ggplot2::scale_colour_gradient(low   = arrow.colours[1],
                                            high  = arrow.colours[2],
                                            guide = "none")
  }
  else{ #Or if we have both increasing and decreasing vectors
    arrow.colours <- set.colours(3, categorical.choice = c("magenta", "grey", "light.blue"))
    p <- p + ggplot2::scale_colour_gradient2(low   = arrow.colours[1],
                                             mid   = arrow.colours[2],
                                             high  = arrow.colours[3],
                                             guide = "none")
  }
  #Dealing with value annotation
  if(annotate){
    start.year <- min(clone[, get(time.var)]) #Get the start year
    end.year <- max(clone[, get(time.var)]) #Set the end year
    if(max(clone[, diff]) > 0){ #For the positive changes

      p <- p + ggplot2::geom_text(data = clone[(diff > 0 & get(time.var) == start.year)], #Set the text for starting year for increase
                                  ggplot2::aes(label = stringr::str_c(scales::comma(clone[(diff > 0 & get(time.var) == start.year), get(x)]), unit.x),
                                               x     = clone[(diff > 0 & get(time.var) == start.year), get(x)] - max.plot * nudge.end, #Subtract because increase implies starting year is on the right
                                               y     = clone[(diff > 0 & get(time.var) == start.year), get(cat)]),
                                  family = "GT-Pressura-Regular",
                                  hjust  = 1,
                                  size = 9*0.352777778,
                                  colour = set.colours(1, categorical.choice = "dark.blue"))

      print(clone[(diff > 0 & get(time.var) == end.year), get(cat)])

      p <- p + ggplot2::geom_text(data = clone[(diff > 0 & get(time.var)==end.year)], #Set the text for ending year for increase
                                  ggplot2::aes(label = stringr::str_c(scales::comma(clone[(diff > 0 & get(time.var) == end.year), get(x)]), unit.x),
                                               x     = clone[(diff > 0 & get(time.var) == end.year), get(x)] + max.plot * nudge.beg, #Add because increase implies ending year is on the right
                                               y     = clone[(diff > 0 & get(time.var) == end.year), get(cat)]),
                                  family = "GT-Pressura-Regular",
                                  hjust  = 0,
                                  size = 9*0.352777778,
                                  colour = set.colours(1, categorical.choice = "dark.blue"))
    }
    #Now dealing with the cases where value decreased
    if(min(clone[,diff])<0){
      p <- p + ggplot2::geom_text(data   = clone[(diff < 0 & get(time.var) == start.year)], #Set the text for starting year for decrease
                                  ggplot2::aes(label = stringr::str_c(scales::comma(clone[(diff < 0 & get(time.var) == start.year), get(x)]), unit.x),
                                               x     = clone[(diff < 0 & get(time.var) == start.year), get(x)] + max.plot * nudge.beg, #Add because decrease imply starting year is on the right
                                               y     = clone[(diff < 0 & get(time.var) ==start.year), get(cat)]),
                                  family = "GT-Pressura-Regular",
                                  hjust  = 1,
                                  size = 9*0.352777778,
                                  colour = set.colours(1, categorical.choice = "magenta")) #Set colour pink here, but probably change so it can be dynamics

      p <- p + ggplot2::geom_text(data   = clone[(diff < 0 & get(time.var) == end.year)], #Set the text for ending year for decrease
                                  ggplot2::aes(label = stringr::str_c(scales::comma(clone[(diff < 0 & get(time.var) == end.year), get(x)]), unit.x),
                                               x     = clone[(diff < 0 & get(time.var) == end.year), get(x)] - max.plot * nudge.end, #Subtract because decrease imply ending year is on the left
                                               y     = clone[(diff < 0 & get(time.var) == end.year), get(cat)]),
                                  family = "GT-Pressura-Regular",
                                  hjust  = 0,
                                  size = 9*0.352777778,
                                  colour = set.colours(1, categorical.choice = "magenta")) #Set colour pink here, but probably change so it can be dynamic
    }
  }
  #Adding the graph explanation
  if(label){
    p <- p + ggplot2::annotate("text",
                               x      = sooner.x,
                               y      = length(unique(clone[,get(cat)])) + 0.5, #Currently hacked together - should try to find ways to show text more beautifully
                               label  = stringr::str_c("In ",min(unique(clone[,get(time.var)]))),
                               family = "GT-Pressura-Regular") +
      ggplot2::annotate("text",
                        x      = later.x,
                        y      = length(unique(clone[,get(cat)])) + 0.5,
                        label  = stringr::str_c("In ",max(unique(clone[, get(time.var)]))),
                        family = "GT-Pressura-Regular")
  }

  p <- p + ggplot2::scale_x_continuous(limits = c(min(clone[, get(x)] - max.plot*nudge.beg, ticks$breaks), #Get the tail - it's complicated because also need to get where the labels are
                                                  max(clone[, get(x)] + max.plot*nudge.beg, ticks$breaks)), #Get the tail - it's complicated because also need to get where the labels are
                                       breaks = ticks$breaks,
                                       labels = ticks$labels) +
    ggplot2::labs(title    = plot.fig.num,
                  subtitle = plot.title,
                  x        = x.axis,
                  y        = y.axis,
                  caption  = caption)
  #Add logo if needed
  if(logo){
    p <- add_logo(p,logo.type)
  }

  return(p)
}
