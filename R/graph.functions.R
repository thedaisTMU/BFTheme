
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
# Wish list:
# Histogram




#' Create a scatter plot with Brookfield formatting. Uses Rooney font family - make sure you install that with extrafonts before using this.
#' @usage plot.scatter.bf(data,x,y,group.by=NULL,p.size=NULL,unit.x="",
#' unit.y="",plot.title="",plot.fig.num="",title.x.axis="",
#' title.y.axis="",legend.title = "",deg.45 = FALSE,
#' export = FALSE,export.name = "")
#' @param data Main data table to plot
#' @param x Character corresponding the column name for x-axis
#' @param y Character corresponding to the column name for y-axis
#' @param group.by Character corresponding to the column name that points are grouped by (colour)
#' @param colours Vector (or set.colour function) of colours to use. If not, default palette is generated.
#' @param p.size Character corresponding to the column name that sets the size for points
#' @param deg.45 TRUE/FALSE denoting whether to add 45 degree line or not
#' @param trend.line TRUE/FALSE add a trend line using OLS
#' @param unit.x Character denoting the unit for x-axis. Special formatting for \% and $
#' @param unit.y Character denoting the unit for y-axis. Special formatting for \% and $
#' @param x.axis Character denoting title for the x axis
#' @param y.axis Character denoting title for the y axis
#' @param plot.limit Vector of form c(min.x,max.x,min.y,max.y) that sets out plot limits
#' @param plot.title Character denoting title of the plot
#' @param plot.fig.num Character denoting plot number (or another plot annotations)
#' @param caption character for caption (sources etc)
#' @param legend.title Character denoting legend titles
#' @param export TRUE/FALSE whether to export file as EPS under default options (height=6 inches, width=9 inches)
#' @param export.name Name of the exported EPS file
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
                            legend.title = "",
                            export = FALSE,
                            export.name = ""){
  #This bit sets up the base Brookfield theme elements
  if(!data.table::is.data.table(data)){ #Chek and coerce into data.table
    clone <- data.table::as.data.table(data)
  }
  else{
    clone <- cbind(data)
    warning("Data supplied is not data.table - forcing it to be data.table; may not produce desirable results")
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
    p <- p + ggplot2::scale_size(range=c(2.3,2.3),guide=FALSE)
  }
  else{ #Scale point size
    p <- p + ggplot2::scale_size(range=c(2.3,7),guide=FALSE)
  }
  #Dealing with tick units
  if(deg.45){
    p <- p + ggplot2::geom_abline(intercept = 0, slope = 1, colour = "#14365D")
  }
  p <- p + ggplot2::scale_x_continuous(expand=c(0,0),limits=plot.limit[c("min.x","max.x")],breaks=ticks.seq.x$breaks,labels=ticks.seq.x$labels) +
    ggplot2::scale_y_continuous(expand=c(0,0),limits=plot.limit[c("min.y","max.y")],breaks=ticks.seq.y$breaks,labels=ticks.seq.y$labels) +
    #Deal with labels
    ggplot2::labs(title = plot.fig.num,
                  subtitle = plot.title,
                  x = x.axis,
                  y = y.axis,
                  caption = caption)
  line.limits <- data.table(x=c(min(ticks.seq.x$breaks),max(ticks.seq.x$breaks)),y=c(min(ticks.seq.y$breaks),max(ticks.seq.y$breaks)))
  names(line.limits) <- c(x,y)
  p <- p + ggthemes::geom_rangeframe(data= line.limits,size=0.5, colour="#DCDDDE")
  #Export into a file if export option is on - with default options

  if(export){
    if(export.name==""){
      export.name <- "Rplot"
    }
    export.bf.plot(export.name,p,p.height=6,p.width=7.25)
  }
  #Returning the plot
  return(p)
}


#' Create a simple bar chart with Brookfield theme. Uses Rooney font family - make sure you install that with extrafonts before using this.
#' @usage plot.column.bf(data,x,cat,plot.title="",plot.fig.num="",
#' order.bar = FALSE, group.by = "", label = FALSE, export = FALSE,
#' export.name = "")
#' @param data Main data table to plot
#' @param x Character corresponding the column name for what's being counted in the bar graph
#' @param cat Character corresponding to the column name categories of the column
#' @param order.bar One of "none", "ascending", or "descending"
#' @param group.by Character corresponding to the column name that columns are grouped by (colour)
#' @param colours Vector (or set.colour function) of colours to use. If not, default palette is generated.
#' @param stacked TRUE/FALSE to stack the columns or not
#' @param label TRUE/FALSE on whether to label the points or not
#' @param label.unit What unit does y axis (and labels if any) will have
#' @param legend.title Title for the legend if multiple colours are used
#' @param label.adjust Factor to adjust the labels by default is 2.5\% or 0.025
#' @param plot.title Character denoting title of the plot
#' @param plot.fig.num Character denoting plot number (or another plot annotations)
#' @param y.axis Character denoting axis title for y (unit)
#' @param caption Character denoting sources and other captions
#' @param export TRUE or FALSE whether to export file as EPS under default options (height=6 inches, width=9 inches)
#' @param export.name Name of the exported EPS file
#' @return Single column plot that conforms to Brookfield styling
#' @examples
#' plot.column.bf(data,"income","automation.risk")
plot.column.bf <- function(data,x,cat,
                           order.bar="No",
                           group.by=NULL,
                           colours=NULL,
                           stacked = FALSE,
                           label=FALSE,
                           label.unit = "",
                           label.adjust = 0.025,
                           plot.title="",
                           plot.fig.num="",
                           y.axis = "",
                           legend.title = "",
                           caption = "",
                           export=FALSE,
                           export.name=""){
  if(!data.table::is.data.table(data)){ #Chek and coerce into data.table
    clone <- data.table::as.data.table(data)
  }
  else{
    clone <- cbind(data)
    warning("Data supplied is not data.table - forcing it to be data.table; may not produce desirable results")
  }
  #Set up basic theme elements
  column.theme <- brookfield.base.theme() +
    theme(axis.text.x = ggplot2::element_text(size=9, margin=ggplot2::margin(t=2), family = "RooneySans-Light", angle=90, hjust = 1, vjust = 0.5),
          axis.ticks.x = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank())
  max.plot <- max(clone[,get(x)]*1.04) #Scale the plot maximum
  if(stacked){
    max.plot <- max(clone[,sum(get(x)),by=cat][,V1])
  }
  n.sig <- 1 #Set number of significant figures - maybe parametarize in the future
  ticks.seq <- set.ticks.seq(max.plot,0,unit=label.unit)
  max.plot <- max(c(ticks.seq$breaks,max.plot))
  nudge.amt <- max.plot*label.adjust #Nudge amount for labels
  #Bar ordering
  if(order.bar != "No"){
    if(order.bar == "ascending"){
      clone[,cat] <- reorder(clone[,get(cat)],clone[,get(x)]) #Reorder bars ascending order
    }
    else if(order.bar == "descending"){
      clone[,cat] <- reorder(clone[,get(cat)],-clone[,get(x)]) #Reorder bars descending order
    }
    else{
      stop("Order has to be either 'ascending' or 'descending'")
    }
  }
  #Set fill by group dependencies
  if(is.null(group.by)){
    if(is.null(colours)){ #Check for specified colours
      colours <- set.colours(1) #If not, generate dark blue basic
    }
    #Set up base plot
    p <- ggplot2::ggplot(clone,ggplot2::aes_string(cat,x)) +
      column.theme +
      ggplot2::geom_col(width=0.6,fill=colours) +
      ggplot2::scale_y_continuous(expand=c(0,0),limits = c(0,max.plot), breaks = ticks.seq$breaks, labels = ticks.seq$labels) +
      ggplot2::scale_fill_manual(values=colours)
  }
  else{
    num.row <- round(sum(nchar(as.character(unique(clone[,get(group.by)]))))/100)+1
    #Set up base plot if colours are different
    if(is.null(colours)){ #Check for specified colours
      colours <- set.colours(length(unique(clone[,get(group.by)]))) #If not, generate colours
    }
    p <- ggplot2::ggplot(clone,ggplot2::aes_string(cat,x,fill=group.by)) +
      column.theme
    if(stacked){
      p <- p + ggplot2::geom_col(width=0.6, position="stack") +
        ggplot2::scale_y_continuous(expand=c(0,0),limits = c(0,max.plot), breaks = ticks.seq$breaks, labels = ticks.seq$labels) +
        ggplot2::scale_fill_manual(values=colours) +
        ggplot2::guides(fill=guide_legend(title=legend.title,nrow=num.row,title.position = "top"))
    }
    else{
      p <- p + ggplot2::geom_col(width=0.6,position="dodge") +
        ggplot2::scale_y_continuous(expand=c(0,0),limits = c(0,max.plot), breaks = ticks.seq$breaks, labels = ticks.seq$labels) +
        ggplot2::scale_fill_manual(values=colours) +
        ggplot2::guides(fill=guide_legend(title=legend.title,nrow=num.row,title.position = "top"))
    }

  }
  #Set numeric label for values into the columns
  if(label){
    p <- p + ggplot2::geom_text(data=clone,ggplot2::aes(label=str_c(scales::comma(round(unlist(clone[,get(x)]),1)),label.unit)),
                                nudge_y=nudge.amt, size=11*0.352777778, family="RooneySans-Regular")
  }
  if(length(unique(clone[,get(cat)]))<= 5){ #If there are more than 10 groups, make the x axis certicle
    p <- p + theme(axis.text.x = ggplot2::element_text(angle=90,size=11, margin=ggplot2::margin(t=0,l=10),hjust=1,vjust=0.5))
  }
  p <- p + ggplot2::labs(subtitle = plot.title, title=plot.fig.num,y=y.axis, caption=caption)
  #Export into file
  if(export){
    if(export.name==""){
      export.name <- "Rplot"
    }
    export.bf.plot(export.name,p,p.height=6,p.width=7.25)
  }
  return(p)
}



#' Plot a waffle chart with dividing lines and labels
#' This function requires you to know the distribution of the squares already.
#' In the future there will be a wrapper that helps you with determining the proportions
#' @param named.vector A named vector that is of the form c("label"=num.square)
#' @param row.num Number of rows to put in a waffle chart
#' @param colours Vector (or set.colour function) of colours to use. If not, default palette is generated.
#' @param plot.title Character denoting title of the plot
#' @param plot.fig.num Character denoting plot number (or another plot annotations)
#' @param plot.cat.title Legend title for the colour of the plot
#' @param caption String that sets the caption: Sources, notes stc
#' @param x.axis Notes to be placed - Recommended place to put what each square means if not clear already
#' @param dividing.line TRUE/FALSE on whether we want to display the dividing lines between the groups
#' @param labels TRUE/FALSE on whether we want to have labels on them or not
#' @param label.unit character string that can add labels to it. Examples include "\%"
#' @param export TRUE/FALSE whether to export file as EPS under default options
#' (height=scaled according to row number and cell number, width=12 inches)
#' @param export.name Name of the exported EPS file
#' @return ggplot2 object with all the right formatting
#' @examples
#' plot.waffle.bf(c("Red"=20,"Blue"=30,"Green"=50),label.unit="%")
plot.waffle.bf <- function(named.vector,
                           row.num=5,
                           colours = NULL,
                           plot.title="",
                           plot.fig.num="",
                           plot.cat.title="",
                           caption = "",
                           x.axis="",
                           dividing.line=FALSE,
                           labels=FALSE,
                           label.unit = "",
                           export = FALSE,
                           export.name = "Rplot") {

  #Set up the main theme element
  waffle.theme <- brookfield.base.theme() +
    theme(panel.spacing = ggplot2::unit(c(0,0,0,0),units=c("cm","cm","cm","cm")),
          axis.text = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.line = ggplot2::element_blank())
  #Calculate the coordinates for the waffle chart
  num.square <- sum(named.vector) #Get the number of squares needed
  most.x <- floor(num.square/row.num) #Find the maximum x value that fits in a square
  left.x.max <- num.square-most.x*row.num #Find out how many squares don't fit
  label.vector <- rep(names(named.vector),named.vector) #Create a vector of labels enumerated by labels
  x.vector <- c(rep(seq(1,most.x),rep(row.num,most.x))) #Get the divisible part of the x coordinates
  y.vector <- c(rep(seq(1,row.num),most.x)) #Get the divisible part of the y coordinates
  id <- seq(1,num.square) #Get id for reference later on if needed
  if(left.x.max!=0){ #If there are non divisible part
    x.vector <- c(x.vector,rep(most.x+1,left.x.max)) #Add the x coordinate for the last few squares
    y.vector <- c(y.vector,seq(1,left.x.max)) #Add the y coordinate for the last few squares
  }
  main.data <- data.table::data.table(id=id,label=label.vector,xc=x.vector,yc=y.vector) #Get the squares into a data table
  main.data <- as.data.table(main.data)
  if(is.null(colours)){
    colours <- set.colours(length(named.vector)) #Get the colour vector for fill
  }
  #Set up base plot
  p <- ggplot2::ggplot(main.data,aes(xc,yc,fill=label)) +
    waffle.theme +
    ggplot2::geom_tile(width=0.75,height=0.75) +
    ggplot2::scale_fill_manual(values=colours,labels = str_c(sort(names(named.vector)),"    ")) +
    ggplot2::coord_fixed(ratio=1)
  #Dealing with dividing line problems - inefficient and can be improved (Future)
  if(dividing.line){
    length.vec <- length(named.vector) - 1 #Only need divicing lines for everything but the last category
    for(n in names(named.vector[1:length.vec])){
      max.x <- max(main.data[label==n,xc])
      max.y <- max(main.data[label==n & xc==max.x,yc])
      min.y <- min(main.data[label==n & xc==max.x,yc])
      p <- p + ggplot2::annotate("segment",x=max.x+0.5,y=min.y-0.5,xend=max.x+0.5,yend=max.y+0.5,linetype=2,colour="#727D84") #Draw all the right lines to border cells
      if(max.y < row.num){ #Check if the top border cell is the top row.
        p <- p + ggplot2::annotate("segment",x=max.x-0.5,y=max.y+0.5,xend=max.x+0.5,yend=max.y+0.5,linetype=2,colour="#727D84") #Draw a border to the top cell
        p <- p + ggplot2::annotate("segment",x=max.x-1+0.5,y=max.y+1-0.5,xend=max.x-1+0.5,yend=row.num+0.5,linetype=2,colour="#727D84") #Draw the right border for rest of the cells
      }
    }
  }
  #Dealing with label stuff
  if(labels){
    prev.x <- 0 #This is to see if labels will be too close to each other - approximate
    prev.y <- -1 #This is to adjust the y height if labels are too close to each other - approximate
    for(n in names(named.vector)){
      random <- sample(0:1,1) #Sometimes put label on the bottom, sometimes on top
      if(random==1){
        max.y <- min(main.data[label==n,yc])
      }
      else{
        max.y <- max(main.data[label==n,yc])
      }
      max.x <- max(main.data[label==n & yc==max.y,xc]) #This jointly with max.y determines which cell the arrow will come from
      label <- stringr::str_c(named.vector[n],label.unit," ",n) #Generate label texts
      if(max.y<row.num/2){
        y.dest <- 0
      }
      else{
        y.dest <- row.num+1
      }
      if(max.x-prev.x<=2 & prev.y==y.dest){ #Check to see if labels are too close to each other
        if(y.dest==0){
          y.dest <- y.dest-0.75
        }
        else{
          y.dest <- y.dest+0.75
        }
      }
      size.text <- nchar(label)/3.7 #Approximate text size so it won't collide with the segments
      p <- p +
        ggplot2::annotate("segment",x = max.x, y = max.y, xend = max.x - 1, yend = y.dest, colour="#727D84", size=0.25) + #Add the diagonal segment
        ggplot2::annotate("segment",x=max.x-1,y=y.dest,xend=max.x-2,yend=y.dest,colour="#727D84",size=0.25) + #Add the horizontal segment
        ggplot2::annotate("text",x=max.x-1-size.text,y=y.dest,label=label,size=11*0.352777778,family="RooneySans-Regular") #Add the text
      prev.x <- max.x
      prev.y <- y.dest
    }
  }
  #Add labels
  p <- p +
    ggplot2::scale_x_continuous(expand=c(0,0),limits=c(0.5,most.x+0.5)) +
    ggplot2::scale_y_continuous(limits=c(-1,row.num+1)) +
    labs(title=plot.fig.num,subtitle=plot.title,x=x.axis,caption=caption) +
    guides(fill=guide_legend(title=plot.cat.title,title.position = "top"))
  #Export things
  if(export){
    ratio = round(row.num/most.x,2)
    f.height = 12*ratio
    export.bf.plot(export.name,p,p.height=f.height,p.width=12)
  }
  return(p)
}

#' Plot a line chart
#' Make sure you don't confuse between x and y. X is what goes on the bottom (Year...)
#' @param data Data to use
#' @param x The X axis the line will be connected along this point
#' @param y The Y axis points will reside along this
#' @param group.by Line colour will be determined by this
#' @param show.points TRUE/FALSE whether to hsow the poitns on the line or not
#' @param cat.order Put the order of categorical variable in here (as a vector) c("cat.1","cat.2","cat.3") and so on.
#' If you don't, a default order will be generated.
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
                         export = FALSE,
                         export.name = ""){
  if(!data.table::is.data.table(data)){ #Chek and coerce into data.table
    clone <- data.table::as.data.table(data)
  }
  else{
    clone <- cbind(data)
    warning("Data supplied is not data.table - forcing it to be data.table; may not produce desirable results")
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
    p <- ggplot2::ggplot(clone,aes_string(x,y),colour=colours) + #Main plot object
      line.theme +
      ggplot2::geom_line(colour=colours,size=1.2)
  }
  else{
    if(is.null(colours)){
      colours <- set.colours(length(unique(clone[,get(group.by)]))) #Set colours according to the group
    }
    p <- ggplot2::ggplot(data,aes_string(x,y,colour=group.by,group=group.by)) + #Main plot object
      line.theme +
      ggplot2::geom_line(aes_string(colour=group.by),size=1.2) +
      ggplot2::scale_colour_manual(values=colours)
  }
  if(show.points){
    p <- p + geom_point(size=2.3) #Add in points
  }
  if(!dum){ #Scale continuous because underlying categories were numeric
    p <- p + scale_x_continuous(breaks = ticks.seq.x, labels = paste0(ticks.seq.x,unit.x))

  }
  if(length(unique(clone[,get(x)]))>= 10){ #If there are more than 10 groups, make the x axis certicle
    p <- p + theme(axis.text.x = ggplot2::element_text(angle=90,size=11, margin=ggplot2::margin(t=0,l=10),hjust=1,vjust=0.5))
  }
  num.row <- round(sum(nchar(as.character(unique(clone[,get(group.by)]))))/100)+1 #Set numnber of legend rows
  p <- p + labs(title=plot.fig.num,subtitle=plot.title,x=x.axis,y=y.axis,caption = caption) + #Add in all the captions
    guides(colour=guide_legend(title=legend.title,nrow=num.row,title.position = "top")) +
    scale_y_continuous(breaks = ticks.seq.y$breaks,labels = ticks.seq.y$labels)
  if(export){
    if(export.name==""){
      export.name <- "Rplot"
    }
    export.bf.plot(export.name,p)
  }
  return(p)
}

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
                            export = FALSE,
                            export.name = ""){

  #Set up basic theme elements
  levels <- unique(data[,get(diff)])
  if(!data.table::is.data.table(data)){ #Chek and coerce into data.table
    clone <- data.table::as.data.table(data)
  }
  else{
    clone <- cbind(data)
    warning("Data supplied is not data.table - forcing it to be data.table; may not produce desirable results")
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
  p <- ggplot2::ggplot(data=clone,aes_string(group,x,fill=diff)) + #Set up the base plot
    pyramid.theme +
    geom_bar(stat="identity") +
    scale_y_continuous(breaks = seq.ticks,labels = ticks.labels, limits = c(-max.plot,max.plot)) +
    scale_fill_manual(values=colours) +
    coord_flip() #Flip it - this is most important to make it look like a pyramid.
  #Handle exporting
  p <- p + labs(title=plot.fig.num,subtitle=plot.title,y=x.axis,x=y.axis,caption=caption) +
    guides(fill=guide_legend(title=legend.title,title.position = "top"))
  if(export){
    if(export.name==""){
      export.name <- "Rplot"
    }
    export.bf.plot(export.name,p)
  }
  #Return p
  return(p)
}


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
  }
  else{
    clone <- cbind(data)
    warning("Data supplied is not data.table - forcing it to be data.table; may not produce desirable results")
  }
  setkeyv(x=clone,y)
  clone[,w:=cumsum(get(x))]
  clone[,wm:=w-get(x)]
  clone[,width:=w-wm]
  mekko.theme <- brookfield.base.theme()
  if(is.null(group.by)){
    if(is.null(colours)){
      colours <- set.colours(1)
    }
    p <- ggplot(data=clone) +
      mekko.theme +
      geom_rect(aes_string(xmin = "wm", xmax = "w",
                    ymax = y, ymin = 0), colour = "white", size = 0.1, fill=colours)
  }
  else{
    if(is.null(colours)){
      colours <- set.colours(length(unique(clone[,get(group.by)])))
    }
    p <- ggplot(data=clone) +
      mekko.theme +
      geom_rect(aes_string(xmin = "wm", xmax = "w",
                         ymax = y, ymin = 0, fill = group.by), colour = "white", size = 0.1) +
      scale_fill_manual(values = colours)
  }
  max.plot.x <- max(clone[,w]) * 1.05
  max.plot.y <- max(clone[,get(y)]) * 1.05
  ticks.x <- set.ticks.seq(max.plot.x,0,unit.x)
  ticks.y <- set.ticks.seq(max.plot.y,0,unit.y)
  p <- p + labs(title = plot.fig.num, subtitle = plot.title, caption = caption, x=x.axis,y=y.axis) +
    scale_x_continuous(expand = c(0,0),limits=c(0,max.plot.x),breaks = ticks.x$breaks, labels = ticks.x$labels) +
    scale_y_continuous(expand=c(0,0),limits=c(0,max.plot.y),breaks = ticks.y$breaks, labels = ticks.y$labels) +
    guides(fill = guide_legend(title = legend.title, title.position = "top"))
  if(export){
    if(export.name==""){
      export.name <- "Rplot"
    }
    export.bf.plot(export.name,p)
  }
  return(p)
}



#' Plot a change graph
#' @param data Data to use
#' @param x String. Column name for the X axis - the value the categories plotted are changing
#' @param cat String. Column name for the category of things that are changing
#' @param time.var String. Column name of variable that denotes time - time has to be numeric
#' @param order.by String. One of "change" or "value", way to order the categories. Either by the magnitude of change, or the absolute magnitude
#' @param plot.title Character denoting title of the plot
#' @param plot.fig.num Character denoting plot number (or another plot annotations)
#' @param unit.x Character denoting the unit for x-axis. Special formatting for \% and $
#' @param x.axis Character denoting the x axis title
#' @param y.axis Character denoting the y axis title
#' @param caption Character that denotes caption (Sources)
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
                                 caption = ""){
  if(!data.table::is.data.table(data)){ #Chek and coerce into data.table
    clone <- data.table::as.data.table(data)
  }
  else{
    clone <- cbind(data)
    warning("Data supplied is not data.table - forcing it to be data.table; may not produce desirable results")
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
  p <- ggplot(clone,aes_string(x, cat, group = cat)) + #Base plot elements. Grouping is done by cat for geom_path
    brookfield.base.theme() + #Set base theme - nothing new for this plot
    geom_path(aes(colour = diff),
              size      = 3,
              arrow     = arrow(angle  = 25,
                                length = unit(0.15, "inches"),
                                type   = "closed"), #All the arrow elements including angle and length, arrow type is closed
              lineend   = "butt",
              linejoin  = "mitre", #Lineend and linejoin as well as mitre set to have a "crisp" end
              linemitre = 4)
  if(min(clone[, diff]) >= 0 & max(clone[, diff]) > 0){ #If we only have increasing vectors
    arrow.colours <- set.colours(2, categorical.choice = c("grey", "light.blue")) #Set colour from grey to light blue
    p <- p + scale_colour_gradient(low   = arrow.colours[1],
                                   high  = arrow.colours[2],
                                   guide = FALSE)
  }
  else if(max(clone[,diff])<=0 & min(clone[,diff])<0){ #If we only have decreasing vectors
    arrow.colours <- set.colours(2, categorical.choice = c("pink", "grey"))
    p <- p + scale_colour_gradient(low   = arrow.colours[1],
                                   high  = arrow.colours[2],
                                   guide = FALSE)
  }
  else{ #Or if we have both increasing and decreasing vectors
    arrow.colours <- set.colours(3, categorical.choice = c("pink", "grey", "light.blue"))
    p <- p + scale_colour_gradient2(low   = arrow.colours[1],
                                    mid   = arrow.colours[2],
                                    high  = arrow.colours[3],
                                    guide = FALSE)
  }
  #Dealing with value annotation
  if(annotate){
    start.year <- min(clone[, get(time.var)]) #Get the start year
    end.year <- max(clone[, get(time.var)]) #Set the end year
    if(max(clone[, diff]) > 0){ #For the positive changes
      p <- p + geom_text(data = clone[(diff > 0 & get(time.var) == start.year)], #Set the text for starting year for increase
                         aes(label = str_c(comma(clone[(diff > 0 & get(time.var) == start.year), get(x)]), unit.x),
                             x     = clone[(diff > 0 & get(time.var) == start.year), get(x)] - max.plot * nudge.end, #Subtract because increase implies starting year is on the right
                             y     = clone[(diff > 0 & get(time.var) == start.year), get(cat)]),
                         family = "RooneySans-Regular",
                         hjust  = 1,
                         colour = set.colours(1, categorical.choice = "dark.blue"))
      p <- p + geom_text(data = clone[(diff > 0 & get(time.var)==end.year)], #Set the text for ending year for increase
                         aes(label = str_c(comma(clone[(diff > 0 & get(time.var) == end.year), get(x)]), unit.x),
                             x     = clone[(diff > 0 & get(time.var) == end.year), get(x)] + max.plot * nudge.beg, #Add because increase implies ending year is on the right
                             y     = clone[(diff > 0 & get(time.var) == end.year), get(cat)]),
                         family = "RooneySans-Regular",
                         hjust  = 0,
                         colour = set.colours(1, categorical.choice = "dark.blue"))
    }
    #Now dealing with the cases where value decreased
    if(min(clone[,diff])<0){
      p <- p + geom_text(data   = clone[(diff < 0 & get(time.var) == start.year)], #Set the text for starting year for decrease
                         aes(label = str_c(comma(clone[(diff > 0 & get(time.var) == start.year), get(x)]), unit.x),
                             x     = clone[(diff > 0 & get(time.var) == start.year), get(x)] + max.plot * nudge.beg, #Add because decrease imply starting year is on the right
                             y     = clone[(diff > 0 & get(time.var) ==start.year), get(cat)]),
                         family = "RooneySans-Regular",
                         hjust  = 1,
                         colour = set.colours(1, categorical.choice = "pink")) #Set colour pink here, but probably change so it can be dynamics

      p <- p + geom_text(data   = clone[(diff < 0 & get(time.var) == end.year)], #Set the text for ending year for decrease
                        aes(label = str_c(comma(clone[(diff > 0 & get(time.var) == end.year), get(x)]), unit.x),
                            x     = clone[(diff > 0 & get(time.var) == end.year), get(x)] - max.plot * nudge.end, #Subtract because decrease imply ending year is on the left
                            y     = clone[(diff > 0 & get(time.var) == end.year), get(cat)]),
                         family = "RooneySans-Regular",
                         hjust  = 0,
                         colour = set.colours(1, categorical.choice = "pink")) #Set colour pink here, but probably change so it can be dynamic
    }
  }
  #Adding the graph explanation
  if(label){
    p <- p +   annotate("text",
                        x      = sooner.x,
                        y      = length(unique(clone[,get(cat)])) + 0.5, #Currently hacked together - should try to find ways to show text more beautifully
                        label  = stringr::str_c("In ",min(unique(clone[,get(time.var)]))),
                        family = "RooneySans-Regular") +
      annotate("text",
               x      = later.x,
               y      = length(unique(clone[,get(cat)])) + 0.5,
               label  = stringr::str_c("In ",max(unique(clone[, get(time.var)]))),
               family = "RooneySans-Regular")
  }

  p <- p + scale_x_continuous(limits = c(min(clone[, get(x)] - max.plot*nudge.beg, ticks$breaks), #Get the tail - it's complicated because also need to get where the labels are
                                         max(clone[, get(x)] + max.plot*nudge.beg, ticks$breaks)), #Get the tail - it's complicated because also need to get where the labels are
                              breaks = ticks$breaks,
                              labels = ticks$labels) +
    labs(title    = plot.fig.num,
         subtitle = plot.title,
         x        = x.axis,
         y        = y.axis,
         caption  = caption)

  return(p)
}


#' Plot an area graph
#' @param data Data to use
#' @param x String. Column name for the X axis
#' @param y String. Column name for the Y axis - main value that is changing
#' @param group.by String. Group the area being plotted by this column
#' @param colours Vector. NULL or Specify colours you want the areas to be filled by
#' @param order.area Vector of String - giving the order you want the areas to be plotted, from bottom to top and from backgroudn to foreground
#' @param order.y Vector of String - giving the order you want the x axis to be plotted - only works if y is non numerical
#' @param unit.x String. Unit for the x axis. Special formatting for \% and $
#' @param unit.y String. Unit for the y axis. Special formatting for \% and $
#' @param x.axis Character denoting the x axis title
#' @param y.axis Character denoting the y axis title
#' @param plot.title Character denoting title of the plot
#' @param plot.fig.num Character denoting plot number (or another plot annotations)
#' @param caption Character that denotes caption (Sources)
#' @return ggplot2 object with all the right formatting
#' @examples
#' plot.area.bf()
plot.area.bf <- function(data,
                         x,
                         y,
                         group.by,
                         colours = NULL,
                         stacked = TRUE,
                         order.area = NULL,
                         order.y = NULL,
                         unit.x = "",
                         unit.y = "",
                         x.axis = "",
                         y.axis = "",
                         legend.title = "",
                         plot.title = "",
                         plot.fig.num = "",
                         caption = ""){
  if(!data.table::is.data.table(data)){ #Chek and coerce into data.table
    clone <- data.table::as.data.table(data)
  }
  else{
    clone <- cbind(data)
    warning("Data supplied is not data.table - forcing it to be data.table; may not produce desirable results")
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
  p <- ggplot(clone,aes_string(x,y,fill="group.by"),colours=NULL) + #Set up base plot with the colour variable at least
    brookfield.base.theme()
  #If position is stacked
  if(stacked){
    max.plot.y <- max(clone[,sum(get(y)),by=get(x)][,V1]) #Find the max plot in y
    min.plot.y <- 0 #Min plot is always 0
    ticks.y <- set.ticks.seq(max.plot.y,min.plot.y,unit = unit.y) #Set tick accordingly
    p <- p + geom_area(position = "stack") #Set up the area so it stacks on top of each other
  }
  else{ #If it's identity - Wish list: specify different levels of opacity
    max.plot.y <- max(clone[,get(y)]) #Get max plot in y
    min.plot.y <- 0 #Min plot is always 0
    ticks.y <- set.ticks.seq(max.plot.y, min.plot.y, unit = unit.y) #Set the ticks accordingly
    p <- p + geom_area(aes(group=group.by), position = "identity", alpha=0.5) #Set up area so it appears one before the other. Alpha is important for opacity and readability
  }
  if(is.null(colours)){ #If colours are not provided, set colours here
    colours <- set.colours(length(unique(clone[,group.by])))
  }
  p <- p + scale_fill_manual(values = colours) #Scale colours accordingly
  p <- p + scale_y_continuous(expand = c(0,0),
                              limits = c(min.plot.y, max.plot.y*1.05),
                              breaks = ticks.y$breaks,
                              labels = ticks.y$labels) #Set all the tick elements for x
  if(!dum){
    p <- p + scale_x_continuous(expand = c(0,0.02)) #
  }
  #Add in all the label components
  p <- p + labs(title    = plot.fig.num,
                subtitle = plot.title,
                x        = x.axis,
                y        = y.axis,
                caption  = caption) +
    guides(fill=guide_legend(title=legend.title)) #Add legend and guide
  return(p)
}


