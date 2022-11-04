
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
#' @param logo logical (TRUE/FALSE) denote whether to add BIIE logo or not
#' @param logo.type Character; either "small" or "big" and activates only when logo is TRUE, decides whether to add full (big) or abridged (small) logo
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
                           logo = FALSE,
                           logo.type = "small",
                           x.axis="",
                           dividing.line=FALSE,
                           labels=FALSE,
                           label.unit = "",
                           export = FALSE,
                           export.name = "") {

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
        ggplot2::annotate("text",x=max.x-1-size.text,y=y.dest,label=label,size=11*0.352777778,family="GT-Pressura-Regular") #Add the text
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
  #Add logo if needed
  if(logo){
    p <- add_logo(p,logo.type)
  }
  #Export things
  if(export){
    ratio = round(row.num/most.x,2)
    f.height = 12*ratio
    if(export.name==""){
      export.name <- "Rplot.pdf"
    }
    export.bf.plot(export.name,p,p.height=f.height,p.width=12)
  }
  return(p)
}
