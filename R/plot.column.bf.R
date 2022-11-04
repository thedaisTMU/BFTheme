
#' Create a simple bar chart with Brookfield theme. Uses Rooney font family - make sure you install that with extrafonts before using this.
#' @usage plot.column.bf(data,x,cat,plot.title="",plot.fig.num="",
#' order.bar = FALSE, group.by = "", label = FALSE, export = FALSE,
#' export.name = "")
#' @param data Main data table to plot
#' @param x Character corresponding the column name for what's being counted in the bar graph
#' @param cat Character corresponding to the column name categories of the column
#' @param order.bar Character One of "none", "ascending", or "descending"
#' @param group.by Character corresponding to the column name that columns are grouped by (colour)
#' @param column.width Numerical Width of the column, ranges from 0 to 1. Default at 0.6
#' @param colours Vector (or set.colour function) of colours to use. If not, default palette is generated.
#' @param col.invert logical TRUE/FALSE Whether to use inverted colours or not
#' @param stacked logical TRUE/FALSE to stack the columns or not still supported but maybe not in the future
#' @param position Character One of "identity", "stacked" or "dodge"
#' @param label logical TRUE/FALSE on whether to label the points or not
#' @param label.unit Character What unit does y axis (and labels if any) will have
#' @param legend.title Character Title for the legend if multiple colours are used
#' @param label.adjust Numeric Factor to adjust the labels by default is 2.5\% or 0.025
#' @param plot.title Character denoting title of the plot
#' @param plot.fig.num Character denoting plot number (or another plot annotations)
#' @param y.axis Character denoting axis title for y (unit)
#' @param caption Character denoting sources and other captions
#' @param logo logical (TRUE/FALSE) denote whether to add BIIE logo or not
#' @param logo.type Character; either "small" or "big" and activates only when logo is TRUE, decides whether to add full (big) or abridged (small) logo
#' @param export logical TRUE or FALSE whether to export file as PDF under default options (height=6 inches, width=9 inches)
#' @param export.name Character Name of the exported PDF file
#' @return Single column plot that conforms to Brookfield styling
#' @examples
#' plot.column.bf(data,"income","automation.risk")
plot.column.bf <- function(data,x,cat,
                           order.bar="No",
                           group.by=NULL,
                           column.width=0.6,
                           colours=NULL,
                           col.invert=FALSE,
                           stacked = FALSE,
                           label=FALSE,
                           label.unit = "",
                           label.adjust = 0.025,
                           plot.title="",
                           plot.fig.num="",
                           y.axis = "",
                           legend.title = "",
                           caption = "",
                           logo = FALSE,
                           logo.type = "small",
                           export=FALSE,
                           export.name=""){
  if(!data.table::is.data.table(data)){ #Chek and coerce into data.table
    clone <- data.table::as.data.table(data)
    warning("Data supplied is not data.table - forcing it to be data.table; may not produce desirable results")
  }
  else{
    clone <- data

  }
  #Set up basic theme elements
  column.theme <- brookfield.base.theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size=10,
                                                       margin=ggplot2::margin(t=2),
                                                       family = "GT-Pressura-Light",
                                                       angle=90,
                                                       hjust = 1,
                                                       vjust = 0.5),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
    )
  if(col.invert){
    column.theme <- brookfield.base.theme(inverted=TRUE) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size=10,
                                                         margin=ggplot2::margin(t=2),
                                                         family = "GT-Pressura-Light",
                                                         angle=90,
                                                         hjust = 1,
                                                         vjust = 0.5,
                                                         color = "white"),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank())
  }
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
      ggplot2::geom_col(width=column.width,fill=colours) +
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
      p <- p + ggplot2::geom_col(width=column.width, position="stack") +
        ggplot2::scale_y_continuous(expand=c(0,0),limits = c(0,max.plot), breaks = ticks.seq$breaks, labels = ticks.seq$labels) +
        ggplot2::scale_fill_manual(values=colours) +
        ggplot2::guides(fill=guide_legend(title=legend.title,nrow=num.row,title.position = "top"))
    }
    else{
      p <- p + ggplot2::geom_col(width=column.width,position=position_dodge(width=column.width)) +
        ggplot2::scale_y_continuous(expand=c(0,0),limits = c(0,max.plot), breaks = ticks.seq$breaks, labels = ticks.seq$labels) +
        ggplot2::scale_fill_manual(values=colours) +
        ggplot2::guides(fill=guide_legend(title=legend.title,nrow=num.row,title.position = "top"))
    }

  }
  #Set numeric label for values into the columns
  if(label){
    if(stacked){ #Label for stacked bar chart - has an extra argument of position = position_stacked(vjust=0.5)
      if(label.unit == "$"){
        p <- p + ggplot2::geom_text(data=clone,
                                    ggplot2::aes(label=stringr::str_c(label.unit,
                                                                      scales::comma(round(unlist(clone[,get(x)]),1)))),
                                    nudge_y=nudge.amt,
                                    position = ggplot2::position_stack(vjust = 0.5),
                                    size=11*0.352777778,
                                    family="GT-Pressura-Regular")
      }
      else{
        p <- p + ggplot2::geom_text(data=clone,
                                    ggplot2::aes(label=stringr::str_c(scales::comma(round(unlist(clone[,get(x)]),1)),
                                                                      label.unit)),
                                    nudge_y=nudge.amt,
                                    position = ggplot2::position_stack(vjust = 0.5),
                                    size=11*0.352777778,
                                    family="GT-Pressura-Regular")
      }
    }
    else{ #Label when it is not a stacked bar
      if(label.unit == "$"){
        p <- p + ggplot2::geom_text(data=clone,ggplot2::aes(label=stringr::str_c(label.unit,
                                                                                 scales::comma(round(unlist(clone[,get(x)]),1)))),
                                    nudge_y=nudge.amt,
                                    position = ggplot2::position_dodge(width=column.width),
                                    size=11*0.352777778,
                                    family="GT-Pressura-Regular")
      }
      else{
        p <- p + ggplot2::geom_text(data=clone,ggplot2::aes(label=stringr::str_c(scales::comma(round(unlist(clone[,get(x)]),1)),
                                                                                 label.unit)),
                                    nudge_y=nudge.amt,
                                    position = ggplot2::position_dodge(width=column.width),
                                    size=11*0.352777778,
                                    family="GT-Pressura-Regular")
      }
    }


  }
  if(length(unique(clone[,get(cat)]))<= 5){ #If there are more than 10 groups, make the x axis certicle
    p <- p + theme(axis.text.x = ggplot2::element_text(angle = 90, size = 11,
                                                       margin = ggplot2::margin(t = 0, l = 10),
                                                       hjust = 1,
                                                       vjust = 0.5))
  }
  p <- p + ggplot2::labs(subtitle = plot.title,
                         title = plot.fig.num,
                         y = y.axis,
                         caption = caption)
  #Add logo if needed
  if(logo){
    p <- add_logo(p,logo.type)
  }
  #Export into file
  if(export){
    if(export.name == ""){
      export.name <- "Rplot.pdf"
    }
    export.bf.plot(export.name, p, p.height=6, p.width=7.25)
  }
  return(p)
}
