
#' Create a map of selected Canadian Provinces with shades
#' @usage plot.map.prov.bf(province.name = c("NL","PE","NS","NB","QC","ON","MB","SK","AB","BC","YT","NT","NU"),
#' label = TRUE,
#'value.vector = NULL,
#'colours = NULL,
#'plot.title = "",
#'plot.fig.num = "",
#'caption = "",
#'legend.title = "")
#' @param province.name A vector of the 2 letter code for provinces to plot. One of:
#' "NL","PE","NS","NB","QC","ON","MB","SK","AB","BC","YT","NT","NU"
#' @param label TRUE/FALSE whether to label the provinces or not
#' @param value.vector a vector with values for the shades - has to be in the order of the provinces
#' @param colours Vector (or set.colour function) of colours to use. If not, default palette is generated.
#' @param null.colour Colour for null values
#' @param plot.title Character denoting title of the plot
#' @param plot.fig.num Character denoting plot number (or another plot annotations)
#' @param legend.title Character denoting legend titles
#' @param caption character for caption (sources etc)
#' @param export TRUE/FALSE whether to export file as EPS under default options (height=6 inches, width=9 inches)
#' @param export.name Name of the exported EPS file
#' @param logo logical (TRUE/FALSE) denote whether to add BIIE logo or not
#' @param logo.type Character; either "small" or "big" and activates only when logo is TRUE, decides whether to add full (big) or abridged (small) logo
#' @return A map plot that conforms to Brookfield style
#' @examples
#' plot.map.prov.bf()
plot.map.prov.bf <- function(province.name = c("NL","PE","NS","NB","QC","ON","MB","SK","AB","BC","YT","NT","NU"),
                             label = TRUE,
                             value.vector = NULL,
                             colours = NULL,
                             null.colour = set.colours(7,type="gradient",gradient.choice="grey")[7],
                             plot.title = "",
                             plot.fig.num = "",
                             caption = "",
                             logo = FALSE,
                             logo.type = "small",
                             legend.title = ""){
  code.comparison <- sort(province.data[abbrev %in% province.name,code]) #Get the Province ID for the provinces in the list to plot
  fill.vector <- NA #Initialize the fill vector
  if(!is.null(value.vector)){ #If the value vector has something
    repetition.vector <- province.data[code %in% code.comparison,row.num] #Need to set the fill value for the shape so get the row number that will need to be repeated for
    fill.vector <- rep(value.vector,repetition.vector) #Create the full fill vector for all the shapes
    #Set the beginning and end colour for the shades of provinces
    if(is.numeric(value.vector)){ #If the values are numeric
      if(is.null(colours)){
        gradient.start <- set.colours(1) #if null, then set dark blue fades to white
        gradient.end <- "white"
      }
      else{
        if(length(colours)>2){ #Right now only supports 2 colour gradients
          stop("Too many colours for now")
        }
        if(length(colours)==2){ #If both are supplied
          gradient.start <- colours[1]
          gradient.end <- colours[2]
        }
        else{ #If only 1 colour is supplied, fade to white from the beginning colour
          gradient.start <- colours
          gradient.end <- "white"
        }
      }
    }
    else{
      stop("At this point, only numeric vectors are supported.") #Categorical variable shades to be added in the future
    }
  }
  p <- ggplot(data=areas.provDF[id %in% code.comparison]) + #Set up base plot - only choose shapes within province chosen
    brookfield.base.theme() + #Base theme
    theme(axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) #All these remove the axis

  if(!is.na(fill.vector)){ #If we have a fill vector
    p <- p + geom_polygon(aes(x=long,
                              y=lat,
                              group=group,
                              fill=fill.vector),colour=null.colour,size=0.1) + #geom_polygon actually draws the shapes
      scale_fill_gradient(low=gradient.end,high=gradient.start) + #Set the colour scale
      ggplot2::coord_map("lambert",parameters=c(49,77),expand=FALSE) #Project it using lambert projection
  }
  else{
    p <- p + geom_polygon(aes(x=long,
                              y=lat,
                              group=group),colour=null.colour,fill=NA,size=0.1) + #Same thing, but without a fill
      ggplot2::coord_map("lambert",parameters=c(49,77),expand=FALSE) #Project is using lambert projection
  }
  if(label){ #Label provinces
    p <- p + geom_text(data=province.data[abbrev %in% province.name],aes(x=long,y=lat,label=province),colour="black",family="RooneySans-Regular")
  }
  p <- p + labs(subtitle = plot.title,title = plot.fig.num, caption = caption) + #Add in all the usual stuff and remove expand axis.
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    guides(fill=guide_legend(title=legend.title))
  #Add logo if needed
  if(logo){
    p <- add_logo(p,logo.type)
  }
  return(p)
}
