# Key Installation point
# 1. You need to have the fonts downloaded
# 2. You need to run extrafont and import those fonts
# 3. You need to manually edit the font family names in the resultatnt table so the viewer and the exporter doesn't get tripped up







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
#' @param plot.title Character denoting title of the plot
#' @param plot.fig.num Character denoting plot number (or another plot annotations)
#' @param legend.title Character denoting legend titles
#' @param caption character for caption (sources etc)
#' @param export TRUE/FALSE whether to export file as EPS under default options (height=6 inches, width=9 inches)
#' @param export.name Name of the exported EPS file
#' @return A map plot that conforms to Brookfield style
#' @examples
#' plot.map.prov.bf()
plot.map.prov.bf <- function(province.name = c("NL","PE","NS","NB","QC","ON","MB","SK","AB","BC","YT","NT","NU"),
                           label = TRUE,
                           value.vector = NULL,
                           colours = NULL,
                           plot.title = "",
                           plot.fig.num = "",
                           caption = "",
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
                            fill=fill.vector),colour=set.colours(1,categorical.choice = "grey"),size=0.1) + #geom_polygon actually draws the shapes
      scale_fill_gradient(low=gradient.end,high=gradient.start) + #Set the colour scale
      ggplot2::coord_map("lambert",parameters=c(49,77),expand=FALSE) #Project it using lambert projection
  }
  else{
    p <- p + geom_polygon(aes(x=long,
                          y=lat,
                          group=group),colour=set.colours(1,categorical.choice = "grey"),fill=NA,size=0.1) + #Same thing, but without a fill
      ggplot2::coord_map("lambert",parameters=c(49,77),expand=FALSE) #Project is using lambert projection
  }
  if(label){ #Label provinces
    p <- p + geom_text(data=province.data[abbrev %in% province.name],aes(x=long,y=lat,label=province),colour="black",family="RooneySans-Regular")
  }
  p <- p + labs(subtitle = plot.title,title = plot.fig.num, caption = caption) + #Add in all the usual stuff and remove expand axis.
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    guides(fill=guide_legend(title=legend.title))
  return(p)
}




#' Create a map of selected Canadian CMA with shades
#' @usage plot.map.cma.bf(province.name = c("NL","PE","NS","NB","QC","ON","MB","SK","AB","BC","YT","NT","NU"),
#' label = TRUE,
#'value.data = NULL,
#'colours = NULL,
#'plot.title = "",
#'plot.fig.num = "",
#'caption = "",
#'legend.title = "")
#' @param province.name A vector of the 2 letter code for provinces to plot. One or more of:
#' "NL","PE","NS","NB","QC","ON","MB","SK","AB","BC","YT","NT","NU"
#' @param value.data a data table with 2 columns - first column must be string that denotes Province-CMA ID. Second column has to be numeric and is the value you want to show
#' @param colours Vector (or set.colour function) of colours to use. If not, default palette is generated.
#' @param plot.title Character denoting title of the plot
#' @param plot.fig.num Character denoting plot number (or another plot annotations)
#' @param legend.title Character denoting legend titles
#' @param caption character for caption (sources etc)
#' @param export TRUE/FALSE whether to export file as EPS under default options (height=6 inches, width=9 inches)
#' @param export.name Name of the exported EPS file
#' @return A map plot that conforms to Brookfield style
#' @examples
#' plot.map.cma.bf()
plot.map.cma.bf <- function(province.name = c("NL","PE","NS","NB","QC","ON","MB","SK","AB","BC","YT","NT","NU"),
                            value.data = NULL,
                            colours = NULL,
                            plot.title = "",
                            plot.fig.num = "",
                            caption = "",
                            legend.title = ""){
  fill.vector <- NULL
  code.comparison <- sort(province.data[abbrev %in% province.name,code])
  p <- ggplot() +
    brookfield.base.theme() +
    theme(axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    geom_path(data=areas.provDF[id %in% code.comparison],aes(x=long,y=lat,group=group),colour=set.colours(1,categorical.choice="grey"),size=0.1)
  if(!is.null(value.data)){
    repetition.data <- cma.data[stringr::str_sub(ID,1,2) %in% code.comparison]
    repetition.vector <- repetition.data[,num.row]
    clone <- value.data
    not.in.data <- repetition.data[-repetition.data[ID %in% clone[,get(names(clone)[1])],which=TRUE],ID]
    dum <- data.table(V1=not.in.data,V2=rep(NA,length(not.in.data)))
    names(dum) <- names(clone)
    clone <- rbindlist(list(clone,dum))
    setorderv(clone,names(clone)[1],na.last=FALSE)
    clone <- clone[str_sub(get(names(clone)[1]),1,2) %in% code.comparison]
    fill.vector <- rep(clone[,get(names(clone)[2])],as.vector(repetition.vector))
    if(is.numeric(value.data[,get(names(value.data)[2])])){
      if(is.null(colours)){
        gradient.start <- "white"
        gradient.end <- set.colours(1)
      }
      else{
        if(length(colours)>2){
          stop("Too many colours for now")
        }
        if(length(colours)==2){
          gradient.start <- colours[1]
          gradient.end <- colours[2]
        }
        else{
          gradient.start <- colours
          gradient.end <- "white"
        }
      }
    }
  }
  if(is.null(fill.vector)){
    p <- p + geom_polygon(data=areas.cmaDF[str_sub(id,1,2) %in% code.comparison],
                          aes(x=long,y=lat,group=group),
                          colour=set.colours(1,categorical.choice = "grey"),fill=NA,size=0.1) +
      ggplot2::coord_map("lambert",parameters=c(49,77),expand=FALSE)
  }
  else{
    p <- p + geom_polygon(data=areas.cmaDF[str_sub(id,1,2) %in% code.comparison],
                          aes(x=long,y=lat,group=group,fill=fill.vector),
                          colour=set.colours(1,categorical.choice = "grey"),size=0.1) +
      ggplot2::coord_map("lambert",parameters=c(49,77),expand=FALSE) +
      scale_fill_gradient(low = gradient.start,high=gradient.end)

  }
  p <- p + labs(subtitle = plot.title,title = plot.fig.num, caption = caption) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    guides(fill=guide_legend(title=legend.title))
  return(p)
}

#' Create a map of selected Canadian CSD with shades
#' @usage plot.map.csd.bf(province.name = c("NL","PE","NS","NB","QC","ON","MB","SK","AB","BC","YT","NT","NU"),
#'value.data = NULL,
#'colours = NULL,
#'plot.title = "",
#'plot.fig.num = "",
#'caption = "",
#'legend.title = "")
#' @param province.name A vector of the 2 letter code for provinces to plot. One or more of:
#' "NL","PE","NS","NB","QC","ON","MB","SK","AB","BC","YT","NT","NU"
#' @param value.data a data table with 2 columns - first column must be string that denotes Province-CSD ID. Second column has to be numeric and is the value you want to show
#' @param colours Vector (or set.colour function) of colours to use. If not, default palette is generated.
#' @param plot.title Character denoting title of the plot
#' @param plot.fig.num Character denoting plot number (or another plot annotations)
#' @param legend.title Character denoting legend titles
#' @param caption character for caption (sources etc)
#' @param export TRUE/FALSE whether to export file as EPS under default options (height=6 inches, width=9 inches)
#' @param export.name Name of the exported EPS file
#' @return A map plot that conforms to Brookfield style
#' @examples
#' plot.map.csd.bf()
plot.map.csd.bf <- function(province.name = c("NL","PE","NS","NB","QC","ON","MB","SK","AB","BC","YT","NT","NU"),
                            label = TRUE,
                            value.data = NULL,
                            colours = NULL,
                            plot.title = "",
                            plot.fig.num = "",
                            caption = "",
                            legend.title = ""){
  fill.vector <- NULL
  code.comparison <- sort(province.data[abbrev %in% province.name,code])
  p <- ggplot() +
    brookfield.base.theme() +
    theme(axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  if(!is.null(value.data)){
    repetition.data <- csd.data[stringr::str_sub(ID,1,2) %in% code.comparison]
    repetition.vector <- repetition.data[,num.row]
    clone <- value.data
    not.in.data <- repetition.data[-repetition.data[ID %in% clone[,get(names(clone)[1])],which=TRUE],ID]
    dum <- data.table(V1=not.in.data,V2=rep(NA,length(not.in.data)))
    names(dum) <- names(clone)
    clone <- rbindlist(list(clone,dum))
    setorderv(clone,names(clone)[1],na.last=FALSE)
    clone <- clone[str_sub(get(names(clone)[1]),1,2) %in% code.comparison]
    fill.vector <- rep(clone[,get(names(clone)[2])],as.vector(repetition.vector))
    if(is.numeric(value.data[,get(names(value.data)[2])])){
      if(is.null(colours)){
        gradient.start <- set.colours(1,categorical.choice = "light.blue")
        gradient.end <- set.colours(1,categorical.choice = "dark.blue")
      }
      else{
        if(length(colours)>2){
          stop("Too many colours for now")
        }
        if(length(colours)==2){
          gradient.start <- colours[1]
          gradient.end <- colours[2]
        }
        else{
          gradient.start <- colours
          gradient.end <- "white"
        }
      }
    }
  }
  if(is.null(fill.vector)){
    p <- p + geom_polygon(data=areas.csdDF[str_sub(id,1,2) %in% code.comparison],
                          aes(x=long,y=lat,group=group),
                          colour=set.colours(1,categorical.choice = "grey"),fill=NA,size=0.01) +
      ggplot2::coord_map("lambert",parameters=c(49,77),expand=FALSE)
  }
  else{
    p <- p + geom_polygon(data=areas.csdDF[str_sub(id,1,2) %in% code.comparison],
                          aes(x=long,y=lat,group=group,fill=fill.vector),
                          colour=set.colours(1,categorical.choice = "grey"),size=0.01) +
      ggplot2::coord_map("lambert",parameters=c(49,77),expand=FALSE) +
      scale_fill_gradient(low=gradient.start,high=gradient.end,na.value = set.colours(1,categorical.choice = "grey"))

  }
  p <- p + labs(subtitle = plot.title,title = plot.fig.num, caption = caption) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    guides(fill=guide_legend(title=legend.title,title.position = "top",title.hjust = 0.5))
  return(p)
}


#' Create a map of selected Canadian ER with shades
#' @usage plot.map.csd.bf(province.name = c("NL","PE","NS","NB","QC","ON","MB","SK","AB","BC","YT","NT","NU"),
#'value.data = NULL,
#'colours = NULL,
#'plot.title = "",
#'plot.fig.num = "",
#'caption = "",
#'legend.title = "")
#' @param province.name A vector of the 2 letter code for provinces to plot. One or more of:
#' "NL","PE","NS","NB","QC","ON","MB","SK","AB","BC","YT","NT","NU"
#' @param value.data a data table with 2 columns - first column must be string that denotes Province-CSD ID. Second column has to be numeric and is the value you want to show
#' @param colours Vector (or set.colour function) of colours to use. If not, default palette is generated.
#' @param plot.title Character denoting title of the plot
#' @param plot.fig.num Character denoting plot number (or another plot annotations)
#' @param legend.title Character denoting legend titles
#' @param caption character for caption (sources etc)
#' @param export TRUE/FALSE whether to export file as EPS under default options (height=6 inches, width=9 inches)
#' @param export.name Name of the exported EPS file
#' @return A map plot that conforms to Brookfield style
#' @examples
#' plot.map.er.bf()
plot.map.er.bf <- function(province.name = c("NL","PE","NS","NB","QC","ON","MB","SK","AB","BC","YT","NT","NU"),
                            label = TRUE,
                            value.data = NULL,
                            colours = NULL,
                            plot.title = "",
                            plot.fig.num = "",
                            caption = "",
                            legend.title = ""){
  fill.vector <- NULL
  code.comparison <- sort(province.data[abbrev %in% province.name,code])
  p <- ggplot() +
    brookfield.base.theme() +
    theme(axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  if(!is.null(value.data)){
    repetition.data <- er.data[stringr::str_sub(ID,1,2) %in% code.comparison]
    repetition.vector <- repetition.data[,num.row]
    clone <- value.data
    not.in.data <- repetition.data[-repetition.data[ID %in% clone[,get(names(clone)[1])],which=TRUE],ID]
    dum <- data.table(V1=not.in.data,V2=rep(NA,length(not.in.data)))
    names(dum) <- names(clone)
    clone <- rbindlist(list(clone,dum))
    setorderv(clone,names(clone)[1],na.last=FALSE)
    clone <- clone[str_sub(get(names(clone)[1]),1,2) %in% code.comparison]
    fill.vector <- rep(clone[,get(names(clone)[2])],as.vector(repetition.vector))
    if(is.numeric(value.data[,get(names(value.data)[2])])){
      if(is.null(colours)){
        gradient.start <- set.colours(1,categorical.choice = "light.blue")
        gradient.end <- set.colours(1,categorical.choice = "dark.blue")
      }
      else{
        if(length(colours)>2){
          stop("Too many colours for now")
        }
        if(length(colours)==2){
          gradient.start <- colours[1]
          gradient.end <- colours[2]
        }
        else{
          gradient.start <- colours
          gradient.end <- "white"
        }
      }
    }
  }
  if(is.null(fill.vector)){
    p <- p + geom_polygon(data=areas.erDF[str_sub(id,1,2) %in% code.comparison],
                          aes(x=long,y=lat,group=group),
                          colour=set.colours(1,categorical.choice = "grey"),fill=NA,size=0.1) +
      ggplot2::coord_map("lambert",parameters=c(49,77),expand=FALSE)
  }
  else{
    p <- p + geom_polygon(data=areas.erDF[str_sub(id,1,2) %in% code.comparison],
                          aes(x=long,y=lat,group=group,fill=fill.vector),
                          colour=set.colours(1,categorical.choice = "grey"),size=0.1) +
      ggplot2::coord_map("lambert",parameters=c(49,77),expand=FALSE) +
      scale_fill_gradient(low=gradient.start,high=gradient.end,na.value = set.colours(1,categorical.choice = "grey"))

  }
  p <- p + labs(subtitle = plot.title,title = plot.fig.num, caption = caption) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    guides(fill=guide_legend(title=legend.title,title.position = "top",title.hjust = 0.5))
  return(p)
}






