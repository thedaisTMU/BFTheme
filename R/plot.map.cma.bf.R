
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
#' @param census.year Numeric. One of 2016 or 2021 - Default is 2021 Census as of version 1.1.0
#' @param colours Vector (or set.colour function) of colours to use. If not, default palette is generated.
#' @param null.colour Colour for null values
#' @param plot.title Character denoting title of the plot
#' @param plot.fig.num Character denoting plot number (or another plot annotations)
#' @param legend.title Character denoting legend titles
#' @param caption character for caption (sources etc)
#' @param logo logical (TRUE/FALSE) denote whether to add BIIE logo or not
#' @param logo.type Character; either "small" or "big" and activates only when logo is TRUE, decides whether to add full (big) or abridged (small) logo
#' @param export TRUE/FALSE whether to export file as EPS under default options (height=6 inches, width=9 inches)
#' @param export.name Name of the exported EPS file
#' @return A map plot that conforms to Brookfield style
#' @examples
#' plot.map.cma.bf()
plot.map.cma.bf <- function(province.name = c("NL","PE","NS","NB","QC","ON","MB","SK","AB","BC","YT","NT","NU"),
                            value.data = NULL,
                            census.year = 2021,
                            colours = NULL,
                            null.colour = set.colours(7,type="gradient",gradient.choice="grey")[7],
                            plot.title = "",
                            plot.fig.num = "",
                            caption = "",
                            logo = FALSE,
                            logo.type = "small",
                            legend.title = ""){
  fill.vector <- NULL
  if(!(census.year %in% c(2016,2021))){
    stop("Please enter a valid census year for the census.year variable (either 2016 or 2021)")
  }
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
    repetition.data <- get(str_c("cma.data",".",census.year,sep=""))[stringr::str_sub(ID,1,2) %in% code.comparison]
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
    p <- p + geom_polygon(data=get(str_c("areas.cma",".",census.year,"DF",sep=""))[str_sub(id,1,2) %in% code.comparison],
                          aes(x=long,y=lat,group=group),
                          colour=null.colour,fill=NA,size=0.1) +
      ggplot2::coord_map("lambert",parameters=c(49,77),expand=FALSE)
  }
  else{
    p <- p + geom_polygon(data=get(str_c("areas.cma",".",census.year,"DF",sep=""))[str_sub(id,1,2) %in% code.comparison],
                          aes(x=long,y=lat,group=group,fill=fill.vector),
                          colour=null.colour,size=0.1) +
      ggplot2::coord_map("lambert",parameters=c(49,77),expand=FALSE) +
      scale_fill_gradient(low = gradient.start,high=gradient.end)

  }
  p <- p + labs(subtitle = plot.title,title = plot.fig.num, caption = caption) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    guides(fill=guide_legend(title=legend.title))
  #Add logo if needed
  if(logo){
    p <- add_logo(p,logo.type)
  }
  return(p)
}
