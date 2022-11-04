
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
# Wish list:
# Histogram
















plot.adjacency.bf <- function(edges.map,
                              vertex.ordering = NULL,
                              bin.size = NA,
                              gradient.base = "dark.blue",
                              x.axis = "",
                              y.axis = "",
                              plot.title = "",
                              plot.fig.num = "",
                              caption = "",
                              logo = FALSE,
                              logo.type = "small"){
  #Make a copy of the original data table so I don't need to make any duplicates
  edge.table <- data.table::copy(edges.map)
  #If edge map also has weight - discard for now. Might add weight based colour in the future
  if(ncol(edges.map)>2){
    warning("More than 2 columns are detected in the edges list. Only the first two columns will be used as the edge list")
    edge.table <- edge.table[,1:2]
  }
  #Rename the table to make sure reference becomes easy
  names(edge.table) <- c("origin","des")
  #Get the list of unique vertices from edge list. Used for comparison
  unique.vertices <- unique(c(edge.table[,unique(origin)],edge.table[,unique(des)]))
  if(is.null(vertex.ordering)){ #If no vertex ordering is provided, make an arbitrary ordering
    vertex.ordering <- data.table(vertex = unique.vertices,
                                  coordinate = seq(1,length(unique.vertices)))
  }
  #If the provided vertx ordering has more than two columns, only extract the first one and warn users.
  if(ncol(vertex.ordering)>2){
    warning("Only the first two columns will be used. The first column should be the vertex name and the second ordering")
  }
  vertex.ordering <- copy(vertex.ordering)
  vertex.ordering <- vertex.ordering[,1:2]
  #Change name of vertex ordering for consistent reference
  names(vertex.ordering) <- c("vertex","coordinate")
  #Check to make sure the number of implied vertex between both tables match - probably want to make a more sophisticated match
  if(nrow(vertex.ordering) != length(unique.vertices)){
    stop("The number of vertex differs from the number of implied vertex from ordering provided.")
  }
  #Merging steps to generate unique coordinates
  setkey(edge.table,origin)
  setkey(vertex.ordering,vertex)
  edge.table <- edge.table[vertex.ordering,nomatch=0]
  names(edge.table) <- c("origin","des","origin.coord")

  setkey(edge.table,des)
  edge.table <- edge.table[vertex.ordering,nomatch=0]
  names(edge.table) <- c("origin","des","origin.coord","des.coord")

  #Get the bin size if no bin size is provided
  if(is.na(bin.size)){
    bin.size <- min(1800,nrow(vertex.ordering))
  }
  #Set up the gradient colours
  gradient.ends <- set.colours(2,gradient.choice = gradient.base)
  #Main plot function
  p <- ggplot2::ggplot(edge.table, aes(origin.coord, des.coord)) +
    brookfield.base.theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size=9, margin=ggplot2::margin(t=2), family = "GT-Pressura-Light")) + #Set axis text. Light to make it less prominent - margin is also precise
    ggplot2::stat_bin_2d(bins = bin.size) +
    scale_fill_gradient(high = gradient.ends[1],low=gradient.ends[2])
    labs(title    = plot.fig.num,
         subtitle = plot.title,
         x        = x.axis,
         y        = y.axis,
         caption  = caption)
  return(p)
    #Add logo if needed
    if(logo){
      p <- add_logo(p,logo.type)
    }
}





