#Helper functions
#Dictionary











#Experimental Function
generate.dot <- function(number,
                         nrow,direction="right",
                         starting.coord=c(0,0),
                         dual.colour.break = 0,
                         colour.title = c("First","Second")){
  clean.col <- floor(number/nrow)
  if(direction == "right"){
    x.nudge <- 1
  }
  if(direction == "left"){
    x.nudge = -1
  }
  end.x <- starting.coord[1] + x.nudge * (clean.col-1)
  end.y <- starting.coord[2] + nrow - 1
  main.data <- data.table::data.table(x = rep(seq(starting.coord[1], end.x), each = nrow),
                                      y = rep(seq(starting.coord[2], end.y), times = clean.col))
  remaining <- number - clean.col*nrow
  if(remaining > 0){
    aux.data <- data.table::data.table(x = rep(end.x + x.nudge,times=remaining),
                                       y = seq(starting.coord[2], starting.coord[2] + remaining - 1))
    main.data <- data.table::rbindlist(list(main.data,aux.data))
  }
  if(dual.colour.break > 0){
    main.data[,colour:=c(rep(colour.title[1],dual.colour.break),rep(colour.title[2],number - dual.colour.break))]
  }
  else{
    main.data[,colour:=colour.title[1]]
  }
  return(main.data)
}

#Calculate label brightness
label.colour <- function(col.input){
  rgb <- col2rbg(col.input)
  brightness <- (rgb[1]*299+rgb[2]*587+rgb[3]*114)/1000
  if(brightness > 123){
    return("black")
  }
  else{
    return("white")
  }
}











