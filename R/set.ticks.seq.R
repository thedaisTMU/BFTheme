
#' Set appropriate ticks. Wrapper for cbreaks in scale that allows for $ and percent ticks
#'
#' @param max numerical max of the ticked plot
#' @param min numerical min of the ticked plot
#' @param unit Unit that goes wit hthe ticks
#' @num.ticks Currently not in use - to specify number of ticks
#' @return A vector with $breaks which are breaks and $labels which are the labelled values
#' @examples
#' set.ticks.seq(100,0,"%")
set.ticks.seq <- function(max,min,unit,num.ticks=5){
  if(unit==""){ #If there are no unit
    ticks <- scales::cbreaks(c(max,min),labels= unit_format(unit=unit,big.mark = ",",sep=""))
  }
  if(unit=="$"){
    ticks <- scales::cbreaks(c(max,min),labels = dollar_format(largest_with_cents = 100)) #Format money
    return(ticks)
  }
  if(unit=="%" & max >= 75){
    ticks <- scales::cbreaks(c(100,0),labels = unit_format(unit="%",big.mark = ",",sep="")) #Format percentage
    return(ticks)
  }
  else{
    ticks <- scales::cbreaks(c(max,min),labels=unit_format(unit=unit,big.mark = ",",sep=" ")) #Format percentage without the percentage sign
    return(ticks)
  }
}
