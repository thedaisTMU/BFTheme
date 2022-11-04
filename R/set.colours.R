
#' Set a colour palette or generate one according to your specification.
#' Refers to the main Style Guide on choosing colours that match your intention and report style.
#'set.colours <- function(n,type = "categorical",gradient.choice = "dark.blue")
#' @param n Integer to select the right set of colours
#' @param type Either "categorical" or "gradient", decides the type of colour you're going to get
#' @param categorical.choice Specify custom colour names as a vector (same name as gradient.choice)One of:
#' c("dark.blue","light.blue","magenta","yellow","pink","brown","green","grey")
#' @param gradient.choice For gradient type, specify the colour. One of: c("dark.blue","light.blue",
#' "pink","yellow","magenta","orange","green","teal","grey")
#' @param special special colours, will be added in the future - opposing colours, opposing gradients
#' @return Character vector containing HEX value for colours
#' @examples
#' set.colours(1,type="categorical",gradient.choice="dark.blue")
set.colours <- function(n,
                        type = "categorical",
                        gradient.choice = "magenta",
                        categorical.choice = NULL,
                        special = NULL){
  #Setting all the base vectors to refer to - precise because I don't trust R's generation of gradients
  base.set <- c("dark.blue"="#14365d","light.blue"="#8ad4df","magenta"="#dd347a","yellow"="#faaf19"
                ,"pink"="#ffa5b9","green"="#3c7d78","brown"="#c37546","grey"="#626466","ecru"="#ebe6de")
  dark.blue <- c("#14365d","#284469","#4f6885","#4c6282","#6f829b","#8a9bae","#8192a8","#94a3b5")
  light.blue <- c("#8ad4df","#94d7e1","#9fdbe4","#a9dfe7","#b4e3ea","#bfe7ed","#c9ebf0","#d4eff3")
  magenta <- c("#dd347a","#e04686","#e35892","#e66b9e","#e97daa","#ec90B6","#efa2c2","#f2b5ce")
  yellow <- c("#ffc800","#ffcd17","#ffd22e","#ffd745","#ffdc5c","#ffe173","#ffe68b","#ffeba2")
  pink <- c("#ffa5b9","#ffafc1","#ffb9c9","#ffc3d0","#ffccd8","#ffd6df","#ffdfe7","#ffe9ee")
  brown <- c("#f7941e","#f79d32","#f8a746","#f9b15b","#f9ba6f","#fac484","#fbce98","#fcd8ad")
  green <- c("#3c7d78","#528b86","#6d9e9a","#679894","#7ca6a2","#9ebebc","#a5c2bf","#cedfdd")
  grey <- c("#626466","#7d8890","#8a949b","#939598","#9a9fa3","#a4acb1","#a5abaf","#b1b8bc")
  #Check if you have way too many categories - 7 is the absolute max!
  if(n > 7){
    stop("You have way too many categories. Reduce it!")
  }
  #Check if the type is categorical
  if(type == "categorical"){
    if(is.null(categorical.choice)){ #Check if a specific colour set was requested
      return(unname(base.set[1:n])) #If not then return sequential from dark blue,light blue to pink
    }
    else{
      if(length(categorical.choice)!=n){ #Check if the length of choice matches requested number of colours
        stop("You didn't have the same number of colours as you specified. Change n or categorical.choice") #This is for sanity check, not because of code
      }
      return(unname(base.set[categorical.choice])) #Return the corresponding set of colours
    }
  }
  if(type == "gradient"){ #On the otherhand, if it's a gradient
    #Set up all the gradient choices
    gra2 <- c(1, 5)
    gra3 <- c(1, 4, 7)
    gra4 <- c(1, 3, 5, 7)
    gra5 <- c(1, 2, 4, 6, 7)
    gra6 <- c(1, 2, 3, 4, 6, 7)
    gra7 <- c(1, 2, 3, 4, 5, 6, 7)
    return(get(gradient.choice)[get(str_c("gra",n))]) #Get the right number of gradients.
  }
}
