#' Meta-data for each provinces
#'
#' A dataset that contains meta data on each of the provinces - used for mapping purposes
#'
#'
#' @format A data table with 13 rows and 6 variables
#' \describe{
#'   \item{code}{Statistics Canada's code for provinces}
#'   \item{province}{English name of the provinces}
#'   \item{region}{Statistics Canada's region for the province}
#'   \item{abbrev}{2 letter abbreviation for the provinces}
#'   \item{long}{longitude of the centroid points}
#'   \item{lat}{latitude of the centroid points}
#'   \item{row.num}{row number for iterating on the colour gradients}
#'   ...
#' }
#' @source \url{https://www.statcan.gc.ca/pub/92-195-x/2011001/geo/prov/tbl/tbl8-eng.htm}
"province.data"

#' Data table for drawing provinces
#'
#' A dataset that contains vector information for drawing provinces
#'
#'
#' @format A data table with 13,205 rows and 8 variables
#' \describe{
#'   \item{long}{longitude of the point}
#'   \item{lat}{latitude of the point}
#'   \item{order}{Order by which points are connected}
#'   \item{hole}{Logical marking if shape drawn is an island or not}
#'   \item{piece}{id of each unbroken piece}
#'   \item{id}{Province ID}
#'   \item{group}{Province ID + id for each unbroken piece}
#'   ...
#' }
"areas.provDF"

#' Meta-data for each CMA/CA
#'
#' A dataset that contains meta data on each of the CMA/CA - used for mapping purposes
#'
#'
#' @format A data table with 156 rows and 5 variables
#' \describe{
#'   \item{ID}{Statistics Canada's code for CMA/CA}
#'   \item{Name}{English name of the CMA/CA}
#'   \item{long}{longitude of the centroid points}
#'   \item{lat}{latitude of the centroid points}
#'   \item{row.num}{row number for iterating on the colour gradients}
#'   ...
#' }
"cma.data"

#' Data table for drawing CMA/CA
#'
#' A dataset that contains vector information for drawing provinces
#'
#'
#' @format A data table with 35,852 rows and 8 variables
#' \describe{
#'   \item{long}{longitude of the point}
#'   \item{lat}{latitude of the point}
#'   \item{order}{Order by which points are connected}
#'   \item{hole}{Logical marking if shape drawn is an island or not}
#'   \item{piece}{id of each unbroken piece}
#'   \item{id}{Province ID}
#'   \item{group}{Province ID + id for each unbroken piece}
#'   ...
#' }
"areas.cmaDF"

#' A named vector for 2016 census column name quick reference
#'
#' @format A named vector with about 200 elements
"census.2016.varnames"

