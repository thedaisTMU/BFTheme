
library(data.table)
library(devtools)
library(stringr)
census.2016.varnames <- fread("2016_CENSUS_VARNAMES.csv")
census.2016.varnames[,Name.dim:=Name]
census.2016.varnames[8:233,Name.dim:=str_c("DIM: ",Name)]
census.2016.varnames[,Name.id:=Name]
census.2016.varnames[8:233,Name.id:=str_c("Member ID: ",Name)]
census.2016.varnames[8:233,Name.notes:=str_c("Notes: ",Name)]

census.2016.varnames.vec <- c(census.2016.varnames[,Variable],str_c(census.2016.varnames[8:233,Variable],".ID"),str_c(census.2016.varnames[8:233,Variable],".NOTES"))
names(census.2016.varnames.vec) <- c(census.2016.varnames[,Name.dim],census.2016.varnames[8:233,Name.id],census.2016.varnames[8:233,Name.notes])

census.2016.varnames <- census.2016.varnames.vec

use_data(census.2016.varnames,overwrite=TRUE)
rm(census.2016.varnames,census.2016.varnames.vec)
