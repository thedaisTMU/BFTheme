
library(rgdal)
library(rgeos)
library(data.table)





#Dealing with Provincial data
areas.prov <- readOGR("lpr_000b16a_e.shp")
proj4string(areas.csd) <- CRS("+proj=lcc +x_0=6200000 +y_0=3000000 +lon_0=-91.866667 +lat_0=63.390675 +datum=NAD83")
areas.prov <- spTransform(areas.prov, CRS=CRS("+proj=longlat +datum=NAD83"))
areas.prov.simp <- gSimplify(areas.prov,tol=0.05,topologyPreserve=TRUE)
areas.provDF <- fortify(areas.prov.simp)
areas.provDF <- as.data.table(areas.provDF)
areas.provDF[id=="12",id:="62"]
areas.provDF[id=="11",id:="61"]
areas.provDF[id=="10",id:="60"]
areas.provDF[id=="9",id:="59"]
areas.provDF[id=="8",id:="48"]
areas.provDF[id=="7",id:="47"]
areas.provDF[id=="6",id:="46"]
areas.provDF[id=="5",id:="35"]
areas.provDF[id=="4",id:="24"]
areas.provDF[id=="3",id:="13"]
areas.provDF[id=="2",id:="12"]
areas.provDF[id=="1",id:="11"]
areas.provDF[id=="0",id:="10"]
areas.provDF <- areas.provDF[num.row>6]
use_data(areas.provDF,overwrite=TRUE)

#Dealing with CMA data
areas.cma <- readOGR("lcma000b16a_e.shp")
proj4string(areas.csd) <- CRS("+proj=lcc +x_0=6200000 +y_0=3000000 +lon_0=-91.866667 +lat_0=63.390675 +datum=NAD83")
areas.cma <- spTransform(areas.cma, CRS=CRS("+proj=longlat +datum=NAD83"))
areas.cma.simp <- gSimplify(areas.cma,tol=0.001,topologyPreserve=TRUE)
areas.cmaDF <- fortify(areas.cma.simp)
areas.cmaDF <- as.data.table(areas.cmaDF)

areas.cmaDF.row <- areas.cmaDF[,.N,by=id]
areas.cmaDF[,id:=rep(areas.cma@data$CMAPUID,areas.cmaDF.row[,N])]
rm(areas.cmaDF.row)
areas.cmaDF[,num.row:=.N,by=group]
areas.cmaDF <- areas.cmaDF[num.row>6]
use_data(areas.cmaDF,overwrite=TRUE)

cma.data <- geography[,.(unique(`CMAname/RMRnom`)),by=`CMAPuid/RMRPidu`]
names(cma.data) <- c("ID","Name")
cma.data[,ID:=as.character(ID)]
cma.data <- cma.data[ID %in% areas.simpleDF[,unique(id)]]
cma.data[,Name:=str_replace_all(Name,"--","-")]
cma.data[,Name:=str_replace_all(Name,"�","e")]
cma.data[,Name:=tstrsplit(Name," /",keep=1)]
cma.data[,Name:=tstrsplit(Name," \\(",keep=1)]
setkey(cma.data,ID)
cma.data[,long:=coordinates(areas.cma)[,1]]
cma.data[,lat:=coordinates(areas.cma)[,2]]
cma.data[,num.row:=areas.cmaDF[,.N,by=id][,2]]
use_data(cma.data,overwrite=TRUE)



#Dealing with CSD data
areas.csd <- readOGR("lcsd000b16a_e.shp")
areas.csd <- spTransform(areas.csd, CRS=CRS("+proj=longlat +datum=NAD83"))
areas.csd.simp <- gSimplify(areas.csd,tol=0.01,topologyPreserve=TRUE)
areas.csdDF <- fortify(areas.csd.simp)
areas.csdDF <- as.data.table(areas.csdDF)
areas.csdDF[,num.row:=.N,by=group]
areas.csdDF.row <- areas.csdDF[,.N,by=id]
areas.csdDF[,id:=rep(csd.data[,ID],areas.csdDF.row[,N])]
rm(areas.csdDF.row)



csd.data <- geography[,.(unique(`CSDname/SDRnom`)),by=`CSDuid/SDRidu`]
names(csd.data) <- c("ID","Name")
csd.data[,ID:=as.character(ID)]
setkey(csd.data,ID)
csd.data[,long:=coordinates(areas.csd.simp)[,1]]
csd.data[,lat:=coordinates(areas.csd.simp)[,2]]
csd.data[,num.row:=areas.csdDF[,.N,by=id][,2]]

#Dealing with ER data
areas.er <- readOGR("ler_000b16a_e.shp")
areas.er <- spTransform(areas.er,CRS=CRS("+proj=longlat +datum=NAD83"))
areas.er.simp <- gSimplify(areas.er,tol=0.05,topologyPreserve=TRUE)
areas.erDF <- fortify(areas.er.simp)
areas.erDF <- as.data.table(areas.erDF)
areas.erDF[,num.row:=.N,by=group]
areas.erDF <- areas.erDF[num.row>6]
areas.erDF.row <- areas.erDF[,.N,by=id]
areas.erDF[,id:=rep(er.data[,ID],areas.erDF.row[,N])]

er.data <- geography[,.(unique(`ERname/REnom`)),by=`ERuid/REidu`]
names(er.data) <- c("ID","Name")
er.data[,ID:=as.character(ID)]
setkey(er.data,ID)
er.data[,long:=coordinates(areas.er.simp)[,1]]
er.data[,lat:=coordinates(areas.er.simp)[,2]]
er.data[,num.row:=areas.erDF[,.N,by=id][,2]]



#General testing
test.plot <- ggplot(data=areas.erDF) +
  brookfield.base.theme() +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_path(aes(x=long,y=lat,group=group),colour=set.colours(1,categorical.choice = "grey"),size=0.1) +
  coord_map("lambert",parameters=c(49,77),expand=FALSE)


