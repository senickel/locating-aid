library(countrycode)
library(sp)
library(maptools)
path<-"" #your local file path where the GADM data will be stored
aiddata<-"path/level_1a.csv" #path to your data
wb<-read.csv(aiddata,colClasses="character",encoding="UTF-8") #loading data

wb<-wb[which(wb[,"precision_code"]<4),] #only precision code 3 and smaller
reg<-strsplit(wb[,"gazetteer_adm_name"],"|",fixed=TRUE) #split the location
rg<-NULL
for (i in 1:length(reg)) rg<-c(rg,reg[[i]][3]) #get the name of the country
rg<-cbind(rg,countrycode(rg,"country.name","iso3c")) #convert countrynames in iso3 code
rg[is.na(rg[,2]),2]<-"XKO" #kosovo could not be transformed in ISO3
### downloading 2nd adm
url1<-"http://biogeo.ucdavis.edu/data/gadm2.8/shp/"
url2<-"_adm_shp.zip"
url<-cbind(rg[,2],paste(url1,rg[,2],url2,sep="")) 
for (i in 1:nrow(url)) {
  temp <- tempfile()
  download.file(url[i,2],temp)
  unzip(temp,exdir=paste(path,
                         url[i,1],sep=""))
  unlink(temp)
}
####

#convert the longitude and latitude of the projects to spatial points
t3<-wb[,c("longitude","latitude")]
coords <- as.data.frame(cbind(as.numeric(t3[,1]),as.numeric(t3[,2])))
points <- SpatialPoints(coords)
p3<-"_adm2.shp"
rgnew<-data.frame(rg,NA,NA,NA)

##the following code is to keep track of the progress of the operation
lol <- nrow(rgnew) 
pb <- txtProgressBar(1, lol, style=3)
TIME <- Sys.time()
for (i in 1:lol) {
  pa<-paste(path,rgnew[i,2],"/",rgnew[i,2],p3,sep="") #filepath for the first dataset
  if (file.exists(pa)==FALSE) next #if the file doesnt exist, the loop jumps to the next iteration
  t<-readShapePoly(pa) #loading the shape file
  x<-over(points[i,],t) #checking in which administrative area the point is in 
  rgnew[i,3]<-x$ID_0 #number for country
  rgnew[i,4]<-x$ID_1 #number for first administrative level in country
  rgnew[i,5]<-x$ID_2 #number for second administrative level in country
  setTxtProgressBar(pb,i)
}
Sys.time()-TIME
wb[,"location"]<-paste(rgnew[,3],rgnew[,4],rgnew[,5],sep="-")
