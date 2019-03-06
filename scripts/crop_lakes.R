library(rjson)
library(rgdal)
library(raster)
library(doParallel)  #Foreach Parallel Adaptor
library(foreach)     #Provides foreach looping construct

#Define how many cores you want to use
UseCores <- detectCores()

#Register CoreCluster
cl <- makeCluster(UseCores-1, outfile="")

registerDoParallel(cl)

## paths
#setwd('/')

in.path <- '/data/datasets/processed/rds/median'
shapes.path <- '/data/datasets'
out.path <- '/data/datasets/output'

# band names
band.names <- c("Ultra Blue","Blue","Green","Red","NIR","SWIR1","SWIR2","Kelvin1","Kelvin1")

## scene files
scene.files <- list.files(path = in.path, full.names = T, recursive = F)

## read shapefiles from rds
file_js <- readRDS(file = gsub(' ','',paste(shapes.path,'/lake_shapes.rds')))

## list lake ids
lake.ids <- c()
invisible(lapply(file_js$features,function(x){lake.ids<<-c(lake.ids,x$properties$SITE_ID)}))
lake.ids <- gsub(':','_',lake.ids)

lake.names <- list.files(path = in.path, full.names = F, recursive = F)
lake.names <- gsub('_med.rds','',lake.names)
lake.names <- gsub('landsat','',lake.names)

print(paste(lake.names[1],lake.ids[1]))

missing.ids <- lake.names[which(!lake.names%in%lake.ids)]
print(paste(length(missing.ids),' missing'));
# write(missing.ids,'~/projects/LakeAI/data/missing_ids.csv')

### crop lakes
#lakes <- c()
#watersheds <- c()
lakes.cropped <- c()

cropLake <- function(i) {
  # out <- tryCatch(
  # {
  
  lakeId <- lake.names[i];
  
  #if (i==512) {i=513}
  #else if (i==513) {i=514}
  #else if (i==514) {i=512};
  
  #print(scene.files[i]);
  lakeScene <- readRDS(file = scene.files[i]);
  
  ## get scene
  scene <- lakeScene;
  
  ## locate lake polygon
  index <- which(lake.ids==lakeId);
  
  if (length(index)>0)
  {
    lakes.cropped <<- c(lakes.cropped,lakeId)
    
    ploygon <- toJSON(file_js$features[[index]]);
    
    ## prepare spatial feature
    sp <- readOGR(toJSON(file_js$features[[index]]), "OGRGeoJSON", verbose = F)
    sp <- spTransform(sp, crs(scene))
    
    ## crop lake
    lake <- mask(scene, sp)
    lake.img <- crop(mask(scene,sp), extent(sp))
    watershed.img <- mask(scene, sp, inverse=T)
    
    ## RDS of lake & watershed
    saveRDS(lake.img, file = gsub(' ','',paste(out.path,'/',lakeId,'_lake.rds')), ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL);
    saveRDS(watershed.img, file = gsub(' ','',paste(out.path,'/',lakeId,'_watershed.rds')), ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL);
    print(paste("cropped lake:", lakeId)); 
    # plot(NDVI(lake,5,4),col=rgb.colrs)
    # plot(NDVI(watershed,5,4),col=rgb.colrs)
  }
  # },
  # error=function(cond) {
  #   message(paste('err:',cond))
  #   # Choose a return value in case of error
  #   return(NA)
  # },
  # warning=function(cond) {
  #   message(paste('warn:',cond))
  #   # Choose a return value in case of warning
  #   return(NULL)
  # },
  # finally={ 
  #   message(paste("\ncropped lake:", lakeId)) 
  # }
  # )    
  # return(out)
}




print('Cropping...');

# #Use foreach loop and %dopar% command
# foreach(i=1:length(lakes.img),.packages='raster') %dopar% {
#   #foreach(i=:1,1.packages='raster') %dopar% {
#   cropLake(lakes.img[i],lakes.name[i]);
# }

for (i in 1:length(scene.files)) {
  cropLake(i);
}
