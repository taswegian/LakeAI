library(raster)

setwd('/')

in.path <- '/data/datasets/processed';
out.path <- '/data/datasets/output';

## read image rds
all.files <- list.files(path = in.path, full.names = T, recursive = F)
#lake.files <- all.files[-grep('watershed',all.files)]
#watershed.files <- all.files[-grep('lake',all.files)]

all.files <- all.files[grep('med',all.files)]

lakes.name <- list.files(path = in.path, full.names = F, recursive = F)
lakes.name <- lakes.name[grep('med',lakes.name)]
lakes.name <- gsub('_med.rds','',lakes.name)

bound <- length(all.files);

print('converting to geotiffs...');

for (i in 1:bound) {
  #print(all.files[i]);
  
  #lake <- readRDS(lake.files[i])
  #watershed <- readRDS(watershed.files[i])
  scene <- readRDS(all.files[i]);

  ## save as .tif
  filename <- gsub(' ','',paste(out.path,'/',lakes.name[i],'.tif'))
  print(filename)
  #writeRaster(lake, filename=gsub(' ','',paste(out.path,'/',lakes.name[i],'_lake.tif')), format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
  #writeRaster(watershed, filename=gsub(' ','',paste(out.path,'/',lakes.name[i],'_watershed_mat.tif')), format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
   writeRaster(scene, filename=filename, format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW")) 

  print(paste(lakes.name[i],' stored'));
}

#EOF#
