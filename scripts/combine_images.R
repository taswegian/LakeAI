### Read and combine images across lakes & bands ###

## packages
library(raster)

setwd('~/projects/LakeAI/data/')

## paths and filenames
# data.path <- '/mnt/volume_nyc3_01/lake_imgs/'
data.path <- '~/projects/LakeAI/data/'
# lakes.path <- '/mnt/volume_nyc3_01/orig_tiffs'
lakes.path <- '~/projects/LakeAI/data/Lake_Landsat/'
lakes.dirs <- list.files(path = lakes.path, full.names = F, recursive = F)
lakes.name <- readRDS('~/projects/LakeAI/data/lake_names.rds')
b.limit <- 9

viable <- c()
count <- c()
invisible(
  sapply(1:length(lakes.dirs),function(i){ 
    curr.dir <- gsub(' ','',paste(lakes.path,'/',lakes.dirs[i]))
    lake.files <- list.files(path = curr.dir, full.names = F, recursive = F); 
    lake.files <- lake.files[which(endsWith(lake.files,'tif'))];
    if (length(lake.files)<3) {
      viable <<- c(viable,gsub('landsat','',lakes.dirs[i]))
      count <<- c(count,length(lake.files))
      
    }
  })
)

viable.lakes <- data.frame(lake=viable,img_count=count)
viable.lakes$lake <- as.character(viable.lakes$lake)

new.lakes <- viable.lakes$lake[which(!viable.lakes$lake %in% lakes.name)]

invisible(
sapply(1:length(lakes.dirs),function(i){ 
  curr.dir <- gsub(' ','',paste(lakes.path,'/',lakes.dirs[i]))
  lake.files <- list.files(path = curr.dir, full.names = F, recursive = F); 
  lake.files <- lake.files[which(endsWith(lake.files,'tif'))];
  if (length(lake.files)>1 && lakes.dirs[i] %in% new.lakes) {
    setwd(curr.dir)
    ## read and stack all bands for all images in dir
    b.stacks <- c();
    invisible(sapply(1:b.limit,function(b){ b.stacks <<- c(b.stacks,stack(lake.files,bands=c(b))) }));
    # 
    # ## create super image
    b.meds <- c();
    invisible(sapply(1:b.limit,function(b){ b.meds <<- c(b.meds,calc(b.stacks[[b]],mean,na.rm=T))}));
    super.img <- stack(b.meds);
    #
    # ## compute per-pixel variance
    super.stack <- stack(b.stacks)
    super.var <- calc(super.stack,var,na.rm=T)
    # 
    # ## RDS of super image and var image
    saveRDS(super.img, file = gsub(' ','',paste(data.path,lakes.dirs[i],'.rds')), ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL);
    saveRDS(super.var, file = gsub(' ','',paste(data.path,lakes.dirs[i],'_var.rds')), ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL);
  }
})
)



#EOF#