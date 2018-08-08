### Read and combine images across lakes & bands ###

## packages
library(raster)

## paths and filenames
data.path <- '/mnt/volume_nyc3_01/lake_imgs/'
lakes.path <- '/mnt/volume_nyc3_01/orig_tiffs'
lakes.dirs <- list.files(path = lakes.path, full.names = F, recursive = F)
b.limit <- 9


invisible(
sapply(1:length(lakes.dirs),function(i){ 
  curr.dir <- gsub(' ','',paste(lakes.path,'/',lakes.dirs[i]))
  lake.files <- list.files(path = curr.dir, full.names = F, recursive = F); 
  lake.files <- lake.files[which(endsWith(lake.files,'tif'))];
  if (length(lake.files)>1) {
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