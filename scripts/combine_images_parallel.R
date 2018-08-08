### Read and combine images across lakes & bands - parallelized ###

## packages
library(raster)
library(doParallel)  #Foreach Parallel Adaptor 
library(foreach)     #Provides foreach looping construct

#Define how many cores you want to use
UseCores <- detectCores() - 1

#Register CoreCluster
cl       <- makeCluster(8, outfile="")
registerDoParallel(cl)

## paths and filenames
data.path <- '/data/output/'
lakes.path <- '/data/geotiffs/'
lakes.dirs <- list.files(path = lakes.path, full.names = F, recursive = F)
#print(length(lakes.dirs))
b.limit <- 9


processLake <- function(lake) {
  out <- tryCatch(
    {
      print(lake);
      curr.dir <- gsub(' ','',paste(lakes.path,lake))
      lake.files <- list.files(path = curr.dir, full.names = F, recursive = F); 
      lake.files <- lake.files[which(endsWith(lake.files,'tif'))];
      
      #print(curr.dir) 
      #print(lake.files);
     
      setwd(curr.dir);
      
      ## read and stack all bands for all images in dir
      b.stacks <- c();
      invisible(sapply(1:b.limit,function(b){ b.stacks <<- c(b.stacks,stack(lake.files,bands=c(b))) })); 
      # 
      # ## create super median image
      b.meds <- c();
      invisible(sapply(1:b.limit,function(b){ b.meds <<- c(b.meds,calc(b.stacks[[b]],median,na.rm=T))}));
      super.med.img <- stack(b.meds);
      # 
      # ## create super mean image
      b.means <- c();
      invisible(sapply(1:b.limit,function(b){ b.means <<- c(b.means,calc(b.stacks[[b]],mean,na.rm=T))}));
      super.mean.img <- stack(b.means);
      #
      # ## compute per-pixel variance
      super.stack <- stack(b.stacks)
      super.var.img <- calc(super.stack,var,na.rm=T)
      # 
      # ## RDS of super image and var image     

      saveRDS(super.med.img, file = gsub(' ','',paste(data.path,lake,'.rds')), ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL);
      saveRDS(super.mean.img, file = gsub(' ','',paste(data.path,lake,'_med.rds')), ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL);
      saveRDS(super.var.img, file = gsub(' ','',paste(data.path,lake,'_var.rds')), ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL);
    },
    error=function(cond) {
      message(cond)
      # Choose a return value in case of error
      return(NA)
    },
    warning=function(cond) {
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    },
    finally={
      message(paste("Processed lake:", lake)) 
    }
  )    
  return(out)
}


#Use foreach loop and %dopar% command
foreach(i=1:length(lakes.dirs),.packages='raster') %dopar% {
#foreach(i=:1,1.packages='raster') %dopar% {
  processLake(lakes.dirs[i]);
}

#EOF#
