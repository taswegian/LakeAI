### Read and combine images across lakes & bands - parallelized ###

## libraries
library(raster)
library(doParallel)  #Foreach Parallel Adaptor 
library(foreach)     #Provides foreach looping construct

#Define how many cores you want to use
UseCores <- detectCores()

#Register CoreCluster
cl <- makeCluster(UseCores, outfile="")
registerDoParallel(cl)

## vars
scene.path <- '/data/datasets/Lake_Landsat/'
out.path <- '/data/datasets/output/'
scene.dirs <- list.files(path = scene.path, full.names = F, recursive = F)
#print(length(lakes.dirs))
band.limit <- 9


processScene <- function(scene.dir) {
  out <- tryCatch(
    {
      #print(scene.dir);
      curr.dir <- gsub(' ','',paste(scene.path,scene.dir))
      
      #list file in current dir
      scene.files <- list.files(path = curr.dir, full.names = F, recursive = F); 
      #only tifs
      scene.files <- scene.files[which(endsWith(scene.files,'tif'))];
      
      #print(curr.dir) 
      #print(scene.files);
      print(length(scene.files));
      setwd(curr.dir);
      
      ## read and stack all bands for all images in dir
      band.stacks <- c();
      invisible(sapply(1:band.limit,function(b){ band.stacks <<- c(band.stacks,stack(scene.files,bands=c(b))) })); 
      super.stack <- stack(band.stacks)
      print('stacked');
      # 
      # ## create super median image
      #band.meds <- c();
      #invisible(sapply(1:band.limit,function(b){ band.meds <<- c(band.meds,calc(band.stacks[[b]],median,na.rm=T))}));
      #super.med.img <- stack(band.meds);
      super.med.img <- calc(super.stack,median,na.rm=T)
      print('computed  median');
      # 
      # ## create super mean image
      #band.means <- c();
      #invisible(sapply(1:band.limit,function(b){ band.means <<- c(band.means,calc(band.stacks[[b]],mean,na.rm=T))}));
      #super.mean.img <- stack(band.means);
      super.mean.img <- calc(super.stack,mean,na.rm=T)
      #
      # ## create super variance image
      super.var.img <- calc(super.stack,var,na.rm=T)
      # 
      # ## RDS of super images

      saveRDS(super.med.img, file = gsub(' ','',paste(out.path,scene.dir,'_med.rds')), ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL);
      saveRDS(super.mean.img, file = gsub(' ','',paste(out.path,scene.dir,'_mean.rds')), ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL);
      saveRDS(super.var.img, file = gsub(' ','',paste(out.path,scene.dir,'_var.rds')), ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL);
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
      message(paste("Processed lake:", scene.dir)) 
    }
  )    
  return(out)
}


#Use foreach loop and %dopar% command
foreach(i=1:length(scene.dirs),.packages='raster') %dopar% {
#foreach(i=:1,1.packages='raster') %dopar% {
  processScene(scene.dirs[i]);
}

#EOF#
