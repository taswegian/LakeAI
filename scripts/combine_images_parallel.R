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
#list dirs in destination dir
done.files <- list.files(path = out.path, full.names = F, recursive = F); 
#print(length(lakes.dirs))
band.limit <- 9


processScene <- function(scene.dir) {
  out <- tryCatch(
    {
      print(scene.dir);
      curr.dir <- gsub(' ','',paste(scene.path,scene.dir))
      
      #list files in current dir
      scene.files <- list.files(path = curr.dir, full.names = F, recursive = F); 
      #only tifs
      scene.files <- scene.files[which(endsWith(scene.files,'tif'))];    
   
      #print(paste(scene.dir,'_med.rds'))
      #print(done.files) 
      if (!gsub(' ','',paste(scene.dir,'_med.rds'))%in%done.files&length(scene.files)>5) { 
      
      print(paste('processing:~',scene.dir,length(scene.files),'files')) 
      #print(scene.files;)
      #print(length(scene.files));
      setwd(curr.dir);

      l <- c()
      invisible(sapply(1:length(scene.files),function(j){l <<- c(l,raster(scene.files[[j]]))}));
      lake.projs <- substr(lapply(l,function(i){return(i@crs@projargs)}),17,19) 

      if (length(unique(lake.projs))>1) {
	#print('#multi projs');
	scene.files1 <- scene.files[which(lake.projs==unique(lake.projs)[1])]
        scene.files2 <- scene.files[which(lake.projs==unique(lake.projs)[2])]

	#print(lake.projs);	
	#print(paste('proj set sizes',length(scene.files1),length(scene.files2)));

        #scene.files <- ifelse(length(scene.files1)>length(scene.files2),scene.files1,scene.files2)
	
	if (length(scene.files1)>length(scene.files2)) { scene.files <- scene.files1 } else { scene.files <- scene.files2 };
        print(paste('actually',length(scene.files),'files'))
      }
   
      ## read and stack all bands for all images in dir
      band.stacks <- c();
      invisible(sapply(1:band.limit,function(b){ band.stacks <<- c(band.stacks,stack(scene.files,bands=c(b))) })); 
      super.stack <- stack(band.stacks)
      #print('stacked');
      # 
      # ## create super median image
      band.meds <- c();
      invisible(sapply(1:band.limit,function(b){ band.meds <<- c(band.meds,calc(band.stacks[[b]],median,na.rm=T))}));
      super.med.img <- stack(band.meds);
      #super.med.img <- calc(super.stack,median,na.rm=T)
      #print('computed  median');
      # 
      # ## create super mean image
      band.means <- c();
      invisible(sapply(1:band.limit,function(b){ band.means <<- c(band.means,calc(band.stacks[[b]],mean,na.rm=T))}));
      super.mean.img <- stack(band.means);
      #super.mean.img <- calc(super.stack,mean,na.rm=T)
      #
      # ## create super variance image
      band.vars <- c();
      invisible(sapply(1:band.limit,function(b){ band.vars <<- c(band.vars,calc(band.stacks[[b]],var,na.rm=T))}));
      super.var.img <- stack(band.vars);

      #super.var.img <- calc(super.stack,var,na.rm=T)
      # 
      # ## RDS of super images

      saveRDS(super.med.img, file = gsub(' ','',paste(out.path,scene.dir,'_med.rds')), ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL);
      saveRDS(super.mean.img, file = gsub(' ','',paste(out.path,scene.dir,'_mean.rds')), ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL);
      saveRDS(super.var.img, file = gsub(' ','',paste(out.path,scene.dir,'_var.rds')), ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL);
      print(paste("Processed lake:", scene.dir)) 
      }
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
      #message(paste("Processed lake:", scene.dir)) 
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
