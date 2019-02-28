### Libraries
library(readr)
library(raster)
library(reshape2)
library(scales)
library(ggplot2)
library(rgdal)
# library(hydrolinks)
library(data.table)
library(cluster)

### ENV
par(mar=c(2,2,2,2))

## set paths
data.path <- '~/projects/LakeAI/data'
figs.path <- '~/projects/LakeAI/figs/'
img.path <- '~/projects/LakeAI/data/processed//'

lakes.name <- list.files(path = img.path, full.names = F, recursive = F)
# lakes.name <- lakes.name[-grep('var',lakes.name)]
# lakes.name <- lakes.name[-grep('med',lakes.name)]
lakes.name <- gsub('.rds','',lakes.name)
lakes.name <- gsub('landsat','',lakes.name)

# color gradient object
rgb.trans <- div_gradient_pal(low="blue", mid="green", high="red", space = "Lab")
rgb.colrs <- rgb.trans(seq(0,1, length.out=200))

byb.trans <- div_gradient_pal(low="brown", mid="yellow", high="blue", space = "Lab")
byb.colrs <- byb.trans(seq(0,1, length.out=100))

b.trans <- div_gradient_pal(mid="blue", space = "Lab")
b.colrs <- b.trans(seq(0,1, length.out=100))

## read data
img.filenames <- list.files(path = img.path, full.names = TRUE, recursive = TRUE)

response_variables <- read_csv("~/projects/LakeAI/data/response_variables.csv")
rvars <- response_variables

# rds
lake.filenames <- img.filenames[grep('lake',img.filenames)]
watershed.filenames <- img.filenames[grep('watershed',img.filenames)]
# tif
img.filenames <- img.filenames[which(endsWith(img.filenames,'tif'))]

# drop var images
# img.filenames <- img.filenames[-grep('var',img.filenames)]

# drop mean images
img.filenames <- img.filenames[grep('med',img.filenames)]

# band names
band.names <- c("Ultra Blue","Blue","Green","Red","NIR","SWIR1","SWIR2","Kelvin1","Kelvin1")

# read scene rds
scene.imgs <- c()
invisible(
  sapply(1:length(img.filenames),function(i){
    img <- readRDS(file = img.filenames[i])
    names(img) <- band.names
    scene.imgs <<- c(scene.imgs,img)
  })
)

# label bands
invisible(
  sapply(1:length(img.filenames),function(i){
    names(scene.imgs[[i]]) <<- band.names
  })
)


# read lake rds
lake.imgs <- c()
invisible(
  sapply(1:length(lake.filenames),function(i){
    img <- readRDS(file = lake.filenames[i])
    names(img) <- band.names
    lake.imgs <<- c(lake.imgs,img)
  })
)

# label bands
invisible(
  sapply(1:length(lake.filenames),function(i){
    names(lake.imgs[[i]]) <<- band.names
  })
)

# read watershed rds
watershed.imgs <- c()
invisible(
  sapply(1:length(watershed.filenames),function(i){
    img <- readRDS(file = watershed.filenames[i])
    names(img) <- band.names
    watershed.imgs <<- c(watershed.imgs,img)
  })
)

# label bands
invisible(
  sapply(1:length(watershed.filenames),function(i){
    names(watershed.imgs[[i]]) <<- band.names
  })
)


# plot and save NDVI of lakes

# testing
img <- scene.imgs[[6]]
img <- lake.imgs[[63]]
img <- watershed.imgs[[5]]

# img <- watersheds.img.sub[166]

ndvi <- NDVI(img,5,4)
ndwi <- NDWI(img,5,6)

plot(ndvi,col=rgb.colrs)
plot(ndwi,col=byb.colrs)


lake.ndvi.mat <- getValues(ndvi)
summary(lake.ndvi.mat)

lake.min <- min(lake.ndvi.mat[!is.na(lake.ndvi.mat)])
lake.max <- max(lake.ndvi.mat[!is.na(lake.ndvi.mat)])
lake.sd <- sd(lake.ndvi.mat[!is.na(lake.ndvi.mat)])
lake.var <- var(lake.ndvi.mat[!is.na(lake.ndvi.mat)])
thresh <- ((lake.max-lake.min)/lake.sd)/3

# water <- calc(ndvi, function(x){x[x >= (lake.min+2.8*lake.sd)] <- NA; return(x)})
# veg <- calc(ndvi, function(x){x[x < (lake.min+2.8*lake.sd)] <- NA; return(x)})

water.pts <- rasterToPoints(ndvi, function(x) x < (lake.min+thresh*lake.sd))
veg.pts <- rasterToPoints(ndvi, function(x) x >= (lake.min+thresh*lake.sd))

water <- extract(ndvi,water.pts[,1:2])
veg <- extract(ndvi,veg.pts[,1:2])

mean(as.matrix(water)[!is.na(as.matrix(water))])
mean(as.matrix(veg)[!is.na(as.matrix(veg))])

####

lakes.ndvi.water <- c()
lakes.ndvi.veg <- c()

# lakes.ndvi0.water <- c()
# lakes.ndvi0.veg <- c()

lakes.evi2.water <- c()
lakes.evi2.veg <- c()

lakes.msavi2.water <- c()
lakes.msavi2.veg <- c()

lakes.ndwi.water <- c()
lakes.ndwi.veg <- c()


invisible(
  sapply(1:bound,
         function(i) {
           lake <- lake.imgs[[i]]
           
           ndvi <- NDVI(lake, 5, 4)
           evi2 <- EVI2(lake, 5, 4)
           msavi2 <- MSAVI2(lake, 5, 4)
           osavi <- OSAVI(lake, 5, 4)
           ndwi <- NDWI(lake, 5, 6)
           
           mat <- getValues(ndvi)
           max <- max(mat[!is.na(mat)])
           min <- min(mat[!is.na(mat)])
           sd <- sd(mat[!is.na(mat)])
           
           thresh <- ((max-min)/sd)/3
           
           # ndvi0.water <- calc(ndvi, function(x){x[x >= (min+2.8*sd)] <- NA; return(x)})
           # ndvi0.veg <- calc(ndvi, function(x){x[x < (min+2.8*sd)] <- NA; return(x)})
           
           if (!is.nan(sd)) {
             
            water.pts <- rasterToPoints(ndvi, function(x) x < (min+thresh*sd))
             veg.pts <- rasterToPoints(ndvi, function(x) x >= (min+thresh*sd))
             
             ndvi.water <- extract(ndvi,water.pts[,1:2])
             ndvi.veg <- extract(ndvi,veg.pts[,1:2])
             
             evi2.water <- extract(evi2,water.pts[,1:2])
             evi2.veg <- extract(evi2,veg.pts[,1:2])
  
             msavi2.water <- extract(msavi2,water.pts[,1:2])
             msavi2.veg <- extract(msavi2,veg.pts[,1:2])
  
             ndwi.water <- extract(ndwi,water.pts[,1:2])
             ndwi.veg <- extract(ndwi,veg.pts[,1:2])
             
             # par(mfrow=c(1,1),mar=c(2,2,2,2));
             # tiff(gsub(' ','',paste(figs.path,'all_lakes_ndvi/',substr(img.filenames[i],gregexpr('/',img.filenames[i])[[1]][length(gregexpr('/',img.filenames[i])[[1]])]+1,nchar(img.filenames[i])-4))), width = 10, height = 10, units = 'in',  res = 300)
             # plot(ndvi, col = rgb.colrs, main="NDVI Index");
             # dev.off()
             # 
             # par(mfrow=c(1,1),mar=c(2,2,2,2));
             # tiff(gsub(' ','',paste(figs.path,'all_lakes_water/',substr(img.filenames[i],gregexpr('/',img.filenames[i])[[1]][length(gregexpr('/',img.filenames[i])[[1]])]+1,nchar(img.filenames[i])-4)),'_water'), width = 10, height = 10, units = 'in',  res = 300)
             # plot(water, col = b.colrs, main = 'Water')
             # dev.off()
             # 
             # par(mfrow=c(1,1),mar=c(2,2,2,2));
             # tiff(gsub(' ','',paste(figs.path,'all_lakes_veg/',substr(img.filenames[i],gregexpr('/',img.filenames[i])[[1]][length(gregexpr('/',img.filenames[i])[[1]])]+1,nchar(img.filenames[i])-4)),'_veg'), width = 10, height = 10, units = 'in',  res = 300)
             # plot(veg, col = rg.colrs, main = 'Veg cover')
             # dev.off()
             
             lakes.ndvi.water <<- c(lakes.ndvi.water,mean(as.matrix(ndvi.water)[!is.na(as.matrix(ndvi.water))]))
             lakes.ndvi.veg <<- c(lakes.ndvi.veg,mean(as.matrix(ndvi.veg)[!is.na(as.matrix(ndvi.veg))]))
             
             lakes.evi2.water <<- c(lakes.evi2.water,mean(as.matrix(evi2.water)[!is.na(as.matrix(evi2.water))]))
             lakes.evi2.veg <<- c(lakes.evi2.veg,mean(as.matrix(evi2.veg)[!is.na(as.matrix(evi2.veg))]))
             
             lakes.msavi2.water <<- c(lakes.msavi2.water,mean(as.matrix(msavi2.water)[!is.na(as.matrix(msavi2.water))]))
             lakes.msavi2.veg <<- c(lakes.msavi2.veg,mean(as.matrix(msavi2.veg)[!is.na(as.matrix(msavi2.veg))]))
             
             lakes.ndwi.water <<- c(lakes.ndwi.water,mean(as.matrix(ndwi.water)[!is.na(as.matrix(ndwi.water))]))
             lakes.ndwi.veg <<- c(lakes.ndwi.veg,mean(as.matrix(ndwi.veg)[!is.na(as.matrix(ndwi.veg))]))  
           } else {
             lakes.ndvi.water <<- c(lakes.ndvi.water,NA)
             lakes.ndvi.veg <<- c(lakes.ndvi.veg,NA)
             
             lakes.evi2.water <<- c(lakes.evi2.water,NA)
             lakes.evi2.veg <<- c(lakes.evi2.veg,NA)
             
             lakes.msavi2.water <<- c(lakes.msavi2.water,NA)
             lakes.msavi2.veg <<- c(lakes.msavi2.veg,NA)
             
             lakes.ndwi.water <<- c(lakes.ndwi.water,NA)
             lakes.ndwi.veg <<- c(lakes.ndwi.veg,NA)
           }
           
           
           
           }))


## Consolidate


lakes.rs <- data.frame(lake=lakes.name, 
                       water_ndvi=lakes.ndvi.water, 
                       veg_ndvi=lakes.ndvi.veg, 
                       water_evi2=lakes.evi2.water, 
                       veg_evi2=lakes.evi2.veg, 
                       water_msavi2=lakes.msavi2.water, 
                       veg_msavi2=lakes.msavi2.veg, 
                       water_ndwi=lakes.ndwi.water, 
                       veg_ndwi=lakes.ndwi.veg)

lakes.rvars <- rvars[which(rvars$SITE_ID%in%as.character(lakes.rs$lake)),]
lakes.rvars <- rvars[which(rvars$SITE_ID%in%as.character(lakes.name$lake)),]

attach(lakes.rvars)
lakes.rvars <- aggregate.data.frame(lakes.rvars,by=list(SITE_ID),FUN = mean)
lakes.rvars <- lakes.rvars[,-2]
colnames(lakes.rvars)[1] <- 'SITE_ID'

# lakes.rvars <- lakes.rvars[-which(duplicated(lakes.rvars$SITE_ID)),]

lakes.rs <- lakes.rs[which(lakes.rs$lake%in%as.character(lakes.rvars$SITE_ID)),]

rownames(lakes.rs) <- 1:nrow(lakes.rs)
rownames(lakes.rvars) <- 1:nrow(lakes.rvars)

lakes.rs <- lakes.rs[order(lakes.rs$lake),]
lakes.rvars <- lakes.rvars[order(lakes.rvars$SITE_ID),]

lakes.all.cols <- cbind(lakes.rs,lakes.rvars[,2:8])
lakes.all.cols <- lakes.all.cols[-c(17,208),]

lakes.all.cols <- lakes.all.cols[which(complete.cases(lakes.all.cols)),]

summary(lakes.all.cols)

# lakes.all.cols$water_mean <- log2(abs(lakes.all.cols$water_mean))


## plot pairs
# lin-lin
tiff(gsub(' ','',paste(figs.path,'all_lakes_indices_measurements_lin_lin')), width = 10, height = 10, units = 'in',  res = 300)
pairs(lakes.all.cols[,2:ncol(lakes.all.cols)],upper.panel=panel.cor,diag.panel=panel.hist,main="All indices + in situ measurements (lin-lin) / 481 lakes")
dev.off()

# lin-log
lakes.all.cols.lin_log <- cbind(lakes.all.cols[,1:9],apply(lakes.all.cols[,10:ncol(lakes.all.cols)],2,log10))
tiff(gsub(' ','',paste(figs.path,'all_lakes_indices_measurements_lin_log')), width = 10, height = 10, units = 'in',  res = 300)
pairs(lakes.all.cols.lin_log[,2:ncol(lakes.all.cols.lin_log)],upper.panel=panel.cor,diag.panel=panel.hist,main="All indices + in situ measurements (lin-log) / 481 lakes")
dev.off()

# log-log
lakes.all.cols.log_log <- cbind(lakes.all.cols.lin_log[,1],apply(lakes.all.cols.lin_log[,2:3],2,log2),lakes.all.cols.lin_log[,4:ncol(lakes.all.cols.lin_log)])
tiff(gsub(' ','',paste(figs.path,'all_lakes_indices_measurements_log_log')), width = 10, height = 10, units = 'in',  res = 300)
pairs(lakes.all.cols.log_log[,2:ncol(lakes.all.cols.log_log)],upper.panel=panel.cor,diag.panel=panel.hist,main="All indices + in situ measurements (log-log) / 481 lakes")
dev.off()

# par(mfrow=c(1,1),mar=c(1,1,1,1));
# tiff(paste(figs.path,'test'), width = 10, height = 10, units = 'in',  res = 300)
# pairs(lakes.all.cols)
# dev.off()

# lakes.sample <- lakes.all.cols[sample(1:nrow(lakes.all.cols),100),]
# 
# pairs(lakes.sample[,2:ncol(lakes.sample)])
# hist(lakes.sample[,2:ncol(lakes.sample)])

lakes.all.cols.long <- melt(lakes.all.cols.lin_log)

ranges <- as.data.frame(invisible(apply(apply(na.omit(lakes.all.cols[,2:ncol(lakes.all.cols)]),2,range),2,function(var){print(var);})))

breaks <- invisible(apply(ranges,2,function(var){pretty(var, n = nclass.FD(var), min.n = 1);}))

bwidth <- sapply(breaks,function(el){el[2]-el[1]})

ggplot(lakes.all.cols.long,aes(x = value)) + 
  facet_wrap(~variable,scales = "free") + 
  geom_histogram()



## Unsupervised clustering

sil_width <- c()
x.dist <- daisy(as.data.frame(na.omit(lakes.all.cols)), metric = "gower")

invisible(sapply(2:9,function(i){pam_fit <- pam(x.dist,diss = TRUE,k = i); sil_width[i] <<- pam_fit$silinfo$avg.width; }))

plot(sil_width,xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(sil_width)

## Regression

lakes.all.lm <- lm(COND~water_ndvi+veg_ndvi+water_evi2+veg_evi2+water_msavi2+veg_msavi2+water_ndwi+veg_ndwi,data = lakes.all.cols.lin_log)
summary(lakes.all.lm)

lakes.all.lm <- lm(COND~water_msavi2,data = lakes.all.cols.lin_log)
summary(lakes.all.lm)

plot(lakes.all.lm)

#EOF#


