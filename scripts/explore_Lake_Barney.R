### Libraries
library(raster)
library(scales)
library(RStoolbox)
library(cluster)
library(ggplot2)

### ENV
par(mar=c(1.5,1.5,1.5,1.5))

## read data
figs.path <- '~/projects/LakeAI/figs/'
data.path <- '~/projects/LakeAI/data/'
img.path <- '~/projects/LakeAI/data/Lake_Landsat/landsatNLA12_WI-134/'
img.path <- '~/projects/LakeAI/data/Lake_Landsat/landsatNLA06608-0068/'
img.filenames <- list.files(path = img.path, full.names = TRUE, recursive = TRUE)
img.filenames <- img.filenames[which(endsWith(img.filenames,'tif'))]

### Constants & Functions

## compute veg index
VI <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}

## compute wc
WC <- function(img, b, r) {
  bb <- img[[b]]
  br <- img[[r]]
  wc <- bb / br
  return(wc)
}

## grayscale colors
grayscale_colors <- gray.colors(100,            # number of different color levels 
                                start = 0.0,    # how black (0) to go
                                end = 1.0,      # how white (1) to go
                                gamma = 2.2,    # correction between how a digital 
                                # camera sees the world and how human eyes see it
                                alpha = NULL)  

trans <- div_gradient_pal(low="blue", mid="green", high="red", space = "Lab")

colrs <- trans(seq(0,1, length.out=100))

### Exploring

## build image list
# img.filename = img.filenames[1]

# img.bands = c() 
# invisible(sapply(1:11,function(i){ img.bands <<- c(img.bands,raster(img.filename,band=i)) }))

# img.list <- c()
# invisible(sapply(1:length(img.filenames),function(i){ img.list <<- c(img.list,raster(img.filenames[i],band=1)) }))

## combine multiple images

## read and stack all bands for all images in dir
b.stacks <- c()
b.limit <- 9
invisible(sapply(1:b.limit,function(b){ b.stacks <<- c(b.stacks,stack(img.filenames,bands=c(b))) }))

# b1.stack <- stack(img.filenames,bands=c(1))
# b1.stk.mean <- calc(b1.stack,mean,na.rm=T)

## create super image
# b.meds <- c()
# invisible(sapply(1:b.limit,function(b){ b.meds <<- c(b.meds,calc(b.stacks[[b]],median,na.rm=T))}))
# super.img <- stack(b.meds)

# names(super.img) <- c("Ultra Blue","Blue","Green","Red","NIR","SWIR1","SWIR2","Kelvin1","Kelvin1","AerosolQA","PixelQA","RadsatQA")
# names(super.var) <- c("Ultra Blue","Blue","Green","Red","NIR","SWIR1","SWIR2","Kelvin1","Kelvin1")


## compute variance
super.stack <- stack(b.stacks)
super.var <- calc(super.stack,var,na.rm=T)
b.vars <- c()
invisible(sapply(1:b.limit,function(b){ b.vars <<- c(b.vars,calc(b.stacks[[b]],var,na.rm=T))}))
super.var <- stack(b.vars)


## compute median
super.med <- calc(super.stack,median,na.rm=T)

## RDS of super image
setwd('~/projects/LakeAI/data/R/')
saveRDS(super.img, file = paste(data.path,'Lake_Barney_super_img_medians.rds'), ascii = FALSE, version = NULL,        compress = TRUE, refhook = NULL)

super.img <- readRDS(file = paste(data.path,'Lake_Barney_super_img_medians.rds'))


# identify image to examine
test.img <- super.img
names(test.img) <- c("Ultra Blue","Blue","Green","Red","NIR","SWIR1","SWIR2","Kelvin1","Kelvin1")

par(mfrow=c(1,1),mar=c(3,5,3,3))
hist(test.img)
plot(test.img, col=colrs, axes=FALSE)

test.img.mat <- cbind(as.data.frame(as.vector(as.matrix(test.img[[4]]))),as.data.frame(as.vector(as.matrix(test.img[[3]]))),as.data.frame(as.vector(as.matrix(test.img[[2]]))),as.data.frame(as.vector(as.matrix(test.img[[5]]))))

colnames(test.img.mat)[1] <- 'Red'
colnames(test.img.mat)[2] <- 'Green'
colnames(test.img.mat)[3] <- 'Blue'
colnames(test.img.mat)[4] <- 'NIR'

ggplot() + 
  geom_histogram(aes(Red),test.img.mat,binwidth = 20,fill = "red", alpha = 0.7) +
  geom_histogram(aes(Green),test.img.mat,binwidth = 20,fill = "green", alpha = 0.7) +
  geom_histogram(aes(Blue),test.img.mat,binwidth = 20,fill = "blue", alpha = 0.7) +
  ggtitle("Lake Barney, WI - Landsat 8 - RGB bands") + xlab("Pixel Value") + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), plot.title=element_text(size=24,face="bold"))

par(mfrow=c(1,10),mar=c(1,1,1,1))
p <- ggplot();
for(i in 1:2) {
  f <- img.filenames[i]
  img.b <- raster(f,band=2);
  img.g <- raster(f,band=3);
  img.r <- raster(f,band=4);
  
  img.mat <- cbind(as.data.frame(as.vector(as.matrix(img.r))),as.data.frame(as.vector(as.matrix(img.g))),as.data.frame(as.vector(as.matrix(img.b))))
  
  colnames(img.mat)[1] <- 'Red'
  colnames(img.mat)[2] <- 'Green'
  colnames(img.mat)[3] <- 'Blue'
  
  p + 
    geom_histogram(aes(Red),img.mat,binwidth = 20,fill = "red", alpha = 0.7) +
    geom_histogram(aes(Green),img.mat,binwidth = 20,fill = "green", alpha = 0.7) +
    geom_histogram(aes(Blue),img.mat,binwidth = 20,fill = "blue", alpha = 0.7)
}

# par(mfrow=c(3,4),mar=c(1,1,1,1))
# invisible(sapply(1:length(b.means),function(b){ hist(b.means[[b]]); }))

# par(mfrow=c(3,4),mar=c(1,1,1,1))
# invisible(sapply(1:length(b.means),function(b){ plot(b.means[[b]], col=colrs, axes=FALSE); }))

## plot rgb
test.img.rgb <- test.img[[c(4,3,2)]]
par(mfrow=c(1,1))
plotRGB(test.img.rgb, r = 1, g = 2, b = 3, axes = TRUE, stretch = "lin", main="RGB Composite")

## analyze bands
tiff(paste(figs.path,' Lake_Barney_band_distribution_scatterplots.tiff'), width = 10, height = 10, units = 'in',  res = 300)
pairs(test.img[[1:b.limit]])
dev.off()

## vegetation indices
ndvi <- VI(test.img, 5, 4)
par(mfrow=c(1,1),mar=c(2,2,2,2))
plot(ndvi, col = colrs, main="NDVI Index")

hist(ndvi,main = "Distribution of NDVI values",xlab = "NDVI",ylab="Frequency",col = "lightgreen",xlim = c(-0.5, 1),breaks = 30, xaxt = 'n')
axis(side=1, at = seq(-0.5,1, 0.05), labels = seq(-0.5,1, 0.05))

## water clarity index?
wc <- WC(test.img, 2, 4)
par(mfrow=c(1,1),mar=c(2,2,2,2))
plot(wc, col = colrs, main="Blue:Red")

## thresholding
veg <- calc(ndvi, function(x){x[x < 0.4] <- NA; return(x)})
plot(veg, main = 'Veg cover (index >= 0.4)')

## peak 1
peak1 <- reclassify(ndvi, c(-Inf,0.53,NA,0.53,0.55,1,0.55,Inf,NA))
plot(peak1, main = 'peak 1')

## peak 2
peak2 <- reclassify(ndvi, c(-Inf,0.7,NA,0.7,0.72,1,0.72,Inf,NA))
plot(peak2, main = 'peak 2')

## multi thresholds
vegc <- reclassify(veg, c(-Inf,0.25,1, 0.25,0.3,2, 0.3,0.5,3, 0.5,0.7,4, 0.7,1,5, 1,Inf,6))
plot(vegc,col = colrs, main = 'NDVI based thresholding')

## PCA
test.img.pts <- as.matrix(test.img)

# test.img.pts <- test.img.pts[-which(is.nan(rowSums(test.img.pts))),]
test.img.pts <- sampleRandom(test.img,dim(test.img.pts)[1])

par(mfrow=c(1,1),mar=c(1,1,1,1))
plot(test.img.pts[,c(4,5)], main = "NIR-Red plot")

ggplot(test.img.mat, aes(x=Red, y=NIR)) +
  geom_point() +
  ggtitle("Lake Barney NIR-Red bands") +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"), plot.title=element_text(size=24,face="bold"))
  

set.seed(99)
test.img.pca <- prcomp(test.img.pts, scale = TRUE)

screeplot(test.img.pca, main = "PCs")

pci <- predict(test.img, test.img.pca, index = 1:2)

plot(pci[[1]], main = "")

pc2 <- reclassify(pci[[2]], c(-Inf,0,1,0,Inf,NA))
par(mfrow = c(1,2))
plotRGB(test.img.rgb, r = 1, g = 2, b = 3, axes = TRUE, stretch = "lin", main = "RGB Composite")
plotRGB(test.img.rgb, r = 1, g = 2, b = 3, axes = TRUE, stretch = "lin", main = "RGB Composite")
plot(pc2, legend = FALSE, add = TRUE)

### Classification
nr <- getValues(ndvi)
nr.nas <- is.na(nr)

sil_width <- c()
x.dist <- daisy(as.data.frame(na.omit(nr)), metric = "gower")
invisible(sapply(2:9,function(i){pam_fit <- pam(x.dist,diss = TRUE,k = i); sil_width[i] <<- pam_fit$silinfo$avg.width; }))

plot(sil_width,xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(sil_width)

set.seed(99)
k <- 4
test.img.km <- kmeans(na.omit(nr), centers = k, iter.max = 500, nstart = 25, algorithm="Lloyd")

knr <- ndvi
knr[which(!nr.nas)] <- test.img.km$cluster

par(mfrow = c(1,2))
plot(ndvi, col = colrs, main = 'Lake Barney NDVI')
plot(knr, main = paste('Kmeans clustering k =',k), col = colrs )

## EOF ##