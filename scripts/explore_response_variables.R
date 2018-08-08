library(readr)
library(reshape2)
library(ggplot2)
library(cluster)
library(mclust)

### Data
response_variables <- read_csv("~/projects/LakeAI/data/response_variables.csv")

rvars <- response_variables

## Distributions
keep.cols <- c(2:8,12:14)
  
ggplot(melt(rvars[,keep.cols]), aes(value)) + geom_histogram() + facet_wrap( ~ variable, scales = "free")

# helper
not.naorinf <- function(df,cols){return(which(!(is.na(rowSums(df[,cols]))|is.infinite(rowSums(df[,cols])))))}

## PCA
rvars.pca <- prcomp(~., data=rvars[not.naorinf(rvars,keep.cols),keep.cols], center = TRUE, scale = TRUE, na.action = na.omit)

autoplot(rvars.pca,loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 5)

rvars.pca$rotation

## Clustering

# dist mat
rvars.dist <- daisy(rvars[not.naorinf(rvars,keep.cols),keep.cols], metric = "gower")

# PAM
sil_width <- c()

invisible(sapply(2:5,function(i){pam_fit <- pam(rvars.dist,diss = TRUE,k = i); sil_width[i] <<- pam_fit$silinfo$avg.width; }))

plot(sil_width,xlab = "Number of clusters",
     ylab = "Silhouette Width")

lines(sil_width)

fit <- pam(rvars.dist,diss = TRUE,k = 2)

# HCLUST
fit <- hclust(rvars.dist, method="ward.D")
plot(fit)
groups <- cutree(fit, k=2)
rect.hclust(fit, k=2, border="red")

# MCLUST
fit <- Mclust(rvars[not.naorinf(rvars,keep.cols),keep.cols])
plot(fit)
summary(fit)

# plot clusts
clusplot(rvars[not.naorinf(rvars,keep.cols),keep.cols], fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
