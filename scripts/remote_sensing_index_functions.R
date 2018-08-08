### Remote Sensing Indeces

## OLI2/4
OLI24 <- function(img, b, r) {
  BLUE <- img[[b]]
  RED <- img[[r]]
  oli24 <- BLUE / RED
  return(oli24)
}


## NDVI
NDVI <- function(img, n, r) {
  NIR <- img[[n]]
  RED <- img[[r]]
  ndvi <- (NIR - RED) / (NIR + RED)
  return(ndvi)
}

## EVI
EVI <- function(img, n, r, b) {
  NIR <- img[[n]]
  RED <- img[[r]]
  BLUE <- img[[b]]
  evi <- 2.5 * ((NIR - RED) / (NIR + C1 * RED - C2 * BLUE + L))
  return(evi)
}

## EVI2
EVI2 <- function(img, n, r) {
  NIR <- img[[n]]
  RED <- img[[r]]
  evi2 <- 2.5 * ((NIR - RED) / (NIR + (2.4 * RED)  + 1))
  return(evi2)
}

## MSAVI2
MSAVI2 <- function(img, n, r) {
  NIR <- img[[n]]
  RED <- img[[r]]
  msavi2 <- (2 * NIR + 1 - sqrt((2 * NIR + 1)^2 - 8 * (NIR - RED))) / 2
  return(msavi2)
}

## SAVI
SAVI <- function(img, n, r) {
  NIR <- img[[n]]
  RED <- img[[r]]
  savi <- ((NIR - RED) / (NIR + RED + L)) * (1 + L)
  return(savi)
}

## OSAVI
OSAVI <- function(img, n, r) {
  NIR <- img[[n]]
  RED <- img[[r]]
  osavi <- ((NIR - RED) / (NIR + RED + 0.16))
  return(osavi)
}

## SATVI
SATVI <- function(img, r, s1, s2) {
  RED <- img[[r]]
  SWIR1 <- img[[s1]]
  SWIR2 <- img[[s2]]
  satvi <- ((SWIR1 - RED) / (SWIR1 + RED + L)) * (1 + L) - (SWIR2 / 2)
  return(satvi)
}

## NDWI
NDWI <- function(img, n, s) {
  NIR <- img[[n]]
  SWIR <- img[[s]]
  ndwi <- ((NIR - SWIR) / (NIR + SWIR))
  return(ndwi)
}

## SATVI
SATVI <- function(img, r, s1, s2) {
  RED <- img[[r]]
  SWIR1 <- img[[s1]]
  SWIR2 <- img[[s2]]
  satvi <- ((SWIR1 - RED) / (SWIR1 + RED + L)) * (1 + L) - (SWIR2 / 2)
  return(satvi)
}