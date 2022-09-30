
# =========== barro colorado island data from the vegan package ===========

setwd("C:/Users/roryj/Documents/PhD/202205_ucl_lassa/teaching/BIOS0002/workshop_bci/")

# dependencies
library(vegan)
library(sp)
library(ggplot2)
library(dplyr)
library(raster)

# all the detail
# https://rdrr.io/cran/vegan/man/BCI.html



# ------------- download data -------------------

# data from vegan package
data("BCI")
data("BCI.env")

# environmental data per site
env <- BCI.env %>% dplyr::mutate(site_id = 1:nrow(BCI.env)) 

# create lat-lon for plot centroids
points <- data.frame(x=env$UTM.EW, y=env$UTM.NS)
spts <- SpatialPoints(points, proj4string=CRS("+proj=utm +zone=17 +datum=WGS84")) 
spts <- spTransform(spts, CRS("+proj=longlat +datum=WGS84"))
env <- cbind(env, coordinates(spts))

# rank plot numbers by 1:5 and 1:10 on the X and Y axes
rpx = env %>% dplyr::select(UTM.EW) %>% distinct() %>% dplyr::arrange(UTM.EW) %>% dplyr::mutate(x_n = 1:10) 
rpy = env %>% dplyr::select(UTM.NS) %>% distinct() %>% dplyr::arrange(desc(UTM.NS)) %>% dplyr::mutate(y_n = 1:5) 
env = env %>%
  left_join(rpx) %>%
  left_join(rpy)

# bci tree community data
bci <- BCI %>%
  dplyr::mutate(site_id = 1:nrow(BCI)) %>%
  reshape2::melt(id.vars = "site_id") %>%
  dplyr::mutate(variable = unlist(lapply(strsplit(as.vector(variable), "[.]"), paste, collapse=" "))) %>%
  left_join(env) %>%
  dplyr::rename("species"=variable, "count"=value)

# save
write.csv(bci, "./data/bci_communities.csv", row.names=FALSE)


