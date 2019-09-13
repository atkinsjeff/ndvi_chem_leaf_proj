# working with drone data

require(raster)
require(sp)
require(rgdal)
require(viridis)


nir <- raster("D:/burnplots_drone_data/BurnPlots_2018_Multispec_Merge_New_transparent_mosaic_nir.tif")

red <- raster("D:/burnplots_drone_data/BurnPlots_2018_Multispec_Merge_New_transparent_mosaic_red.tif")

x11()
plot(red)

ndvi <- (nir - red) / (nir + red)

x11()
plot(ndvi, col = rev(viridis(32)),
     main = "UMBS Burn Plots")

#old model
#map.n <- (2.8574 * ndvi) + 1.0467
map.n <- (5.661 * ndvi) + 1.64

map.n[map.n < 0] <- NA

x11()
plot(map.n, col = rev(viridis(32)),
     main = "UMBS Burn Plots - Nitrogen")


# trying to extract points
points <- read.csv("./data/burn_plot_coords.csv")

x11()
plot(ndvi, col = rev(viridis(32)),
     main = "UMBS Burn Plots")
points(points$easting, points$northing, pch=0, cex = 2 )
text(points$easting, points$northing, labels = points$plot, pos = 4, offset = 1) # add labels


r <- raster(nrows=4, ncols=4)
r <- setValues(r, 1:ncell(r))
plot(r)
text(r)

plot(r)
text(r, halo=TRUE, hc='blue', col='white', hw=0.2)

# lets extract some gosh dang values
## create SPDF: SpatialPointsDataFrame()
# specify the northing (columns 4) & easting (column 3) in order
# specify CRS proj4string: borrow CRS from chm 
# specify raster
centroid_spdf = SpatialPointsDataFrame(points[,4:5], proj4string = ndvi@crs, points)


# extract circular, 5m buffer
cent_max.5 <- extract(ndvi,             # raster layer
                       centroid_spdf,   # SPDF with centroids for buffer
                       buffer = 5,     # buffer size, units depend on CRS
                       fun=max,         # what to value to extract
                       df=TRUE)         # return a dataframe? 

# view
cent_max.5

# grab the names of the plots from the centroid_spdf
cent_max.5$plot_id <- centroid_spdf$plot

# extract circular, 10m buffer
cent_max.10 <- extract(ndvi,             # raster layer
                       centroid_spdf,   # SPDF with centroids for buffer
                       buffer = 10,     # buffer size, units depend on CRS
                       fun=max,         # what to value to extract
                       df=TRUE)         # return a dataframe? 

# view
cent_max.10

# grab the names of the plots from the centroid_spdf
cent_max.10$plot_id <- centroid_spdf$plot

# extract circular, 20m buffer
cent_max.20 <- extract(ndvi,             # raster layer
                       centroid_spdf,   # SPDF with centroids for buffer
                       buffer = 20,     # buffer size, units depend on CRS
                       fun=max,         # what to value to extract
                       df=TRUE)         # return a dataframe? 

# view
cent_max.20

# grab the names of the plots from the centroid_spdf
cent_max.20$plot_id <- centroid_spdf$plot


# extract circular, 20m buffer
cent_max.20 <- extract(ndvi,             # raster layer
                    centroid_spdf,   # SPDF with centroids for buffer
                    buffer = 20,     # buffer size, units depend on CRS
                    fun=max,         # what to value to extract
                    df=TRUE)         # return a dataframe? 

# view
cent_max.20

# grab the names of the plots from the centroid_spdf
cent_max.20$plot_id <- centroid_spdf$plot


#### add buffer size
cent_max.5$buffer <- 5
cent_max.10$buffer <- 10
cent_max.20$buffer <- 20

#bind!
ndvi_drone_points <- rbind(cent_max.5, cent_max.10, cent_max.20)

#write.csv(ndvi_drone_points, "./data/drone_ndvi.csv")

