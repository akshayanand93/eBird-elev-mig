library(raster)
library(sf)
library(gdalUtils)
library(rgdal)
#load in list of raster files ending with "_dem.tif" and store in an object
list_of_files <- list.files(path = "C:/Users/Akshay/OneDrive - University of Florida/Documents/WG-elev-migration/ASTER-GDEM", 
                            pattern = "_dem.tif$")
# Method 1 using gdal. Didn't work. error in the location of gdal_translate.exe
WG <- read_sf("C:/Users/Akshay/OneDrive - University of Florida/Documents/WG-elev-migration/shapefiles/WG.shp")
e <- extent(WG)
template <- raster(e)
projection(template) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
writeRaster(template, file="ASTER-GDEM-WG.tif", format="GTiff", overwrite = TRUE)
mosaic_rasters(gdalfile = list_of_files , dst_dataset="ASTER-GDEM-WG.tif",of="GTiff")
gdalinfo("ASTER-GDEM-WG.tif")

#method 2 using raster. worked. but slower.
raster_list <- lapply(list_of_files, raster)
r <- do.call(raster::merge, raster_list)
writeRaster(r, file = "aster-gdem-wg.tiff", format = "GTiff")
