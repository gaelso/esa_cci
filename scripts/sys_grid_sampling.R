datadir <- "/home/dannunzio/kenya/data/"
setwd(datadir)

map_18_file    <- paste0(datadir,"maps/lc_2018.tif")

#################### CREATE MAP START DATE
system(sprintf("gdal_calc.py -A %s --co COMPRESS=LZW --outfile=%s --calc=\"%s\"",
               map_18_file,
               paste0(datadir,"maps/mask.tif"),
               paste0("(A>0)*1+(A==0)*0")
))


system(sprintf("gdal_polygonize.py -nomask -f \"geoJSON\" %s %s",
               paste0(datadir,"maps/mask.tif"),
               paste0(datadir,"maps/mask.geojson")
))

aoi_bb <- readOGR(paste0(datadir,"maps/mask.geojson"))
aoi <- aoi_bb[aoi_bb@data$DN ==1,]
plot(aoi)

(bb    <- extent(aoi))

### What grid size do we need ? 
grid_size <- 2000          ## in meters

### GENERATE A GRID
sqr_df <- generate_grid(aoi,grid_size)

nrow(sqr_df)
utm_proj <- "+proj=utm +zone=37 +south +a=6378249.145 +rf=293.465 +units=m +no_defs +type=crs"
proj4string(sqr_df) <- proj4string(aoi) <- utm_proj

### Select a vector from location of another vector
sqr_df_selected <- sqr_df[aoi,]
nrow(sqr_df_selected)

### Give the output a decent name, with unique ID
names(sqr_df_selected@data) <- "tileID" 
sqr_df_selected@data$tileID <- row(sqr_df_selected@data)[,1]

tiles <- sqr_df_selected

## select one point per tile
spts   <- lapply(1:nrow(tiles),function(x){spsample(tiles[x,],1,"random")})
coords <- data.frame(t(sapply(1:length(spts),function(x){spts[[x]]@coords})))
coords$pointID <- row(coords)[,1]
head(coords)

spdf   <- SpatialPointsDataFrame(coords = coords[,c("X1","X2")],
                                 coords,
                                 proj4string = CRS(utm_proj))

spdf$tileID <- over(spdf,tiles)$tileID

map_02_file    <- "maps/lc_2002.tif"
map_06_file    <- "maps/lc_2006.tif"
map_10_file    <- "maps/lc_2010.tif"
map_14_file    <- "maps/lc_2014.tif"
map_18_file    <- "maps/lc_2018.tif"


spdf$lc_2018 <- raster::extract(raster(map_18_file),spdf)
spdf$lc_2014 <- raster::extract(raster(map_14_file),spdf)
spdf$lc_2010 <- raster::extract(raster(map_10_file),spdf)
spdf$lc_2006 <- raster::extract(raster(map_06_file),spdf)
spdf$lc_2002 <- raster::extract(raster(map_02_file),spdf)
spdf$esa2020 <- raster::extract(raster(esa_20_file),spdf)

table(spdf$lc_2018)

writeOGR(tiles,
         paste0(datadir,"grid/tiles.gpkg"),
         layer = "tiles",
         driver = "GPKG",
         overwrite_layer = T)

writeOGR(spdf, 
         paste0(datadir,"grid/points_grid.gpkg"),
         layer = "points_grid",
         driver = "GPKG",
         overwrite_layer = T)

table(spdf$lc_2014,spdf$lc_2018)
table(spdf$lc_2014,spdf$esa2020)


head(spdf)
cedb <- read_xlsx(paste0(datadir,"ceo_db/Verification_Sheet2014_2018_verNov2019_Final.xlsx"),sheet="Collect_Earth")
cedb <- cedb[duplicated(cedb$Point_ID) == F,]

spce   <- SpatialPointsDataFrame(coords = cedb[,c("location_x","location_y")],
                                 cedb,
                                 proj4string = CRS("+init=epsg:4326"))

writeOGR(spce, paste0(datadir,"ceo_db/ce_1418_geo.gpkg"),layer = "ce_1418_geo",driver = "GPKG",overwrite_layer = T)
