## Calculation chain to download ESA CCI land cover maps and derive deforestation rate
## Gael Sola, 2022 
## Expanded on Remi D'Anunzio original script from: 
## https://github.com/lecrabe/esa_cci

## Steps:
## + Get ESA CCI images
## + Get AOI boundaries shapefile
## + Prepare change combinations
## + Calc stats
## + Make a sampling grid


## USER INPUTS
wd        <- "auto" ## Choose "auto" if in a Rproject or "default" to get to default machine dir (/home or C:/Users/XXX/Documents)
prj_name  <- "TLS"
aoi_path  <- "data/TimorLeste.geoJSON" ## NULL or relative path to aoi file 
attr_zone <- NULL ## NULL or variable for zoning
need_gadm <- "no" ## "no" if you have local file, ISO code for downloading from GADM otherwise 
esa_path  <- "download" ## sub-directory for ESA raster data
date_b    <- 2016 ## year beginning
date_e    <- 2020 ## Year end


## Source code
source("R/libraries.R")

source("R/config.R")

source("R/prepa-esa.R")

