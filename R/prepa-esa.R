## Prepare ESA data for deforestation calculation

## Get AOI
if (need_gadm == "no"){
  
  sf_aoi <- sf::st_read(aoi_path)
  
} else {
  
  sf_aoi    <- raster::getData('GADM', country=prj_name, level=1) %>% st_as_sf()
  ## NOT WORKING DESPITE SUGGESTED REPLACEMENT
  #sf_aoi <- geodata::gadm(country = prj_name, level = 1, path = data_path)
  attr_zone <- "NAME_1" 
  
}

if (is.null(attr_zone)) {
  
  sf_aoi <- sf_aoi %>% mutate(esa_zone = 1:nrow(.))
  
} else if (attr_zone %in% names(sf_aoi)) {
  
  zone_list <- sf_aoi %>% dplyr::pull(attr_zone) %>% unique()
  zones     <- dplyr::bind_cols(seq_along(zone_list), zone_list)
  names(zones) <- c("esa_zone", attr_zone)
  zones
  
  sf_aoi <- sf_aoi %>% left_join(zones, by = attr_zone)
  
} else {
  
  sf_aoi <- sf_aoi %>% mutate(esa_zone = 1:nrow(.))
  
}

if (sf::st_crs(sf_aoi)$srid != "EPSG::4326") sf_aoi <- sf_aoi %>% sf::st_transform(crs = 4326)


rs_esa_b <- terra::rast(
  file.path(esa_path, paste0("C3S-LC-L4-LCCS-Map-300m-P1Y-", date_b,"-v2.1.1.nc"))
  ) %>%
  terra::crop(terra::vect(sf_aoi)) %>%
  terra::mask(terra::vect(sf_aoi))

rs_esa_b
#plot(rs_esa_b)

rs_esa_e <- terra::rast(
  file.path(esa_path, paste0("C3S-LC-L4-LCCS-Map-300m-P1Y-", date_e,"-v2.1.1.nc"))
) %>%
  terra::crop(terra::vect(sf_aoi)) %>%
  terra::mask(terra::vect(sf_aoi))

rs_esa_e
#plot(rs_esa_e)

## Make matrix
df_esa <- left_join(
  terra::as.data.frame(rs_esa_b, xy = TRUE),
  terra::as.data.frame(rs_esa_e, xy = TRUE),
  by = c("x", "y"),
  suffix = c("_b", "_e")
  ) %>%
  as_tibble() %>%
  mutate(
    combi = lccs_class_b * 1000 + lccs_class_e,
    chg   = lccs_class_b != lccs_class_e
  )




