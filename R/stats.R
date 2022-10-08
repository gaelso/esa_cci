## Calculate land cover change statistics

esa_lccs_init <- rbind(  
    list(  0, "No Data"           , 0, 0, 0       ), 
    list( 10, "Cropland, rainfed",  255, 255, 100),
    list( 20, "Cropland, irrigated or post-flooding", 170, 240, 240),
    list( 30, "Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)", 220, 240, 100),
    list( 40, "Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%)", 200, 200, 100),
    list( 50, "Tree cover, broadleaved, evergreen, closed to open (>15%)",  0, 100, 0),
    list( 60, "Tree cover, broadleaved, deciduous, closed to open (>15%)", 0, 160, 0),
    list( 70, "Tree cover, needleleaved, evergreen, closed to open (>15%)", 0, 60, 0),
    list( 80, "Tree cover, needleleaved, deciduous, closed to open (>15%)", 40, 80, 0),
    list( 90, "Tree cover, mixed leaf type (broadleaved and needleleaved)", 120, 130, 0),
    list(100, "Mosaic tree and shrub (>50%) / herbaceous cover (<50%)", 140, 160, 0),
    list(110, "Mosaic herbaceous cover (>50%) / tree and shrub (<50%)", 190, 150, 0),
    list(120, "Shrubland", 150, 100, 0),
    list(130, "Grassland", 255, 180, 50),
    list(140, "Lichens and mosses",  255, 220, 210),
    list(150, "Sparse vegetation (tree, shrub, herbaceous cover) (<15%)", 255, 235, 175),
    list(160, "Tree cover, flooded, fresh or brackish water", 0, 120, 90),
    list(170, "Tree cover, flooded, saline water", 0, 150, 120),
    list(180, "Shrub or herbaceous cover, flooded, fresh/saline/brackish water", 0, 220, 130),
    list(190, "Urban areas", 195, 20, 0),
    list(200, "Bare areas", 255, 245, 215),
    list(210, "Water bodies", 0, 70, 200),
    list(220, "Permanent snow and ice", 255, 255, 255)
    ) 

esa_lccs <- tibble(
  id    = unlist(esa_lccs_init[,1]), 
  name  = unlist(esa_lccs_init[,2]), 
  r     = unlist(esa_lccs_init[,3]),
  g     = unlist(esa_lccs_init[,4]),
  b     = unlist(esa_lccs_init[,5])
)

esa_lccs$color
