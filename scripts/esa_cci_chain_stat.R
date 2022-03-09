################ PACKAGES AND DIRECTORIES
rootdir    <- paste0(normalizePath("~"),"/esa_cci/")
conf_file  <- paste0(rootdir,"scripts/config.R")

############################################################################################################################################
############################################################################################################################################
####################################                                       INPUT
############################################################################################################################################
############################################################################################################################################


################ INPUT PROJECT NAME
prj_name   <- "KEN"


################ INPUT YEARS 
date_b     <- 2015
date_e     <- 2020


################ INPUT AREA OF INTEREST 
#aoi_file   <- "/home/dannunzio/angola/angola_strata_4326.shp"
#attr_zone  <- "gez_name"
attr_zone  <- "NAME_1"

############################################################################################################################################
############################################################################################################################################
####################################                                       OUTPUT
############################################################################################################################################
############################################################################################################################################


################ OUTPUT VARIABLES
data_dir   <- paste0(rootdir,"data/",prj_name,"/")

aoi_shp    <- paste0(data_dir,"aoi_",prj_name,".shp")
aoi_tif    <- paste0(data_dir,"aoi_",prj_name,".tif")

esa_b      <- paste0(data_dir,"esa_bb_",prj_name,"_",date_b,".tif")
esa_e      <- paste0(data_dir,"esa_bb_",prj_name,"_",date_e,".tif")

esa_chg    <- paste0(data_dir,"esa_change_",prj_name,"_",date_b,"_",date_e,".tif")
esa_combi  <- paste0(data_dir,"esa_combin_",prj_name,"_",date_b,"_",date_e,".tif")
esa_clump  <- paste0(data_dir,"esa_clump_" ,prj_name,"_",date_b,"_",date_e,".tif")

stats_b    <- paste0(data_dir,"stats_",prj_name,"_",date_b,".txt")
stats_e    <- paste0(data_dir,"stats_",prj_name,"_",date_e,".txt")
stats_z    <- paste0(data_dir,"stats_",prj_name,"_zone.txt")
stats_o    <- paste0(data_dir,"stats_",prj_name,"_",date_b,"_",date_e,".csv")
stats_m    <- paste0(data_dir,"stats_metrics",prj_name,"_",date_b,"_",date_e,".csv")

sim_file   <- paste0(data_dir,"simulation_",prj_name,"_",date_b,"_",date_e,".csv")
sim_file_s <- paste0(data_dir,"simulation_",prj_name,"_",date_b,"_",date_e,"_summary.csv")
################ INITIALIZE
source(conf_file)

dir.create(rootdir,showWarnings = F)
dir.create(data_dir,showWarnings = F)

#aoi        <- readOGR(aoi_file)
aoi        <- getData('GADM', country=prj_name, level=1)
dbf        <- aoi@data
dbf$sortid <- row(dbf)[,1] 

zone_list   <- unique(dbf[,attr_zone])
strata_max  <- length(zone_list)
zones       <- data.frame(cbind(1:strata_max,zone_list))
names(zones)<- c("esa_zone",attr_zone)
mdbf        <- merge(dbf,zones,by.x=attr_zone,by.y=attr_zone,all.x=T)
aoi@data    <- arrange(mdbf,sortid)

aoi_geo    <- spTransform(aoi,CRS('+init=epsg:4326'))
#writeOGR(aoi_geo,aoi_shp,paste0("aoi_",prj_name),"ESRI Shapefile",overwrite_layer = T)

################ BOUNDING BOX

aoi_ext    <- extent(aoi_geo)
xmin       <- aoi_ext@xmin
ymin       <- aoi_ext@ymin
xmax       <- aoi_ext@xmax
ymax       <- aoi_ext@ymax 




################ EXTRACT TO BOUNDING BOXES
for(year in c(date_b,date_e)){
  
  if(year <  2016){collection <- paste0(rootdir,"download/","ESACCI-LC-L4-LCCS-Map-300m-P1Y-",year,"-v2.0.7cds.nc")}
  if(year >= 2016){collection <- paste0(rootdir,"download/","C3S-LC-L4-LCCS-Map-300m-P1Y-",   year,"-v2.1.1.nc")}
  
  system(sprintf("gdalwarp -of Gtiff -co COMPRESS=LZW -co TILED=YES -ot Int32 -te %s %s %s %s -tr %s %s -t_srs EPSG:4326 NETCDF:%s:lccs_class %s -overwrite",
                 xmin,
                 ymin,
                 xmax,
                 ymax,
                 0.002777777777778,
                 0.002777777777778,
                 collection,
                 paste0(data_dir,"esa_bb_",prj_name,"_",year,".tif")
  ))
}


################ ALIGN ZONES
system(sprintf("oft-rasterize_attr.py -v %s -i %s -o %s -a %s",
               aoi_shp,
               esa_b,
               paste0(data_dir,"aoi_tmp.tif"),
               "esa_zone"
))

system(sprintf("gdal_translate -ot Int32  -co COMPRESS=LZW %s %s",
               paste0(data_dir,"aoi_tmp.tif"),
               aoi_tif
))

################ COMPUTE CHANGE
system(sprintf("gdal_calc.py -A %s -B %s -C %s --type Byte --co COMPRESS=LZW  --outfile=%s --overwrite --calc=\"%s\"",
               esa_b,
               esa_e,
               aoi_tif,
               esa_chg,
               "(C>0)*(A!=B)"
))

################ COMPUTE COMBINATION
system(sprintf("gdal_calc.py -A %s -B %s -C %s --type Int32 --co COMPRESS=LZW  --outfile=%s --overwrite  --calc=\"%s\"",
               esa_b,
               esa_e,
               aoi_tif,
               esa_combi,
               "(C>0)*(A*1000+B)"
))

################ CLUMP INDIVIDUAL COMBINATION PATCHES
system(sprintf("oft-clump %s %s",
               esa_combi,
               esa_clump
               
))

################ COMPUTE HISTOGRAM YEAR BEGINNING
system(sprintf("oft-stat -i %s -um %s -o %s ",
               esa_b,
               esa_clump,
               stats_b
))

################ COMPUTE HISTOGRAM YEAR END
system(sprintf("oft-stat -i %s -um %s -o %s",
               esa_e,
               esa_clump,
               stats_e
))

################ COMPUTE HISTOGRAM ZONE
system(sprintf("oft-stat -i %s -um %s -o %s",
               aoi_tif,
               esa_clump,
               stats_z
))



################ READ STATS AND COMPILE
df_b <- read.table(stats_b)
df_e <- read.table(stats_e)
df_z <- read.table(stats_z)

names(df_e) <- names(df_b)  <-  names(df_z) <- c("clump_id","total","mean","std")

df <- cbind(df_b[,c("clump_id","total","std","mean")],
            df_e[,c("mean")],
            df_z[,c("mean","std")]
)

names(df) <- c("clump_id","total","std_code","code_beg","code_end","esa_zone","std_zone")


################ CREATE A CHANGE VARIABLE
df$change <- 0
df[df$code_beg != df$code_end,"change"] <- 1


################ ASSIGN A ZONE FOR ZONEVERLAPPING CLUMPS
df$esa_zone <- floor(df$esa_zone +0.5)


################ TAKE ONLY CLUMPS WITHIN THE BOUNDARIES OF THE AOI AND MERGE WITH ORIGINAL NAME
mdf <- merge(df[df$esa_zone > 0 ,],zones,by.x="esa_zone",by.y= "esa_zone",all.x=T)


################ CHECK THAT THE TOTAL AREA IS IN THE BALL PARK
res_pix <- 300
sum(mdf$total)*res_pix*res_pix*cos((ymin+ymax)/2*pi/180)/10000/1000

mdf$code_combi <- mdf$code_beg*1000+ mdf$code_end

head(mdf)



################ COMPUTE METRICS PER ZONE and CHANGE

################ NUMBER OF PIXELS
esa_m             <- data.frame(tapply(mdf$total,mdf[,c(attr_zone,"change")],sum))
names(esa_m)      <- c("pxl_stb","pxl_chg")
esa_m$pxl_tot     <- esa_m$pxl_stb + esa_m$pxl_chg

################ NUMBER OF OBJECTS
esa_m$obj_stb     <- table(mdf[,c(attr_zone,"change")])[,1]
esa_m$obj_chg     <- table(mdf[,c(attr_zone,"change")])[,2]
esa_m$obj_tot     <- esa_m$obj_stb + esa_m$obj_chg

################ MAX SIZE OF OBJECTS
esa_m[,c("max_stb","max_chg")] <- data.frame(tapply(mdf$total,mdf[,c(attr_zone,"change")],max))

################ AVERAGE SIZE OF OBJECTS
esa_m$avg_stb     <- esa_m$pxl_stb / esa_m$obj_stb
esa_m$avg_chg     <- esa_m$pxl_chg / esa_m$obj_chg

################ PROPORTION OF CHANGE PER PIXEL AND OBJECTS
esa_m$chp_pxl     <- round(esa_m$pxl_chg / esa_m$pxl_tot *100,1)
esa_m$chp_obj     <- round(esa_m$obj_chg / esa_m$obj_tot *100,1)

################ PROPORTION OF CHANGE ON AVERAGE AND MAX 
esa_m$chp_avg     <- round(esa_m$avg_chg / esa_m$avg_stb * 100,1)
esa_m$chp_max     <- round(esa_m$max_chg / esa_m$max_stb * 100,1)

esa_m[,attr_zone] <- row.names(esa_m)



################ SAMPLING DESIGN
pop_bb  <- sum(df$total)
pop_aoi <- sum(esa_m$pxl_tot)
obj_avg <- mean(mdf$total)
max_sz  <- ceiling(pop_bb / obj_avg * 10)

area_bbx <- (xmax-xmin)*(ymax-ymin)*110320*110320
area_aoi <- area_bbx * pop_aoi/ pop_bb  

test_sizes <- max_sz/c(1,2,3,4,5,10,20,100)^2 


target        <- data.frame(tapply(mdf$total,mdf[,c("code_combi")],sum))
names(target) <- "pxl_map"

fill_sim      <- function(sim,code){
  if(code %in% names(sim)){sim[code]}else{0}
}


sim_SYS   <- function(sim_sz){
  spdf        <- sampleRegular(raster(esa_combi),sim_sz,sp=T)
  names(spdf) <- c("esa_combi")
  spdf        <- spdf[spdf@data$esa_combi >0,]
  tmp         <- spdf@data
  size        <- nrow(tmp)
  sim         <- table(tmp$esa_combi)/size*pop_aoi
  sapply(row.names(target),function(x){fill_sim(sim,x)})
}

sim_SRS   <- function(sim_sz){
  spdf        <- sampleRandom(raster(esa_combi),sim_sz,sp=T)
  names(spdf) <- c("esa_combi")
  spdf        <- spdf[spdf@data$esa_combi >0,]
  tmp         <- spdf@data
  size        <- nrow(tmp)
  sim         <- table(tmp$esa_combi)/size*pop_aoi
  sapply(row.names(target),function(x){fill_sim(sim,x)})
}

sim_set_sys <- data.frame(sapply(test_sizes,sim_SYS))
names(sim_set_sys) <- paste0("sim_SYS_",test_sizes * pop_aoi/ pop_bb )

sim_set_srs <- data.frame(sapply(test_sizes,sim_SRS))
names(sim_set_srs) <- paste0("sim_SRS_",test_sizes * pop_aoi/ pop_bb )

res <- cbind(target,sim_set_sys,sim_set_srs)

dif <- res - res$pxl_map
names(dif) <- paste0("diff_",names(res))

abs <- abs(dif)
names(abs) <- paste0("abs_",names(res))

prp <- abs / res$pxl_map * 100
names(prp) <- paste0("prop_",names(res))

res$code   <- 1000000+as.numeric(row.names(res))
res$code_b <- as.numeric(substr(res$code,2,4))
res$code_e <- as.numeric(substr(res$code,5,7))
res$change <- res$code_b != res$code_e

out <- cbind(res,dif,abs,prp)
out <- arrange(out,change)
head(out)

df_sys <- data.frame(cbind(colSums(out[out$change == F ,grepl("abs_sim_SYS",names(out))])/sum(out[out$change == F ,"pxl_map"])*100,
                           colSums(out[out$change == T ,grepl("abs_sim_SYS",names(out))])/sum(out[out$change == T ,"pxl_map"])*100
))

df_srs <- data.frame(cbind(colSums(out[out$change == F ,grepl("abs_sim_SRS",names(out))])/sum(out[out$change == F ,"pxl_map"])*100,
                           colSums(out[out$change == T ,grepl("abs_sim_SRS",names(out))])/sum(out[out$change == T ,"pxl_map"])*100
))

head(out[out$change == T,])

names(df_sys) <- names(df_srs) <- c("stable","change")

df_sys$size <- as.numeric(substr(row.names(df_sys),13,50))
df_srs$size <- as.numeric(substr(row.names(df_srs),13,50))

df_sys$spacing <- floor(sqrt(area_aoi/df_sys$size))
df_srs$spacing <- floor(sqrt(area_aoi/df_srs$size))


plot(df_sys$size,df_sys$stable,xlab="Sampling size",ylab="Cumulated relative error (%)",main="Stable classes")
points(df_srs$size,df_srs$stable,col="red")

plot(df_sys$size,df_sys$change,xlab="Sampling size",ylab="Cumulated relative error (%)",main="Change classes")
points(df_srs$size,df_srs$change,col="red")

################ PLOTS
plot(raster(esa_b))


write.csv(out,sim_file)
write.csv(df_sys,sim_file_s,row.names = F)
write.csv(mdf,stats_o,row.names = F)
write.csv(esa_m,stats_m)
head(mdf)

max_sz/c(1,2,3,4,5,10,20,100)^2 * pop_aoi/ pop_bb

sim_sz      <- max_sz/25
spdf        <- sampleRegular(raster(esa_combi),sim_sz,sp=T)
names(spdf) <- c("esa_combi")
spdf        <- spdf[spdf@data$esa_combi >0,]
tmp         <- spdf@data
size        <- nrow(tmp)
sim         <- table(tmp$esa_combi)/size*pop_aoi
sapply(row.names(target),function(x){fill_sim(sim,x)})
tmp$code   <- 1000000+as.numeric(tmp$esa_combi)
tmp$code_b <- as.numeric(substr(tmp$code,2,4))
tmp$code_e <- as.numeric(substr(tmp$code,5,7))
tmp$change <- tmp$code_b != tmp$code_e

spdf@data <- tmp
writeOGR(spdf,paste0(data_dir,"points_",size,".shp"),paste0("points_",size),"ESRI Shapefile",overwrite_layer = T)
head(spdf@data)
table(tmp$change)
