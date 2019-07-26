####################################################################################################
####################################################################################################
## Tiling of an AOI (shapefile defined)
## Contact remi.dannunzio@fao.org 
## 2019/03/11
####################################################################################################
####################################################################################################

### READ PROSCAL BOUNDARIES
aoi   <- getData('GADM',
                 path=gadm_dir, 
                 country= countrycode, 
                 level=0)

proj4string(aoi)
(bb    <- extent(aoi))

### What grid size do we need ? 
grid_size <- tile_size*1000          ## in meters

### GENERATE A GRID
sqr_df <- generate_grid(aoi,grid_size/111320)

nrow(sqr_df)

### Select a vector from location of another vector
sqr_df_selected <- sqr_df[aoi,]
nrow(sqr_df_selected)

### Give the output a decent name, with unique ID
names(sqr_df_selected@data) <- "tileID" 
sqr_df_selected@data$tileID <- row(sqr_df_selected@data)[,1]

### Reproject in LAT LON
tiles   <- spTransform(sqr_df_selected,CRS("+init=epsg:4326"))
aoi_geo <- spTransform(aoi,CRS("+init=epsg:4326"))


### Plot the results
plot(tiles)
plot(aoi_geo,add=T,border="blue")


### Assign each tile with a username
df        <- data.frame(cbind(tiles@data[,"tileID"],users))
names(df) <- c("tileID","username")

df$tileID <- as.numeric(df$tileID)
table(df$username)
nbatch <- ceiling(max(table(df$username))/nbatchmax)

df <- arrange(df,username)
df <- cbind(df,rep(1:nbatch,ceiling(nrow(df)/nbatch))[1:nrow(df)])
names(df) <- c("tileID","username","batch")

df <- arrange(df,tileID)

table(df$username,df$batch)

df$user_batch <- paste0(df$username,"_",df$batch)
df <- arrange(df,df$user_batch)

### Create a final subset corresponding to your username
tiles@data <- df 

### Export ALL TILES as KML
export_name <- paste0("tiling_system_national_",countrycode)

writeOGR(obj=tiles,
         dsn=paste(tile_dir,export_name,".kml",sep=""),
         layer= export_name,
         driver = "KML",
         overwrite_layer = T)

countrycode

# for(username in users){
#   for(batch in 1:nbatch){
#   my_tiles <- tiles[tiles$tileID %in% df[df$username == username & df$batch == batch,"tileID"],]
# 
#   plot(my_tiles,add=T,col="green")
#   length(my_tiles)
#   
#   ### Export the final subset
#   export_name <-  paste0(countrycode,"_tiles_",username,"_batch",batch)
#   
#   writeOGR(obj=my_tiles,
#            dsn=paste(tile_dir,export_name,".kml",sep=""),
#            layer= export_name,
#            driver = "KML",
#            overwrite_layer = T)
#   
#   # ### Export the ONE TILE IN THE subset
#   # export_name <- paste0("jour_j_",username)
#   # 
#   # writeOGR(obj=my_tiles[51:100,],
#   #          dsn=paste(tile_dir,export_name,".kml",sep=""),
#   #          layer= export_name,
#   #          driver = "KML",
#   #          overwrite_layer = T)
#   }
# }
