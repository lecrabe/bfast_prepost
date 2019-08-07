res_dir <- "/home/cniaf/results_likouala_16_18/"
setwd(res_dir)

############### MAKE A LIST OF RESULTS
list_res <- list.files(res_dir,pattern = glob2rx(paste0("final_results*.tif")))

####################  CREATE A VRT OUTPUT
system(sprintf("gdalbuildvrt %s %s",
               paste0(res_dir,"results_likouala.vrt"),
               paste0(res_dir,list_res,collapse=' ')
))


## Compress final result
system(sprintf("gdal_translate -co COMPRESS=LZW %s %s",
               paste0(res_dir,"results_likouala.vrt"),
               paste0(res_dir,"results_likouala.tif")
))

####################  EXTRACT MAGNITUDE
system(sprintf("gdal_translate -b 2 %s %s",
               paste0(res_dir,"results_likouala.tif"),
               paste0(res_dir,"results_magnitude.tif")
))

####################  COMPUTE  STATS FOR MAGNITUDE
res   <- paste0(res_dir,"results_magnitude.tif")
stats <- paste0(res_dir,"stats_likouala.txt")

system(sprintf("gdalinfo -stats %s > %s",
               res,
               stats
))

s <- readLines(stats)
maxs_b2   <- as.numeric(unlist(strsplit(s[grepl("STATISTICS_MAXIMUM",s)],"="))[2])
mins_b2   <- as.numeric(unlist(strsplit(s[grepl("STATISTICS_MINIMUM",s)],"="))[2])
means_b2  <- as.numeric(unlist(strsplit(s[grepl("STATISTICS_MEAN",s)],"="))[2])
stdevs_b2 <- as.numeric(unlist(strsplit(s[grepl("STATISTICS_STDDEV",s)],"="))[2])

num_class <-9
eq.reclass <-   paste0('(A<=',(maxs_b2),")*", '(A>',(means_b2+(stdevs_b2*floor(num_class/2))),")*",num_class,"+" ,
                       paste( 
                         " ( A >",(means_b2+(stdevs_b2*1:(floor(num_class/2)-1))),") *",
                         " ( A <=",(means_b2+(stdevs_b2*2:floor(num_class/2))),") *",
                         (ceiling(num_class/2)+1):(num_class-1),"+",
                         collapse = ""), 
                       '(A<=',(means_b2+(stdevs_b2)),")*",
                       '(A>', (means_b2-(stdevs_b2)),")*1+",
                       '(A>=',(mins_b2),")*",
                       '(A<', (means_b2-(stdevs_b2*4)),")*",ceiling(num_class/2),"+",
                       paste( 
                         " ( A <",(means_b2-(stdevs_b2*1:(floor(num_class/2)-1))),") *",
                         " ( A >=",(means_b2-(stdevs_b2*2:floor(num_class/2))),") *",
                         2:(ceiling(num_class/2)-1),"+",
                         collapse = "")
)
eq.reclass2 <- as.character(substr(eq.reclass,1,nchar(eq.reclass)-2))

####################  COMPUTE THRESHOLDS LAYER
system(sprintf("gdal_calc.py -A %s --co=COMPRESS=LZW --type=Byte --overwrite --outfile=%s --calc=\"%s\"",
               res,
               paste0(res_dir,"tmp_results_magnitude.tif"),
               eq.reclass2
               
))     

####################  CREATE A PSEUDO COLOR TABLE
cols <- col2rgb(c("black","beige","yellow","orange","red","darkred","palegreen","green2","forestgreen",'darkgreen'))
pct <- data.frame(cbind(c(0:9),
                        cols[1,],
                        cols[2,],
                        cols[3,]
))

write.table(pct,paste0(res_dir,'color_table.txt'),row.names = F,col.names = F,quote = F)

################################################################################
## Add pseudo color table to result
system(sprintf("(echo %s) | oft-addpct.py %s %s",
               paste0(res_dir,'color_table.txt'),
               paste0(res_dir,"tmp_results_magnitude.tif"),
               paste0(res_dir,"tmp_results_magnitude_pct.tif")
))

## Compress final result
system(sprintf("gdal_translate -ot Byte -co COMPRESS=LZW %s %s",
               paste0(res_dir,"tmp_results_magnitude_pct.tif"),
               paste0(res_dir,"results_likouala_threshold.tif")
))

system(sprintf("rm -r -f %s",
               paste0(res_dir,"tmp*.tif"))
)

#################### FONCITON ALIGNEMENT
align <- function(mask, input, output){
  system(sprintf("gdalwarp -co COMPRESS=LZW -t_srs \"%s\" -te %s %s %s %s -tr %s %s %s %s -overwrite",
                 proj4string(raster(mask)),
                 extent(raster(mask))@xmin,
                 extent(raster(mask))@ymin,
                 extent(raster(mask))@xmax,
                 extent(raster(mask))@ymax,
                 res(raster(mask))[1],
                 res(raster(mask))[2],
                 input,
                 ouput
  ))}

#################### CANEVAS
mask   <- paste0(res_dir,"results_likouala_threshold.tif")

system(sprintf("ogr2ogr -t_srs EPSG:4326 %s %s",
               paste0(res_dir,"geo_likouala.shp"),
               paste0(res_dir,"Likouala.shp")))

#################### RASTERIZE
system(sprintf("python %s -v %s -i %s -o %s -a %s",
               paste0("/home/cniaf/bfast_prepost/scripts_misc/oft-rasterize_attr.py"),
               paste0(res_dir,"geo_likouala.shp"),
               paste0(res_dir,"results_likouala_threshold.tif"),
               paste0(res_dir,"limites_likouala.tif"),
               "CODE"
))

#################### ALIGN
input  <- paste0(res_dir,"c0014_16.tif")
ouput  <- paste0(res_dir,"aligned_c0014_16.tif")

align(mask, input, output)

#################### ALIGN
thresh  <- paste0(res_dir,"results_likouala_threshold.tif")
lc_map  <- paste0(res_dir,"aligned_c0014_16.tif")
limites <- paste0(res_dir,"limites_likouala.tif")

#################### CREATE MAP 2000-2014 AT THRESHOLD (0 no data, 1 forest, 2 non-forest, 3 loss, 4 gain)
system(sprintf("gdal_calc.py -A %s -B %s -C %s --co COMPRESS=LZW --outfile=%s --calc=\"%s\"",
               thresh,
               lc_map,
               limites,
               paste0(res_dir,"tmp_map_bfast.tif"),
               paste0("(B==255)*0+(C==0)*0+",         # HORS LIMITES DEPARTEMENT
                      "(B<255)*(C>0)*(",
                      
                      "(B==4)*60+",                   # EAU
                      
                      "((B==2)+(B>30))*(",            
                         "((A==8)+(A==9))*50 +",      # GAINS 
                          "(A<8)*40)+",               # NON FORET
                      
                      "((B==11)+(B==12)+(B==13))*(", 
                        "((A==4)+(A==5))*(B+20)+",    # DEFORESTATION
                        "((A==3))*(B+10)+",           # DEGRADATION
                        "((A<3)+(A>5))*B)",           # FORET
                      ")"
                      )
            ))

####################  CREATE A PSEUDO COLOR TABLE
cols <- col2rgb(c("black",
                  "darkgreen","palegreen","green2",
                  "orange1","orange2","orange3",
                  "red1","red2","red3",
                  "lightgrey","purple","blue"))

pct <- data.frame(cbind(c(0,11,12,13,21,22,23,31,32,33,40,50,60),
                        cols[1,],
                        cols[2,],
                        cols[3,]
))

write.table(pct,paste0(res_dir,'color_table_map.txt'),row.names = F,col.names = F,quote = F)

################################################################################
## Add pseudo color table to result
system(sprintf("(echo %s) | oft-addpct.py %s %s",
               paste0(res_dir,'color_table_map.txt'),
               paste0(res_dir,"tmp_map_bfast.tif"),
               paste0(res_dir,"tmp_map_bfast_pct.tif")
))

## Compress final result
system(sprintf("gdal_translate -ot Byte -co COMPRESS=LZW %s %s",
               paste0(res_dir,"tmp_map_bfast_pct.tif"),
               paste0(res_dir,"results_likouala_map.tif")
))

system(sprintf("rm -r -f %s",
               paste0(res_dir,"tmp*.tif"))
)
