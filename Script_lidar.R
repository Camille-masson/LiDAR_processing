
#### 0. LIBRARIES AND CONSTANTS ####
#----------------------------------#
## Introduction :
# Processing IGN Lidar data
# version 1
# Ph. Choler (parts 1,2) / A. Bayle (parts 3,4)

## Packages : 

library(lidR)
library(terra)


## Variables :
site <- "Grande-Cabane" # Study site (name used for organizing cases and files)
RES    <- 5  # resolution of outputs in meters


## Config : 
source("config.R")







#### 1. DOWNLOAD LAZ files ####
#-----------------------------#
if (TRUE){
  # Comment : use wget to download web data
  # Warning to first install that on our computer
  # select 1km2 areas at https://diffusion-lidarhd.ign.fr/
  # place LAZ files in input/LAZ_"site"
  # name -> liste_dalle.txt
  
  
 
  # ENTREE
  # Creation of sous-dossier : input/LAZ_"site"
  site_case <- file.path(input_dir, paste0("LAZ_",site))
  if (!dir.exists(site_case)) {
    dir.create(site_case, recursive = TRUE)
  }
  
  # !! WARNING       Place LAZ files in this case, to continue        WARNING !!
  
  
  
  # SORTIE
  # Création du sous-sous-dossier : input/LAZ_"site"/downloads
  download_case <- file.path(site_case, "downloads") 
  if (!dir.exists(download_case)) {
    dir.create(download_case, recursive = TRUE)
  }
  
  
  
  # CODE
  # Téléchargement des tules nécessaire : input
  tmpFILENAME <- read.table(file.path(site_case, "liste_dalle.txt"))[,1]
  for (i in 1:length(tmpFILENAME)) {
    print(i)
    shell(cmd = paste('wget -nc -c -nd --no-check-certificate -P', shQuote(download_case), tmpFILENAME[i], sep=" "))
  }
  
  
  
  
  

  }
  
#### 2.  COMPUTE Digital Terrain Model (DTM) ####
#-----------------------------------------------#
if (TRUE){
  
  
  # ENTREE
  # Sous-dossier : input/LAZ_"site"
  site_case <- file.path(input_dir, paste0("LAZ_",site))
  
  # Sous-sous-dossier : input/LAZ_"site"/downloads
  download_case <- file.path(site_case, "downloads") 
  
  # SORTIE
  # Creation of sous-dossier : DTM_"siste"
  site_DTM_case <- file.path(output_dir, paste0("DTM_",site))
  if (!dir.exists(site_DTM_case)) {
    dir.create(site_DTM_case, recursive = TRUE)
  }
  
  # Un .TIF pour le DTM
  output_DTM_file = file.path(site_DTM_case,paste0("DTM_",RES,"_",site,".tif"))
  
  
  #CODE 

  # select data
  LFlaz <- list.files(download_case, "\\.laz$", full.names = TRUE)
  if (length(LFlaz) == 0) stop("Aucun fichier .laz trouvé dans download_case")
  ctg <- lidR::readLAScatalog(
    LFlaz,
    filter = "-drop_class 1 -drop_extended_class 65 -drop_return 0"
  )
  
  # rasterize terrain
  opt_output_files(ctg)    <- file.path(site_DTM_case, paste0("{*}_dtm"))
  opt_laz_compression(ctg) <- TRUE
  dtm <- lidR::rasterize_terrain(
    ctg,
    res       = RES,
    algorithm = tin(),
    pkg       = "terra",
    overwrite = TRUE
  )
  
  
  # Optional: assemble if multiple DTM tiles were produced and renamme if just one tiles
  tiles <- list.files(
    site_DTM_case,
    pattern    = "\\.tif$",
    full.names = TRUE
  )
  
  if (length(tiles) == 1) {
    # If only one tile exists, rename it to the final output name
    file.rename(tiles, output_DTM_file)
    
  } else if (length(tiles) > 1) {
    # If multiple tiles exist, use the original sprc + mosaic routine
    BFIL <- tiles
    tmp  <- lapply(BFIL, function(x) terra::rast(x))
    rsrc <- terra::sprc(tmp)
    m    <- terra::mosaic(rsrc)
    terra::writeRaster(m, output_DTM_file, overwrite = TRUE)
    
  } else {
    stop("No DTM tiles found for assembly.")
  }
  
  
  
  
}

#### 3.  Création du Hillshade ####
#---------------------------------#
 if (TRUE){
  
   # ENTREE
   
   # Sous-dossier : DTM_"siste"
   site_DTM_case <- file.path(output_dir, paste0("DTM_",site))
   
   # Un .TIF du DTM 
   input_DTM_file = file.path(site_DTM_case,paste0("DTM_",RES,"_",site,".tif"))
  
  
  
   # SORTIE 
   # Un .TIF du Hillshade
   output_HILL_file = file.path(site_DTM_case,paste0("Hill_",RES,"_",site,".tif"))
   
   
   # CODE 
   
   # 1. Read your DTM
   # 1. Read the DTM  
   dtm_r <- terra::rast(input_DTM_file)
   
   # 2. Compute slope & aspect in radians  
   slope_r  <- terra::terrain(dtm_r, v = "slope",  unit = "radians")  
   aspect_r <- terra::terrain(dtm_r, v = "aspect", unit = "radians")  
   
   # 3. Generate hillshade with sun at 60° elevation from the south  
   hill <- terra::shade(
     slope_r, 
     aspect_r,
     angle     = 45,   # sun elevation ≈ 60° for mid-May in northern France
     direction = 315   # 180° = due south, shadows on north-facing slopes
   )
   
   # 4. Write out the result  
   terra::writeRaster(hill, output_HILL_file, overwrite = TRUE)
  
  
 }
  
  



#### 3.  Création de l’indice DAH ####
#----------------------------------#
if (TRUE){
  
  # ENTREE
  
  # Sous-dossier : DTM_"site"
  site_DTM_case <- file.path(output_dir, paste0("DTM_", site))
  
  # Un .TIF du DTM 
  input_DTM_file <- file.path(site_DTM_case, paste0("DTM_", RES, "_", site, ".tif"))
  
  # SORTIE 
  # Un .TIF de l’indice DAH (au lieu de Hillshade)
  output_DAH_file <- file.path(site_DTM_case, paste0("DAH_", RES, "_", site, ".tif"))
  
  
  # CODE 
  
  # 1. Lire le DTM
  dtm_r <- terra::rast(input_DTM_file)
  
  # 2. Calculer la pente (β) et l'aspect (α) en radians
  slope_r  <- terra::terrain(dtm_r, v = "slope",  unit = "radians")
  aspect_r <- terra::terrain(dtm_r, v = "aspect", unit = "radians")
  
  # 3. Définir l’azimut solaire (α_max) pour l’indice DAH
  #    Ici, on prend α_max = 1.125 * π (≈ 202,5°), soit la direction de soleil maximal
  alpha_max <- 1.125 * pi
  
  # 4. Calculer l’indice DAH :
  #    DAH = cos(α_max − α) * atan(β)
  #    avec β = slope_r (en radians), α = aspect_r (en radians)
  dah <- terra::app(
    c(slope_r, aspect_r),
    fun = function(vals) {
      beta  <- vals[1]    # slope en radians
      alpha <- vals[2]    # aspect en radians
      return( cos(alpha_max - alpha) * atan(beta) )
    }
  )
  
  # 5. Écrire le résultat en fichier GeoTIFF
  terra::writeRaster(dah, output_DAH_file, overwrite = TRUE)
  
}
  
  
  
#### 4.  COMPUTE Canopy Height Model (CHM) ####
#---------------------------------------------#
if (TRUE){
  
  
  # -- Define CHM output directory for this site
  chm_case <- file.path(output_dir, paste0("CHM_", site))
  if (!dir.exists(chm_case)) {
    dir.create(chm_case, recursive = TRUE)
  }
  
  # -- Define the final CHM filename
  output_CHM_file <- file.path(
    chm_case,
    paste0("CHM_", RES, "_", site, ".tif")
  )
  
  # -- 1. Load raw LAZ files
  laz_files <- list.files(download_case, "\\.laz$", full.names = TRUE)
  if (length(laz_files) == 0) {
    stop("No .laz files found in download_case")
  }
  ctg <- lidR::readLAScatalog(
    laz_files,
    filter = "-drop_class 1 -drop_extended_class 65 -drop_return 0"
  )
  
  # -- 2. Normalize heights and write normalized LAZ into chm_case
  opt_output_files(ctg)    <- file.path(chm_case, "{*}_norm")
  opt_laz_compression(ctg) <- TRUE
  ctg_norm <- lidR::normalize_height(ctg, tin())
  
  # -- 3. Load normalized catalog
  norm_files <- list.files(chm_case, "_norm\\.laz$", full.names = TRUE)
  ctg_norm  <- lidR::readLAScatalog(
    norm_files,
    filter = "-drop_class 1 -drop_extended_class 65 -drop_return 0"
  )
  
  # -- 4. Rasterize canopy to CHM at resolution RES
  chm <- lidR::rasterize_canopy(ctg_norm, RES, p2r())
  
  # -- 5. Save CHM to disk
  terra::writeRaster(chm, output_CHM_file, overwrite = TRUE)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
# 4. COMPUTE Canopy Height Model (CHM) ----

DIR.CHM  <- "D:/DATA_ORLU/CHM/"  # set your CHM directory
NAM.CHM  <- "CHM_5M_ORLU.tif"    # set your CHM output filename

setwd(DIR.LAZ)
LFlaz  <- list.files(pattern=".laz")
ctg    <- lidR::readLAScatalog(LFlaz, filter = "-drop_class 1 -drop_extended_class 65 -drop_return 0")

# Normalize Z (remove ground z) and rewrite LAZ files in a CHM directory
opt_output_files(ctg)    <-  paste0(DIR.CHM, "{*}_norm")
opt_laz_compression(ctg) <-  TRUE
ctg_norm                 <-  lidR::normalize_height(ctg, tin()) 
# open a plot window with the status of each scene 
# tin is a Spatial Interpolation Algorithm

# Load normalized LAZ files
setwd(DIR.CHM)
LIST     <- list.files(full.names=T)
ctg_norm <- lidR::readLAScatalog(LIST, filter = "-drop_class 1 -drop_extended_class 65 -drop_return 0")

# Produce Canopy Height Model from ctg_norm at resolution RES
RES <- 5  # in meters
chm_norm <- lidR::rasterize_canopy(ctg_norm, RES, p2r())

terra::writeRaster(chm_norm,NAM.CHM)

# Graphical checking
terra::plot(terra::rast(NAM.CHM),range=c(0,40))





























































## EN PLUS : 

# Processing IGN Lidar data
# version 1
# Ph. Choler (parts 1,2) / A. Bayle (parts 3,4)

rm(list=ls())
library(lidR)
library(terra)

# 1. FIND URL PATHs of LAZ files ----
# option 1. find the URL corresponding to a specif point ----
# achtung : might generate errors for near-frontier areas, please check/improve !

# data frame of 50km x 50 km tiles/zone coordinates
# only for the Alps !
XZonseq      <- seq(886,986,50)
YZonseq      <- seq(6283,6533,50)
df.Zon       <- expand.grid(XZonseq,YZonseq)
tmp          <- expand.grid(LETTERS[16:18],rev(LETTERS[11:16]))
df.Zon$DALLE <- paste0(tmp[,1],tmp[,2])

# example using decimal degrees
LON  = 6.143; LAT = 45.464 # Etable in North of Belledonne
tmp         <- terra::vect(cbind(x=LON,y=LAT),crs="epsg:4326")
tmp1        <- terra::project(tmp,"epsg:2154")

Xll         <- floor(crds(tmp1)[,1]/1000)
Yll         <- ceiling(crds(tmp1)[,2]/1000)

DALLE       <- paste0(paste0("0",Xll),"_",Yll)
PREFI1      <- "https://storage.sbg.cloud.ovh.net/v1/AUTH_63234f509d6048bca3c9fd7928720ca1/ppk-lidar/"

ID.Zon      <- which((Xll-df.Zon[,1])==min((Xll-df.Zon[,1])[which((Xll-df.Zon[,1])>=0)]) & (Yll-df.Zon[,2])==min((Yll-df.Zon[,2])[which((Yll-df.Zon[,2])>=0)])) # just ugly...

ZONE        <- df.Zon[ID.Zon,"DALLE"]
PREFI2      <- "/LHD_FXX_"
SUFFI       <- ifelse(length(grep(substr(ZONE,2,2),c("J","K","L")))==1,
                      "_PTS_C_LAMB93_IGN69.copc.laz",
                      "_PTS_O_LAMB93_IGN69.copc.laz")
# use C for the rows xJ, xK and xL   
# use O for the rows xM, xN and xO
# there are unexplained exceptions to that rule...

tmpFILENAME <- paste0(PREFI1,ZONE,PREFI2,DALLE,SUFFI)










# option 2. download the URL list of selected files ----
# select 1km2 areas at https://diffusion-lidarhd.ign.fr/
# -> liste_dalle.txt



