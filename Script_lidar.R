
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
site <- "Nyer" # Study site (name used for organizing cases and files)
RES    <- 1  # resolution of outputs in meters


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
  
#### 4.  Création de l’indice DAH ####
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
  
  
  
#### 5.  COMPUTE Canopy Height Model (CHM) ####
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
  
  
  
  
  
  
  
  
  
}
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  