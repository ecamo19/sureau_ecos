# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### 
# Launcher Inversion de la Reserve Utile (via Van-Genuchten formulation) 
# Authors : Arsene Druel   (arsene.druel[at]umr-cnrm.fr)
#           Nicolas Martin (nicolas.martin@inrae.fr)
# Date : 08/11/2022           
# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### 

# Initialization ---------------------------------------------------------------
# R CMD BATCH '--args split_simulation_ID=1 split_simulation_NB=10' Launcher_InversionRU.R
args=(commandArgs(TRUE))
if(length(args)==0) rm(list = ls()) # Clear environment
gc()            # Clear memory

#####################
# GLOBAL PARAMETERS #
#####################

nameProject    <- "RU_inversion_ICOS_test_bug1" # "RU_inversion_Europe_01" "RU_tests_Barbeau"
forest_type    <- "all" # select one among "lcForest","lc60","lc70","lc90","lc100","lc120" to select only one, or "all" to do all and simulate the dominant
idigit         <- 1 # dependent of input grid and define output file names

# To load and select the points
overwritePts   <- FALSE # To load or compute the points and associated data
limFracForest  <- 0.0
siteNames      <- NULL


# Set common paths (writt and load common files)
pathData        <- getwd()
inputDataFolder <- file.path(pathData,"Inversion_Ecos_TAW") # Regrid data folder

# Name of columns
clim_site <- "climName"
x_site    <- "x"
y_site    <- "y"
id_site   <- NULL


# MAIN PARAMETERS TO LAUCHN INVERSION #
#######################################
simuName       <- "Icos_Pinuspinaster" # EUR_5m
simuNbYear     <- 33
startYear      <- 1990
nbCores        <- 8
freeCore       <- 0 # nb of not used core in any case (mostly for personal computer: put 1 or 2)
SurEau_mainDir    <- getwd()
soil_files_folder <- file.path(inputDataFolder,"soilFiles/sureau_file_inversion")    # where write and read temporary soil files


# PARAMETER TO SPLIT THE WORKING POINTS. Only if there is numerous points to be distributed between computer / nodes
if(length(args)<=1){
  print("No arguments supplied for splitting.")
  ##supply default values
  split_simulation_NB = 1 # By default ==> put 1
  split_simulation_ID = 1
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
  if ( !exists("split_simulation_NB") || !exists("split_simulation_ID")) stop("The arguments for splitting are not well given in input")
}


#####################################################
# PARAMETERS TO LOAD THE POINTS AND ASSOCIATED DATA #
#####################################################

path_climate         <- file.path(inputDataFolder, "climFiles/")
pathClimateClassfile <- file.path(inputDataFolder,"world_koppen/hdr.adf")
map_grid             <- file.path(inputDataFolder,"era5-hourly_Europe-allV-1-1981.nc")

# Define select points
if ( FALSE ) {
  # workPoints <- c(5.67865,43.24079) # if point
  workPoints <- c(-0.798,44.73) # landes
} else if ( FALSE ) {
  # workPoints <- '/home/ardruel/Documents/FORGENIUS/SiteProposal_IS_v4_3.csv' # if list
} else if ( FALSE ) {
  workPoints <- c(-10,45,30,81) # if region
} else if ( TRUE ) {
  # ICOS #https://www.icos-cp.eu/observations/national-networks/france http://www.icos-etc.eu/icos/
  workPoints <- NULL
  workPoints[["FR-Barbeau"]]         = c( 2.780096, 48.476357) # Barbeau(Fontainebleau)
  workPoints[["FR-FondBlanche"]]     = c( 5.67865 , 43.24079 ) # FondBlanche
  workPoints[["FR-Hess"]]            = c( 7.06465 , 48.6741  ) # Hess
  workPoints[["FR-Montiers"]]        = c( 5.31187 , 48.53816 ) # Montiers
  workPoints[["FR-Puechabon"]]       = c( 3.5957  , 43.7413  ) # Puechabon
  workPoints[["FR-Bilos"]]           = c(-0.956092, 44.493652) # Salles
  workPoints[["IT-Renon"]]           = c(11.43369 , 46.58686)
  workPoints[["IT-Bosco"]]           = c(10.741966, 45.197754)
  workPoints[["IT-Rossore2"]]        = c(10.29091 , 43.73202)
  workPoints[["IT-Castelporziano2"]] = c(12.357293, 41.704266)
  workPoints[["IT-Capodimonte"]]     = c(14.2504  , 40.8741) # Parco Urbano di Capodimonte
  workPoints[["CH-Davos"]]           = c( 9.85591 , 46.81533)
  workPoints[["CZ-Lanzhot"]]         = c(16.946331, 48.68155)
  workPoints[["CZ-Bily_Kriz"]]       = c(18.536882, 49.502075)
  workPoints[["BE-Brasschaat"]]      = c( 4.51984 , 51.30761)
  workPoints[["BE-Vielsalm"]]        = c( 5.998099, 50.304962)
  workPoints[["NL-Loobos"]]          = c( 5.743556, 52.16658)
  #workPoints[["DE-Kienhorst"]]       = c( , )
  workPoints[["DE-Hohes_Holz"]]      = c(11.22235 , 52.08656)
  workPoints[["DE-Tharandt"]]        = c(13.56515 , 50.96256)
  workPoints[["DE-Hetzdorf"]]        = c(13.48978 , 50.96381)
  workPoints[["DE-Hainich"]]         = c(10.452089, 51.079407)
  workPoints[["DE-Wustebach"]]       = c(6.3309627, 50.50493)
  workPoints[["DE-Mooseurach"]]      = c(11.456168, 47.80918)
  workPoints[["DE-Hartheim"]]        = c( 7.5981  , 47.933)
  workPoints[["DK-Risoe"]]           = c(12.101398, 55.680683)
  workPoints[["DK-Soroe"]]           = c(11.644645, 55.48587)
  workPoints[["DK-Gludsted"]]        = c( 9.3341  , 56.0737)
  workPoints[["NO-Hurdal"]]          = c(11.07949 , 60.37163)
  workPoints[["SE-Hyltemossa"]]      = c(13.41897 , 56.09763)
  workPoints[["SE-Norunda"]]         = c(17.479504, 60.0865)
  workPoints[["SE-Svartberget"]]     = c(19.7745  , 64.25611)
  workPoints[["FI-Hyytiala"]]        = c(24.29477 , 61.84741)
  workPoints[["FI-Varrio"]]          = c(29.61    , 67.7549)
  workPoints[["FI-Sodankyla"]]       = c(26.63859 , 67.36239)
  workPoints[["FI-Kenttarova"]]      = c(24.24301 , 67.98721)
  siteNames = names(workPoints)
}

# Different file characteristics
# common files characteristics
areaFileName = "_10W-45N-30E-81N"

# climate file characteristics
# climate_type_file = ".csv"
prefix = 'ERA_'
suffix = '_1990-2022'
check_clim_exist  = TRUE
check_clim_values = FALSE ## Check climate file (to remove NA) <== PUT TRUE ONCE

# soil file characteristics
soil_prefix = "SOIL_resample_ERA5_3soil_"
soil_suffix = paste0("_EUPTF_US_only_coarseF",areaFileName)
typeSoilInfo = "USDA"
listSoils    = c("0-30","30-100","100-200","0-200")


# LAI file characteristics
lai_prefix = "LAI_resample_ERA5_"
lai_suffix = ".nc"
laiyears   = c(1999,2022)


# Simulation config
# specie = "Pinus pinaster"



# fraction of forest input
forest_prefix    = "Forest_contrib_ERA5_"
forest_suffix    = ".nc"


##################################
# PARAMETERS TO LAUCHN INVERSION #
##################################

# Configuration of inversion RU
iPtsDiag      <- TRUE     # Write diagnostic (text)
save_pdf      <- TRUE     # For each site, save a plot
QPLCl9_target <- 12  # Leaf PLC target (quantile 90)
max_simu      <- 7   # Maximum of simulation authorized before to stop
model_varLim  <- 10  # limit of the RU variation of the model accepted
QPLCl9_target_tolerence = 0.5 # limit of the QPLCl9 target tolerance, only in some conditions
maxCoarseFrag <- 99   # Maximum content in coarse fragments


# Simulation CONFIG
surEauClimate_path <- file.path(path_climate) #pathHome,"data/climate",regionNameClim,paste0(climInputName,'_',climInputType,nameMnt))#"data/climate/Forgenius_Era5land/ERA_land_sureau_PtsEx_daily")
methodPedoTransf   <- "VG" #Campbell" #"VG"
sureauTimeStep     <- 2 #defaul = 1
sureauOptionEvapo  <- 'Fast'
transpiModel       <- 'Jarvis' #'Granier'
overWriteOutput    <- TRUE
thresholdMortality <- 100 
resetSWC           <- T # fill the soil with water at the beginning of each year (mainly for the case where the water resources are external (not rain))
#outputType = "simple_daily" # "diagnostic_subdaily"  "LFMC_subdaily"  "simple_daily"  "simple_subdaily"  "simple_yearly"  "yearly_forSA"
outputType         <- "yearly_forSA"
# Path of folder where soil data file are write (initialization) and used (run sureau)


# Create soil files
profondeur_soil = 2
select_RU = "US"
# (soil data)
select_RU_bis = select_RU
SE_coarseFrag_1 = "coarseFrag_0-30_ori"
SE_coarseFrag_2 = paste0("coarseFrag_30-100_ori")
SE_coarseFrag_3 = paste0("coarseFrag_100-",profondeur_soil,"00_ori")
SE_fieldCap   = paste0("fc_vg_",select_RU,"_0-",profondeur_soil,"00") #"wfc_0-200"  #"fc_vg_0-200" "fc_c_0-200" "wfc_0-200"
SE_wiltPoint  = paste0("wp_vg_",select_RU,"_0-",profondeur_soil,"00") #"wilt_0-200" #"wp_vg_0-200" "wp_c_0-200" "wilt_0-200"
SE_vg_alpha   = paste0("alp_",select_RU_bis,"_0-",profondeur_soil,"00")
SE_vg_n       = paste0("n_",  select_RU_bis,"_0-",profondeur_soil,"00")
SE_vg_I       = paste0("L_",  select_RU_bis,"_0-",profondeur_soil,"00") # L_ (euptf) or I_ (constant) 
SE_vg_ksat    = paste0("K0_US_0-",profondeur_soil,"00") #K0_US_0-200 (euptf) or  Ksat_0- (bonan)
SE_vg_sat_cap = paste0("ths_",select_RU_bis,"_0-",profondeur_soil,"00")
SE_vg_res_cap = paste0("thr_",select_RU_bis,"_0-",profondeur_soil,"00")
SE_gsoil      = 30
SE_depth1     = 0.5 # 0.3
SE_depth2     = 2   # 1
SE_depth3     = 5   # 2
SE_offSetPsoil= 0.3
# SE_c_b        = paste0("b_c_0-",profondeur_soil,"00")
# SE_c_spie     = paste0("Ψsat_c_0-",profondeur_soil,"00") # is after by divided by ~1000
# SE_c_sat_cap  = paste0("θsat_c_0-",profondeur_soil,"00")
# SE_c_ksat     = paste0("Ksat_c_0-",profondeur_soil,"00")




######################
#### START SCRIPT ####
######################
library(stringr)
library(data.table)
library(terra)


# A # LOAD POINT DATA #
#######################
if ( forest_type == "all" ) {
  list_forest = c("lcForest","lc60","lc70","lc90","lc100","lc120")
  forestT_selectPoints = NULL
} else (
  list_forest = forest_type
)
for ( iforest_type in list_forest ) {
  
  cat("START LOAD DATA FOR THE FOREST TYPE", iforest_type)
  
  # Properties function of "forest_type" ----------------------------------------
  # Name of temporal files points
  fileWithInData <- file.path(inputDataFolder,paste0(nameProject,"_",iforest_type,".csv")) #paste0("ICOS_forest_Europe.csv")) # NULL

  if ( overwritePts || is.null(fileWithInData) || !file.exists(fileWithInData)) {

    # Name of inmput files
    pathSoilFile = file.path(inputDataFolder,paste0(soil_prefix,iforest_type,soil_suffix,".nc"))
    pathLAIfile = file.path(inputDataFolder,paste0(lai_prefix,iforest_type,areaFileName,"_"))
    if ( iforest_type != "lcForest" ) {
      pathForestfile   = file.path(inputDataFolder,paste0(forest_prefix,iforest_type,areaFileName,"_"))
      forest_list_type = ""
      forest_meanfix   = ""
    } else {
      pathForestfile   = file.path(inputDataFolder,forest_prefix)
      forest_list_type = c("lcForest","lc60","lc70","lc90","lc100","lc120") # "lcForest"
      forest_meanfix   = paste0(areaFileName,"_")
    }
    
    # Load characteristics of points for forest_type
    source(file.path(SurEau_mainDir,"scripts_base_simulations/Fonctions_InversionRU.R"))
    selectPoints = loadPoints(workPoints, map_grid,
                              pathLAIfile, laiyears, lai_suffix,
                              pathSoilFile, typeSoilInfo, listSoils,
                              path_climate, idigit, prefix, suffix, clim_site, pathClimateClassfile, 
                              pathForestfile, forest_suffix, forest_type = iforest_type, forest_list_type = forest_list_type, forest_meanfix = forest_meanfix, check_clim_exist  = check_clim_exist, check_clim_values = check_clim_values)
    if ( length(list_forest) >  1 ) forestT_selectPoints[[iforest_type]] = selectPoints #if forest_type == "all"
    if ( length(list_forest) == 1 && nrow(selectPoints) == length(siteNames)) selectPoints = cbind(siteNames, selectPoints) # forest_type != "all"
    if (!is.null(fileWithInData) && (!file.exists(fileWithInData) || overwritePts) ) fwrite(selectPoints,file=fileWithInData,row.names=F,col.names=T,sep=";")
  } else {
    selectPoints = fread(fileWithInData)
    if ( length(list_forest) >  1 ) forestT_selectPoints[[iforest_type]] = selectPoints #if forest_type == "all"
  }
  
  cat(" --> done\n")
  
}


# B # SELECT THE POINTS #
#########################

# Compute number of points
if ( FALSE ) {
  nb_points = NULL
  for ( iforest in forest_list_type ) {
    nb_points = c(nb_points, sum(selectPoints[[iforest]] > limFracForest))
  }
  nb_points
}

# Extract mean LAI
if ( FALSE ) {
  if ( forest_type == "all" ) {
    list_forest = c("lc60","lc70","lc90","lc100","lc120")
  } else (
    list_forest = forest_type
  )
  
  LAI_mean = NULL
  for ( iforest_type in list_forest ) {
    
    # Select the good data
    if ( length(list_forest) >  1 ) tmp_pointsData = forestT_selectPoints[[iforest_type]][,c(1:which(colnames(selectPoints)=="climName"))] else tmp_pointsData = selectPoints[,c(1:which(colnames(selectPoints)=="climName"))]
    
    # remove points without enough data
    if ( !is.null(limFracForest) && limFracForest>0 ) tmp_pointsData = tmp_pointsData[tmp_pointsData[,"forest_frac"] > limFracForest,]
    
    LAI_tmp = LAImeanLC_Koppen(tmp_pointsData, LAIname = "LAI_max")
    
    if ( is.null(LAI_mean) ) {
      LAI_mean = cbind(LAI_mean, LAI_tmp)
    } else {
      LAI_mean_tmp = NULL
      for ( irow in unique(c(rownames(LAI_mean),names(LAI_tmp))) ) {
        if ( irow%in%rownames(LAI_mean) && is.null(LAI_tmp[[irow]]) ) {
          LAI_mean_tmp = rbind(LAI_mean_tmp, c(LAI_mean[irow,], NA) )
        } else if ( irow%in%rownames(LAI_mean) ) {
          LAI_mean_tmp = rbind(LAI_mean_tmp, c(LAI_mean[irow,], LAI_tmp[[irow]]) )
        } else {
          LAI_mean_tmp = rbind(LAI_mean_tmp, c(rep(NA, ncol(LAI_mean)), LAI_tmp[[irow]]) )
        }
      }
      rownames(LAI_mean_tmp) = unique(c(rownames(LAI_mean),names(LAI_tmp)))
      LAI_mean = LAI_mean_tmp
    }
    # LAI_mean = cbind(LAI_mean, LAI_tmp)
  }
  colnames(LAI_mean) = list_forest
}

# In case of choice of dominant forest, create the new list of data
if ( forest_type == "all" ) {
  cat("Select the main specie for each point")
  mainVeget = forestT_selectPoints[["lcForest"]][["mainForest"]]
  selectPoints = NULL
  for ( iPts in c(1:nrow(forestT_selectPoints[["lcForest"]])) ) {
    iPts_corresp = which(as.numeric(forestT_selectPoints[["lcForest"]][iPts,'x'])==as.numeric(unlist(forestT_selectPoints[[mainVeget[iPts]]][,'x'])) & as.numeric(forestT_selectPoints[["lcForest"]][iPts,'y'])==as.numeric(unlist(forestT_selectPoints[[mainVeget[iPts]]][,'y'])))
    if ( length(iPts_corresp) > 0 ) selectPoints = rbind(selectPoints, cbind(classForest = mainVeget[iPts],forestT_selectPoints[[mainVeget[iPts]]][iPts_corresp[1],]))
    # if ( forestT_selectPoints[["lcForest"]][iPts,'x']!=forestT_selectPoints[[mainVeget[iPts]]][iPts,'x'] || forestT_selectPoints[["lcForest"]][iPts,'y']!=forestT_selectPoints[[mainVeget[iPts]]][iPts,'y'] ) stop ('Error in the Matrix (not corresponding coordinate).\n')
    # selectPoints = rbind(selectPoints, forestT_selectPoints[[mainVeget[iPts]]][iPts,])
  }
  if ( nrow(selectPoints) == length(siteNames)) selectPoints = cbind(siteNames, selectPoints)
  cat(" --> done\n")
}

# selectPoints[["species"]] = "Pinus pinaster"
# selectPoints[['LAI_max']] = 3
# selectPoints[[paste0("SoilClass_0-",profondeur_soil,"00")]] = "ref" #pueachabon_ok 
# selectPoints[[SE_coarseFrag_1]] = 75
# selectPoints[[SE_coarseFrag_2]] = 82
# selectPoints[[SE_coarseFrag_3]] = 94
# selectPoints[[SE_fieldCap]]     = 0.38
# selectPoints[[SE_wiltPoint]]    = 0.12379
# selectPoints[[SE_vg_alpha]]     = 0.0005
# selectPoints[[SE_vg_n]]         = 1.55
# selectPoints[[SE_vg_I]]         = 0.5
# selectPoints[[SE_vg_ksat]]      = 1.69
# selectPoints[[SE_vg_sat_cap]]   = 0.5
# selectPoints[[SE_vg_res_cap]]   = 0.098
# # SE_depth1     = 0.5 # 0.3
# # SE_depth2     = 2   # 1
# # SE_depth3     = 5   # 2
# # SE_offSetPsoil= 0.3

if ( FALSE ) {
  # tests barbeau
  if  (!exists("selectPoints_save")) selectPoints_save = selectPoints
  iNB = which(selectPoints_save[["siteNames"]]=="FR-Barbeau")
  
  source(file.path(SurEau_mainDir,"scripts_base_simulations/Fonctions_InversionRU.R"))
  library(euptf)
  soilTextureId = c("C", "SiC", "SC", "SiCL", "CL", "SCL", "Si", "SiL", "L", "SL", "LS", "S", "O")
  isoil = paste0("0-",profondeur_soil,"00")
    # # soil_classes <- data.frame(value = seq_along(soilTextureId), soilClass = soilTextureId)
    # convFactor = 13.45750708 # cm.h-1 --> mol/s/Mpa/m
    # soilKsatUS = c(0.20*convFactor, 0.02*convFactor, 0.12*convFactor,  0.07*convFactor,  0.26*convFactor, 1.31*convFactor, 0.25*convFactor, 
    #                0.45*convFactor, 1.04*convFactor, 4.42*convFactor, 14.59*convFactor, 29.70*convFactor, 4.37*convFactor )
    # # Conversion between % soil type and soil characteristics (from Gordon Bonan, Modelling Climate Change on ecosystems 2019). α in cm-1, Ksat in mol/s/Mpa/m, Ψsat_c in cm,
    # # Organic soils are defined as soils having clay content >= 60 % clay and organic carbon >= 18 % , or having clay content < 60 % and organic carbon >= (12+clay*0.1). 
    # # But: not good correlation for organic to Ksat: ==> https://doi.org/10.5194/essd-13-1593-2021. We take the mean of other data
  selectPoints = NULL
  for ( iclass in soilTextureId ) {
    iline_tmp = selectPoints_save[iNB,]
    iline_tmp[[paste0("SoilClass_0-",profondeur_soil,"00")]] = iclass
    if ( !("l_US_0-200"%in%colnames(iline_tmp)) ) colnames(iline_tmp)[which(colnames(iline_tmp)=="I_US_0-200")] = "l_US_0-200"# A supprimer dès qu'on relance le script pour creer selectsoil...
    
    iSymbLayer = '-'
    soilType = "sub"
    if ( substr(isoil,1,1)=="0") soilType = "top"
    idepth= (as.numeric(strsplit(isoil,iSymbLayer)[[1]][2]) - as.numeric(strsplit(isoil,iSymbLayer)[[1]][1]))/100
    
    ptfList = c(NA,NA,'PTF19') #c('PTF08','PTF11','PTF19')
    soilData = data.frame(SAMPLE_ID = c(1),TOPSOIL = soilType, TEXT_US = iline_tmp[[paste0("SoilClass_",isoil)]])
    soilData = euptf_maker(soilData, ptfList, iline_tmp[[paste0("coarseFrag_",isoil,'_ori')]], idepth, 'US_', isoil, wfcSpi = 0.033, wiltSpi = 1.5848)
    
    for ( icol in c(1:ncol(soilData)) ) {
      iicol = which(colnames(iline_tmp) == colnames(soilData)[icol])
      iline_tmp[[iicol]] = soilData[[icol]]
    }
    
    selectPoints = rbind(selectPoints, iline_tmp)
  }
  
  # Add soil ref fro Puechabon_ok
  iline_tmp = selectPoints_save[iNB,]
  iline_tmp[[paste0("SoilClass_0-",profondeur_soil,"00")]] = "PuechRef" #pueachabon_ok
  if ( !("l_US_0-200"%in%colnames(iline_tmp)) ) colnames(iline_tmp)[which(colnames(iline_tmp)=="I_US_0-200")] = "l_US_0-200"# A supprimer dès qu'on relance le script pour creer selectsoil...
  iline_tmp[[SE_coarseFrag_1]] = 75
  iline_tmp[[SE_coarseFrag_2]] = 82
  iline_tmp[[SE_coarseFrag_3]] = 94
  iline_tmp[[SE_fieldCap]]     = 0.38
  iline_tmp[[SE_wiltPoint]]    = 0.12379
  iline_tmp[[SE_vg_alpha]]     = 0.0005
  iline_tmp[[SE_vg_n]]         = 1.55
  iline_tmp[[SE_vg_I]]         = 0.5
  iline_tmp[[SE_vg_ksat]]      = 1.69
  iline_tmp[[SE_vg_sat_cap]]   = 0.5
  iline_tmp[[SE_vg_res_cap]]   = 0.098
  selectPoints = rbind(selectPoints, iline_tmp)
  
  # Add soil from ESDAC (EU_soilHydrogrids_250m_France)
  iline_tmp = selectPoints_save[iNB,]
  iline_tmp[[paste0("SoilClass_0-",profondeur_soil,"00")]] = "ESDAC_sl3"
  if ( !("l_US_0-200"%in%colnames(iline_tmp)) ) colnames(iline_tmp)[which(colnames(iline_tmp)=="I_US_0-200")] = "l_US_0-200"# A supprimer dès qu'on relance le script pour creer selectsoil...
  iline_tmp[[SE_coarseFrag_1]] = 75
  iline_tmp[[SE_coarseFrag_2]] = 82
  iline_tmp[[SE_coarseFrag_3]] = 94
  iline_tmp[[SE_fieldCap]]     = 0.3
  iline_tmp[[SE_wiltPoint]]    = 0.14
  iline_tmp[[SE_vg_alpha]]     = 0.0347 #0.0163
  iline_tmp[[SE_vg_n]]         = 1.1931 # 1.2851
  iline_tmp[[SE_vg_I]]         = -4.3
  iline_tmp[[SE_vg_ksat]]      = 14.168/100*13.45750708 # 66.23/100*13.45750708
  iline_tmp[[SE_vg_sat_cap]]   = 0.4912 #0.4741
  iline_tmp[[SE_vg_res_cap]]   = 0 #0.0407
  selectPoints = rbind(selectPoints, iline_tmp)
  
  iline_tmp = selectPoints_save[iNB,]
  iline_tmp[[paste0("SoilClass_0-",profondeur_soil,"00")]] = "ESDAC_MRCsl3"
  if ( !("l_US_0-200"%in%colnames(iline_tmp)) ) colnames(iline_tmp)[which(colnames(iline_tmp)=="I_US_0-200")] = "l_US_0-200"# A supprimer dès qu'on relance le script pour creer selectsoil...
  iline_tmp[[SE_coarseFrag_1]] = 75
  iline_tmp[[SE_coarseFrag_2]] = 82
  iline_tmp[[SE_coarseFrag_3]] = 94
  iline_tmp[[SE_fieldCap]]     = 0.3
  iline_tmp[[SE_wiltPoint]]    = 0.14
  iline_tmp[[SE_vg_alpha]]     = 0.0163
  iline_tmp[[SE_vg_n]]         = 1.2851
  iline_tmp[[SE_vg_I]]         = -4.3
  iline_tmp[[SE_vg_ksat]]      = 66.23/100*13.45750708
  iline_tmp[[SE_vg_sat_cap]]   = 0.4741
  iline_tmp[[SE_vg_res_cap]]   = 0.0407
  selectPoints = rbind(selectPoints, iline_tmp)
  
  
  # selectPoints[["species"]] = "Pinus pinaster"
  selectPoints[['LAI_max']] = 3
  
  simuName = "ICOS_5m_Tests_Barbeau_LAI3"
  
}

if ( FALSE ) {
  simuName="ICOS_France_5m"
  if  (!exists("selectPoints_save")) selectPoints_save = selectPoints
  
  selectPoints = NULL
  
  iline_tmp = selectPoints_save[which(selectPoints_save[["siteNames"]]=="FR-Hess"),]
  iline_tmp[['species']] <- "Fagus sylvatica"
  iline_tmp[['LAI_max']] <- 6.5
  selectPoints = rbind(selectPoints, iline_tmp)
  
  iline_tmp = selectPoints_save[which(selectPoints_save[["siteNames"]]=="FR-Puechabon"),]
  iline_tmp[['species']] <- "Quercus ilex"
  iline_tmp[['LAI_max']] <- 2.1
  selectPoints = rbind(selectPoints, iline_tmp)
  
  iline_tmp = selectPoints_save[which(selectPoints_save[["siteNames"]]=="FR-Barbeau"),]
  iline_tmp[['species']] <- "Quercus robur"
  iline_tmp[['LAI_max']] <- 5.5
  selectPoints = rbind(selectPoints, iline_tmp)
  
  iline_tmp = selectPoints_save[which(selectPoints_save[["siteNames"]]=="FR-FondBlanche"),]
  iline_tmp[['species']] <- "Quercus ilex - Pinus halepensis"
  iline_tmp[['LAI_max']] <- 2.7
  selectPoints = rbind(selectPoints, iline_tmp)
  
  iline_tmp = selectPoints_save[which(selectPoints_save[["siteNames"]]=="FR-Bilos"),]
  iline_tmp[['species']] <- "Pinus pinaster"
  iline_tmp[['LAI_max']] <- 2.2
  selectPoints = rbind(selectPoints, iline_tmp)
  
}


# select only the points where the fraction of forest on the point is above a limit
cat("Select points where the minimum fraction of forest is OK.\n")
if ( !is.null(limFracForest) && limFracForest>0 ) selectPoints = selectPoints[c(selectPoints[,"forest_frac"] > limFracForest),]

# selectPtsExtract = selectPoints[,c(1:which(colnames(selectPoints)=="climName"),which(colnames(selectPoints)==paste0("SoilClass_",listSoils[length(listSoils)])))]
# fwrite(selectPtsExtract,file="test.csv",row.names=F,col.names=T,sep=";")


# SPLIT BEWTEEN COMPUTER / NODES
name_spliID = NULL
if ( split_simulation_NB > 1 && split_simulation_NB <= nrow(selectPoints) ) {
  selectPoints = selectPoints[split(c(1:nrow(selectPoints)), c(1:nrow(selectPoints)-1)%%split_simulation_NB+1)[[split_simulation_ID]],]
  # split(c(1:nrow(selectPoints)), sort(c(1:nrow(selectPoints))%%split_simulation_NB))
  # split(c(1:nrow(selectPoints)), rep(c(1:split_simulation_NB),floor(nrow(selectPoints)/split_simulation_NB)+1)[1:nrow(selectPoints)])
  name_spliID = paste0("splitID",split_simulation_ID,"_")
  cat('slitID = ',name_spliID, ' with ', nrow(selectPoints), ' points.\n')
}
# which(selectPoints[['x']]==-6.25&selectPoints[['y']]==40.5)
# test = selectPoints[split(c(1:nrow(selectPoints)), c(1:nrow(selectPoints)-1)%%split_simulation_NB+1)[[9]],]
# which(test[['x']]==-6.25&test[['y']]==40.5)



# C # START INVERSION SCRIPT #
##############################

# Config parallel simulations
runParallel = FALSE
if ( nbCores > 1) {
  library(parallel)
  nbCores <- min(parallel::detectCores() - freeCore, nbCores)
  if ( nbCores > 1 )
  {
    runParallel = TRUE
    library(foreach)
    library(doParallel)
    library(itertools)
  }
}

cat("START SIMULATIONS !!!\n")
if ( !runParallel ) {
  
  source(file.path(SurEau_mainDir,"Inversion_Ecos_TAW/Fonctions_InversionRU.R"))
  mainDir=SurEau_mainDir
  source(file.path(SurEau_mainDir,'functions/load.SurEau_Ecos.R')) # do not modify
  modeling_options <- create.modeling.options(transpirationModel = transpiModel, PedoTransferFormulation = methodPedoTransf, timeStepForEvapo = sureauTimeStep, compOptionsForEvapo = sureauOptionEvapo, printProg = F, thresholdMortality = thresholdMortality, resetSWC = resetSWC)
  
  TAW_results = inversionTAW(selectPoints, id_site, x_site, y_site,clim_site, forest_type, idigit,
                             QPLCl9_target, max_simu, model_varLim, QPLCl9_target_tolerence, maxCoarseFrag, iPtsDiag, save_pdf,
                             nameProject, simuName, startYear, simuNbYear, SurEau_mainDir, outputType, methodPedoTransf, soil_files_folder, surEauClimate_path, overWriteOutput, modeling_options,
                             profondeur_soil, SE_coarseFrag_1, SE_coarseFrag_2, SE_coarseFrag_3,
                             SE_fieldCap, SE_wiltPoint, SE_vg_alpha, SE_vg_n, SE_vg_I, SE_vg_ksat,
                             SE_vg_sat_cap, SE_vg_res_cap, SE_gsoil, SE_depth1, SE_depth2, SE_depth3, SE_offSetPsoil,graphPerPlot = 1)
  
  fwrite(data.frame(TAW_results), file=file.path(SurEau_mainDir,'simus',paste0(name_spliID,nameProject,'_',simuName,'_TAW_Res.csv')),row.names=F,col.names=T,sep=";")
    
} else {
  clust       <- parallel::makeCluster(nbCores)
  doParallel::registerDoParallel(clust)
  iter        <- itertools::isplitIndices(n=nrow(selectPoints), chunks = nbCores)
  index = 0
  TAW_results <- foreach::foreach(i=iter, .combine = 'rbind')%dopar%{

    source(file.path(SurEau_mainDir,"scripts_base_simulations/Fonctions_InversionRU.R"))
    loadSurEauEcos(SurEau_mainDir)
    modeling_options <- create.modeling.options(transpirationModel = transpiModel, PedoTransferFormulation = methodPedoTransf, timeStepForEvapo = sureauTimeStep, compOptionsForEvapo = sureauOptionEvapo, printProg = F, thresholdMortality = thresholdMortality, resetSWC = resetSWC)

    selectPoints_tmp = selectPoints[i,]
    TAW_results = inversionTAW(selectPoints_tmp, id_site, x_site, y_site,clim_site, forest_type, idigit,
                               QPLCl9_target, max_simu, model_varLim, QPLCl9_target_tolerence, maxCoarseFrag, iPtsDiag, save_pdf,
                               nameProject, simuName, startYear, simuNbYear, SurEau_mainDir, outputType, methodPedoTransf, soil_files_folder, surEauClimate_path, overWriteOutput, modeling_options,
                               profondeur_soil, SE_coarseFrag_1, SE_coarseFrag_2, SE_coarseFrag_3,
                               SE_fieldCap, SE_wiltPoint, SE_vg_alpha, SE_vg_n, SE_vg_I, SE_vg_ksat,
                               SE_vg_sat_cap, SE_vg_res_cap, SE_gsoil, SE_depth1, SE_depth2, SE_depth3, SE_offSetPsoil)

    return(TAW_results)
  }
  parallel::stopCluster(clust)
  
  fwrite(data.frame(TAW_results), file=file.path(SurEau_mainDir,'simus',paste0(name_spliID,nameProject,'_',simuName,'_TAW_Res.csv')),row.names=F,col.names=T,sep=";")
}


# Small script to plot #
########################
if ( FALSE ) {
  # Plot localisations of the points
  library(rgdal)
  LandMask <- readOGR("/mnt/data/KrigR/Data/Shapes","ne_10m_land", verbose = FALSE) # read
  LandMask = raster::crop(LandMask, c(min(selectPoints[['x']])-5,max(selectPoints[['x']])+5,min(selectPoints[['y']])-5,max(selectPoints[['y']])+5))
  if (save_pdf) pdf(file.path(output_Dir,paste0(outname_prefix,'_map_sites.pdf')),width=6, height=6)
  plot(LandMask)
  points(selectPoints[['x']], selectPoints[['y']], pch=21, bg=3)
  text(selectPoints[['x']], selectPoints[['y']], labels = substr(selectPoints[["siteNames"]],1,6), pos=3, offset=0.3, cex = 0.5)
  title("Forest ICOS sites")
  if (save_pdf) dev.off()
  
  
  # PLOT LAI
  if (save_pdf) pdf(file.path(output_Dir,paste0(outname_prefix,'_plot_LAImax.pdf')),width=6, height=6)
  #plot(c(1:nrow(selectPoints)),selectPoints[['LAI_max']])
  plot(selectPoints[['y']],selectPoints[['LAI_max']],pch=21, bg=3,ylab="LAI max (-)", xlab= "Longitude")
  title("quantile 0.9 of yearly LAI max")
  if ( any(colnames(selectPoints)=="siteNames") ) text(selectPoints[['y']],selectPoints[['LAI_max']], labels = substr(selectPoints[["siteNames"]],1,6), pos=3, offset=0.3, cex = 0.5)
  if (save_pdf) dev.off()
  
  
  # PLOT RU target
  if (save_pdf) pdf(file.path(output_Dir,paste0(outname_prefix,'_plot_RU_targets.pdf')),width=6, height=6)
  plot(selectPoints[['y']],RU_results[['RU_target']],pch=21, bg=3,ylab="RU (mm)", xlab= "Longitude")
  title("RU target with quantile 0.9 of PLC max")
  text(selectPoints[['y']]+0.5,RU_results[['RU_target']], labels = selectPoints[["SoilClass_0-200"]], pos=3, offset=0.2, cex = 0.5)
  if (save_pdf) dev.off()
  
  
  
  
  # "/home/ardruel/SurEau_Ecos_RU_Inversion/simus/RU_inversion_Pinus_pinaster/ICOS_5m_Pinus_pinaster_RUinversion_Res.csv"
  #  "ICOS_2models_5m_Pinus_pinaster"
  # Inverser les résultats
  RU_results_inv = RU_results_save
  #iPts = 23
  themodel1  = NULL
  themodel2 = NULL
  themodel3 = NULL
  for ( iPts in c(1:nrow(RU_results_inv))) {
    ResAnalysis = data.frame( RU = 0 , QPLCl9 = 100 )
    for ( isimu in c(1:max_simu) ) {
      if ( !is.na(RU_results_inv[iPts,paste0("RU_simu",isimu)]) ) ResAnalysis = rbind(ResAnalysis, c(RU_results_inv[iPts,paste0("RU_simu",isimu)],RU_results_inv[iPts,paste0("PLCmax_Q.9_sim",isimu)]))
    }
    RU_cible_ori = RU_results_inv[iPts,"RU_target"]
    
    siteN =""
    if ( any(colnames(selectPoints) == "siteNames")) siteN = paste0(" - ",selectPoints[["siteNames"]][iPts])
    
    if (save_pdf) pdf(file.path(output_Dir,paste0(outname_prefix,'_',selectPoints[[clim_site]][iPts],'.pdf')),width=6, height=6)
    returnModels = plotIPtsModels(ResAnalysis, QPLCl9_target, selectPoints[iPts,], paste0("Point ",locPointName(selectPoints[iPts,],paste0("%.",idigit,"f")),siteN), profondeur_soil, RU_cible=RU_cible_ori) #doModels = c(TRUE, TRUE, TRUE), RU_cible_ori=NA
    if (save_pdf) dev.off()
    
    themodel1[[iPts]] = returnModels[[1]]
    themodel2[[iPts]] = returnModels[[2]]
    themodel3[[iPts]] = returnModels[[3]]
    
    # modeSelectted = 0
    # if ( class(model1) != "try-error" )   {
    #   if (   summary(model1)$sigma < summary(model1)$sigma ) modeSelectted = 1 
    # } else modeSelectted = 1 
    # if ( modeSelectted == 0 ) {
    #   cat(" THE MODEL TRADITIONEL IS SELECTED FOR POINT",iPts,"\n") 
    #   mtext("Selected model = Sigmoide",at=max_RU*0.75, line = -4)
    # } else {
    #   cat(" The 1/x model is used here for point ",iPts,"\n")
    #   mtext("Selected model = 1/x",at=max_RU*0.75, line = -4,col="green")
    # }
    
  }
  RU_results[,1:3]
  head(selectPoints)
  
  # Plot Transpiration
  # Load data
  library(ggplot2)
  if ( forest_type == "all" ) forestClass <- as.character(unlist(selectPoints[,"classForest"])) else forestClass <- rep(forest_type,nrow(selectPoints))
  if ( "siteNames"%in%colnames(selectPoints) ) id_site = "siteNames"
  siteN =""
  output_Dir         <- file.path(SurEau_mainDir,'simus',paste0(nameProject))   #,"_",forestClass[iPts])
  for ( iPts in c(1:nrow(selectPoints)) ) {
    if ( !is.null(id_site) ) { name_site = paste0("_",selectPoints[[id_site]][iPts]) } else { name_site = "" }
    species     <- str_split(selectPoints[iPts,"species"]," - ")[[1]]
    for ( ispecie in species ) {
      specieName  <- paste(str_split(ispecie," ")[[1]],collapse = "_")
      outname_prefix     <- paste(simuName,forestClass[iPts],specieName,sep='_')
      output_path_sureau = file.path(output_Dir,paste0(outname_prefix,'_',selectPoints[[clim_site]][iPts],name_site,'_',outputType,'_',methodPedoTransf,'.csv'))
      
      simu_data = fread(output_path_sureau)
      
      if (save_pdf) pdf(file.path(output_Dir,paste0(outname_prefix,'_',selectPoints[[clim_site]][iPts],name_site,'_yearlyTranspi.pdf')),width=6, height=6)
      if ( outputType == "diagnostic_subdaily") {
        coeff = 1/100
        date_format = "%Y-%m-%d/%H:%M:%S"  #"%d %B %Y %Hh%M:%S"
        simu_data[["Time"]] = as.POSIXct(simu_data[["Time"]],format = date_format, tz = "GMT")
        perday = as.Date(simu_data[["Time"]])
        # means(simu_data[["transpiration_mm"]],perday) fonction perso
        # by(simu_data[["transpiration_mm"]],perday,mean)
        simu_data2 = data.table(Time = unique(perday), transpiration_mm = aggregate(simu_data[["transpiration_mm"]],list(perday),sum)[['x']], gs_lim =  aggregate(simu_data[["gs_lim"]],list(perday),max)[['x']])
        theplot <- ggplot()+ geom_line(data=simu_data2, aes(x=Time, y=transpiration_mm)) + stat_smooth(data=simu_data2, aes(x=Time, y=transpiration_mm)) + # , color=plot_col[isimu]) + scale_colour_manual("", breaks =  names(plot_col),values = plot_col)
          geom_line(data=simu_data2, aes(x=Time, y=gs_lim*coeff), color="red" , lty=2)  #+# color=plot_col[isimu],
        name1 = "transpiration (mm)"
        name2 = "Gs lim (max)"
        max_x = max(aggregate(simu_data[["transpiration_mm"]],list(perday),sum)[['x']])
      } else if ( outputType == "yearly_forSA") {
        coeff = -50
        theplot <- ggplot()+ geom_line(data=simu_data, aes(x=Time, y=yearly_transpiration_mm)) + stat_smooth(data=simu_data, aes(x=Time, y=yearly_transpiration_mm)) + # , color=plot_col[isimu]) + scale_colour_manual("", breaks =  names(plot_col),values = plot_col)
                            geom_line(data=simu_data, aes(x=Time, y=yearly_Psi_LApoMin*coeff), color="red" , lty=2)  #+# color=plot_col[isimu],
        name1 = "yearly transpiration (mm)"
        name2 = "Psi LApoMin"
        max_x = max(simu_data[["yearly_transpiration_mm"]])
      }

        # geom_label(data= data.table(data_tmp[nrow(data_tmp),], iname = iname),aes(x=Time, y=daily_Psi_LApoMin*coeff,label = iname),
        #            size = 3 , fill=NA, label.size = NA)
      if ( any(colnames(selectPoints) == "siteNames")) siteN = paste0(" - ",selectPoints[["siteNames"]][iPts])
      theplot <- theplot + xlab("year") +
        scale_y_continuous(name = name1,  sec.axis = sec_axis(~./coeff, name=name2), limits = c(0,NA))  +
        theme_bw() + # ylim(0,NA) +
        theme( axis.title.y = element_text(color = "blue", size=13),
               axis.text.y.left = element_text(color = "blue"),
               axis.text.y.right = element_text(color = "red"),
               axis.title.y.right = element_text(color = 'red', size=13),
               legend.position = c(0.15,0.82),
               plot.title = element_text(hjust = 0.5),
               plot.subtitle = element_text(hjust = 0.5)) + # ylab(jvar)
        labs(title=paste0("Point ",locPointName(selectPoints[iPts,],paste0("%.",idigit,"f")),siteN)) #,typeTestName,")")) #, subtitle =  typeTestName) 
      if ( outputType == "diagnostic_subdaily") {
        theplot <- theplot +  annotate(geom="text", x=perday[round(length(perday)*0.85)], y=max_x*1,    label=paste0("LAI max = ",round(selectPoints[["LAI_max"]][iPts],1))) #,   color="red")
        theplot <- theplot +  annotate(geom="text", x=perday[round(length(perday)*0.85)], y=max_x*0.95, label=paste0("soil = ",selectPoints[[paste0("SoilClass_0-",profondeur_soil,"00")]][iPts])) #,   color="red")
        theplot <- theplot +  annotate(geom="text", x=perday[round(length(perday)*0.85)], y=max_x*0.9 , label=paste0("Specie = ",ispecie)) #,   color="red")
        theplot <- theplot +  annotate(geom="text", x=perday[round(length(perday)*0.85)], y=max_x*0.85, label=paste0("Year = ",year(perday[1])),   color="red")
        theplot <- theplot +  annotate(geom="text", x=perday[round(length(perday)*0.85)], y=max_x*0.8, label=paste0("Tot transp. = ",round(sum(simu_data[["transpiration_mm"]])),' mm'),   color="blue")
      }
      # mtext(paste0("LAI max = ",round(selectPoints[["LAI_max"]][iPts],1)),at=max_x*0.75, line = -1)
      print(theplot)
      if (save_pdf) dev.off()
      
    }
  }
  
  
  
  # plotKoppen
  climate_map = loadKoppen(pathClimateClassfile)
  plot(climate_map)
  
  
  climate_map = crop(climate_map,workPoints)
  test = levels(climate_map)
  values(climate_map)[values(climate_map)==4 | values(climate_map)==5] = 0
  values(climate_map)[values(climate_map)==6 | values(climate_map)==7 | values(climate_map)==7 | values(climate_map)==8 | values(climate_map)==9 | values(climate_map)==14 | values(climate_map)==17 | values(climate_map)==18 | values(climate_map)==19] = 1
  values(climate_map)[values(climate_map)==15 | values(climate_map)==16] = 2
  values(climate_map)[values(climate_map)==25 | values(climate_map)==26] = 3
  values(climate_map)[values(climate_map)==27 | values(climate_map)==29 | values(climate_map)==30] = 4
  values(climate_map)[values(climate_map)==31 | values(climate_map)==32] = 5
  levels(climate_map) =  data.frame(bG_code  = c(0:5),
                                    bG_class = c("Desert","Xerophyllous mediterranean","Temperate oceanic","Central continental","Boreal continental","High mountain"))
  # plot(climate_map)
  # bioGeoColor = grDevices::hcl.colors(6, "viridis")
  # bioGeoColor[1] = "azure2"
  bioGeoColor = c("brown1","darkgoldenrod1","chartreuse3","cyan3","darkcyan","darkorchid")
  plot(climate_map, col = bioGeoColor)

}