# TO launch Test simulations !



library(readxl)
library(data.table)
library(ggpubr)
library(stringr)



#"SureauSoilData_[...]5.2E-48.5N.csv"

simus_folder = "/home/ardruel/SurEau_Ecos_RU_Inversion/simus/SpeciesTest"
speciesLAIFile = "/home/ardruel/Téléchargements/The_table/vegetation_Parameters_MultiSpecies.xlsx" # Only for variable LAI




simuNbYear     <- 1
startYear      <- 1990
x_site         <- 6.3500000
y_site         <- 8.7436111
SurEau_mainDir    <- file.path("/home/ardruel/SurEau_Ecos_RU_Inversion") #"SurEau-Ecos-master")

# Simulation CONFIG
surEauClimate_path <- file.path(simus_folder, "Climat_constant_test_champenoux.csv") #pathHome,"data/climate",regionNameClim,paste0(climInputName,'_',climInputType,nameMnt))#"data/climate/Forgenius_Era5land/ERA_land_sureau_PtsEx_daily")
soilParameters_path<- file.path(simus_folder, "SureauSoilData_test.csv") #RU320
methodPedoTransf   <- "VG" #Campbell" #"VG"
transpiModel       <- 'Jarvis' #'Granier'
sureauTimeStep     <- 2 #defaul = 1
sureauOptionEvapo  <- 'Fast'
overWriteOutput    <- TRUE
thresholdMortality <- 100 
resetSWC           <- T # fill the soil with water at the beginning of each year (mainly for the case where the water resources are external (not rain))
outputType         <- "simple_daily" # "diagnostic_subdaily"  "LFMC_subdaily"  "simple_daily"  "simple_subdaily"  "simple_yearly"  "yearly_forSA"
constantClimate    <- TRUE

save_pdf = TRUE
plotAllinOne = TRUE
ForceEvergreen = TRUE

addName = ""



selectSpeciesFull <- function(forest_type, koppenClass)
{
  
  if ( forest_type == "lc60" ) {
    if ( koppenClass == "Xero_med" ) {             # Vegetation xérophylle Méditeranéenne
      ispecie = "Quercus pubescens"
    } else if ( koppenClass == "Temp_oce" ) {      # Végétation océanique tempérée
      ispecie = c("Quercus robur", "Fagus sylvatica") # In second "Quercus petraea" is missing
    } else if ( koppenClass == "Cent_cont" ) {     # Végétation continentale centrale
      ispecie = c("Fagus sylvatica", "Quercus robur")
    } else if ( koppenClass == "Bor_cont" ) {      # Végétation continentale boréale
      ispecie = c("Betula pendula", "Fagus sylvatica") # Because "Betula pubescens" is missing
    } else if ( koppenClass == "H_mount" ) {       # Vegetation haute Montagne
      ispecie = "Fagus sylvatica" # "Sorbus aucuparia" is missing
    } else ispecie = NA  # nothing + desert (BWh - BWk) 
  } else if ( forest_type == "lc70" ) {
    if ( koppenClass == "Xero_med" ) {             # Vegetation xérophylle Méditeranéenne
      ispecie = "Pinus halepensis" # "Pinus pinaster" is missing
    } else if ( koppenClass == "Temp_oce" ) {      # Végétation océanique tempérée
      ispecie = c("Pinus sylvestris", "Picea abies", "Abies alba")
    } else if ( koppenClass == "Cent_cont" ) {     # Végétation continentale centrale
      ispecie = c("Abies alba", "Picea abies", "Pinus sylvestris")
    } else if ( koppenClass == "Bor_cont" ) {      # Végétation continentale boréale
      ispecie = c("Pinus sylvestris", "Picea abies", "Larix decidua")
    } else if ( koppenClass == "H_mount" ) {       # Vegetation haute Montagne
      ispecie = c("Abies alba", "Larix decidua") # "Pinus uncinata" is missing
    } else ispecie = NA
  } else if ( forest_type == "lc90" ) {
    if ( koppenClass == "Xero_med" ) {             # Vegetation xérophylle Méditeranéenne
      ispecie = c("Quercus ilex","Pinus halepensis")
    } else if ( koppenClass == "Temp_oce" ) {      # Végétation océanique tempérée
      ispecie = c("Quercus robur", "Pinus sylvestris", "Fagus sylvatica", "Picea abies")
    } else if ( koppenClass == "Cent_cont" ) {     # Végétation continentale centrale
      ispecie = c("Fagus sylvatica","Abies alba")
    } else if ( koppenClass == "Bor_cont" ) {      # Végétation continentale boréale
      ispecie = c("Betula pendula","Pinus sylvestris")  # First "Betula pendula" Because "Betula pubescens" is missing
    } else if ( koppenClass == "H_mount" ) {       # Vegetation haute Montagne
      ispecie = c("Fagus sylvatica","Abies alba")
    } else ispecie = NA
  } else if ( forest_type == "lc100" ) {
    if ( koppenClass == "Xero_med" ) {             # Vegetation xérophylle Méditeranéenne
      ispecie = c("Quercus ilex", "Juniperus communis") # "Juniperus communis" Because Juniperus oxycedrus is missing
    } else if ( koppenClass == "Temp_oce" ) {      # Végétation océanique tempérée
      ispecie = c("Quercus robur", "Fagus sylvatica", "Corylus avellana") # Missing in last "Carpinus betulus"
    } else if ( koppenClass == "Cent_cont" ) {     # Végétation continentale centrale
      ispecie = "Quercus robur"
    } else if ( koppenClass == "Bor_cont" ) {      # Végétation continentale boréale
      ispecie = "Betula pendula" # Because "Betula pubescens" is missing
    } else if ( koppenClass == "H_mount" ) {       # Vegetation haute Montagne
      ispecie = c("Fagus sylvatica", "Abies alba")
    } else ispecie = NA
  } else if ( forest_type == "lc120" ) {
    if ( koppenClass == "Xero_med" ) {             # Vegetation xérophylle Méditeranéenne
      ispecie = c("Quercus ilex", "Juniperus communis") # "Juniperus communis" Because Juniperus oxycedrus is missing
    } else if ( koppenClass == "Temp_oce" ) {      # Végétation océanique tempérée
      ispecie = "Corylus avellana" # "Carpinus betulus" and "Cornus sanguinea" are missing
    } else if ( koppenClass == "Cent_cont" ) {     # Végétation continentale centrale
      ispecie = "Corylus avellana"
    } else if ( koppenClass == "Bor_cont" ) {      # Végétation continentale boréale
      ispecie = "Betula pendula" # Because "Betula pubescens" is missing
    } else if ( koppenClass == "H_mount" ) {       # Vegetation haute Montagne
      ispecie = "Juniperus communis" # Rhododendron is missing
    } else ispecie = NA
  } else stop('The forest type is not yet define in "selectSpecies" function')
  # if ( is.na(ispecie)) cat(koppenClass,"  ")
  
  return(ispecie)
}


loadSurEauEcos <- function(SurEau_mainDir)
{
  #load functions and libraries for SurEau-Ecos 
  #Should be deleted for packaging 
  
  # Required libraries----------------------------------------------------------
  library(insol)
  library(lubridate)
  
  
  # load functions ----------------------------------------------------------
  source(paste0(SurEau_mainDir, "/functions/run.SurEau_Ecos.R"))  
  source(paste0(SurEau_mainDir, "/functions/functionsWBveg.R"))
  source(paste0(SurEau_mainDir, "/functions/functionsWBoutput.R"))
  source(paste0(SurEau_mainDir, "/functions/functionsWBsoil.R"))
  source(paste0(SurEau_mainDir, "/functions/functionsWBclim.R"))
  
  source(paste0(SurEau_mainDir, "/functions/create.modeling.options.R"))
  source(paste0(SurEau_mainDir, "/functions/create.simulation.parameters.R"))
  source(paste0(SurEau_mainDir, "/functions/create.climate.data.R"))
  source(paste0(SurEau_mainDir, "/functions/create.stand.parameters.R"))
  source(paste0(SurEau_mainDir, "/functions/create.soil.parameters.R"))
  source(paste0(SurEau_mainDir, "/functions/create.vegetation.parameters.R"))
  
  source(paste0(SurEau_mainDir, "/functions/climate.utils.R"))
  source(paste0(SurEau_mainDir, "/functions/plant.utils.R"))
  source(paste0(SurEau_mainDir, "/functions/soil.utils.R"))
  source(paste0(SurEau_mainDir, "/functions/plot.utils.R"))
}


SureauLauncherTest <- function(LAImax, x_site, y_site, startYear, simuNbYear, pathSureauSoil, climateData_path, output_path, 
                               outputType, mainDir, resolutionOutput, overWriteOutput, modeling_options, vegetationParameters_path)
{
  
  # Create input list files and define options -----------------------------------
  simulation_parameters <- create.simulation.parameters(startYearSimulation = startYear,                       
                                                        endYearSimulation = startYear+simuNbYear-1,
                                                        mainDir = mainDir,
                                                        resolutionOutput = resolutionOutput,       # "subdaily", "daily",  "yearly"
                                                        outputType = outputType,        # diagnostic_subdaily  LFMC_subdaily  simple_daily  simple_subdaily  simple_yearly  yearly_forSA
                                                        overWrite = overWriteOutput,
                                                        outputPath = output_path)
  climate_data          <- create.climate.data(filePath = climateData_path, 
                                               modeling_options = modeling_options,
                                               simulation_parameters = simulation_parameters) #
  stand_parameters      <- create.stand.parameters(LAImax = round(LAImax,2), lat = y_site, lon = x_site)
  soil_parameters       <- create.soil.parameters(filePath = pathSureauSoil, 
                                                  modeling_options = modeling_options) 
  vegetation_parameters <- create.vegetation.parameters(filePath = vegetationParameters_path, 
                                                        stand_parameters = stand_parameters, 
                                                        soil_parameter = soil_parameters,
                                                        modeling_options = modeling_options)
  
  
  # run SurEau-Ecos --------------------------------------------------------------
  run.SurEau_Ecos(modeling_options = modeling_options ,
                  simulation_parameters = simulation_parameters, 
                  climate_data = climate_data,
                  stand_parameters = stand_parameters, 
                  soil_parameters = soil_parameters,
                  vegetation_parameters = vegetation_parameters)
}



typeTestName = "LAI_cst"
# typeTestName = "LAI_esp"
# typeTestName = "LAI_esp_Mean"
if ( typeTestName == "LAI_cst" ) {
  LAImaxCST = 4
  listSpecies = c("Abies alba", "Betula pendula", "Corylus avellana", "Fagus sylvatica", "Juniperus communis", "Larix decidua", "Picea abies", "Pinus halepensis", "Pinus nigra", "Pinus sylvestris", "Quercus ilex", "Quercus pubescens", "Quercus robur", "Pinus uncinata", "Pinus pinaster" )
  transpiModel       <- c('Jarvis','Granier','Granier','Granier','Jarvis','Jarvis','Jarvis','Jarvis','Jarvis','Jarvis','Granier','Granier','Granier','Jarvis','Jarvis')
  thresholdMortality <- c(50,100,100,100,50,50,50,50,50,50,100,100,100,50,50)
} else if ( str_sub(typeTestName,1,7) == "LAI_esp" ) 
  {
  LAI_LC_Koppen = data.table(read_excel(speciesLAIFile, sheet = "LAI_FOREST_Europe"))
  list_forest = c("Tree broadleaved deciduous", "Tree needleleaved evergreen", "Tree mixed", "Mosaic tree and shrub", "Shrubland")
  list_forest_cor = c("lc60","lc70","lc90","lc100","lc120")
  list_KoppenBGC = c("Xerophyllous mediterranean", "Temperate oceanic", "Central continental", "Boreal continental", "High mountain")
  list_KoppenBGC_cor = c("Xero_med", "Temp_oce", "Cent_cont", "Bor_cont", "H_mount")
  listSimu = NULL
  for ( iforest in LAI_LC_Koppen[[1]] ) { 
    for ( iKoppen in colnames(LAI_LC_Koppen)[-1]) {
      
      listSpecies = selectSpeciesFull(list_forest_cor[which(list_forest==iforest)], list_KoppenBGC_cor[which(list_KoppenBGC == iKoppen)])
      for ( ispecies in listSpecies ) {
        if ( LAI_LC_Koppen[[iKoppen]][which(c(LAI_LC_Koppen[,1])[[1]]==as.character(iforest))]!="NA" )
          listSimu = rbind(listSimu, c(ispecies, iforest, iKoppen, LAI_LC_Koppen[[iKoppen]][which(c(LAI_LC_Koppen[,1])[[1]]==as.character(iforest))]))
        # listSimu = rbind(listSimu, c(ispecies, list_forest_cor[which(list_forest==iforest)], list_KoppenBGC_cor[which(list_KoppenBGC == iKoppen)],
        #                              LAI_LC_Koppen[[iKoppen]][which(c(LAI_LC_Koppen[,1])[[1]]==as.character(iforest))]))
      }
    }
  }
  listSpecies = listSimu[,1]
  if ( typeTestName == "LAI_esp_Mean" ) {
    listSimu_tmp = NULL
    for ( ispecies in unique(listSpecies) ) {
      listSimu_tmp = rbind(listSimu_tmp, c(ispecies, mean(as.numeric(listSimu[listSimu[,1]==ispecies,4])) ))
    }
    listSimu = listSimu_tmp
    listSpecies = listSimu[,1]
    colnames(listSimu) = c("Specie", "LAI")
  } else {
    colnames(listSimu) = c("Specie", "Forest", "Koppen", "LAI")
  }
  # listSimu[,"LAI"] = as.numeric(listSimu[,"LAI"])
} else { stop("Wrong typeTestName!") }

# surEauClimate_path <- file.path(simus_folder, "ERA_Puechabon_3.5E-43.8N_1990-2022.csv") #pathHome,"data/climate",regionNameClim,paste0(climInputName,'_',climInputType,nameMnt))#"data/climate/Forgenius_Era5land/ERA_land_sureau_PtsEx_daily")
soilParameters_path<- file.path(simus_folder, "Soil_Puechabon_OK_LFMC_VG2.csv") #RU100
# soilParameters_path<- file.path(simus_folder, "SureauSoilData-Puechabon_RU130_lc70_3.5E-43.8N.csv")
# soilParameters_path<- file.path(simus_folder, "SureauSoilData-Puechabon_RU130_2.5m_lc70_3.5E-43.8N.csv")
# soilParameters_path<- file.path(simus_folder, "Soil_Generic_Pierroton.csv")
# vegetationParameters_path <- file.path(simus_folder, "vegetation_Pinus_halepensis_mod.csv")
# constantClimate = FALSE
# listSpecies     = "Pinus halepensis"
# typeTestName    = "test_Puechabon"
# ForceEvergreen  = FALSE
# LAImax = 3
# addName = "vegetModv1_soilNew" # addName = "vegetModv1_soilRU130"  addName = "vegetModv1_soilRU130_depth2.5" addName = "vegetModv1_soilPierroton"
# outputType         <- "diagnostic_subdaily" 
# addName = "climChampenoux_soilPuechOK_transpiVar_mortalityVar"

loadSurEauEcos(SurEau_mainDir)
if ( outputType %in% c("diagnostic_subdaily", "LFMC_subdaily", "simple_subdaily")){
  resolutionOutput = "subdaily"
} else if ( outputType %in% c("simple_daily")) {
  resolutionOutput = "daily"
} else if ( outputType %in% c("simple_yearly", "yearly_forSA")) {
  resolutionOutput = "yearly"
} 

if ( !dir.exists(file.path(simus_folder,typeTestName)) ) dir.create(file.path(simus_folder,typeTestName))

lastDaySpecies = NULL
allData = NULL
speData = NULL
for ( ispecieNB in seq_along(listSpecies) ) {
  ispecie = listSpecies[ispecieNB]
  
  if (length(transpiModel)>1) transpiModel_tmp = transpiModel[ispecieNB] else transpiModel_tmp = transpiModel
  if (length(thresholdMortality)>1) thresholdMortality_tmp = thresholdMortality[ispecieNB] else thresholdMortality_tmp = thresholdMortality
  modeling_options <- create.modeling.options(transpirationModel = transpiModel_tmp , PedoTransferFormulation = methodPedoTransf, timeStepForEvapo = sureauTimeStep, compOptionsForEvapo = sureauOptionEvapo, printProg = F, thresholdMortality = thresholdMortality_tmp, resetSWC = resetSWC, constantClimate = constantClimate)
  
  specieName  <- paste(str_split(ispecie," ")[[1]],collapse = "_")
  
  vegetationParameters_path <- file.path(simus_folder,"vegetation_parameters",paste0('vegetation_',specieName,'.csv'))
  
  outname_prefix <- paste(typeTestName,specieName,addName,sep='_')
  plot_title     <- ispecie
  
  if ( ForceEvergreen ) {
    if ( TRUE ) { # Normal way
      wasDeciduous = FALSE
      vegetParam = fread(vegetationParameters_path)
      ifoliage = which(vegetParam[,1]=="Foliage")

      if ( vegetParam[ifoliage,2]=="Deciduous" ) {
        wasDeciduous = TRUE
        vegetParamEver = vegetParam
        # iparam = which(vegetParam[,1]=="fRootToLeaf")
        # vegetParamEver[iparam,2] = 0.0002
        vegetParamEver[ifoliage,2] = "Evergreen"

        # # To test others parameters
        # outname_prefix = paste(outname_prefix,'test',sep='_')
        # vegetParamEver[which(vegetParam[,1]=="P50_VC_leaf"),2] = "-2.5"
        # vegetParamEver[which(vegetParam[,1]=="P50_VC_Stem"),2] = "-2.5"
        # vegetParamEver[which(vegetParam[,1]=="P88_gs"),2] = "-1.8"

        fwrite(vegetParamEver, file=vegetationParameters_path,row.names=F,col.names=T,sep=";")
        outname_prefix <- paste(outname_prefix,"Evergreen",sep='_')
        plot_title     <- paste(plot_title,"Evergreen",sep='_')
      }
    } else {
      vegetParam = fread(vegetationParameters_path)
      ifoliage = which(vegetParam[,1]=="Foliage")
      wasDeciduous = TRUE
      vegetParamEver = vegetParam
      if ( vegetParam[ifoliage,2]=="Deciduous" )  {
        vegetParamEver[ifoliage,2] = "Evergreen"
        outname_prefix <- paste(outname_prefix,"Evergreen",sep='_')
        plot_title     <- paste(plot_title,"Evergreen",sep='_')
      }
      
      # # To test others parameters
      # outname_prefix = paste(outname_prefix,'test',sep='_')
      # vegetParamEver[which(vegetParam[,1]=="P50_VC_leaf"),2] = "-2.5"
      # vegetParamEver[which(vegetParam[,1]=="P50_VC_Stem"),2] = "-2.5"
      # vegetParamEver[which(vegetParam[,1]=="P88_gs"),2] = "-1.8"
      vegetParamEver[which(vegetParam[,1]=="fRootToLeaf"),2] = "0.0002" 
      
      fwrite(vegetParamEver, file=vegetationParameters_path,row.names=F,col.names=T,sep=";")

    }
  }
  
  if ( typeTestName == "LAI_cst" ) {
    LAImax         <- LAImaxCST
  } else if ( str_sub(typeTestName,1,7) == "LAI_esp" ) {
    LAImax         <- as.numeric(listSimu[ispecieNB,"LAI"])
    if ( typeTestName == "LAI_esp" ) {
      plot_title     <- paste(plot_title, list_forest_cor[which(list_forest==listSimu[ispecieNB,"Forest"])], list_KoppenBGC_cor[which(list_KoppenBGC == listSimu[ispecieNB,"Koppen"])],sep=" - ")
      outname_prefix <- paste(outname_prefix,list_forest_cor[which(list_forest==listSimu[ispecieNB,"Forest"])], list_KoppenBGC_cor[which(list_KoppenBGC == listSimu[ispecieNB,"Koppen"])],sep='_')
    } else {
      plot_title     <- paste(plot_title, "LAI mean",sep=" - ")
      outname_prefix <- paste(outname_prefix, "mean" ,sep='_')
    }
  }
  output_path <- file.path(simus_folder,typeTestName,paste0(outname_prefix,'_',outputType,'_',methodPedoTransf,'.csv'))
  
  
  SureauLauncherTest(LAImax, x_site, y_site, startYear, simuNbYear, soilParameters_path, surEauClimate_path, output_path, 
                     outputType, SurEau_mainDir, resolutionOutput, overWriteOutput, modeling_options, vegetationParameters_path)
  
  if ( ForceEvergreen && wasDeciduous ) fwrite(vegetParam, file=vegetationParameters_path,row.names=F,col.names=T,sep=";")
  
  simuResults = fread(output_path)
  lastDay = as.Date(as.character(simuResults[nrow(simuResults),"Time"]))
  
  if ( typeTestName == "LAI_cst" ) {
    lastDaySpecies=rbind(lastDaySpecies, data.table(species = ispecie, nb_day = nrow(simuResults),last_day = format(lastDay, "%d-%b")) )
  } else if ( typeTestName == "LAI_esp" ) {
    lastDaySpecies=rbind(lastDaySpecies, data.table(species = ispecie, forest_type = listSimu[ispecieNB,"Forest"], BioGeo_Koppen = listSimu[ispecieNB,"Koppen"], LAImax = LAImax,  nb_day = nrow(simuResults), last_day = format(lastDay, "%d-%b")) )
  } else {
    lastDaySpecies=rbind(lastDaySpecies, data.table(species = ispecie, LAImax = LAImax,  nb_day = nrow(simuResults), last_day = format(lastDay, "%d-%b")) )
  }

  if (save_pdf) {
    data_tmp = simuResults # data.frame(Time = as.Date(dataTime), value = dataVar)
    data_tmp[["Time"]] = as.Date(data_tmp[["Time"]])
    
    if ( outputType == "simple_daily" ) {
      coeff = -1/4
      pdf(file.path(simus_folder,typeTestName,paste0("plot_",outname_prefix,'_',outputType,'_',methodPedoTransf,'.pdf')),width=6, height=6)
      theplot  <- ggplot(data = data_tmp)+
        geom_line(aes(x=Time, y=daily_transpiration_mm),color="#69b3a2") + 
        geom_line(aes(x=Time, y=daily_Psi_LApoMin*coeff),color="blue") + 
        # geom_line(data = jobsAFAM2, aes(x = data_date, y = Percent.Change), color = "blue") +
        xlab("") + scale_x_date(date_breaks = "1 month", date_labels = "%b") +
        # ylim(0,NA) +
        scale_y_continuous(name = "daily transpiration (mm)",  sec.axis = sec_axis(~./coeff, name="Ps LApoMin"))  + 
        theme_bw() +
        theme( axis.line.y.left = element_line(color = "#69b3a2"), 
               axis.ticks.y.left = element_line(color = "#69b3a2"),
               axis.text.y.left = element_text(color = "#69b3a2"),
               axis.title.y = element_text(color = "#69b3a2", size=13),
               axis.line.y.right = element_line(color = "blue"), 
               axis.ticks.y.right = element_line(color = "blue"),
               axis.text.y.right = element_text(color = "blue"),
               axis.title.y.right = element_text(color = "blue", size=13),
               plot.title = element_text(hjust = 0.5),
               plot.subtitle = element_text(hjust = 0.5)) + # ylab(jvar) 
        labs(title=plot_title, subtitle = paste0("(LAImax=", round(LAImax,2),", last day= ",format(lastDay, "%b %d"),")" )) 
      print(theplot)
      dev.off()
    } else if ( outputType == "diagnostic_subdaily" ) {
      
      pdf(file.path(simus_folder,typeTestName,paste0("plot_Psi_",outname_prefix,'_',outputType,'_',methodPedoTransf,'.pdf')),width=12, height=6)
      theplot  <- ggplot(data = data_tmp)+
        geom_line(aes(x=Time, y=Psi_LSym, colour = "LSym")) + 
        geom_line(aes(x=Time, y=PsiSoil1, colour = "Soil1")) + 
        geom_line(aes(x=Time, y=PsiSoil2, colour = "Soil2")) + 
        geom_line(aes(x=Time, y=PsiSoil3, colour = "Soil3")) + 
        scale_color_manual(name = "", values = c("LSym" = "deeppink4", "Soil1" = "aquamarine3", "Soil2" = "darkcyan", "Soil3" = "darkolivegreen")) +
        # geom_line(data = jobsAFAM2, aes(x = data_date, y = Percent.Change), color = "blue") +
        xlab("") + scale_x_date(date_breaks = "1 month", date_labels = "%b") +
        scale_y_continuous(name = "Psi soil")  + 
        theme_bw() +
        theme( axis.title.y = element_text(size=13),
               plot.title = element_text(hjust = 0.5),
               plot.subtitle = element_text(hjust = 0.5)) + # ylab(jvar) 
        labs(title=plot_title, subtitle = paste0("(LAImax=", round(LAImax,2),", last day= ",format(lastDay, "%b %d"),")" )) 
      print(theplot)
      dev.off()
      
      pdf(file.path(simus_folder,typeTestName,paste0("plot_Elim_",outname_prefix,'_',outputType,'_',methodPedoTransf,'.pdf')),width=12, height=6)
      theplot  <- ggplot(data = data_tmp)+
        geom_line(aes(x=Time, y=Elim), color="#69b3a2") + 
        # geom_line(data = jobsAFAM2, aes(x = data_date, y = Percent.Change), color = "blue") +
        xlab("") + scale_x_date(date_breaks = "1 month", date_labels = "%b") +
        scale_y_continuous(name = "E_lim")  + 
        theme_bw() +
        theme( axis.title.y = element_text(size=13),
               plot.title = element_text(hjust = 0.5),
               plot.subtitle = element_text(hjust = 0.5)) + # ylab(jvar) 
        labs(title=plot_title, subtitle = paste0("(LAImax=", round(LAImax,2),", last day= ",format(lastDay, "%b %d"),")" )) 
      print(theplot)
      dev.off()
      
      pdf(file.path(simus_folder,typeTestName,paste0("plot_gs_lim_",outname_prefix,'_',outputType,'_',methodPedoTransf,'.pdf')),width=12, height=6)
      theplot  <- ggplot(data = data_tmp)+
        geom_line(aes(x=Time, y=gs_lim), color="#69b3a2") + 
        # geom_line(data = jobsAFAM2, aes(x = data_date, y = Percent.Change), color = "blue") +
        xlab("") + scale_x_date(date_breaks = "1 month", date_labels = "%b") +
        scale_y_continuous(name = "gs_lim")  + 
        theme_bw() +
        theme( axis.title.y = element_text(size=13),
               plot.title = element_text(hjust = 0.5),
               plot.subtitle = element_text(hjust = 0.5)) + # ylab(jvar) 
        labs(title=plot_title, subtitle = paste0("(LAImax=", round(LAImax,2),", last day= ",format(lastDay, "%b %d"),")" )) 
      print(theplot)
      dev.off()
    }
    
  }
  
  if ( plotAllinOne ) {
    data_tmp = simuResults[,c("Time","daily_transpiration_mm","daily_Psi_LApoMin")] # data.frame(Time = as.Date(dataTime), value = dataVar)
    data_tmp[["Time"]] = as.Date(data_tmp[["Time"]])
    allData[[outname_prefix]] = data_tmp
    
    speData_tmp = fread(vegetationParameters_path)
    speData = rbind(speData,data.table(specie = ispecie, P50 = as.numeric(speData_tmp[which(speData_tmp[,1]=="P50_VC_Stem"),2]), Pgs88 = as.numeric(speData_tmp[which(speData_tmp[,1]=="P88_gs"),2])))
  }

}
if (!ForceEvergreen) ALL_simu_name = "ALL_simus_" else ALL_simu_name = "ALL_simus_Evergreen_"
if (addName!="") ALL_simu_name = paste0(ALL_simu_name,addName,"_")
fwrite(lastDaySpecies, file.path(simus_folder,typeTestName,paste0(ALL_simu_name,typeTestName,"_",methodPedoTransf,'.csv')),row.names=F,col.names=T,sep=";")

if ( plotAllinOne ) {
  pdf(file.path(simus_folder,typeTestName,paste0(ALL_simu_name,"plot_",typeTestName,'_',methodPedoTransf,'.pdf')),width=12, height=12)
  coeff = -1/5
  theplot  <- ggplot()
  plot_col = viridis::turbo(length(allData))
  names(plot_col) = listSpecies  #names(allData)
  for ( isimu in seq_along(names(allData)) ) {
    iname = names(allData)[isimu]
    lname = paste0(listSpecies[isimu],"")
    data_tmp = allData[[iname]]
    data_tmp[['Time']] = day(days(data_tmp[['Time']]-data_tmp[['Time']][1])) # remove the date
    theplot <- theplot + geom_line(data=data_tmp, aes(x=Time, y=daily_transpiration_mm), color=plot_col[isimu]) + #,  color=plot_col[isimu] ) +
                         geom_line(data=data_tmp, aes(x=Time, y=daily_Psi_LApoMin*coeff), color=plot_col[isimu], lty=2)  + #
                         geom_label(data= data.table(data_tmp[nrow(data_tmp),], iname = lname),aes(x=Time, y=daily_Psi_LApoMin*coeff,label = iname),
                                    size = 3 , fill=NA, label.size = NA)
      # scale_colour_manual("", breaks =  names(plot_col),values = plot_col)
  }
  theplot <- theplot+ geom_line(data=data_tmp[1:2,],aes(x=Time, y=daily_Psi_LApoMin*coeff,colour = "")) + scale_colour_manual("", breaks =  names(plot_col),values = plot_col)  + xlab("")
  if ( is.Date(data_tmp[['Time']]) ) theplot <- theplot + scale_x_date(date_breaks = "1 month", date_labels = "%b")
  theplot <- theplot +
      # ylim(0,NA) +
      scale_y_continuous(name = "daily transpiration (mm)",  sec.axis = sec_axis(~./coeff, name="Ps LApoMin"))  + 
      theme_bw() +
      theme( axis.title.y = element_text(size=13),
             axis.title.y.right = element_text(size=13),
             legend.position = c(0.15,0.82),
             plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5)) + # ylab(jvar) 
      labs(title=paste0("Plot all simulations (",typeTestName,")")) #, subtitle =  typeTestName)
    print(theplot)

  dev.off()
  

  pdf(file.path(simus_folder,typeTestName,paste0(ALL_simu_name,"barplot_",typeTestName,'_',methodPedoTransf,'.pdf')),width=6, height=8)
  
  ratioUpDown = 2
  theplot1  <- ggplot(data = lastDaySpecies, aes(x=species, y=nb_day))+
    geom_bar( stat="identity") +
    geom_text(data = lastDaySpecies[lastDaySpecies[,nb_day]>=max(lastDaySpecies[,nb_day])/ratioUpDown,],aes(label=species), color="white", angle = 90, hjust = 1.2,  size=3.5) + # au dessus vjust -0.3 en dessous 1.6
    geom_text(data = lastDaySpecies[lastDaySpecies[,nb_day]<max(lastDaySpecies[,nb_day])/ratioUpDown,], aes(label=species), color="black", angle = 90, hjust = -0.1, size=3.5) + # au dessus vjust -0.3 en dessous 1.6
    theme_minimal() + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  # print(theplot1)
  
  theplot2  <- ggplot(data = speData, aes(x=specie, y=P50))+
    geom_bar( stat="identity") +
    geom_text(data = speData[speData[,P50]>=min(speData[,P50])/ratioUpDown,],aes(label=specie), color="black", angle = 90, hjust = 1.1,  size=3.5) + # au dessus vjust -0.3 en dessous 1.6
    geom_text(data = speData[speData[,P50]<min(speData[,P50])/ratioUpDown,], aes(label=specie), color="white", angle = 90, hjust = -0.1, size=3.5) + # au dessus vjust -0.3 en dessous 1.6
    theme_minimal() + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  # print(theplot2)
  
  theplot3  <- ggplot(data = speData, aes(x=specie, y=Pgs88))+
    geom_bar( stat="identity") +
    geom_text(data = speData[speData[,Pgs88]>=min(speData[,Pgs88])/ratioUpDown,],aes(label=specie), color="black", angle = 90, hjust = 1.1,  size=3.5) + # au dessus vjust -0.3 en dessous 1.6
    geom_text(data = speData[speData[,Pgs88]<min(speData[,Pgs88])/ratioUpDown,], aes(label=specie), color="white", angle = 90, hjust = -0.1, size=3.5) + # au dessus vjust -0.3 en dessous 1.6
    theme_minimal() + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  # print(theplot3)
  
  allPlot <-ggarrange(theplot1, theplot2, theplot3, 
                      ncol = 1, nrow = 3)
  
  print(allPlot)
  dev.off()
}




###################
#### FUNCTIONS ####
###################
plotDataVar <- function(dataTime, dataVar, plotCoord, varMinMax, varDir, jvar, legendSide = "bottom") #right
{
  # NOT USE FOR THE MOMENT
  

  simuResults = fread(output_path)
  lastDay = as.Date(as.character(simuResults[nrow(simuResults),"Time"]))
  sureauVarList    <- c(  "daily_transpiration_mm", "daily_Psi_LApoMin", "daily_PLC_Stem_max")
  sureauVarNames   <- c("transpiration - mm","Psi LApoMin","PLC max - stem")
  sureauDirList    <- c(1,-1,1)
  
  plotCoord = NULL
  #plotCoord = coordsInput
  plotCoord=c(5.25+0.0045,  5.35-0.0045, 44.15+0.005, 44.25)
  plotMinMax = NULL
  plotMinMax[["yearly_dayOfDeath"]] =  c(100,366)
  plotMinMax[["daily_Psi_LApoMin"]] = c(-5,-0.5) #Ventoux: until -2.5 Ventoux_TC: until -0.5
  plotMinMax[["daily_PLC_Stem_max"]] = c(0,90)  # entoux, from 20 #Ventoux_TC:
  plotMinMax[["daily_transpiration_mm"]] = c(0,5) # VentouxTC -556 +620
  
  
  
  plotDataVar(simuResults[["Time"]], simuResults[[sureauVarList[ivar],"Time"]], plotCoord, plotMinMax[[sureauVarList[ivar]]], sureauDirList[ivar], sureauVarList[ivar])
  
  
  minVal = min(dataVar,na.rm=TRUE)
  maxVal = max(dataVar,na.rm=TRUE)
  if (!is.null(varMinMax)) {
    minVal = min(minVal,varMinMax[1])
    if (minVal!=varMinMax[1]) cat('WARNING: For ',jvar,' you indicate a to high minimum value (the real min is ',minVal,')!',sep='')
    maxVal = max(maxVal,varMinMax[2])
    if (maxVal!=varMinMax[2]) cat('WARNING: For ',jvar,' you indicate a to low maximum value (the real min is ',maxVal,')!',sep='')
  } 
  
  coeff = -1/4
  data_tmp = simuResults # data.frame(Time = as.Date(dataTime), value = dataVar)
  data_tmp[["Time"]] = as.Date(data_tmp[["Time"]])
  theplot  <- ggplot(data = data_tmp)+
    geom_line(aes(x=Time, y=daily_transpiration_mm),color="#69b3a2") + 
    geom_line(aes(x=Time, y=daily_Psi_LApoMin*coeff),color="blue") + 
    # geom_line(data = jobsAFAM2, aes(x = data_date, y = Percent.Change), color = "blue") +
    xlab("") + scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    # ylim(0,NA) +
    scale_y_continuous(name = "daily transpiration (mm)",  sec.axis = sec_axis(~./coeff, name="Ps LApoMin"))  + 
    theme_bw() +
    theme( axis.line.y.left = element_line(color = "#69b3a2"), 
           axis.ticks.y.left = element_line(color = "#69b3a2"),
           axis.text.y.left = element_text(color = "#69b3a2"),
           axis.title.y = element_text(color = "#69b3a2", size=13),
           axis.line.y.right = element_line(color = "blue"), 
           axis.ticks.y.right = element_line(color = "blue"),
           axis.text.y.right = element_text(color = "blue"),
           axis.title.y.right = element_text(color = "blue", size=13),
           plot.title = element_text(hjust = 0.5),
           plot.subtitle = element_text(hjust = 0.5)) + # ylab(jvar) 
    labs(title=ispecie, subtitle = paste0("(LAImax=", LAImax,", last day= ",format(lastDay, "%b %d"),")" ))     # ggtitle(paste0(specieName))
  
    # annotate(geom="text", x=as.Date(dataTime[20]), y=2, label="Scatter plot",
    #          color="red")+
  #geom_text(hjust=0, vjust=0, label="Scatter plot")("Attention")
  print(theplot)
  
  theplot = ggplot(data = cbind(dataTime,dataVar)) #+
    # scale_fill_viridis_c(name = NULL, labels = scales::label_number(), limits = c(minVal,maxVal), direction = -varDir) +
    # scale_fill_distiller(palette = "Spectral", limits = c(minVal,maxVal)) # Spectral # oob = squish
    # scale_fill_hypso_tint_c(
    #   palette = "moon", #gmt_globe
    #   #labels = scales::label_number(),
    #   #breaks = c(-10000, -5000, 0, 2500, 5000, 8000),
    #   guide = guide_colorbar(
    #     direction = "horizontal",
    #     title.position = "top",
    #     barwidth = 20
    #   )
    # ) +
  
  # geom_tile(aes(x=x, y=y, fill=pr), alpha=0.8) + 
  #   scale_fill_viridis(na.value="white") +
  #   coord_equal() +
  #   theme(legend.position="bottom") +
  #   theme(legend.key.width=unit(2, "cm"))+
  #   scale_color_continuous(limits=c(-10,10),breaks=brkbias)
  
  theme_minimal() + 
    theme(legend.key.width=unit(2, "cm"))+ #height size
    theme(legend.position = legendSide) 
    # coord_sf(xlim = plotCoord[1:2], ylim = plotCoord[3:4]) #+
  #   scale_x_discrete(labels(5,10))
  # 
  # 
  # +
  #   labs(colorbar = "Your title here") + 
  #   guides(colorbar = guide_legend(override.aes = list(size = 5)))
  #   theme(legend.title = "bottom")
  #   guides(fill=guide_legend(title="New Legend Title"))
  
  print(theplot)
}

