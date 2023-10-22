# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### 
# Launcher to run SurEau-Ecos (updated version) on an exemple case
# Date : 14/04/2023
# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### 

# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory
# Set paths  -----------------------------------------------------------------
mainDir <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))  

#-------------------------------------------------------------------------------
#Preparing climate from Tanguy files
datarene = read.table("Tanguy_test_renecofor/clim_SP63.txt", sep="\t", dec=".", head=F)
names(datarene) <- c("YEAR",	"MONTH", "DOY", "Tair_mean", "Tair_min",	"Tair_max",	"PPT_sum", "Rainy_Days",	"RG_sum",		"RHair_max", "RHair_min",	"WS_mean")
datarene$DATE <- seq(as.Date("1969-01-01"), as.Date("2020-12-31"), by="days")
datarene$DATE <-format.Date(datarene$DATE, format="%d/%m/%Y")
datarene$PPT_sum <- datarene$PPT_sum*10
Rh=cbind.data.frame(datarene$RHair_max,datarene$RHair_min)
#apply(Rh,1, mean)
datarene$RHair_mean <- apply(Rh,1, mean)
plot(datarene$PPT_sum, type='h')

datarene[nrow(datarene),]

write.table(datarene, "Tanguy_test_renecofor/datarene_tanguy.csv", sep = ";", dec=".", row.names=F)

#-------------------------------------------------------------------------------
climateData_path          <- paste0(mainDir,'/Tanguy_test_renecofor/datarene_tanguy.csv')
soilParameters_path       <- paste0(mainDir,'/Tanguy_test_renecofor/Soil_Params_RENECOFOR_SP63.csv')
vegetationParameters_path <- paste0(mainDir,'/Tanguy_test_renecofor/vegetation_Abies_alba.csv')
output_path               <- paste0(mainDir,'/Tanguy_test_renecofor/example_output_subdaily.csv')

# Load model -------------------------------------------------------------------
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R')) # do not modify 
# Create input list files and define options -----------------------------------


modeling_options <- create.modeling.options(transpirationModel = c('Granier'),
                                            constantClimate = F)

simulation_parameters <- create.simulation.parameters(startYearSimulation = 2002,                       
                                                      endYearSimulation = 2003,
                                                      mainDir = mainDir,
                                                      outputType = 'LFMC_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)

climate_data          <- create.climate.data(filePath = climateData_path, 
                                             modeling_options = modeling_options,
                                             simulation_parameters = simulation_parameters) #


stand_parameters      <- create.stand.parameters(LAImax = 6, lat = 48.73, lon = 6.23)

soil_parameters       <- create.soil.parameters(filePath = soilParameters_path, 
                                                modeling_options = modeling_options) 

TTT =read.vegetation.file(vegetationParameters_path, modeling_options = modeling_options)
#TTT$gCrown0 = 250 
TTT$gmin20 = 1.5
TTT$gmin_S = 1.5
vegetation_parameters <- create.vegetation.parameters(listOfParameters =  TTT, 
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



# Plot outputs -----------------------------------------------------------------
# load output file   

DATA      = read.csv(output_path,header=T, dec='.', sep="")
DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')
DATA$Year = strftime(DATA$Time, format="%Y")

plot(DATA$Tair, type='l')
# plot water fluxes 
plot(DATA$Time,DATA$transpiration_mm,type='l',col='green',xlab='Time',ylab='water fluxes (mm/timestep)')

tapply(DATA$transpiration_mm, DATA$Year, sum)
tapply(DATA$soilEvaporation_mm, DATA$Year, sum)


# plot Plant water potentials
plot(DATA$Time,DATA$Psi_LSym,type='l', col='springgreen2',ylim=c(-4,0),xlab='Time',ylab='Psi (MPa)')
lines(DATA$Time,DATA$Psi_LApo,type='l',col='springgreen4')
lines(DATA$Time,DATA$Psi_SSym,type='l',col='firebrick1',ylim=c(-6,0))
lines(DATA$Time,DATA$Psi_SApo,type='l',col='firebrick4')
lines(DATA$Time,DATA$Psi_AllSoil,col='grey20',lwd=2)
legend('bottomright',
       legend=c('Psi_Leaf_Symplasm','Psi_Leaf_Apoplasm','Psi_Stem_Symplasm','Psi_Stem_Apoplasm','Psi_Soil'),
       col=c('springgreen2','springgreen4','firebrick1','firebrick4','grey30'),lty=1,lwd=2,cex=0.8)
par(new=T)
# plot cavitation 
plot(DATA$Time,DATA$PLC_Leaf,type='l', col='springgreen4',ylim=c(0,70),xlab='Time',ylab='', yaxt="n")
lines(DATA$Time,DATA$PLC_Stem,type='l',col='brown')
legend('topleft',legend=c('PLC_Leaf','PLC_Stem'),
       col=c('springgreen4','brown'),lty=1,lwd=2,cex=0.8)
axis(4)

# plot meteorological conditions 
plot(DATA$Time,DATA$Tair,type='l',col='firebrick4',ylab='Air temperature (degC)', xlab='Time')
par(new=T)
barplot(DATA$PPT,col='blue',border='blue',axes=F,ylab='',xlab='',ylim=c(0,60))
axis(4,col='blue',col.ticks='blue')

# plot water fluxes 
plot(DATA$Time,DATA$transpiration_mm,type='l',col='blue',xlab='Time',ylab='water fluxes (mm/timestep)')
lines(DATA$Time,DATA$Emin_mm,col='forestgreen')
lines(DATA$Time,DATA$Emin_S_mm,col='brown4')
lines(DATA$Time,DATA$soilEvaporation_mm,type='l',col='grey30')
legend('topright',legend=c('Transpiration','Emin','Emin_S','Soil'),
       col=c('blue','forestgreen','brown4','grey30'),lty=1,lwd=2,cex=0.8)
par(new='T')
plot(DATA$Time, DATA$LAI, type='l',  col='springgreen4',xlab='',ylab='', yaxt="n", xaxt="n", ylim=c(0,6))
axis(4)

# plot cavitation 
plot(DATA$Time,DATA$PLC_Leaf,type='l', col='springgreen4',ylim=c(0,70),xlab='Time',ylab='PLC')
lines(DATA$Time,DATA$PLC_Stem,type='l',col='brown')
legend('topleft',legend=c('PLC_Leaf','PLC_Stem'),
       col=c('springgreen4','brown'),lty=1,lwd=2,cex=0.8)

# plot fuel moisture 
plot(DATA$Time,DATA$LFMCSymp,type='l', col='springgreen4',xlab='Time',ylab='Fuel moisture content (% dry weight)',ylim=c(20,150))
lines(DATA$Time,DATA$LFMCApo,type='l',col='brown3',lwd=2)
lines(DATA$Time,DATA$LFMC,col='grey30')
legend('bottomleft',legend=c('LFMC_Symplasm','LFMC_Apoplasm','FMC_Canopy'),
       col=c('springgreen4','brown3','grey30'),lty=1,lwd=2,cex=0.8)


