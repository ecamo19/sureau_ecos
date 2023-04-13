# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### 
# Functions Inversion de la Reserve Utile (via Van-Genuchten formulation) 
# Authors : Arsene Druel   (arsene.druel[at]umr-cnrm.fr)
#           Nicolas Martin (nicolas.martin@inrae.fr)
# Date : 06/12/2022           
# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### 



# INVERSION SCRIPT #
##############################
inversionTAW <- function(selectPoints, id_site, x_site, y_site,clim_site, forest_type, idigit,
                         QPLCl9_target, max_simu, model_varLim, QPLCl9_target_tolerence, maxCoarseFrag, iPtsDiag, save_pdf,
                         nameProject, simuName, startYear, simuNbYear, SurEau_mainDir, outputType, methodPedoTransf, soil_files_folder, surEauClimate_path, overWriteOutput, modeling_options,
                         profondeur_soil, SE_coarseFrag_1, SE_coarseFrag_2, SE_coarseFrag_3,
                         SE_fieldCap, SE_wiltPoint, SE_vg_alpha, SE_vg_n, SE_vg_I, SE_vg_ksat,
                         SE_vg_sat_cap, SE_vg_res_cap, SE_gsoil, SE_depth1, SE_depth2, SE_depth3, SE_offSetPsoil, lastSimu = TRUE, graphPerPlot = 1)
{
  library(stringr)
  library(data.table)
  library(terra)
  
  if ( "siteNames"%in%colnames(selectPoints) ) id_site = "siteNames"
  
  # Sureau simulation config
  if ( outputType %in% c("diagnostic_subdaily", "LFMC_subdaily", "simple_subdaily")){
    resolutionOutput = "subdaily"
  } else if ( outputType %in% c("simple_daily")) {
    resolutionOutput = "daily"
  } else if ( outputType %in% c("simple_yearly", "yearly_forSA")) {
    resolutionOutput = "yearly"
  } 
  
  

  
  if ( forest_type == "all" ) forestClass <- as.character(unlist(selectPoints[,"classForest"])) else forestClass <- rep(forest_type,nrow(selectPoints))
  soilParameters_path = paste0("SureauSoilData_",forestClass,"_",locPointName(selectPoints,paste0("%.",idigit,"f")),".csv")
  
  ngraph = 1
  nplot  = 1
  RU_results    <- NULL
  for ( iPts in c(1:nrow(selectPoints)) ) {
    
    # define and create output folder (for simulations)
    simu_folder        <- paste0(nameProject) #,"_",forestClass[iPts])
    output_Dir         <- file.path(SurEau_mainDir,'simus',simu_folder)
    if (!dir.exists(output_Dir)) dir.create(output_Dir)
    
    # Add id site name in  output if available
    if ( !is.null(id_site) ) { name_site = paste0("_",selectPoints[[id_site]][iPts]) } else { name_site = "" }
    
    species     <- str_split(selectPoints[iPts,"species"]," - ")[[1]]
    for ( ispecie in species ) {
      specieName  <- paste(str_split(ispecie," ")[[1]],collapse = "_")
      vegetationParameters_path <- file.path(output_Dir,paste0('vegetation_',specieName,'.csv'))
      
      # copy vegetation file (for simulations)
      if (!file.exists(vegetationParameters_path)) file.copy(file.path(SurEau_mainDir,'simus',paste0('vegetation_',specieName,'.csv')),output_Dir)
      
      outname_prefix     <- paste(simuName,forestClass[iPts],specieName,sep='_')
      
      #SE_depth1 = 0.25; SE_depth2=1; SE_depth3=2.5
      
      # Select non variable parameters
      # RU_vg_ori = selectPoints[iPts,paste0("RU_vg_",select_RU,"_0-",profondeur_soil,"00")]
      # RU_vg_ori = RU_from_wp_fc(selectPoints[iPts,SE_fieldCap], selectPoints[iPts,SE_wiltPoint], selectPoints[iPts,paste0("coarseFrag_0-",profondeur_soil,"00_ori")], profondeur_soil)
      coarseFragOri = (selectPoints[[SE_coarseFrag_1]][iPts]*SE_depth1 + selectPoints[[SE_coarseFrag_2]][iPts]*(SE_depth2-SE_depth1) + selectPoints[[SE_coarseFrag_3]][iPts]*(SE_depth3-SE_depth2)) / SE_depth3
      RU_vg_ori = RU_from_wp_fc(selectPoints[[SE_fieldCap]][iPts], selectPoints[[SE_wiltPoint]][iPts], coarseFragOri, SE_depth3)
      
      
      output_path_sureau = file.path(output_Dir,paste0(outname_prefix,'_',selectPoints[[clim_site]][iPts],name_site,'_',outputType,'_',methodPedoTransf,'.csv'))
      RU_vg_max = RU_from_wp_fc(selectPoints[[SE_fieldCap]][iPts], selectPoints[[SE_wiltPoint]][iPts], 0, SE_depth3)
      RU_vg_min = RU_from_wp_fc(selectPoints[[SE_fieldCap]][iPts], selectPoints[[SE_wiltPoint]][iPts], maxCoarseFrag, SE_depth3)
      
      
      # for point diagnostic
      if ( iPtsDiag ) {
        # Les points sur lesquels projeter ces courbes
        siteN =""
        if ( any(colnames(selectPoints) == "siteNames")) siteN = paste0(" - ",selectPoints[["siteNames"]][iPts])
        cat("For the point ",iPts,siteN," (",selectPoints[[clim_site]][iPts],"): LAI max = ",selectPoints[["LAI_max"]][iPts]," and soil type = ",selectPoints[[paste0("SoilClass_0-",profondeur_soil,"00")]][iPts],".\n",sep="")
      }
      
      # rajouter une limite de LAI ?
      #LAI min > 1
      
      RU_cible   <- 200
      fracFind   <- FALSE
      illBeBack  <- FALSE
      model_Pval <- NULL
      model_Sval <- NULL
      ResAnalysis = data.frame( RU = 0 , QPLCl9 = 100 )
      while( ( nrow(ResAnalysis) < (max_simu+1) && !fracFind ) || (( nrow(ResAnalysis) < (max_simu+2) && illBeBack) ) ) {
        # Compute the fraction of coarse and the corresponding RU (can be a litlle diffrent than obj.)
        # newcoarseFrag = selectPoints[iPts,paste0("coarseFrag_0-",profondeur_soil,"00_ori")]+(RU_vg_ori-RU_cible)/RU_vg_ori*(100-selectPoints[iPts,paste0("coarseFrag_0-",profondeur_soil,"00_ori")])
        
        # Compute soil coarse fragment corresponding to target RU
        if ( RU_cible <= RU_vg_min ) {
          listCoarseFragNew = rep(maxCoarseFrag,3)
        } else if ( RU_cible == RU_vg_min ) {
          listCoarseFragNew = c(selectPoints[[SE_coarseFrag_1]][iPts], selectPoints[[SE_coarseFrag_2]][iPts], selectPoints[[SE_coarseFrag_3]][iPts])
        } else if ( RU_cible >= RU_vg_max ) {
          listCoarseFragNew = rep(0,3)
        } else if ( RU_cible >= RU_vg_ori ) { 
          newcoarseFrag = coarseFragOri + (RU_vg_ori-RU_cible)/RU_vg_ori*(100-coarseFragOri)
          coarseFragCoef = newcoarseFrag / coarseFragOri #selectPoints[iPts,paste0("coarseFrag_0-",profondeur_soil,"00_ori")]
          listCoarseFragNew = c(selectPoints[[SE_coarseFrag_1]][iPts], selectPoints[[SE_coarseFrag_2]][iPts], selectPoints[[SE_coarseFrag_3]][iPts]) * coarseFragCoef
        } else  { # if ( RU_vg_ori > RU_cible )
          listCoarseFragOri = c(selectPoints[[SE_coarseFrag_1]][iPts], selectPoints[[SE_coarseFrag_2]][iPts], selectPoints[[SE_coarseFrag_3]][iPts])
          orderCF = order(listCoarseFragOri)
          
          newcoarseFrag = coarseFragOri + (RU_vg_ori-RU_cible)/RU_vg_ori*(100-coarseFragOri)
          coarseFragCoef = newcoarseFrag / coarseFragOri #selectPoints[iPts,paste0("coarseFrag_0-",profondeur_soil,"00_ori")]
          if ( listCoarseFragOri[which(orderCF==3)]*coarseFragCoef <= maxCoarseFrag ) {
            # realnewcoarseFrag = (selectPoints[[SE_coarseFrag_1]][iPts]*coarseFragCoef*SE_depth1 + selectPoints[[SE_coarseFrag_2]][iPts]*coarseFragCoef*(SE_depth2-SE_depth1) + selectPoints[[SE_coarseFrag_3]][iPts]*coarseFragCoef*(SE_depth3-SE_depth2)) / SE_depth3
            listCoarseFragNew = listCoarseFragOri * coarseFragCoef
          } else {
            listDepth = c(SE_depth1,SE_depth2-SE_depth1,SE_depth3-SE_depth2)
            whichFC = which(orderCF==3)
            
            RU_vg_done          = RU_from_wp_fc(selectPoints[[SE_fieldCap]][iPts], selectPoints[[SE_wiltPoint]][iPts], maxCoarseFrag, listDepth[whichFC])
            coarseFragOri_left  = (listCoarseFragOri[-whichFC][1]*listDepth[-whichFC][1] + listCoarseFragOri[-whichFC][2]*listDepth[-whichFC][2]) / sum(listDepth[-whichFC])
            RU_vg_ori_left      = RU_from_wp_fc(selectPoints[[SE_fieldCap]][iPts], selectPoints[[SE_wiltPoint]][iPts], coarseFragOri_left, sum(listDepth[-whichFC]))
            
            newcoarseFrag = coarseFragOri_left + (RU_vg_ori_left-(RU_cible-RU_vg_done))/RU_vg_ori_left*(100-coarseFragOri_left)
            coarseFragCoef = newcoarseFrag / coarseFragOri_left #selectPoints[iPts,paste0("coarseFrag_0-",profondeur_soil,"00_ori")]
            
            if ( listCoarseFragOri[which(orderCF==2)]*coarseFragCoef <= maxCoarseFrag ) {
              # realnewcoarseFrag = ( maxCoarseFrag*coarseFragCoef*listDepth[whichFC] + listCoarseFragOri[-whichFC][1]*listDepth[-whichFC][1]*coarseFragCoef + listCoarseFragOri[-whichFC][2]*listDepth[-whichFC][2]*coarseFragCoef) / SE_depth3
              listCoarseFragNew = listCoarseFragOri * coarseFragCoef
              listCoarseFragNew[whichFC] = maxCoarseFrag
            } else {
              RU_vg_done          = RU_vg_done + RU_from_wp_fc(selectPoints[[SE_fieldCap]][iPts], selectPoints[[SE_wiltPoint]][iPts], maxCoarseFrag, listDepth[which(orderCF==2)])
              coarseFragOri_left  = listCoarseFragOri[which(orderCF==1)]
              RU_vg_ori_left      = RU_from_wp_fc(selectPoints[[SE_fieldCap]][iPts], selectPoints[[SE_wiltPoint]][iPts], coarseFragOri_left, listDepth[which(orderCF==1)])
              
              newcoarseFrag = coarseFragOri_left + (RU_vg_ori_left-(RU_cible-RU_vg_done))/RU_vg_ori_left*(100-coarseFragOri_left)
              coarseFragCoef = newcoarseFrag / coarseFragOri_left #selectPoints[iPts,paste0("coarseFrag_0-",profondeur_soil,"00_ori")]
              
              listCoarseFragNew = listCoarseFragOri * coarseFragCoef
              listCoarseFragNew[orderCF!=1] = maxCoarseFrag
            }
          }
        } 
        # realnewcoarseFrag = (max(0,min(99,selectPoints[[SE_coarseFrag_1]][iPts]*coarseFragCoef))*SE_depth1 + max(0,min(99,selectPoints[[SE_coarseFrag_2]][iPts]*coarseFragCoef))*(SE_depth2-SE_depth1) + max(0,min(99,selectPoints[[SE_coarseFrag_3]][iPts]*coarseFragCoef))*(SE_depth3-SE_depth2)) / SE_depth3
        # RU_vg_new = RU_from_wp_fc(selectPoints[iPts,SE_fieldCap], selectPoints[iPts,SE_wiltPoint], realnewcoarseFrag, SE_depth3)
        RU_vg_new = RU_from_wp_fc(selectPoints[[SE_fieldCap]][iPts], selectPoints[[SE_wiltPoint]][iPts], listCoarseFragNew[1], SE_depth1) + RU_from_wp_fc(selectPoints[[SE_fieldCap]][iPts], selectPoints[[SE_wiltPoint]][iPts], listCoarseFragNew[2], SE_depth2-SE_depth1) + RU_from_wp_fc(selectPoints[[SE_fieldCap]][iPts], selectPoints[[SE_wiltPoint]][iPts], listCoarseFragNew[3], SE_depth3-SE_depth2)
        
        # Launch simulation
        createSoilFileWithCF(selectPoints[iPts,], file.path(soil_files_folder,soilParameters_path[iPts]), listCoarseFragNew,
                             SE_fieldCap, SE_wiltPoint, SE_vg_alpha, SE_vg_n, SE_vg_I, SE_vg_ksat,
                             SE_vg_sat_cap, SE_vg_res_cap, SE_gsoil, SE_depth1, SE_depth2, SE_depth3, SE_offSetPsoil)
        
        SureauLauncher(selectPoints, selectPoints[["LAI_max"]], soilParameters_path, iPts, output_path_sureau,
                       x_site, y_site, clim_site, startYear, simuNbYear, soil_files_folder, surEauClimate_path,
                       outputType, SurEau_mainDir, resolutionOutput, overWriteOutput, modeling_options, vegetationParameters_path, methodPedoTransf)
        
        # file.copy(output_path_sureau, file.path(output_Dir,paste0(outname_prefix,name_site,"_RU",round(RU_vg_new),'_',selectPoints[[clim_site]][iPts],'_',outputType,'_',methodPedoTransf,'.csv')))
        
        # Load result of simulation
        ResAnalysis = rbind.data.frame(ResAnalysis, c(RU=RU_vg_new, Q90Stem=quantile(fread(output_path_sureau, sep=" ")$yearly_PLC_Stem_max, 0.9)))
        #colnames(ResAnalysis) = c("RU","QPLCl9")
        
        # ResAnalysis = ResAnalysis_save[1:7,] #ok6
        
        if ( nrow(ResAnalysis)==2 ) {
          cat('After the first simiuation, the second is selected function of PLC value simulated:', ResAnalysis[2,2])
          if ( ResAnalysis[2,2] > 50 ) {
            if ( abs(ResAnalysis[2,2]-(QPLCl9_target)) < QPLCl9_target_tolerence && abs(ResAnalysis[2,1]-RU_vg_max) < model_varLim ) {
              fracFind = TRUE
              RU_cible = ResAnalysis[2,1]
            } else if ( abs(ResAnalysis[2,1]-RU_vg_max) < model_varLim ) {
              fracFind = TRUE
              RU_cible = NA
            } else {
              RU_cible = 350
            }
          } else if ( ResAnalysis[2,2] < 10 ) {
            RU_cible = 50
          } else {
            RU_cible = 100
          }
          RU_cible = min(max(RU_cible, RU_vg_min),RU_vg_max)
          cat("new RU target =",RU_cible, '\n')
        } else if ( (all(ResAnalysis[-1,2] > 80) && min(ResAnalysis[  ,2])>QPLCl9_target) ||
                    (all(ResAnalysis[-1,2] < 5 ) && min(ResAnalysis[-1,2])<QPLCl9_target) ||
                    (all((ResAnalysis[-1,2] > max(90, QPLCl9_target)) | (ResAnalysis[-1,2] < min(9, QPLCl9_target))) && ( sum(ResAnalysis[-1,2] < min(9, QPLCl9_target))<=1 || (max(ResAnalysis[-1,2][ResAnalysis[-1,2] < min(9, QPLCl9_target)])-min(ResAnalysis[-1,2][ResAnalysis[-1,2] < min(9, QPLCl9_target)])) ) ) )  {
          cat("There is only extreme values (close to 100 pr close to 0). Try to find intermediate values if it's possible.\n")
          diffTarget = min(abs(ResAnalysis[-1,2]-QPLCl9_target))
          if ( diffTarget < QPLCl9_target_tolerence  ) {
            fracFind = TRUE
            RU_cible = ResAnalysis[which(abs(ResAnalysis[-1,2]-QPLCl9_target) == diffTarget)+1,1]
          } else if ( all(ResAnalysis[-1,2] > 80) && min(ResAnalysis[,2])>QPLCl9_target ) {
            if ( abs(max(ResAnalysis[,1])-RU_vg_max) < model_varLim ) {
              fracFind = TRUE
              RU_cible = NA
            } else {
              RU_cible = RU_vg_max
            }
          } else if ( all(ResAnalysis[-1,2] < 5 ) && min(ResAnalysis[-1,2])<QPLCl9_target ) {
            if ( abs(min(ResAnalysis[-1,1])-RU_vg_min) < min(model_varLim,RU_vg_min/10) ) {
              fracFind = TRUE
              RU_cible = NA
            } else {
              RU_cible = RU_vg_min
            }
          } else { # case: all((ResAnalysis[-1,2] > max(90, QPLCl9_target)) | (ResAnalysis[-1,2] < min(10, QPLCl9_target))) )
            RU_justabove = which(ResAnalysis$QPLCl9[order(ResAnalysis$QPLCl9)] > QPLCl9_target)[1]
            RU_cible = mean(ResAnalysis$RU[order(ResAnalysis$QPLCl9)[c(RU_justabove-1,RU_justabove)]])
          }
          if ( !is.na(RU_cible) && min(abs(ResAnalysis[-1,1] - RU_cible)) < model_varLim ) { 
            fracFind = TRUE # TOTO I'LL BE BACK (relancer une dernier fois ?)
            if ( lastSimu ) illBeBack = TRUE
          }

        } else {
          
          # Try to create the model
          # Chose the value of P (and slope?)
          # model = tryNLSmodel(ResAnalysis)
          RUmodel = RUfromModels(ResAnalysis, QPLCl9_target, bavard = TRUE)
          
          if ( illBeBack && !is.null(RUmodel) ) {
            if ( abs(min(max(RUmodel,RU_vg_min), RU_vg_max) - RU_cible) > (model_varLim*2) ) { #  || abs( ResAnalysis[-1,][which.min(abs(ResAnalysis[-1,1]-RUmodel)),2] - QPLCl9_target) > (QPLCl9_target_tolerence*2) ) {
              cat("########################################################################\n# WARNING: with ILLBEBACK THE NEW VALUE IS FAR FROM TARGET VALUE !!!!! #\n########################################################################\n")
              illBeBack = FALSE
            }
          }
          
          if ( !illBeBack ) {
          
            #if ( class(model) != "try-error" ) {
            if ( !is.null(RUmodel) ) {
              #if ( !is.null(model_Pval) && abs((model_Pval - summary(model)$parameters['P',1])/model_Pval)<model_varLim && abs((model_Sval - summary(model)$parameters['slope',1])/model_Sval)<(6*model_varLim) ) {
              # RU_target_old = RU_cible

              RU_cible      = min(max(RUmodel,RU_vg_min), RU_vg_max)
              # model_Pval = summary(model)$parameters['P',1]
              # model_Sval = summary(model)$parameters['slope',1]
              # RU_cible = model_Pval - log(100/QPLCl9_target - 1) * 25/model_Sval
              
              # Check if the model it's not wrong ==> we are on a wrong side of an existing value
              resOrder_above = ResAnalysis$QPLCl9[order(ResAnalysis$QPLCl9)] > QPLCl9_target
              res_justAbove = ResAnalysis[order(ResAnalysis$QPLCl9)[which(resOrder_above)[1]],]
              if ( resOrder_above[1] == FALSE ) { # case when there is a value above and a value below QPLCl9_target (which is above 10)
                res_justBelow = ResAnalysis[order(ResAnalysis$QPLCl9)[which(resOrder_above)[1]-1],]
                if ( RU_cible > res_justBelow$RU || RU_cible < res_justAbove$RU ) {
                  # RU_cible = mean(ResAnalysis$RU[order(ResAnalysis$QPLCl9)[c(which(resOrder_above)[1]-1,which(resOrder_above)[1])]])  # simple mean
                  RU_cible = res_justAbove$RU + (1-(QPLCl9_target-res_justBelow$QPLCl9)/(res_justAbove$QPLCl9-QPLCl9_target))*(res_justBelow$RU-res_justAbove$RU) # mean pondéré (régression linéaire)
                  cat( "NB: The model give a not logical value. Try manually....\n ")
                }
              } else if ( RU_cible < res_justAbove$RU ) { # Only value above target and RU_cible below maw RU compute
                RU_cible = min(max(ResAnalysis[,1]) + (RU_vg_max-RU_vg_min)/3,RU_vg_max)
              }
              
              pt_closest    = which.min(abs(ResAnalysis[-1,1]-RU_cible) )
              # PLC_closest   = ResAnalysis[-1,][pt_closest,2]
              RU_target_closest = ResAnalysis[-1,][pt_closest,1]

              if ( iPtsDiag )  cat("For simu ", nrow(ResAnalysis)-1, " the difference with MODEL between target and closest is ",abs(RU_target_closest - RU_cible),"\n",sep="")
              if ( abs(RU_target_closest - RU_cible) < (model_varLim*2) ) {
                fracFind = TRUE
                if ( RU_cible != RUmodel && !(abs(RU_target_closest - RUmodel) < (model_varLim*2)) && (RU_cible>RUmodel || QPLCl9_target<ResAnalysis[-1,][pt_closest,2]) ) { RU_cible = NA } else if ( lastSimu ) { illBeBack = TRUE } # else TOTO I'LL BE BACK (relancerune dernier fois)
              }
            } else { 
              if (  min(abs(ResAnalysis[-1,2]-QPLCl9_target)) < QPLCl9_target_tolerence  ) {
                fracFind = TRUE
                RU_cible = ResAnalysis[which(abs(ResAnalysis[-1,2]-QPLCl9_target)== min(abs(ResAnalysis[-1,2]-QPLCl9_target)))+1,1]
              } else if ( min(ResAnalysis[,2]) > min(10,QPLCl9_target) ) {
                # if ( abs(min(ResAnalysis[,2])-(QPLCl9_target)) < QPLCl9_target_tolerence && abs(max(ResAnalysis[,1])-RU_vg_max) < model_varLim ) {
                #   fracFind = TRUE
                #   RU_cible = max(ResAnalysis[,1]) } else
                if ( min(ResAnalysis[,2])>QPLCl9_target && abs(max(ResAnalysis[,1])-RU_vg_max) < model_varLim ) { # Permet que si on est dans une simulation intermédiare, ça ne s'arrete pas
                  fracFind = TRUE # Si on est dans le cas de PLC > PLC_cible alors que RU ~= RU_max
                  RU_cible = NA
                } else if ( abs(max(ResAnalysis[,1])-RU_vg_max) < model_varLim ) { # une PLC <= PLC_cible et RU ~= RU_max (==> donc valeur 100 (au dessus) et au moins une valeur en dessous) 
                  resOrder_above = ResAnalysis$QPLCl9[order(ResAnalysis$QPLCl9)] > QPLCl9_target
                  if ( resOrder_above[1] == FALSE ) { # case when there is a value above and a value below QPLCl9_target (which is above 10)
                    res_justAbove = ResAnalysis[order(ResAnalysis$QPLCl9)[which(resOrder_above)[1]],]
                    res_justBelow = ResAnalysis[order(ResAnalysis$QPLCl9)[which(resOrder_above)[1]-1],]
                    # RU_cible = mean(ResAnalysis$RU[order(ResAnalysis$QPLCl9)[c(which(resOrder_above)[1]-1,which(resOrder_above)[1])]])  # simple mean
                    RU_cible = res_justAbove$RU + (1-(QPLCl9_target-res_justBelow$QPLCl9)/(res_justAbove$QPLCl9-QPLCl9_target))*(res_justBelow$RU-res_justAbove$RU) # mean pondéré (régression linéaire)
                  } else { # seulement des valeurs au dessus de QPLCl9_target
                    RU_cible = min(max(ResAnalysis[,1]) + (RU_vg_max-RU_vg_min)/3,RU_vg_max)
                  }
                  if ( abs(min(ResAnalysis$RU - RU_cible)) < model_varLim ) { # on vérifie que l'on a pas fait une simulation similaire....
                    fracFind  = TRUE   # else TOTO I'LL BE BACK (relancerune dernier fois ?)
                    if ( lastSimu ) illBeBack = TRUE
                  }
                } else {
                  RU_cible = min(max(ResAnalysis[,1]) + (RU_vg_max-RU_vg_min)/3,RU_vg_max)
                }
              } else if (  max(ResAnalysis[-1,2]) < max(60,QPLCl9_target) && min(ResAnalysis[-1,1])> (RU_vg_min + model_varLim) ) { # Cas où on veut un point avec plus faible PLC (<60 ou < target) mais RU!=RU_min
                RU_cible = max(min(ResAnalysis[-1,1]) / 2, RU_vg_min)
                # } else if (  abs(max(ResAnalysis[-1,2])-(QPLCl9_target)) < QPLCl9_target_tolerence && min(ResAnalysis[-1,1])<20 ) {
                #   fracFind = TRUE
                #   RU_cible = min(ResAnalysis[-1,1])
              } else if (  max(ResAnalysis[-1,2]) < QPLCl9_target && abs(min(ResAnalysis[-1,1])-RU_vg_min) < model_varLim ) { # Cas où on a pas de point possible (< target + RU = RU_min)
                fracFind = TRUE
                RU_cible = NA
              } else {
                # listeRU = c(200,150,250,100,300,50)
                # sort(listeRU)
                resOrder_above = ResAnalysis$QPLCl9[order(ResAnalysis$QPLCl9)] > QPLCl9_target
                if ( resOrder_above[1] == FALSE ) { # case when there is a value above and a value below QPLCl9_target (which is above 10)
                  res_justAbove = ResAnalysis[order(ResAnalysis$QPLCl9)[which(resOrder_above)[1]],]
                  res_justBelow = ResAnalysis[order(ResAnalysis$QPLCl9)[which(resOrder_above)[1]-1],]
                  # RU_cible = mean(ResAnalysis$RU[order(ResAnalysis$QPLCl9)[c(which(resOrder_above)[1]-1,which(resOrder_above)[1])]])  # simple mean
                  RU_cible = res_justAbove$RU + (1-(QPLCl9_target-res_justBelow$QPLCl9)/(res_justAbove$QPLCl9-QPLCl9_target))*(res_justBelow$RU-res_justAbove$RU) # mean pondéré (régression linéaire)
                } else { # seulement des valeurs au dessus de QPLCl9_target / a priori pas possible
                  RU_cible = min(max(ResAnalysis[,1]) + (RU_vg_max-RU_vg_min)/3,RU_vg_max)
                }
                if ( abs(min(ResAnalysis$RU - RU_cible)) < model_varLim ) { # on vérifie que l'on a pas fait une simulation similaire....
                  fracFind  = TRUE   # else TOTO I'LL BE BACK (relancerune dernier fois ?)
                  if ( lastSimu ) illBeBack = TRUE
                }
                # RU_justabove = which(ResAnalysis$QPLCl9[order(ResAnalysis$QPLCl9)] > QPLCl9_target)[1]
                # RU_cible = mean(ResAnalysis$RU[order(ResAnalysis$QPLCl9)[c(RU_justabove-1,RU_justabove)]])
                # if ( abs(RU_justabove - RU_cible) < model_varLim ) { 
                #   fracFind = TRUE # TOTO I'LL BE BACK (relancer une dernier fois ?)
                #   if ( lastSimu ) illBeBack = TRUE
                # }
                if ( iPtsDiag )  cat("For simu ", nrow(ResAnalysis)-1, " the difference between target is ",min(abs(ResAnalysis$RU-RU_cible)),"mm.\n",sep="")
              }
            }
            
          } else {
            illBeBack = FALSE
          }
        }
        if ( illBeBack && min(abs(ResAnalysis[-1,1] - RU_cible)) <  1 ) illBeBack = FALSE
      }
      if ( fracFind == FALSE )  RU_cible = NA
      
      
      
      # TOTO ==> relancer une dernière fois (si condition activé ?)  dans 3 cas
      # model  =  try(nls(QPLCs9 ~ 100/(1+exp(slope/25*(P-RU))), data=ResAnalysis, start=c(slope=-2,P=80)), silent = TRUE )
      # # model  =  try(nls(QPLCs9 ~ ((100-min(ResAnalysis[,2]))/(1+exp(slope/25*(P-RU)))+min(ResAnalysis[,2])), data=ResAnalysis, start=c(slope=-2,P=80)), silent = TRUE )
      # 
      # # inversion du model
      # # summary(model)$parameters['slope',1]
      # # summary(model)$parameters['P',1]
      # # QPLCs9 ~ 100/(1+exp(slope/25*(P-RU)))
      # # 1+exp(slope/25*(P-RU)) ~ 100/QPLCs9
      # # exp(slope/25*(P-RU)) ~ 100/QPLCs9 - 1
      # # slope/25*(P-RU) ~ ln(100/QPLCs9 - 1)
      # # (P-RU) ~ ln(100/QPLCs9 - 1) * 25/slope
      # # -RU ~ ln(100/QPLCs9 - 1) * 25/slope - P
      # # RU ~ P - ln(100/QPLCs9 - 1) * 25/slope
      # 
      # # QPLCl9 ~ 100/(sigmo + exp(slope/25 * (P - RU)))
      # # sigmo+exp(slope/25*(P-RU)) ~ 100/QPLCs9
      # # exp(slope/25*(P-RU)) ~ 100/QPLCs9 - sigmo
      # # slope/25*(P-RU) ~ ln(100/QPLCs9 - sigmo)
      # # (P-RU) ~ ln(100/QPLCs9 - sigmo) * 25/slope
      # # -RU ~ ln(100/QPLCs9 - sigmo) * 25/slope - P
      # # RU ~ P - ln(100/QPLCs9 - sigmo) * 25/slope
      #
      # #  QPLCl9 ~ slope/(RU + P)
      # #  RU + P ~ slope / QPLCl9
      # #  RU ~ slope / QPLCl9 - P
      
      # PLOT!
      if ( iPtsDiag )  {
        if ( graphPerPlot == 1 ) {
          if (save_pdf) pdf(file.path(output_Dir,paste0(outname_prefix,'_',selectPoints[[clim_site]][iPts],name_site,'_',outputType,'_',methodPedoTransf,'.pdf')),width=6, height=6)
          par(mfrow=c(1,1), pty='s')
          tmp = plotIPtsModels(ResAnalysis, QPLCl9_target, selectPoints[iPts,], paste0("Point ",locPointName(selectPoints[iPts,],paste0("%.",idigit,"f")),siteN), profondeur_soil, doModels = c(TRUE, TRUE, TRUE, TRUE), RU_cible=RU_cible)
          if (save_pdf) dev.off()
        } else {
          
          
          if ( ngraph == 1 && save_pdf ) {
            if (save_pdf) pdf(file.path(output_Dir,paste0(simuName,'_PlotNB',nplot,'_',outputType,'_',methodPedoTransf,'.pdf')),width=20, height=14)
            nrow = 1
            if ( graphPerPlot > 3 && graphPerPlot <= 8) nrow = 2 else if ( graphPerPlot > 8 ) nrow = 3
            par(mfrow=c(nrow,graphPerPlot/nrow), pty='s')
            #layout(matrix(c(1:graphPerPlot),nrow = nrow))
          } 
          
          tmp = plotIPtsModels(ResAnalysis, QPLCl9_target, selectPoints[iPts,], paste0("Point ",locPointName(selectPoints[iPts,],paste0("%.",idigit,"f")),siteN), profondeur_soil, doModels = c(TRUE, TRUE, TRUE, TRUE), RU_cible=RU_cible)

          if (ngraph == graphPerPlot ) {
            ngraph = 1
            nplot = nplot + 1
            if (save_pdf) dev.off()
          } else {
            ngraph = ngraph + 1
          }
        }
          
        # max_RU = max(ResAnalysis[,1], RU_cible, na.rm = TRUE)
        # nd = data.frame(seq(0, max_RU, 1))
        # names(nd) = "RU"
        # par(mfrow=c(1,1), pty='s')
        # plot(ResAnalysis[,"QPLCl9"] ~ ResAnalysis[,"RU"],pch=21, bg=4, ylab="Qantile .9 PLC Max", xlab= "RU (fc-wp)", cex=c(1.5,rep(2,nrow(ResAnalysis)-1)),  ylim=c(0,100), ) #col=c("red",rep("blue",nrow(ResAnalysis)-1))
        # for ( isimu in c(2:nrow(ResAnalysis))) {
        #   model  =  tryNLSmodel(ResAnalysis[1:isimu,])
        #   ResAnalysis_tmp = ResAnalysis[1:isimu,]
        #   #model  =  try(nls(QPLCl9 ~ 100/(1+exp(slope/25*(P-RU))), data=ResAnalysis_tmp, start=c(slope=-2,P=80)), silent = TRUE )
        #   model1  =  try(nls(QPLCl9 ~ slope/(RU+P), data=ResAnalysis_tmp, start=c(slope = 50, P=0.5)), silent = TRUE )
        #   if ( isimu == nrow(ResAnalysis) ) lty_simu=1 else lty_simu=2
        #   lwd_simu = 2*isimu/nrow(ResAnalysis)
        #   if ( class(model) != "try-error" ) {
        #     FitRU = cbind.data.frame(RU=nd, Q90PLC=predict(model, newdata=nd))
        #     lines(FitRU$Q90PLC~FitRU$RU, lwd=lwd_simu, type='l',lty=lty_simu)
        #   }
        # }
        # title(paste0("Point ",selectPoints[[clim_site]][iPts]))
        # mtext(paste0("LAI max = ",round(selectPoints[["LAI_max"]][iPts],1)),at=max_RU*0.75, line = -1)
        # mtext(paste0("Soil type = '",selectPoints[[paste0("SoilClass_0-",profondeur_soil,"00")]][iPts],"'"),at=max_RU*0.75, line = -2)
        # mtext(paste0("Selected RU = ",round(RU_cible),"mm"),at=max_RU*0.75, line = -3)
        # abline(h=QPLCl9_target,col="blue",lty=2)
        # points(RU_cible,QPLCl9_target,col="red",pch=3,lwd=2)
      }
      
      if ( lastSimu ) RU_result_tmp    = rep(NA,3+max_simu*2) else RU_result_tmp    = rep(NA,1+max_simu*2)
      RU_result_tmp[1] = RU_cible
      for ( isimu in c(2:nrow(ResAnalysis))){
        RU_result_tmp[isimu*2-2] = ResAnalysis[isimu,1]
        RU_result_tmp[isimu*2-1] = ResAnalysis[isimu,2]
      }
      
      # RU_result_tmp = c(as.numeric(selectPoints[iPts,c('x','y')]),forestClass[iPts],ispecie,RU_result_tmp)
      RU_result_tmp = c(as.numeric(selectPoints[iPts,c('x','y','LAI_max')]),selectPoints[[paste0("SoilClass_0-",profondeur_soil,"00")]][iPts],forestClass[iPts],ispecie,RU_result_tmp)
      if ( "siteNames"%in%colnames(selectPoints) ) RU_result_tmp = c(selectPoints[iPts,"siteNames"], RU_result_tmp)
      
      RU_results = rbind(RU_results,unlist(RU_result_tmp))
      # # dowload.database
      # # getdatatypesoil(lebray)
      cat('\n')
      
    } # end of species on one points
    
    # Write temporal file (if stop before end)
    fwrite(data.frame(RU_results), file=file.path(SurEau_mainDir,'simus',paste0("tmp_",Sys.getpid(),"_",nameProject,'_',simuName,'_TAW_Res_tmp.csv')),row.names=F,col.names=T,sep=";")

  }
  listNames = NULL
  if ( lastSimu ) max_simu_tmp = max_simu + 1 else max_simu_tmp = max_simu
  for ( isimu in c(1:max_simu_tmp) ) listNames=c(listNames,paste0(c("RU_simu","PLCmax_Q.9_sim"),isimu))
  if ( "siteNames"%in%colnames(selectPoints) ) addSiteNames =  "siteNames" else addSiteNames = NULL
  colnames(RU_results) = c(addSiteNames,'x','y','LAI_max',paste0("SoilClass_0-",profondeur_soil,"00"),'ForestClass','Specie',"RU_target",listNames)
  rownames(RU_results) = NULL
  
  #file.remove(file.path(SurEau_mainDir,'simus',paste0("tmp_",Sys.getpid(),"_",nameProject,simuName,'_TAW_Res_tmp.csv')))
  
  return(RU_results)
}


###################
#### FUNCTIONS ####
###################

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

loadPoints <- function(workPoints, map_grid,
                       pathLAIfile, laiyears, lai_suffix,
                       pathSoilFile, typeSoilInfo, listSoils,
                       path_climate, idigit, prefix, suffix, clim_site, pathClimateClassfile,
                       pathForestfile, forest_suffix, forest_type="", forest_list_type = "", forest_meanfix = "", climateClassification = "koppen", check_clim_exist  = TRUE, check_clim_values = FALSE)

{
  
  # load all points
  selectPoints = convertInPoints(workPoints,map_grid) #lonLatSelect=NULL,id=NULL
  
  
  #### LOAD POINT INFORMATIONS (LAI, fraction forest, SOIL,...)
  # load LAI and compute max (quantile 0.9 of yearly maximum LAI)
  LAI_max_years = NULL
  for ( iyear in c(laiyears[1]:laiyears[2])) {
    ilai = rast(paste0(pathLAIfile,iyear,'-',iyear,lai_suffix))
    iLAI_max = suppressWarnings(apply(values(ilai), MARGIN=c(1), max, na.rm=F))
    iLAI_max[iLAI_max==-Inf]=NA
    LAI_max_years = rbind(LAI_max_years, iLAI_max)
  }
  LAI_max = apply(LAI_max_years, MARGIN=c(2), quantile, 0.9,na.rm=T)
  LAI_max = cbind(crds(ilai,na.rm=F),LAI_max)
  LAI_max = LAI_max[rowSums(is.na(LAI_max)) <= 0,]
  #which(LAI_max[,'x']==2.75 & LAI_max[,'y']==48.50)
  
  # rm(LAI_max_years, ilai, iLAI_max)
  # selectPoints = merge_selectPoints2(selectPoints, LAI_max)
  # rm(LAI_max)
  # 
  LAI_max0.7 = apply(LAI_max_years, MARGIN=c(2), quantile, 0.7,na.rm=T)
  LAI_max0.7 = cbind(crds(ilai,na.rm=F),LAI_max0.7)
  LAI_max0.7 = LAI_max0.7[rowSums(is.na(LAI_max0.7)) <= 0,]
  
  LAI_max0.5 = apply(LAI_max_years, MARGIN=c(2), quantile, 0.5,na.rm=T)
  LAI_max0.5 = cbind(crds(ilai,na.rm=F),LAI_max0.5)
  LAI_max0.5 = LAI_max0.5[rowSums(is.na(LAI_max0.5)) <= 0,]
  
  LAI_mean = apply(LAI_max_years, MARGIN=c(2), mean, na.rm=T)
  LAI_mean = cbind(crds(ilai,na.rm=F),LAI_mean)
  LAI_mean   = LAI_mean[rowSums(is.na(LAI_mean)) <= 0,]

  rm(LAI_max_years, ilai, iLAI_max)
  selectPoints = merge_selectPoints2(selectPoints, LAI_max)
  selectPoints = merge_selectPoints2(selectPoints, LAI_max0.7)
  selectPoints = merge_selectPoints2(selectPoints, LAI_max0.5)
  selectPoints = merge_selectPoints2(selectPoints, LAI_mean)
  rm(LAI_max, LAI_max0.7, LAI_max0.5, LAI_mean)
  
  
  # Add fraction on forest Type
  # pathForestfile   = "/home/ardruel/data/Regrid_with_ERA5_2/Forest_contrib_ERA5_"
  # forest_list_type = c("lcForest","lc60","lc70","lc90","lc100","lc120")
  # forest_meanfix   = "_10W-45N-30E-81N_"
  # forest_suffix    = ".nc"
  for ( iforest in forest_list_type ) {
    iforestStart = values(rast(paste0(pathForestfile,iforest,forest_meanfix,laiyears[1],'-',laiyears[1],forest_suffix))) 
    iforestEnd   = values(rast(paste0(pathForestfile,iforest,forest_meanfix,laiyears[2],'-',laiyears[2],forest_suffix)))
    iforestStart[is.na(iforestStart)]=0
    iforestEnd[is.na(iforestEnd)]=0
    forest_mean = (iforestStart+iforestEnd)/2
    if ( iforest == "" ) colnames(forest_mean) = "forest_frac" else colnames(forest_mean) = iforest
    forest_mean = cbind(crds(rast(paste0(pathForestfile,iforest,forest_meanfix,laiyears[1],'-',laiyears[1],forest_suffix)),na.rm=F), forest_mean)
    forest_mean = forest_mean[rowSums(is.na(forest_mean)) <= 0,]
    rm()
    selectPoints = merge_selectPoints2(selectPoints, forest_mean)
  }
  rm(iforestStart, iforestEnd, forest_mean)
  
  
  # Select Koppen information
  # pathClimateClassfile = "/home/ardruel/Téléchargements/Koppen/Raster files/world_koppen/hdr.adf"
  if ( climateClassification == "koppen") {
    climate_map = loadKoppen(pathClimateClassfile)

    climateClass = climate_map[terra::cellFromXY(climate_map,as.matrix(selectPoints[,c('x','y')]))]
    listNA = which(is.na(climateClass))
    for ( iNA in listNA ) {
      iloc = selectPoints[iNA,c('x','y')]
      for ( rayon in c(1,2) ) {
        if ( is.na(climateClass[iNA,1]) ) {
          izone = theCrop(climate_map,as.numeric(c(iloc['x']-rayon*res(climate_map)[1],iloc['x']+rayon*res(climate_map)[1],iloc['y']-rayon*res(climate_map)[2],iloc['y']+rayon*res(climate_map)[2])))
          # climate_map[cellFromXY(climate_map,c(iloc['x']-res(climate_map)[1],iloc['x']+res(climate_map)[1],iloc['y']-res(climate_map)[2],iloc['y']+res(climate_map)[2]))]
          if ( class(levels(izone)[[1]]) == "character" ) izoneval = table(levels(izone)[[1]][values(izone)+1]) else izoneval = table(levels(izone)[[1]][values(izone)+1,2])
          iCCval = names(izoneval)[which.max(izoneval)]
          if ( !is.null(iCCval) ) climateClass[iNA,1] = iCCval
        }
      }
    }
    selectPoints = cbind(selectPoints, climateClass)
    
    # select specie from Koppen climate classification
    selectPoints = cbind(selectPoints,  selectSpecies(selectPoints,forest_type, forest_list_type))
  }
  
  
  # load climate names
  latlonDigit=paste0("%.",idigit,"f")
  climName = paste0(prefix,locPointName(selectPoints,latlonDigit),suffix)
  selectPoints = cbind(selectPoints, climName)
  
  
  # load soil
  soil_data_in = rast(pathSoilFile)
  if (typeSoilInfo == "USDA") {
    library(euptf)
    soilTextureId = c("C", "SiC", "SC", "SiCL", "CL", "SCL", "Si", "SiL", "L", "SL", "LS", "S", "O")
    # soil_classes <- data.frame(value = seq_along(soilTextureId), soilClass = soilTextureId)
    convFactor = 13.45750708 # cm.h-1 --> mol/s/Mpa/m
    soilKsatUS = c(0.20*convFactor, 0.02*convFactor, 0.12*convFactor,  0.07*convFactor,  0.26*convFactor, 1.31*convFactor, 0.25*convFactor, 
                   0.45*convFactor, 1.04*convFactor, 4.42*convFactor, 14.59*convFactor, 29.70*convFactor, 4.37*convFactor )
    # Conversion between % soil type and soil characteristics (from Gordon Bonan, Modelling Climate Change on ecosystems 2019). α in cm-1, Ksat in mol/s/Mpa/m, Ψsat_c in cm,
    # Organic soils are defined as soils having clay content >= 60 % clay and organic carbon >= 18 % , or having clay content < 60 % and organic carbon >= (12+clay*0.1). 
    # But: not good correlation for organic to Ksat: ==> https://doi.org/10.5194/essd-13-1593-2021. We take the mean of other data
    
    for ( isoil in listSoils ) {
      # levels(soil_data_in[[paste0("SoilClass_",isoil)]]) = soil_classes
      soil_USDA   = values(soil_data_in[[paste0("SoilClass_",isoil)]])
      soilKsatData= data.frame(soilKsatUS[soil_USDA])
      soil_USDA   = data.frame(soilTextureId[soil_USDA])
      soil_coarse = values(soil_data_in[[paste0("coarseFrag_",isoil)]])
      soil_data_tmp = cbind(crds(soil_data_in,na.rm=F),soil_USDA,soil_coarse,soilKsatData)
      soil_data_tmp = soil_data_tmp[rowSums(is.na(soil_data_tmp)) <= 0,]
      colnames(soil_data_tmp)[3] = paste0("SoilClass_",isoil)
      colnames(soil_data_tmp)[4] = paste0("coarseFrag_",isoil,'_ori')
      colnames(soil_data_tmp)[5] = paste0("Ksat_",isoil)
      selectPoints = merge_selectPoints2(selectPoints, soil_data_tmp)
      
      iSymbLayer = '-'
      soilType = "sub"
      if ( substr(isoil,1,1)=="0") soilType = "top"
      idepth= (as.numeric(strsplit(isoil,iSymbLayer)[[1]][2]) - as.numeric(strsplit(isoil,iSymbLayer)[[1]][1]))/100
      ptfList = c(NA,NA,'PTF19') #c('PTF08','PTF11','PTF19')
      soilData = data.frame(SAMPLE_ID = c(1:nrow(selectPoints)),TOPSOIL = soilType, TEXT_US = selectPoints[[paste0("SoilClass_",isoil)]])
      soilData = euptf_maker(soilData, ptfList, selectPoints[[paste0("coarseFrag_",isoil,'_ori')]], idepth, 'US_', isoil, wfcSpi = 0.033, wiltSpi = 1.5848)
      selectPoints = cbind(selectPoints, soilData)
    }
  }
  rm(soilTextureId,convFactor,soilKsatUS,soil_USDA,soilKsatData,soil_data_tmp,iSymbLayer,soilType,idepth,ptfList,soilData)
  

  # Check climate files
  if ( check_clim_exist ) selectPoints = removePtsWithoutClim2(path_climate, selectPoints, clim_site=clim_site, id_site=clim_site)
  if ( check_clim_values) checkValuesAndCorrectClimFiles(path_climate, selectPoints, clim_site=clim_site)
  
  return(selectPoints)
}

merge_selectPoints <- function(selectPoints, otherData) {
  new_selecPoints <- NULL
  selectPoints = data.frame(selectPoints)
  otherData    = data.frame(otherData)
  for ( ipts in c(1:nrow(selectPoints)) ) {
    corresp_point = which(otherData[,1]==selectPoints[ipts,"x"] & otherData[,2]==selectPoints[ipts,'y'])
    if (length(corresp_point)>0) {
      if (length(corresp_point)!=1) stop("There is a point in double for selectpoint point ",ipts)
      new_selecPoints = rbind(new_selecPoints, data.frame(selectPoints[ipts,],otherData[corresp_point,3:length(colnames(otherData))]))
      #new_selecPoints = rbind(new_selecPoints, unlist(c(selectPoints[ipts,],otherData[corresp_point,3:length(colnames(otherData))])))
    }
  }
  colnames(new_selecPoints) = c(colnames(selectPoints),colnames(otherData)[3:length(colnames(otherData))])
  return(as.data.frame(new_selecPoints))
}

merge_selectPoints2 <- function(selectPoints, otherData) {
  new_selecPoints <- NULL
  # selectPoints = data.frame(selectPoints)
  # otherData    = data.frame(otherData)
  corresp_list = NULL
  for ( ipts in c(1:nrow(selectPoints)) ) {
    corresp_point = which(otherData[,1]==selectPoints[ipts,'x'] & otherData[,2]==selectPoints[ipts,'y'])
    if (length(corresp_point)>0) {
      if (length(corresp_point)!=1) stop("There is a point in double for selectpoint point ",ipts)
      corresp_list = c(corresp_list, corresp_point)
    } else {
      corresp_list = c(corresp_list, NA)
    }
  }
  new_selecPoints = cbind(selectPoints[!is.na(corresp_list),],otherData[corresp_list[!is.na(corresp_list)],3:length(colnames(otherData))])
  colnames(new_selecPoints) = c(colnames(selectPoints),colnames(otherData)[3:length(colnames(otherData))])
  return(as.data.frame(new_selecPoints))
}

locPointName <- function(selectPoint, latlonDigit)
{
  nameLon = c('lon', 'longitude', 'Longitude', 'LON', 'LongitudeD','x', 'X', 'long', 'LONG')
  nameLat = c('lat', 'latitude', 'Latitude', 'LAT', 'LatitudeD','y', 'Y')
  nlon = intersect(colnames(selectPoint),nameLon)
  nlat = intersect(colnames(selectPoint),nameLat)
  
  ilonEW = rep('W',nrow(selectPoint))
  ilonEW[selectPoint[[nlon]] >= 0] = 'E'
  ilon = paste0(sprintf(latlonDigit, abs(selectPoint[[nlon]])),ilonEW)
  
  ilatNS = rep('S',nrow(selectPoint))
  ilatNS[selectPoint[[nlat]] >= 0] = 'N'
  ilat = paste0(sprintf(latlonDigit, abs(selectPoint[[nlat]])),ilatNS)
  return(paste0(ilon,'-',ilat))
}

theCrop <- function(regrid_map, workPoints) {
  workPoints_tmp=workPoints # To realy take all studied zone
  workPoints_tmp[1] = xmin(regrid_map)+floor(round(abs(xmin(regrid_map)-workPoints[1])/res(regrid_map)[1],5))*res(regrid_map)[1]
  workPoints_tmp[2] = xmin(regrid_map)+ceiling(round(abs(xmin(regrid_map)-workPoints[2])/res(regrid_map)[1],5))*res(regrid_map)[1]
  workPoints_tmp[3] = ymin(regrid_map)+floor(round(abs(ymin(regrid_map)-workPoints[3])/res(regrid_map)[2],5))*res(regrid_map)[2]
  workPoints_tmp[4] = ymin(regrid_map)+ceiling(round(abs(ymin(regrid_map)-workPoints[4])/res(regrid_map)[2],5))*res(regrid_map)[2]
  return(crop(regrid_map,workPoints_tmp))
}

removePtsWithoutClim <- function(path_climate, selectPoints, clim_site="climName", id_site = "EUFGIS_ID")
{
  nbERROR = 0
  # listError <- NULL
  selectPoints_tmp <- NULL
  for ( iPts in c(1:nrow(selectPoints))) {
    if ( !file.exists(paste0(path_climate, selectPoints[iPts,clim_site], '.csv')) ) {
      nbERROR = nbERROR + 1
      # listError = rbind(listError, selectPoints[iPts,])
      cat('Le point', as.character(data.frame(selectPoints)[iPts,id_site]), ' na pas de fichier de forçage (remove).\n')
    } else {
      selectPoints_tmp = rbind(selectPoints_tmp, selectPoints[iPts,])
    }
  }
  if ( nbERROR > 0) cat("==> They was ",nbERROR,' missing climate files!\n',sep="")
  #fwrite(listError,file="/home/ardruel/data/soil/missingPoints.csv",row.names=F,col.names=T,sep=";")
  return(selectPoints_tmp)
}

removePtsWithoutClim2 <- function(path_climate, selectPoints, clim_site="climName", id_site = "EUFGIS_ID", bavard=FALSE)
{
  listError <- NULL
  listOK    <- NULL
  allClimFiles = list.files(path_climate)
  for ( iPts in c(1:nrow(selectPoints))) {
    if ( !(paste0(selectPoints[iPts,clim_site], '.csv')%in%allClimFiles) ) {
      listError = c(listError, selectPoints[iPts,clim_site])
    } else {
      listOK = c(listOK, iPts)
    }
  }
  if ( length(listError) > 0) {
    if ( bavard) cat('The points ', as.character(data.frame(listError)), ' do not have forcing file.\n')
    cat("==> They was ",length(listError),' missing climate files! (removed from list)\n',sep="")
  }
  #fwrite(listError,file="/home/ardruel/data/soil/missingPoints.csv",row.names=F,col.names=T,sep=";")
  return(selectPoints[listOK,])
}

checkValuesAndCorrectClimFiles <- function(path_climate, selectPoints,clim_site="climName")
{
  cat( "Start check values of climatic files and correct if necessary.\n" )
  for ( iPts in c(1:nrow(selectPoints))) {
    climPathname = paste0(path_climate, selectPoints[iPts,clim_site], '.csv')
    if ( file.exists(climPathname)) {
      climLoadData = fread(climPathname)
      if (anyNA(climLoadData)) {
        saveCorrect = TRUE
        cat("The file ",climPathname, " contails NA. Try to remove...")
        for ( icol in c(1:ncol(climLoadData))) {
          if ( anyNA(climLoadData[[icol]]) && saveCorrect) {
            cat(" correct ", colnames(climLoadData)[icol])
            clim_val = climLoadData[[icol]]
            if (all(is.na(clim_val))) {
              saveCorrect = FALSE
              cat(' ==> THERE IS NO VALUE. THIS CLIMATE WILL BE NOT CORRECT')
            } else if (saveCorrect) {
              for ( ival in c(1:length(clim_val)) ) {
                if ( is.na(clim_val[ival]) ) {
                  cat(' num ',ival)
                  clim_val[ival] = mean(clim_val[c((ival-1):(ival+1))], na.rm=TRUE)
                }
              }
              climLoadData[[icol]] = clim_val
              if ( anyNA(climLoadData[[icol]])) {
                saveCorrect = FALSE
                cat( "TAKE CARE! THERE IS STILL NA ! NOT SAVE ")
              }
            }
          }
        }
        if (saveCorrect) {
          sep_tmp = as.character(read.csv(climPathname)[1,])
          if ( grepl('\t',sep_tmp) ) {
            sepClim = '\t'
          } else if ( grepl(';',sep_tmp) ) {
            sepClim = ';'
          } else stop('No value identified for separation (sep in fwrite)!')
          fwrite(data.frame(climLoadData),file=climPathname,row.names=F,col.names=T,sep=sepClim)
          cat(" done! \n")
        } else cat(" \n")
      }
    }
  }
}

extractPoints <- function(filePoints,lonLatSelect=NULL,id=NULL)
{
  library(data.table)
  nameLon = c('lon', 'longitude', 'Longitude', 'LON', 'LongitudeD','x', 'X', 'long')
  nameLat = c('lat', 'latitude', 'Latitude', 'LAT', 'LatitudeD','y', 'Y')
  
  inputDataPts = fread(filePoints)
  
  nlon = intersect(colnames(inputDataPts),nameLon)
  nlat = intersect(colnames(inputDataPts),nameLat)
  if ( is.null(lonLatSelect) && is.null(id) ) {
    inputDataPts = data.frame(inputDataPts[[nlon]], inputDataPts[[nlat]] ) #data.frame( inputDataPts$lon, inputDataPts$lat )
    colnames(inputDataPts)=c('lon','lat')
  } else if ( is.null(lonLatSelect) ) {
    inputDataPts = data.frame(inputDataPts[[id]], inputDataPts[[nlon]], inputDataPts[[nlat]]) #data.frame( inputDataPts$lon, inputDataPts$lat )
    colnames(inputDataPts)=c('id','lon','lat')
  } else if ( is.null(id) ) {
    inputDataPts = data.frame(inputDataPts[[nlon]], inputDataPts[[nlat]] , inputDataPts[[lonLatSelect[1]]])
    colnames(inputDataPts)=c('lon','lat',lonLatSelect[1])
    inputDataPts = inputDataPts[inputDataPts[[lonLatSelect[1]]]==lonLatSelect[2],]
  } else {
    inputDataPts = data.frame(inputDataPts[[id]], inputDataPts[[nlon]], inputDataPts[[nlat]] , inputDataPts[[lonLatSelect[1]]])
    colnames(inputDataPts)=c('id', 'lon','lat',lonLatSelect[1])
    inputDataPts = inputDataPts[inputDataPts[[lonLatSelect[1]]]==lonLatSelect[2],]
  }
  # On retire les lignes ou il y a au moins un NA
  selectPoints = inputDataPts[rowSums(is.na(inputDataPts)) <= 0,]
  return(selectPoints)
}

# Select or/and load point of interest
convertInPoints <- function(workPoints,regrid_map,lonLatSelect=NULL,id=NULL)
{
  
  if ( is.character(regrid_map) ) regrid_map = rast(regrid_map)
  
  if ( is.character(workPoints) ) {
    selectPoints = extractPoints(workPoints,lonLatSelect,id)
  } else if (is.list(workPoints)) {
    ilon = vector()
    ilat = vector()
    for ( i in 1:length(workPoints) ) {
      ilon[i] = workPoints[[i]][1]
      ilat[i] = workPoints[[i]][2]
    }
    selectPoints = data.frame(lon = ilon, lat = ilat)
  } else if (is.numeric(workPoints)  && length(workPoints)==2) {
    selectPoints = data.frame(lon = workPoints[1], lat = workPoints[2])
  } else if (is.numeric(workPoints)  && length(workPoints)==4) {
    workPoints_tmp=workPoints # To realy take all studied zone
    workPoints_tmp[1] = xmin(regrid_map)+floor(round(abs(xmin(regrid_map)-workPoints[1])/res(regrid_map)[1],5))*res(regrid_map)[1]
    workPoints_tmp[2] = xmin(regrid_map)+ceiling(round(abs(xmin(regrid_map)-workPoints[2])/res(regrid_map)[1],5))*res(regrid_map)[1]
    workPoints_tmp[3] = ymin(regrid_map)+floor(round(abs(ymin(regrid_map)-workPoints[3])/res(regrid_map)[2],5))*res(regrid_map)[2]
    workPoints_tmp[4] = ymin(regrid_map)+ceiling(round(abs(ymin(regrid_map)-workPoints[4])/res(regrid_map)[2],5))*res(regrid_map)[2]
    regrid_map_crop   = crop(regrid_map,workPoints_tmp)
    selectPoints = crds(regrid_map_crop)
  } else {
    stop('The indicate work points are not extractable from ', workPoints, '!')
  }
  
  if ( !(is.numeric(workPoints)  && length(workPoints)==4) ){
    selectPoints = crds(regrid_map,na.rm=F)[terra::cellFromXY(regrid_map, as.matrix(selectPoints)),]
    if ( !is.matrix(selectPoints)) {
      selectPoints = data.frame( x = selectPoints[['x']], y = selectPoints[['y']])
    } else {
      selectPoints = data.frame(selectPoints)
    }
  } 
  return(selectPoints)
}

 
selectSpecies <- function(selectPoints,forest_type, forest_list_type = "")
{
  if ( forest_type == "lcForest" && any(forest_list_type!="") && any(forest_list_type==forest_type) ) forest_list_type = forest_list_type[-which(forest_list_type==forest_type)]
  if ( forest_type == "lcForest" && length(forest_list_type)==0 ) stop('If you want to select the specie of major forest type, you have to indicate the liste of forest type!') 
  forest_type_ori = forest_type
  speEsp = " - "
  
  mainForest = NULL
  species_list = NULL
  for ( iPts in c(1:nrow(selectPoints)) ) {
    koppenClass = selectPoints[iPts,"bG_class"]
    if ( forest_type_ori == "lcForest" ) {
      forest_type = forest_list_type[which.max(selectPoints[iPts,forest_list_type])]
      mainForest = c(mainForest, forest_type)
    } 
    if ( forest_type == "lc60" ) {
      if ( koppenClass%in%c("Dsa","Dsb","Dsc","BSh","BSk","Csa","Csb","Cfa") ) { # Vegetation xérophylle Méditeranéenne
        ispecie = "Quercus pubescens"
      } else if ( koppenClass%in%c("Cfb","Cfc") ) {      # Végétation océanique tempérée
        ispecie = "Quercus robur"
      } else if ( koppenClass%in%c("Dfa","Dfb") ) {      # Végétation continentale centrale
        ispecie = "Fagus sylvatica"
      } else if ( koppenClass%in%c("Dfc","ET", "EF") ) { # Végétation continentale boréale
        ispecie = "Betula pendula" # Because "Betula pubescens" is missing
      } else if ( koppenClass%in%c("ET2_montagne") ) {   # Vegetation haute Montagne
        ispecie = "Fagus sylvatica"
      } else ispecie = NA  # nothing + desert (BWh - BWk) 
    } else if ( forest_type == "lc70" ) {
      if ( koppenClass%in%c("Dsa","Dsb","Dsc","BSh","BSk","Csa","Csb","Cfa") ) { # Vegetation xérophylle Méditeranéenne
        ispecie = "Pinus halepensis"
      } else if ( koppenClass%in%c("Cfb","Cfc") ) {      # Végétation océanique tempérée
        ispecie = "Pinus sylvestris"
      } else if ( koppenClass%in%c("Dfa","Dfb") ) {      # Végétation continentale centrale
        ispecie = "Abies alba"
      } else if ( koppenClass%in%c("Dfc","ET", "EF") ) { # Végétation continentale boréale
        ispecie = "Pinus sylvestris"
      } else if ( koppenClass%in%c("ET2_montagne") ) {   # Vegetation haute Montagne
        ispecie = "Abies alba"
      } else ispecie = NA
    } else if ( forest_type == "lc90" ) {
      if ( koppenClass%in%c("Dsa","Dsb","Dsc","BSh","BSk","Csa","Csb","Cfa") ) { # Vegetation xérophylle Méditeranéenne
        ispecie = paste("Quercus ilex","Pinus halepensis",sep=speEsp)
      } else if ( koppenClass%in%c("Cfb","Cfc") ) {      # Végétation océanique tempérée
        ispecie = paste("Quercus robur", "Pinus sylvestris",sep=speEsp)
      } else if ( koppenClass%in%c("Dfa","Dfb") ) {      # Végétation continentale centrale
        ispecie = paste("Fagus sylvatica","Abies alba",sep=speEsp)
      } else if ( koppenClass%in%c("Dfc","ET", "EF") ) { # Végétation continentale boréale
        ispecie = paste("Betula pendula","Pinus sylvestris",sep=speEsp)  # First "Betula pendula" Because "Betula pubescens" is missing
      } else if ( koppenClass%in%c("ET2_montagne") ) {   # Vegetation haute Montagne
        ispecie = paste("Fagus sylvatica","Abies alba",sep=speEsp)
      } else ispecie = NA
    } else if ( forest_type == "lc100" ) {
      if ( koppenClass%in%c("Dsa","Dsb","Dsc","BSh","BSk","Csa","Csb","Cfa") ) { # Vegetation xérophylle Méditeranéenne
        ispecie = "Quercus ilex"
      } else if ( koppenClass%in%c("Cfb","Cfc") ) {      # Végétation océanique tempérée
        ispecie = "Quercus robur"
      } else if ( koppenClass%in%c("Dfa","Dfb") ) {      # Végétation continentale centrale
        ispecie = "Quercus robur"
      } else if ( koppenClass%in%c("Dfc","ET", "EF") ) { # Végétation continentale boréale
        ispecie = "Betula pendula" # Because "Betula pubescens" is missing
      } else if ( koppenClass%in%c("ET2_montagne") ) {   # Vegetation haute Montagne
        ispecie = "Fagus sylvatica"
      } else ispecie = NA
    } else if ( forest_type == "lc120" ) {
      if ( koppenClass%in%c("Dsa","Dsb","Dsc","BSh","BSk","Csa","Csb","Cfa") ) { # Vegetation xérophylle Méditeranéenne
        ispecie = "Quercus ilex"
      } else if ( koppenClass%in%c("Cfb","Cfc") ) {      # Végétation océanique tempérée
        ispecie = "Corylus avellana"
      } else if ( koppenClass%in%c("Dfa","Dfb") ) {      # Végétation continentale centrale
        ispecie = "Corylus avellana"
      } else if ( koppenClass%in%c("Dfc","ET", "EF") ) { # Végétation continentale boréale
        ispecie = "Betula pendula" # Because "Betula pubescens" is missing
      } else if ( koppenClass%in%c("ET2_montagne") ) {   # Vegetation haute Montagne
        ispecie = "Juniperus communis"
      } else ispecie = NA
    } else stop('The forest type is not yet define in "selectSpecies" function')
    # if ( is.na(ispecie)) cat(koppenClass,"  ")
    species_list = c(species_list, ispecie)
  }
  species_list = cbind(species = species_list, mainForest = mainForest)
  #if ( forest_type_ori == "lcForest" ) colnames(species_list) = 
  return(species_list)
}

LAImeanLC_Koppen <- function(pointsData, LAIname = "LAI_max")
{
  BioGeoZone_list = selectBioGeoZone(pointsData)
  BioGeoZone_list = BioGeoZone_list[!is.na(BioGeoZone_list)]
  
  LAI = NULL
  for ( iBGZ in unique(BioGeoZone_list) ) {
    LAItmp = pointsData[[LAIname]][BioGeoZone_list==iBGZ]
    LAI[[iBGZ]] = mean(LAItmp)
  }
  
  return(LAI)
}

selectBioGeoZone <- function(selectPoints)
{
  BioGeoZone_list = NULL
  for ( iPts in c(1:nrow(selectPoints)) ) {
    koppenClass = selectPoints[iPts,"bG_class"]

    if ( koppenClass%in%c("Dsa","Dsb","Dsc","BSh","BSk","Csa","Csb","Cfa") ) { # Vegetation xérophylle Méditeranéenne
      iBGZ = "Xerophyllous mediterranean"
    } else if ( koppenClass%in%c("Cfb","Cfc") ) {      # Végétation océanique tempérée
      iBGZ = "Temperate oceanic"
    } else if ( koppenClass%in%c("Dfa","Dfb") ) {      # Végétation continentale centrale
      iBGZ = "Central continental"
    } else if ( koppenClass%in%c("Dfc","ET", "EF") ) { # Végétation continentale boréale
      iBGZ = "Boreal continental"
    } else if ( koppenClass%in%c("ET2_montagne") ) {   # Vegetation haute Montagne
      iBGZ = "High mountain"
    } else iBGZ = NA  # nothing + desert (BWh - BWk) 
    # if ( is.na(iBGZ)) cat(koppenClass,"  ")
    BioGeoZone_list = c(BioGeoZone_list, iBGZ)
  }
  return(BioGeoZone_list)
}


loadKoppen <- function(koppenPath, simpleNames = TRUE)
{
  koppen=rast(koppenPath)
  unique(values(koppen))
  levels(koppen)
  
  listBioGeoID   = c(0:32)
  if ( simpleNames ) {
    listBioGeoName = c("water", "Af", "Am", "Aw",
                       "BWh", "BWk",
                       "BSh", "BSk",
                       "Csa", "Csb", "Csc",
                       "Cwa", "Cwb", "Cwc",
                       "Cfa", "Cfb", "Cfc",
                       "Dsa", "Dsb", "Dsc", "Dsd",
                       "Dwa", "Dwb", "Dwc", "Dwd",
                       "Dfa", "Dfb", "Dfc", "Dfd",
                       "ET", "EF",
                       "ET2_montagne","ras")
  } else {
    listBioGeoName = c("water", "Tropical rainforest", "Tropical - monsoon", "Tropical - savannah",
                       "Arid - desert hot", "Arid - desert cold",
                       "Arid - steppe hot", "Arid - steppe cold",
                       "Temperate - dry & hot summer", "Temperate - dry & warm summer", "Temperate - dry & cold summer",
                       "Temperate - dry winter - hot summer", "Temperate - dry winter - warm summer", "Temperate - dry winter - cold summer",
                       "Temperate - without dry season - hot summer", "Temperate - without dry season - warm summer", "Temperate - without dry season - cold summer",
                       "Cold - dry & hot summer", "Cold - dry & warm summer", "Cold - dry & cold summer", "Cold - dry summer & very cold winter",
                       "Cold - dry winter - hot summer", "Cold - dry winter - warm summer", "Cold - dry winter - cold summer", "Cold - dry & very cold winter",
                       "Cold - without dry season - hot summer", "Cold - without dry season - warm summer", "Cold - without dry season - cold summer", "Cold - without dry season - very cold winter",
                       "Polar - tundra", "Polar - frost",
                       "high mountains","ras")
  }
  
  
  bioGeo_classes <- data.frame(bG_code  = listBioGeoID,
                               bG_class = listBioGeoName)
  levels(koppen)=bioGeo_classes
  
  # bioGeoColor = grDevices::hcl.colors(33, "viridis")
  # bioGeoColor[1] = "white"
  # plot(crop(koppen,c(-10,45,30,81)), col = bioGeoColor)
  
  # koppenClass = c("water", "Af", "Am", "Aw", "BWh", "BWk", "BSh", "BSk",
  #                 "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc", "Cfa", "Cfb", "Cfc",
  #                 "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd", "Dfa", "Dfb", "Dfc", "Dfd",
  #                 "ET", "EF", "ET2_montagne")
  # test = rast("/home/ardruel/Téléchargements/Koppen/Raster files/world_koppen/hdr.adf")
  # #values(test)[is.na(values(test))] = 1
  # test[] = factor(koppenClass[test[]])
  # koppen_classes <- data.frame(value = c(1:length(koppenClass)), koppenClass = c( koppenClass))
  # levels(test)=koppen_classes
  # plot(test)
  
  return(koppen)
}

euptf_maker <- function(soilData, ptfList, coarseFrag, layer_thickness, speName, iname, wfcSpi, wiltSpi)
{
  # pftList: c(fc, wilt, vg)
  res_all <- NULL
  soilData$TOPSOIL <- factor(soilData$TOPSOIL, levels = c("sub", "top"))
  if ( !(is.null(ptfList[1]) || is.na(ptfList[1])) ) fc = predict.ptf(newdata = soilData, ptf = ptfList[1])
  if ( !(is.null(ptfList[2]) || is.na(ptfList[2])) ) wp = predict.ptf(newdata = soilData, ptf = ptfList[2])
  if ( !(is.null(ptfList[3]) || is.na(ptfList[3])) ) {
    res_vg = data.frame(predict.ptf(newdata = soilData, ptf = ptfList[3]))
    fc_vg = vanGenuchten(thetaSat = res_vg[["ths"]], thetaRes = res_vg[["thr"]], alpha = res_vg[["alp"]], n = res_vg[["n"]], PsiTarget = wfcSpi)
    wp_vg = vanGenuchten(thetaSat = res_vg[["ths"]], thetaRes = res_vg[["thr"]], alpha = res_vg[["alp"]], n = res_vg[["n"]], PsiTarget = wiltSpi)
    RU_vg = RU_from_wp_fc(fc_vg, wp_vg, coarseFrag, layer_thickness)
    res_all = data.frame(res_vg, l = 0.5, fc_vg, wp_vg, RU_vg)
  }
  if ( !(is.null(ptfList[1]) || is.na(ptfList[1])) ) {
    if ( is.null(res_all) ) {
      res_all = data.frame(fc)
    } else {
      res_all = data.frame(res_all, fc)
    }
  }
  if ( !(is.null(ptfList[2]) || is.na(ptfList[2])) ) {
    if ( is.null(res_all) ) {
      res_all = data.frame(wp)
    } else {
      res_all = data.frame(res_all, wp)
    }
  }
  
  if ( !(is.null(ptfList[1]) || is.na(ptfList[1])) &&  !(is.null(ptfList[2]) || is.na(ptfList[2]))) {
    RU = RU_from_wp_fc(fc, wp, coarseFrag, layer_thickness)
    res_all = data.frame(res_all, RU)
  }
  
  colnames(res_all) = paste0(colnames(res_all), '_', speName, iname)
  
  return(res_all)
}

vanGenuchten <- function(thetaSat, thetaRes, alpha, n, PsiTarget) # PsiTarget=0.033 (fc) / 1.5 or 3.5 or...  (wp)
{
  return( thetaRes+(thetaSat-thetaRes)/(1+(alpha*PsiTarget*10000)^n)^(1-1/n) )
}

RU_from_wp_fc <- function(fieldCap, wiltingPoint, coarseFrag, layer_thickness) {   # coarseFrag in % / layer_thickness in m
  return((fieldCap - wiltingPoint) * (1 - (coarseFrag / 100)) * layer_thickness * 1000)
}

createSoilFile <- function(selectPoints, soil_pathename, coarseFragCoef,
                           SE_coarseFrag_1, SE_coarseFrag_2, SE_coarseFrag_3, SE_fieldCap, SE_wiltPoint, SE_vg_alpha, SE_vg_n, SE_vg_I, SE_vg_ksat,
                           SE_vg_sat_cap, SE_vg_res_cap, SE_gsoil, SE_depth1, SE_depth2, SE_depth3, SE_offSetPsoil)
{
  sureauSoil = c("Name","Value")
  sureauSoil = rbind(sureauSoil, c("RFC_1",max(0,min(99,selectPoints[[SE_coarseFrag_1]]*coarseFragCoef))))
  sureauSoil = rbind(sureauSoil, c("RFC_2",max(0,min(99,selectPoints[[SE_coarseFrag_2]]*coarseFragCoef))))
  sureauSoil = rbind(sureauSoil, c("RFC_3",max(0,min(99,selectPoints[[SE_coarseFrag_3]]*coarseFragCoef)))) #99
  sureauSoil = rbind(sureauSoil, c("field_capacity",selectPoints[[SE_fieldCap]])) #"wfc_0-200"
  sureauSoil = rbind(sureauSoil, c("wilting_point",selectPoints[[SE_wiltPoint]]))  #"wilt_0-200"
  sureauSoil = rbind(sureauSoil, c("alpha_vg",selectPoints[[SE_vg_alpha]]))        #"α_0-200"
  sureauSoil = rbind(sureauSoil, c("n_vg",selectPoints[[SE_vg_n]]))                #"n_0-200"
  sureauSoil = rbind(sureauSoil, c("I_vg",selectPoints[[SE_vg_I]]))                #"I_0-200"
  sureauSoil = rbind(sureauSoil, c("Ksat_vg",selectPoints[[SE_vg_ksat]]))
  sureauSoil = rbind(sureauSoil, c("saturation_capacity_vg",selectPoints[[SE_vg_sat_cap]])) #"θsat_30-200"
  sureauSoil = rbind(sureauSoil, c("residual_capacity_vg",selectPoints[[SE_vg_res_cap]]))   #"θres_30-200"
  sureauSoil = rbind(sureauSoil, c("gSoil0",SE_gsoil))
  sureauSoil = rbind(sureauSoil, c("depth1",SE_depth1))
  sureauSoil = rbind(sureauSoil, c("depth2",SE_depth2))
  sureauSoil = rbind(sureauSoil, c("depth3",SE_depth3))
  sureauSoil = rbind(sureauSoil, c("offSetPsoil",SE_offSetPsoil))
  # sureauSoil = rbind(sureauSoil, c("b_camp",-selectPoints[[SE_c_b]][iPts]))
  # sureauSoil = rbind(sureauSoil, c("psie",-selectPoints[[SE_c_spie]][iPts]/999.972))
  # sureauSoil = rbind(sureauSoil, c("saturation_capacity_campbell",selectPoints[[SE_c_sat_cap]][iPts]))
  # sureauSoil = rbind(sureauSoil, c("ksat_campbell",selectPoints[[SE_c_ksat]][iPts]))
  fwrite(data.frame(sureauSoil),file=soil_pathename,row.names=F,col.names=F,sep=";")
}

createSoilFileWithCF <- function(selectPoints, soil_pathename, listCoarseFrag,
                           SE_fieldCap, SE_wiltPoint, SE_vg_alpha, SE_vg_n, SE_vg_I, SE_vg_ksat,
                           SE_vg_sat_cap, SE_vg_res_cap, SE_gsoil, SE_depth1, SE_depth2, SE_depth3, SE_offSetPsoil)
{
  sureauSoil = c("Name","Value")
  sureauSoil = rbind(sureauSoil, c("RFC_1",listCoarseFrag[1]))
  sureauSoil = rbind(sureauSoil, c("RFC_2",listCoarseFrag[2]))
  sureauSoil = rbind(sureauSoil, c("RFC_3",listCoarseFrag[3])) #99
  sureauSoil = rbind(sureauSoil, c("field_capacity",selectPoints[[SE_fieldCap]])) #"wfc_0-200"
  sureauSoil = rbind(sureauSoil, c("wilting_point",selectPoints[[SE_wiltPoint]]))  #"wilt_0-200"
  sureauSoil = rbind(sureauSoil, c("alpha_vg",selectPoints[[SE_vg_alpha]]))        #"α_0-200"
  sureauSoil = rbind(sureauSoil, c("n_vg",selectPoints[[SE_vg_n]]))                #"n_0-200"
  sureauSoil = rbind(sureauSoil, c("I_vg",selectPoints[[SE_vg_I]]))                #"I_0-200"
  sureauSoil = rbind(sureauSoil, c("Ksat_vg",selectPoints[[SE_vg_ksat]]))
  sureauSoil = rbind(sureauSoil, c("saturation_capacity_vg",selectPoints[[SE_vg_sat_cap]])) #"θsat_30-200"
  sureauSoil = rbind(sureauSoil, c("residual_capacity_vg",selectPoints[[SE_vg_res_cap]]))   #"θres_30-200"
  sureauSoil = rbind(sureauSoil, c("gSoil0",SE_gsoil))
  sureauSoil = rbind(sureauSoil, c("depth1",SE_depth1))
  sureauSoil = rbind(sureauSoil, c("depth2",SE_depth2))
  sureauSoil = rbind(sureauSoil, c("depth3",SE_depth3))
  sureauSoil = rbind(sureauSoil, c("offSetPsoil",SE_offSetPsoil))
  # sureauSoil = rbind(sureauSoil, c("b_camp",-selectPoints[[SE_c_b]][iPts]))
  # sureauSoil = rbind(sureauSoil, c("psie",-selectPoints[[SE_c_spie]][iPts]/999.972))
  # sureauSoil = rbind(sureauSoil, c("saturation_capacity_campbell",selectPoints[[SE_c_sat_cap]][iPts]))
  # sureauSoil = rbind(sureauSoil, c("ksat_campbell",selectPoints[[SE_c_ksat]][iPts]))
  fwrite(data.frame(sureauSoil),file=soil_pathename,row.names=F,col.names=F,sep=";")
}

RUfromModels <- function(ResAnalysis, QPLCl9_target, doModels = c(TRUE, TRUE, TRUE, TRUE), bavard = FALSE)
{
  if (!any(doModels)) stop("You have to chose at least one model !")
  
  if ( doModels[1] ) model1 = tryNLSmodel(ResAnalysis)
  if ( doModels[2] ) model2 = tryNLSmodel2(ResAnalysis)
  if ( doModels[3] ) model3 = tryNLSmodel3(ResAnalysis)
  if ( doModels[4] ) model4 = tryNLSmodel4(ResAnalysis)
  

  sigmaRes = rep(Inf,4)
  if ( doModels[1] && class(model1) != "try-error" ) sigmaRes[1] = summary(model1)$sigma
  if ( doModels[2] && class(model2) != "try-error" ) sigmaRes[2] = summary(model2)$sigma
  if ( doModels[3] && class(model3) != "try-error" ) sigmaRes[3] = summary(model3)$sigma
  if ( doModels[4] && class(model4) != "try-error" ) sigmaRes[4] = summary(model4)$sigma
  if ( nrow(ResAnalysis)<=3 && min(sigmaRes)%in%sigmaRes[3:4] && all(sigmaRes[3:4] > 0.5) ) sigmaRes = sigmaRes*Inf # Remove choice on the sigmoide at simu 2 if there is an another choice with 1/x.... but to be confirmed
  minSigmaModel = 0
  if ( any(sigmaRes!=Inf) ) minSigmaModel = which(sigmaRes==min(sigmaRes))
  
  RU_cible = NULL
  if ( 2 %in% minSigmaModel ) {
    if ( bavard ) cat("The model sigmoid NEW is selected for RU target. ")
    model_Pval = summary(model2)$parameters['P',1]
    model_Sval = summary(model2)$parameters['slope',1]
    model_Sigval = summary(model2)$parameters['sigmo',1]
    RU_cible = model_Pval - log(100/QPLCl9_target - model_Sigval) * 25/model_Sval
  } else if ( 1 %in% minSigmaModel ) {
    if ( bavard ) cat("The model sigmoid original is selected for RU target. ")
    model_Pval = summary(model1)$parameters['P',1]
    model_Sval = summary(model1)$parameters['slope',1]
    RU_cible = model_Pval - log(100/QPLCl9_target - 1) * 25/model_Sval
  } else if ( 3 %in% minSigmaModel ) {
    if ( bavard ) cat("The model 1/x is selected for RU target. ")
    model_Pval = summary(model3)$parameters['P',1]
    model_Sval = summary(model3)$parameters['slope',1]
    RU_cible = model_Sval / QPLCl9_target - model_Pval
  } else if ( 4 %in% minSigmaModel ) {
    if ( bavard ) cat("The model 1/x+offs is selected for RU target. ")
    model_Pval = summary(model4)$parameters['P',1]
    model_Sval = summary(model4)$parameters['slope',1]
    model_Oval = summary(model4)$parameters['offS',1]
    RU_cible = model_Sval / (QPLCl9_target-model_Oval) - model_Pval
  }
  return(RU_cible)
}

plotIPtsModels <- function(ResAnalysis, QPLCl9_target, iselectPoints, plot_name, profondeur_soil, doModels = c(TRUE, TRUE, TRUE, TRUE), RU_cible=NULL)
{
  max_RU = max(ResAnalysis[,1], RU_cible, na.rm = TRUE)
  nd = data.frame(seq(0, max_RU, 1))
  names(nd) = "RU"
  plot(ResAnalysis[,"QPLCl9"] ~ ResAnalysis[,"RU"],pch=21, bg=4, ylab="Qantile .9 PLC Max", xlab= "TAW (fc-wp)", cex=c(1.5,rep(2,nrow(ResAnalysis)-1)),  ylim=c(0,100), ) #col=c("red",rep("blue",nrow(ResAnalysis)-1))
  for ( isimu in c(2:nrow(ResAnalysis))) {
    cat('Simu ',isimu-1)
    if ( isimu == nrow(ResAnalysis) ) lty_simu=1 else lty_simu=2
    lwd_simu = 2*isimu/nrow(ResAnalysis)
    
    if ( doModels[1] ) {
      model1  =  tryNLSmodel(ResAnalysis[1:isimu,])
      if ( class(model1) != "try-error" ) {
        FitRU = cbind.data.frame(RU=nd, Q90PLC=predict(model1, newdata=nd))
        lines(FitRU$Q90PLC~FitRU$RU, lwd=lwd_simu, type='l',lty=lty_simu,col="grey")
        cat(" => OLD model sigma value ", summary(model1)$sigma)
        
      }
    } 
    if ( doModels[2] ) {
      model2 = tryNLSmodel2(ResAnalysis[1:isimu,])
      if ( class(model2) != "try-error" ) {
        FitRU = cbind.data.frame(RU=nd, Q90PLC=predict(model2, newdata=nd))
        lines(FitRU$Q90PLC~FitRU$RU, lwd=lwd_simu, type='l',lty=lty_simu,col="blue")
        cat(" => new model sigma value ", summary(model2)$sigma)
      }
    }
    if ( doModels[3] ) {
      model3  =  tryNLSmodel3(ResAnalysis[1:isimu,])
      if ( class(model3) != "try-error" ) {
        FitRU = cbind.data.frame(RU=nd, Q90PLC=predict(model3, newdata=nd))
        lines(FitRU$Q90PLC~FitRU$RU, lwd=lwd_simu, type='l',lty=lty_simu,col="green")
        cat(" => 1/x sigma value ", summary(model3)$sigma)
      }
    }
    
    if ( doModels[4] ) {
      model4  =  tryNLSmodel4(ResAnalysis[1:isimu,])
      if ( class(model4) != "try-error" ) {
        FitRU = cbind.data.frame(RU=nd, Q90PLC=predict(model4, newdata=nd))
        lines(FitRU$Q90PLC~FitRU$RU, lwd=lwd_simu, type='l',lty=lty_simu,col="darkgreen")
        cat(" => 1/x+offs sigma value ", summary(model4)$sigma)
      }
    }
    cat('\n')
    
  }
  
  title(plot_name)#paste0("Point ",selectPoints[[clim_site]][iPts])) 
  iline = -1
  if ("LAI_max"%in%colnames(iselectPoints)) {
    mtext(paste0("LAI max = ",round(iselectPoints[["LAI_max"]],1)),at=max_RU*0.75, line = iline)
    iline = iline - 1
  }
  if (paste0("SoilClass_0-",profondeur_soil,"00")%in%colnames(iselectPoints)) {
    mtext(paste0("Soil type = '",iselectPoints[[paste0("SoilClass_0-",profondeur_soil,"00")]],"'"),at=max_RU*0.75, line = iline)
    iline = iline - 1
  } 
  abline(h=QPLCl9_target,col="blue",lty=2)
  
  if ( any(doModels) ) {
    sigmaRes = rep(Inf,4)
    if ( doModels[1] && class(model1) != "try-error" ) sigmaRes[1] = summary(model1)$sigma
    if ( doModels[2] && class(model2) != "try-error" ) sigmaRes[2] = summary(model2)$sigma
    if ( doModels[3] && class(model3) != "try-error" ) sigmaRes[3] = summary(model3)$sigma
    if ( doModels[3] && class(model4) != "try-error" ) sigmaRes[4] = summary(model4)$sigma
    minSigmaModel = which(sigmaRes==min(sigmaRes))
    
    if ( 2 %in% minSigmaModel ) {
      modelSelect = "New Sigmoid"
      modelColor  = "blue"
      if ( is.null(RU_cible) ) {
        model_Pval = summary(model2)$parameters['P',1]
        model_Sval = summary(model2)$parameters['slope',1]
        model_Sigval = summary(model2)$parameters['sigmo',1]
        RU_cible = model_Pval - log(100/QPLCl9_target - model_Sigval) * 25/model_Sval
      }

    } else if ( 1 %in% minSigmaModel ) {
      modelSelect = "Ori. Sigmoid"
      modelColor  = "grey"
      if ( is.null(RU_cible) ) {
        model_Pval = summary(model1)$parameters['P',1]
        model_Sval = summary(model1)$parameters['slope',1]
        RU_cible = model_Pval - log(100/QPLCl9_target - 1) * 25/model_Sval
      }
    } else if ( 3 %in% minSigmaModel ) {
      modelSelect = "1/x"
      modelColor  = "green"
      if ( is.null(RU_cible) ) {
        model_Pval = summary(model3)$parameters['P',1]
        model_Sval = summary(model3)$parameters['slope',1]
        RU_cible = model_Sval / QPLCl9_target - model_Pval
      }
    }  else if ( 4 %in% minSigmaModel ) {
      modelSelect = "1/x+offs"
      modelColor  = "darkgreen"
      if ( is.null(RU_cible) ) {
        model_Pval = summary(model4)$parameters['P',1]
        model_Sval = summary(model4)$parameters['slope',1]
        model_Oval = summary(model4)$parameters['offS',1]
        RU_cible = model_Sval / (QPLCl9_target-model_Oval) - model_Pval
      }
    }
    cat("The model ",modelSelect, " is selected (Point x = ",iselectPoints[['x']], ' y = ',iselectPoints[['y']],").\n",sep="")
    if ( is.null(RU_cible) ) {
      mtext(paste0("No corresp. TAW find!"),at=max_RU*0.75, line = iline)
      mtext(paste0("Best model = ",modelSelect),at=max_RU*0.75, line = iline-1,col=modelColor)
    } else {
      mtext(paste0("Selected TAW = ",round(RU_cible),"mm"),at=max_RU*0.75, line = iline)
      mtext(paste0("With model = ",modelSelect),at=max_RU*0.75, line = iline-1,col=modelColor)
      points(RU_cible,QPLCl9_target,col="red",pch=3,lwd=2)
    }
  }
  
  returnModels <- NULL
  returnModels[[4]] = NULL
  if ( doModels[1] ) returnModels[[1]] = model1
  if ( doModels[2] ) returnModels[[2]] = model2
  if ( doModels[3] ) returnModels[[3]] = model3
  if ( doModels[4] ) returnModels[[4]] = model4

  return(returnModels)
}

# make nls model
tryNLSmodel <- function(ResAnalysis) {
  if ( any(ResAnalysis[-1,2]>= 40 & ResAnalysis[-1,2] <= 60) ) {
    Pval = ResAnalysis[-1,1][which(ResAnalysis[-1,2]>= 40 & ResAnalysis[-1,2] <= 60)[1]]
  } else if ( any(ResAnalysis[-1,2]>= 15 & ResAnalysis[-1,2] < 40) && any(ResAnalysis[-1,2]> 60 & ResAnalysis[-1,2] <= 85) ) {
    Pval = mean(max(ResAnalysis[-1,1][which(ResAnalysis[-1,2]>= 15 & ResAnalysis[-1,2] < 40)]),min(ResAnalysis[-1,1][which(ResAnalysis[-1,2]> 60 & ResAnalysis[-1,2] <= 85)]))
  } else {
    Pval = c(80,160,320,640,960,1280,40,20)
  }
  
  modelOK = FALSE
  for ( iPval in Pval ) {
    if ( !modelOK ) {
      # if (iPval <50) {
      #   slopeVal = -5
      if (iPval <100) {
        slopeVal = -2
      } else if (iPval <200) {
        slopeVal = -1
      } else  {
        slopeVal = -0.5
      }
      model  =  try(nls(QPLCl9 ~ 100/(1+exp(slope/25*(P-RU))), data=ResAnalysis, start=c(slope=slopeVal,P=iPval)), silent = TRUE )
      # model  =  try(nls(QPLCl9 ~ ((100-min(ResAnalysis[,2]))/(1+exp(slope/25*(P-RU)))+min(ResAnalysis[,2])), data=ResAnalysis, start=c(slope=-2,P=80)), silent = TRUE )
      if ( class(model) != "try-error" ) modelOK = TRUE
    }
  }
  
  return(model)
}

tryNLSmodel2 <- function(ResAnalysis) {
  Pval = NULL
  if ( any(ResAnalysis[-1,2]>= 40 & ResAnalysis[-1,2] <= 60) ) {
    Pval = ResAnalysis[-1,1][which(ResAnalysis[-1,2]>= 40 & ResAnalysis[-1,2] <= 60)[1]]
  } else if ( any(ResAnalysis[-1,2]>= 15 & ResAnalysis[-1,2] < 40) && any(ResAnalysis[-1,2]> 60 & ResAnalysis[-1,2] <= 85) ) {
    Pval = mean(max(ResAnalysis[-1,1][which(ResAnalysis[-1,2]>= 15 & ResAnalysis[-1,2] < 40)]),min(ResAnalysis[-1,1][which(ResAnalysis[-1,2]> 60 & ResAnalysis[-1,2] <= 85)]))
  }
  Pval = c(Pval, 80, -30, 160, 320, 640, 960, 1280, 40, 20)
  
  modelOK = FALSE
  for ( iPval in Pval ) {
    if ( !modelOK ) {
      if (iPval <0) {
        slopeVal = -0.75
        sigmoVal = -2
      } else if (iPval <100) {
        slopeVal = -0.6
        sigmoVal = 0.8
      } else if (iPval <200) {
        slopeVal = -0.4
        sigmoVal = 0.8
      } else  {
        slopeVal = -0.4
        sigmoVal = 1
      }
      model  =  try(nls(QPLCl9 ~ 100/(sigmo+exp(slope/25*(P-RU))), data=ResAnalysis, start=c(slope=slopeVal,P=iPval,sigmo=sigmoVal)), silent = TRUE )
      # model  =  try(nls(QPLCl9 ~ ((100-min(ResAnalysis[,2]))/(1+exp(slope/25*(P-RU)))+min(ResAnalysis[,2])), data=ResAnalysis, start=c(slope=-2,P=80)), silent = TRUE )
      if ( class(model) != "try-error" ) modelOK = TRUE
    }
  }
  
  return(model)
}

tryNLSmodel3 <- function(ResAnalysis) {
  
  model = try(nls(QPLCl9 ~ slope/(RU+P), data=ResAnalysis, start=c(slope = 50, P=0.5)), silent = TRUE )
  
 # if ( class(model) != "try-error" && nrow(ResAnalysis)<=3 && summary(model)$sigma>0.5 ) model = try(nls(QPLCl9 ~ slope/(RU+P), data=ResAnalysis[1,], start=c(slope = 50, P=0.5)), silent = TRUE )
 # if ( class(model) != "try-error" && summary(model)$sigma>50 ) model = try(nls(QPLCl9 ~ slope/(RU+P), data=ResAnalysis[1,], start=c(slope = 50, P=0.5)), silent = TRUE )
  
  return(model)
}

tryNLSmodel4 <- function(ResAnalysis) {
  model4 = try(nls(QPLCl9 ~ slope/(RU+P) + offS, data=ResAnalysis, start=c(slope = 50, P=0.5, offS = 0)), silent = TRUE ) # add goffS
  return(model4)
}




# Function to run SurEau
########################
SureauLauncher <- function(selectPoints, vegLAI, soilParameters_path, iPts, output_path,
                           x_site, y_site, clim_site, startYear, simuNbYear, pathSureauSoil, surEauClimate_path,
                           outputType, mainDir, resolutionOutput, overWriteOutput, modeling_options, vegetationParameters_path, methodPedoTransf)
{
  climateData_path = file.path(surEauClimate_path,paste0(selectPoints[[clim_site]][iPts],'.csv'))
  
  
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
  stand_parameters      <- create.stand.parameters(LAImax = round(vegLAI[iPts],2), lat = selectPoints[[x_site]][iPts], lon = selectPoints[[y_site]][iPts])
  soil_parameters       <- create.soil.parameters(filePath = file.path(pathSureauSoil,soilParameters_path[iPts]), 
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
