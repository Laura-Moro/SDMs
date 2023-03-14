library(rgdal)
library(raster)
library(ENMeval)
library(dplyr)



# LOAD ENVIRONMENTAL PREDICTOR VARIABLES
envs <- stack(list.files("Data/Derived/envs", full.names = TRUE))

#LOAD OCCURENCES RECORD AND FILTER 
load("Data/FINAL_RECS.Rdata")

#without considering the islands
GEO_ext <-raster::extract(envs[[1]], full[,c('LONGDEC','LATDEC')])
oc <- dplyr::filter(full, !is.na(GEO_ext)) #these are the cleaned occurencese

envs[[1]] <- as.factor(envs[[1]])

#RUNNING THE MODELS 
for (sp in 1:length(unique(oc$CODE))){
  
  # This line to skip some species that dont work
  if(!sp %in% c(253,387,472,515,541)){
    
    focsp <- unique(oc$CODE)[sp]
    occs <- oc[oc$CODE==focsp,2:1]
    bg <- oc[oc$CODE!=focsp,2:1]
    
    message(paste('working on', focsp))
    print(Sys.time())
    
    if(nrow(occs)>10){
      
      if(nrow(occs)>=15){
        partition <- "checkerboard2"
      } else {
        partition <- "jackknife"
      }
      
# HOW ARE 'DUPLICATE' RECORDS BEING TREATED? (AS IN, OCCS IN THE SAME GRID CELL?)
      mod <- ENMevaluate(occs=occs, envs=envs, bg=bg, 
                         algorithm='maxnet', 
                         partitions=partition,
                         categoricals="GEO",
                         partition.settings=list(aggregation.factor=c(5,5)),
                         tune.args=list(fc = c("L","LQ","LQH","H"), rm = 1:5))
      # tune.args=list(fc = c("L","H"), rm = 1:5))
      
      filename <- paste0("Results/2022-12-07_ENMeval_results/", focsp, ".RDA")
      saveRDS(mod, file=filename)
    }
  }
}

#MODEL SELECTION 
#load mod files 
modfiles <- list.files("/Users/laumo791/Documents/PR/C1/Results/2022-12-07_ENMeval_results" , full.names = T)
#create empty lists 
res <- list() #this is for the results 
pred <- list() # this is for the raster model results 

#select the models with the the lowest omission rate at 10p and the hisghest AUC
for(i in seq_along(modfiles)){
  print(i)
  mod <- readRDS(modfiles[i])
  
  tmpres <- mod@results %>% 
    filter(or.10p.avg == min(or.10p.avg)) %>% 
    filter(auc.val.avg == max(auc.val.avg))
  
  res[[i]] <- tmpres[1,]
  
  pred[[i]] <- mod@predictions[[which(names(mod@predictions)==res[[i]]$tune.args)]]
}

#tables of all of the results 
resall <- do.call(rbind, res)
hist(resall$auc.train)

#raster stack of all of the predictions 
predall <- do.call(stack, pred)

