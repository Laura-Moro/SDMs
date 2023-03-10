library(rgdal)
library(raster)
library(ENMeval)
library(dplyr)



# LOAD ENVIRONMENTAL PREDICTOR VARIABLES
envs <- stack(list.files("Data/Derived/envs", full.names = TRUE))

#LOAD OCCURENCES RECORD AND FILTER 
load("Data/FINAL_RECS.Rdata")
# full <- full[!is.na((full[,c('LONGDEC','LATDEC')])),] #I'm not sure what this line does 

#without considering the islands
GEO_ext <-raster::extract(envs[[1]], full[,c('LONGDEC','LATDEC')])
oc <- dplyr::filter(full, !is.na(GEO_ext)) #these are the cleaned occurencese

envs[[1]] <- as.factor(envs[[1]])

#running the models 
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

#MODEL SELECTION code by bob ################################################## 

modfiles <- list.files("Results/2022-12-07_ENMeval_results" , full.names = T)

res <- list() #this is for the results 
pred <- list() # this is for the raster model results 

for(i in seq_along(modfiles)){
  mod <- readRDS(modfiles[i])
  res[[i]] <- mod@results[which(mod@results$cbi.trains==max(mod@results$cbi.train))]
  pred[[i]] <- mod@predictions[[which(mod@results$cbi.train==max(mod@results$cbi.train))]]
}

res_summary <- do.call(rbind, res)
pred_stack <- do.call(raster::stack, pred)

#####code by Laura #################################################################
#I would actually like to reproduce this code for the jemie Kass vinette  but i cannot make it work 
# remove the models with NA AICc
slected_model <- res %>% filter(delta.AICc==NA)
opt.aicc
#slect the model with the lowest omission rate and the highest AUC
slected_model <- res %>% 
  filter(or.10p.avg == min(or.10p.avg)) %>% 
  filter(auc.val.avg == max(auc.val.avg))

####  this is what is have tried! 
##here I'm only using the omission rate to try to run the loop but i could not relly make it work 
for(i in seq_along(modfiles)){
  mod <- readRDS(modfiles[i])
  res[[i]] <- mod@results[which(mod@results$or.10p.avgs==min(mod@results$or.10p.avg))]
  pred[[i]] <- mod@predictions[which (mod@results$or.10p.avg == min(mod@results$or.10p.avg))]
}
###