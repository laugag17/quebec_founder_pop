#### 
# Laurence Gagnon
# August 2022
# Bootstrap of the genetic contributionn (GC) of the MRCA
####


### Library ###
rm(list = ls());
library("ggplot2")
library("Hmisc")
library("plyr")
library("dplyr")
library("tidyr")
library("gtools")
library("GENLIB")
library("groupdata2") # fct pour le downsampling 
library("data.table")



### Genetic contribution data ##
cg_file <- args[1]  ## Output for the genetic contribution of the MRCA in : compute_CG_for_MRCA.R
cg <- read.table(cg_file, header = T) 
cg$proband12 <- paste0(cg$proband1, "_", cg$proband2)
cg$distance <- as.character(cg$distance)
cg$inbreeding <- as.numeric(cg$inbreeding)


### Graphic parameters ###
official_palete <- c("chocolate1", "green", "blue", "burlywood4", "darkorchid1", "yellow", "cyan", "red")
level_order = c("MTL", "QUE", "SAG", "NSH", "GFC", "GCI", "GLO", "GAC")


### Info about the probands ###
imp_info_file <- args[2] ## File with the correspondence ID of the probands (genetic & genealogy) and their home region
imp_info <- read.table(imp_info_file , header=T, sep="\t")
imp_info$reg <- substr(imp_info$real_final_ID, 1, 3)





### Start the loop for GC ###
number_of_loop <- 1000

for (i in 1:number_of_loop){
    print(i)
    
    downsample <- downsample(imp_info, cat_col = "reg")
    all_combinaison <- setDT(downsample)[, transpose(combn(geneal_num, 2, FUN = list)), by = reg]
    all_combinaison$combn1 <- paste0(all_combinaison$V1, "_", all_combinaison$V2)
    all_combinaison$combn2 <- paste0(all_combinaison$V2, "_", all_combinaison$V1)
    cg_downsample <- cg[cg$proband12 %in% all_combinaison$combn1 | cg$proband12 %in% all_combinaison$combn2, ]

  
    ## Analyse CG - sum   
    cg_sum_result <- cg_downsample %>% group_by(reg_meiose) %>% summarise_at(vars(CG), c(sum))
    cg_sum_result <- as.data.frame(cg_sum_result)
    
    row.names(cg_sum_result) <- cg_sum_result$reg_meiose
    cg_sum_result$reg_meiose <- NULL
    cg_sum_result_clean <- t(cg_sum_result)
    cg_sum_result_clean <- as.data.frame(cg_sum_result_clean)
    
    assign(paste("CG_sum", i, sep="_"), cg_sum_result_clean)
  
  
}  



### Get the mean, std, min and max of analyse CG
## sum data
graphB2_mean_all = rbindlist(as.list(mget(paste0("CG_sum_", 1:number_of_loop))),fill=T)[,lapply(.SD,mean,na.rm=TRUE)]
graphB2_mean_all <- t(graphB2_mean_all)
graphB2_mean_all <- as.data.frame(graphB2_mean_all)
graphB2_mean_all$reg <- rownames(graphB2_mean_all)
row.names(graphB2_mean_all) <- NULL
colnames(graphB2_mean_all) <- c("mean", "reg")

graphB2_std_all = rbindlist(as.list(mget(paste0("CG_sum_", 1:number_of_loop))),fill=T)[,lapply(.SD,sd,na.rm=TRUE)]
graphB2_std_all <- t(graphB2_std_all)
graphB2_std_all <- as.data.frame(graphB2_std_all)
graphB2_std_all$reg <- rownames(graphB2_std_all)
row.names(graphB2_std_all) <- NULL
colnames(graphB2_std_all) <- c("std", "reg")

graphB2_min_all = rbindlist(as.list(mget(paste0("CG_sum_", 1:number_of_loop))),fill=T)[,lapply(.SD,min,na.rm=TRUE)]
graphB2_min_all <- t(graphB2_min_all)
graphB2_min_all <- as.data.frame(graphB2_min_all)
graphB2_min_all$reg <- rownames(graphB2_min_all)
row.names(graphB2_min_all) <- NULL
colnames(graphB2_min_all) <- c("min", "reg")

graphB2_max_all = rbindlist(as.list(mget(paste0("CG_sum_", 1:number_of_loop))),fill=T)[,lapply(.SD,max,na.rm=TRUE)]
graphB2_max_all <- t(graphB2_max_all)
graphB2_max_all <- as.data.frame(graphB2_max_all)
graphB2_max_all$reg <- rownames(graphB2_max_all)
row.names(graphB2_max_all) <- NULL
colnames(graphB2_max_all) <- c("max", "reg")

graphB2_mean_std <- join(graphB2_mean_all, graphB2_std_all, by = c("reg"), type="left")
graphB2_mean_std_min <- join(graphB2_mean_std, graphB2_min_all, by = c("reg"), type="left")
graphB2_mean_std_min_max <- join(graphB2_mean_std_min, graphB2_max_all, by = c("reg"), type="left")

graphB2_mean_std_min_max <- graphB2_mean_std_min_max %>% separate(reg, c("reg", "meiose"))
graphB2_mean_std_min_max$meiose <- as.integer(graphB2_mean_std_min_max$meiose)

## Saving
print("Saving the file gfor cg mean")
out_file_B2 <- args[3] ## Output for the CG of the MRCA 
write.table(graphB2_mean_std_min_max, file=out_file_B2, quote=FALSE, row.names = FALSE)








