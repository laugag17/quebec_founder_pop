#### 
# Laurence Gagnon
# 24 may 2022
# MRCA boostrap
####


### Packages ###
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



### Load the MRCA files ###
setwd("/your/directory")
all_files <- intersect(list.files(pattern = "your/pattern"), list.files(pattern = ".txt")) ## Files created in : compute_MRCA_for_each_pair.R (this create many file)
all_files <- as.data.frame(all_files)
data_list = lapply(all_files, read.table, dec="," , header = TRUE)

## Mettre tous les fichers ensemble 
MRCA <- do.call(rbind, data_list)

## Transform the data to do the grpoh 
MRCA$distance <- as.character(MRCA$distance)
MRCA$inbreeding <- as.numeric(MRCA$inbreeding)



### Graph parameters ###
official_palete <- c("chocolate1", "green", "blue", "burlywood4", "darkorchid1", "yellow", "cyan", "red")
level_order = c("MTL", "QUE", "SAG", "CNO", "GFC", "GCI", "GLO", "GAC")



### Load the important info about the probands ###
imp_info_file <- args[1] ## File with the correspondence ID of the probands (genetic & genealogy) and their home region
imp_info <- read.table(imp_info_file, header=T, sep="\t")
imp_info$reg <- substr(imp_info$final_id, 1, 3)


### The code to do a 1000 loop bootstrap ###
MRCA_loop <- MRCA
MRCA_loop$proband12 <- paste0(MRCA_loop$proband1, "_", MRCA_loop$proband2)
number_of_loop <- 1000

for (i in 1:number_of_loop){
  print(paste0("Loop : ", i))
  downsample <- downsample(imp_info, cat_col = "reg")
  all_combinaison <- setDT(downsample)[, transpose(combn(geneal_num, 2, FUN = list)), by = reg]
  
  all_combinaison$combn1 <- paste0(all_combinaison$V1, "_", all_combinaison$V2)
  all_combinaison$combn2 <- paste0(all_combinaison$V2, "_", all_combinaison$V1)
  
  MRCA_downsample <- MRCA_loop[MRCA_loop$proband12 %in% all_combinaison$combn1 | MRCA_loop$proband12 %in% all_combinaison$combn2, ]


  numberMRCAmeiose <- table(MRCA_downsample$reg, MRCA_downsample$distance)
  numberMRCAmeiose <- as.data.frame(numberMRCAmeiose)

  numberMRCAmeiose$reg_meiose <- paste0(numberMRCAmeiose$Var1, "_", numberMRCAmeiose$Var2)
  row.names(numberMRCAmeiose) <- numberMRCAmeiose$reg_meiose
  numberMRCAmeiose$Var1 <- NULL
  numberMRCAmeiose$Var2 <- NULL
  numberMRCAmeiose$reg_meiose <- NULL
  numberMRCAmeiose_clean <- t(numberMRCAmeiose)
  numberMRCAmeiose_clean <- as.data.frame(numberMRCAmeiose_clean)

  assign(paste("resultgraphAbymeiose", i, sep="_"), numberMRCAmeiose_clean)

}  

### Put the data in a good format ###
graphAV2_mean_all = rbindlist(as.list(mget(paste0("resultgraphAbymeiose_", 1:number_of_loop))),fill=T)[,lapply(.SD,mean,na.rm=TRUE)]
graphAV2_mean_all <- t(graphAV2_mean_all)
graphAV2_mean_all <- as.data.frame(graphAV2_mean_all)
graphAV2_mean_all$reg <- rownames(graphAV2_mean_all)
row.names(graphAV2_mean_all) <- NULL
colnames(graphAV2_mean_all) <- c("mean", "reg")

graphAV2_std_all = rbindlist(as.list(mget(paste0("resultgraphAbymeiose_", 1:number_of_loop))),fill=T)[,lapply(.SD,sd,na.rm=TRUE)]
graphAV2_std_all <- t(graphAV2_std_all)
graphAV2_std_all <- as.data.frame(graphAV2_std_all)
graphAV2_std_all$reg <- rownames(graphAV2_std_all)
row.names(graphAV2_std_all) <- NULL
colnames(graphAV2_std_all) <- c("std", "reg")

graphAV2_min_all = rbindlist(as.list(mget(paste0("resultgraphAbymeiose_", 1:number_of_loop))),fill=T)[,lapply(.SD,min,na.rm=TRUE)]
graphAV2_min_all <- t(graphAV2_min_all)
graphAV2_min_all <- as.data.frame(graphAV2_min_all)
graphAV2_min_all$reg <- rownames(graphAV2_min_all)
row.names(graphAV2_min_all) <- NULL
colnames(graphAV2_min_all) <- c("min", "reg")

graphAV2_max_all = rbindlist(as.list(mget(paste0("resultgraphAbymeiose_", 1:number_of_loop))),fill=T)[,lapply(.SD,max,na.rm=TRUE)]
graphAV2_max_all <- t(graphAV2_max_all)
graphAV2_max_all <- as.data.frame(graphAV2_max_all)
graphAV2_max_all$reg <- rownames(graphAV2_max_all)
row.names(graphAV2_max_all) <- NULL
colnames(graphAV2_max_all) <- c("max", "reg")

graphAV2_mean_std <- join(graphAV2_mean_all, graphAV2_std_all, by = c("reg"), type="left")
graphAV2_mean_std_min <- join(graphAV2_mean_std, graphAV2_min_all, by = c("reg"), type="left")
graphAV2_mean_std_min_max <- join(graphAV2_mean_std_min, graphAV2_max_all, by = c("reg"), type="left")

graphAV2_mean_std_min_max <- graphAV2_mean_std_min_max %>% separate(reg, c("reg", "meiose"))
graphAV2_mean_std_min_max$meiose <- as.integer(graphAV2_mean_std_min_max$meiose)

## Saving
out_file_AV2 <- args[2] ## the ouput file of the MRCA boostrap
write.table(graphAV2_mean_std_min_max, file=out_file_AV2, quote=FALSE, row.names = FALSE)





