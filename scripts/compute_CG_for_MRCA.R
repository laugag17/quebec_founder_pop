#### 
# Laurence Gagnon
# August 2022
# Compute genetic contribution of the MRCA
####


### Packages ###
rm(list = ls());
args <- commandArgs(trailingOnly = TRUE) 
library("ggplot2")
library("Hmisc")
library("plyr")
library("dplyr")
library("tidyr")
library("gtools")
library("GENLIB")
library("groupdata2") # fct pour le downsampling 
library("data.table")


### Load MRCA file ###
setwd("/your/path/")
all_files <- intersect(list.files(pattern = "your/file/pattern"), list.files(pattern = ".txt")) ## Find the MRCA file created in : compute_MRCA_for_each_pairs.R
all_files <- as.data.frame(all_files)
data_list = lapply(all_files, read.table, dec="," , header = TRUE)

## Mettre tous les fichers ensemble 
MRCA <- do.call(rbind, data_list)



### Graph parameters ### 
official_palete <- c("chocolate1", "green", "blue", "burlywood4", "darkorchid1", "yellow", "cyan", "red")
level_order = c("MTL", "QUE", "SAG", "CNO", "GFC", "GCI", "GLO", "GAC")



### Important info about the probands ###
imp_info_file <- args[1] ## File with the correspondence ID of the probands (genetic & genealogy) and their home region
imp_info <- read.table(imp_info_file, header=T, sep="\t")
imp_info$reg <- substr(imp_info$final_id, 1, 3)




### Get genealogy file and object ###
geneal_file <- args[2]  ## A genealogy file with 4 columns including the individuals, fathers, mothers and sex
anc_df <- read.table(geneal_file,header=TRUE,sep="\t") ###sep est un espace et non une tabulation
gen<-gen.genealogy(anc_df)
probands <- gen.pro(gen)



### Add the Genetic Contribution to every MRCA ###
print(paste0("There is : ", nrow(MRCA), " rows"))

for (i in 1:nrow(MRCA)){
  print(paste0(i, " / ", nrow(MRCA)))
  
  MRCA$CG[i] <- gen.gc(gen, pro = c(MRCA[i,2], MRCA[i,3]), ancestors = MRCA[i,1], typeCG = "PRODUCT")
  
}

## Saving
print("Saving the file")
out_file <- args[3]  ## Output for the genetic contribution of the MRCA
write.table(MRCA, file=out_file, quote=FALSE, row.names = FALSE)

print("Done")



