#### 
# Laurence Gagnon
# Spring 2022
# Compute mean kinship per pop per 25 years
####

### Packages
rm(list = ls());
args <- commandArgs(trailingOnly = TRUE) 
library("GENLIB")
library("plyr")
library("dplyr")
library("ggplot2")
library("reshape2")
library("stringr")
library("groupdata2") # fct pour le downsampling 
library("data.table")  



### Get the data ###
## Info about the probands
imp_info_file <- args[1] ## File with the correspondence ID of the probands (genetic & genealogy) and their home region
imp_info <-  read.table(imp_info_file, header=T, sep="\t")
imp_info$reg <- substring(imp_info$final_id,1,3)

## Get the ancestors table 
ancetre_path <- args[2] ## The ancestor table created in : create_ancestors_probands_matrix.py 
ancetre <- read.table(ancetre_path, check.names=F)


### Function ###
myfunc<-function(xx){
  from=imp_info$geneal_num;
  to=imp_info$final_id;
  for (i in 1:1000){
    xx[xx==from[i]]<-to[i];
  }
  return(xx);
}



### Generate the mean kinship of ancestor --> but 47 inds/pop, so we need to boostrap 
number_of_loop <- 1000

for (i in 1:number_of_loop){
  print(paste0("Loop : ", i))
  
  downsample <- downsample(imp_info, cat_col = "reg")
  ancetre_subsampling <- ancetre_famcleaning[ , which(names(ancetre_famcleaning) %in% downsample$geneal_num)]
  
  level_order = c("WES", "CEN", "SAG", "CNO", "GFC", "CHA", "LOY", "ACA")
  colnames_ID <- myfunc(colnames(ancetre_subsampling))
  colnames(ancetre_subsampling) <- colnames_ID
  
  for ( reg in level_order) {
    assign(paste(reg, "df", sep="_"), as.data.frame(apply(ancetre_subsampling[,substr(colnames(ancetre_subsampling), 1, 3) == reg], 1, sum)))
  }
  
  ancestor_reg <- cbind(WES_df, CEN_df, SAG_df, CNO_df, GFC_df, CHA_df, LOY_df, ACA_df)
  colnames(ancestor_reg) <- level_order
  
  ## Kinship loop
  for (kstart in seq(1625, 1925, by=25)) {
    kinterval = 25
    kend = kstart - kinterval
    
    kinship_file <- paste0("/your/path" , "/kinship_file/created/in/compute_kinship_and_MDS.R", kstart, "_to_", kend, ".txt") ## Because the compute_kinship_and_MDS.R create 1 file per 25-year interval
    kinship_data <- read.table(kinship_file, check.names = F)
    
    for (reg in level_order ){
      region <- subset(ancestor_reg, select=reg)
      region <- subset(region, region[,1] != 0)
      
      ind_to_consider <- intersect(rownames(region), names(kinship_data))
      
      ancetre_for_reg_for_year <- kinship_data[which(names(kinship_data) %in% ind_to_consider),  which(names(kinship_data) %in% ind_to_consider)]
      ancetre_for_reg_for_year <- as.matrix(ancetre_for_reg_for_year)
      
      ## Mean kinship compute here
      mean_kinship <- gen.phiMean(ancetre_for_reg_for_year)
      
      meankinship <- as.data.frame(mean_kinship)
      meankinship$Region <- reg
      meankinship$start <- kstart
      meankinship$end <- kend
      meankinship$type <- "Kinship"
      
      assign(paste("kinship", reg, sep="_"),  meankinship)
      
    }
    final_mean_kinship <- rbind(kinship_WES, kinship_CEN, kinship_SAG, kinship_CNO, kinship_GFC, kinship_CHA, kinship_LOY, kinship_ACA)
    assign(paste("kinship_pop", kstart, sep="_"), final_mean_kinship)
    
  }
  ### Put in one variable 
  all_vars <- as.list(mget(paste("kinship_pop_",seq(1625, 1925, by=25), sep="")))
  final_data<- bind_rows(all_vars)
  
  assign(paste("result_loop", i, sep="_"), final_data)
}



### Put all the data of the 1000 loop together ###
df_mean = rbindlist(as.list(mget(paste0("result_loop_", 1:number_of_loop))))[,lapply(.SD,mean), list(Region, start, end, type)]
colnames(df_mean) <- c("Region", "start","end", "type", "mean")

df_std = rbindlist(as.list(mget(paste0("result_loop_", 1:number_of_loop))))[,lapply(.SD,sd), list(Region, start, end, type)]
colnames(df_std) <- c("Region", "start","end", "type", "std")

df_min = rbindlist(as.list(mget(paste0("result_loop_", 1:number_of_loop))))[,lapply(.SD,min), list(Region, start, end, type)]
colnames(df_min) <- c("Region", "start","end", "type", "Min")

df_max = rbindlist(as.list(mget(paste0("result_loop_", 1:number_of_loop))))[,lapply(.SD,max), list(Region, start, end, type)]
colnames(df_max) <- c("Region", "start","end", "type", "Max")

df_mean_std <- join(df_mean, df_std, by = c("Region", "start","end", "type"), type="left")
df_mean_min <- join(df_mean, df_min, by = c("Region", "start","end", "type"), type="left")
df_mean_min_max <- join(df_mean_min, df_max, by = c("Region", "start","end", "type"), type="left")



### Saving the file ###
out_file <- args[3] ## The output file of the kinship data
write.table(df_mean_min_max , file=out_file, quote=FALSE, row.names = FALSE)
















