#### 
# Laurence Gagnon
# Spring 2022
# Compute mean inbreeding per region per 25 years
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
library("ggpubr")




### Compute inbreeding for all the genealogy ###
## Genealogy data
geneal_file <- args[1]  ## A genealogy file with 4 columns including the individuals, fathers, mothers and sex
anc_df <- read.table(geneal_file,header=TRUE,sep="\t") 

## Create genealogycal object
gen <- gen.genealogy(anc_df)
probands <- gen.pro(gen)


## Compute inbreeding coefficient
coeff <- gen.f(gen, pro = anc_df$ind)
coeff <- as.data.frame(coeff)
coeff$inds <- row.names(coeff)

output_inbreeding <- args[2] ## output file to save the inbreeding data
write.table(coeff, file=output_inbreeding, sep="\t", quote=FALSE, row.names = FALSE)






### Do the bootstrap on the inbreeding compute earlier ###

### Load data
## Get the probands information data
imp_info_file <- args[3] ## File with the correspondence ID of the probands (genetic & genealogy) and their home region
imp_info <-  read.table(imp_info_file, header=T, sep="\t")
imp_info$reg <- substring(imp_info$final_id,1,3)

## Get the ancestors table 
ancetre_path <- args[4] ## The ancestor table created in : create_ancestors_probands_matrix.py 
ancetre <- read.table(ancetre_path, check.names=F)

## Load the inbreeding file 
inbreed <- read.table(output_inbreeding, header=T)
colnames(inbreed) <- c("inbreeding_coeff", "IndId")

## Open mariage info file
mariage_file <- args[5] ## This file contain localisation and year of mariage of the parents of each individuals
mariage <- read.table(mariage_file, sep="\t",  header=T, na.strings=c(" ", "NULL", "NA"))


### Function
myfunc<-function(xx){
  from=imp_info$geneal_num;
  to=imp_info$final_id;
  for (i in 1:1000){
    xx[xx==from[i]]<-to[i];
  }
  return(xx);
}




### Add year and location of parent's wedding on the inbreeding data
ind_commun <- mariage[mariage$IndId %in% inbreed$IndId,]
inbreed_all_data <- join(inbreed, ind_commun, by = c("IndId"), type="left")

inbreed_all_data$new_reg <- ifelse(inbreed_all_data$Region == "SAGUENAY (LAC ST JEAN)", "SAG", inbreed_all_data$Region)
inbreed_all_data$new_reg <- ifelse(inbreed_all_data$new_reg == "GASPESIE", "GAS", inbreed_all_data$new_reg)
inbreed_all_data$new_reg <- ifelse(inbreed_all_data$new_reg == "QUEBEC", "CEN", inbreed_all_data$new_reg)
inbreed_all_data$new_reg <- ifelse(inbreed_all_data$new_reg == "BEAUCE", "CEN", inbreed_all_data$new_reg)
inbreed_all_data$new_reg <- ifelse(inbreed_all_data$new_reg == "MONTREAL", "WES", inbreed_all_data$new_reg)
inbreed_all_data$new_reg <- ifelse(inbreed_all_data$new_reg == "COTE NORD", "CNO", inbreed_all_data$new_reg)



### Generate the mean kinship of ancestor --> but 47 inds/pop, so we need to boostrap 
number_of_loop <- 1000


for (i in 1:number_of_loop){
  print(paste0("Loop : ", i))
  downsample <- downsample(imp_info, cat_col = "reg")
  ancetre_subsampling <- ancetre_famcleaning[ , which(names(ancetre_famcleaning) %in% downsample$geneal_num)]

  colnames_ID <- myfunc(colnames(ancetre_subsampling))
  colnames(ancetre_subsampling) <- colnames_ID

  level_order = c("WES", "CEN", "SAG", "CNO", "GFC", "CHA", "LOY", "ACA")
  for (reg in level_order) {
    assign(paste(reg, "df", sep="_"), as.data.frame(apply(ancetre_subsampling[,substr(colnames(ancetre_subsampling), 1, 3) == reg], 1, sum)))
  }

  ancestor_reg <- cbind(WES_df, CEN_df, SAG_df, CNO_df, GFC_df, CHA_df, LOY_df, ACA_df)
  colnames(ancestor_reg) <- level_order


  ### Inbreeding loop
  for (kstart in seq(1625, 1925, by=25)) {
    kinterval = 25
    kend = kstart - kinterval

    ## Get the inds of the good year interval
    df <- subset(inbreed_all_data, DateMariage <= kstart & DateMariage > kend )
    

    for (reg in level_order ){
      region <- subset(ancestor_reg, select=reg)
      region <- subset(region, region[,1] != 0)

      ind_to_consider <- intersect(rownames(region), df$IndId) 
      df_for_reg_for_year <- df[which(df$IndId %in% ind_to_consider),  ]
      
      ## Do mean inbreeding
      mean_inbreed <- mean(df_for_reg_for_year$inbreeding_coeff)
      meaninbreed <- as.data.frame(mean_inbreed)

      meaninbreed$Region <- reg
      meaninbreed$start <- kstart
      meaninbreed$end <- kend
      meaninbreed$type <- "Inbreeding"

      assign(paste("inbreed", reg, sep="_"),  meaninbreed)

    }
    final_mean_inbreed <- rbind(inbreed_WES, inbreed_CEN, inbreed_SAG, inbreed_CNO, inbreed_GFC, inbreed_CHA, inbreed_LOY, inbreed_ACA)
    assign(paste("inbreed_pop", kstart, sep="_"), final_mean_inbreed )

  }
  ### Put in one variable
  all_vars <- as.list(mget(paste("inbreed_pop_",seq(1625, 1925, by=25), sep="")))
  final_data<- bind_rows(all_vars)

  assign(paste("result_loop", i, sep="_"), final_data)
}


### Put all the data of the 1000 loop together ###
df_mean = rbindlist(as.list(mget(paste0("result_loop_", 1:number_of_loop))))[,lapply(.SD,mean), list(Region, start, end, type)]
colnames(df_mean) <- c("Region", "start","end", "type", "mean_inbreed")

df_std = rbindlist(as.list(mget(paste0("result_loop_", 1:number_of_loop))))[,lapply(.SD,sd), list(Region, start, end, type)]
colnames(df_std) <- c("Region", "start","end", "type", "std_inbreed")

df_min = rbindlist(as.list(mget(paste0("result_loop_", 1:number_of_loop))))[,lapply(.SD,min), list(Region, start, end, type)]
colnames(df_min) <- c("Region", "start","end", "type", "Min_inbreed")

df_max = rbindlist(as.list(mget(paste0("result_loop_", 1:number_of_loop))))[,lapply(.SD,max), list(Region, start, end, type)]
colnames(df_max) <- c("Region", "start","end", "type", "Max_inbreed")

df_mean_std <- join(df_mean, df_std, by = c("Region", "start","end", "type"), type="left")
df_mean_min <- join(df_mean_std, df_min, by = c("Region", "start","end", "type"), type="left")
df_mean_min_max <- join(df_mean_min, df_max, by = c("Region", "start","end", "type"), type="left")


### Saving the file ###
out_file <- args[6] ## Output file of the inbreeding boostrap data
write.table(df_mean_min_max , file=out_file, quote=FALSE, row.names = FALSE)






