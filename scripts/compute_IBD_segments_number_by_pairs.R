####
# Laurence Gagnon
# August 2022
# The number of IBD segments by pairs 
####


### Packages
rm(list = ls());
args <- commandArgs(trailingOnly = TRUE) 
library("ggplot2")
library("plyr")
library("dplyr")
library("tidyr")
library("reshape2")




### Get the IBD file ###
setwd("/your/path")
all_files <- intersect(list.files(pattern = "your_pattern")) ## The IBD file are created with : run_refinedIBD.sh
nom_col <- c("IND1","haplotype1", "IND2", "haplotype2", "chr", "start", "end", "LOD", "IBD_segment")
data_list = lapply(all_files, read.table, dec="," , header = FALSE, col.names=nom_col)

## Mettre tous les fichers ensemble
IBD <- do.call(rbind, data_list)



### Important info about the probands ###
imp_info_file <- args[1] ## File with the correspondence ID of the probands (genetic & genealogy) and their home region
to_keep <- read.table(imp_info_file, header=T)


### Add the good ID
to_keep_ind1 <- to_keep[,c(1,12)]
colnames(to_keep_ind1) <- c("IND1", "new_id_IND1")
to_keep_ind2 <- to_keep[,c(1,12)]
colnames(to_keep_ind2) <- c("IND2", "new_id_IND2")

IBD_cM_ok <- IBD_noabi_noout_famcleaning  %>% separate(IND1, c(NA, "IND1"))
IBD_cM_ok <- IBD_cM_ok %>% separate(IND2, c(NA, "IND2"))

data_commun <- join(IBD_cM_ok, to_keep_ind1, by = c("IND1"), type="left")
data_commun <- join(data_commun, to_keep_ind2, by = c("IND2"), type="left")

data_commun$reg_1 <- substr(data_commun$new_id_IND1,1,3)
data_commun$reg_2 <- substr(data_commun$new_id_IND2,1,3)


### Just keep intra-pop
data_final <- data_commun[data_commun$reg_1 == data_commun$reg_2,]



### Do binds ###
## Prepare DF 
data_final$pairs <- paste0(data_final$new_id_IND1, "_", data_final$new_id_IND2)
data_final$IBD_segment_round <- round(data_final$IBD_segment)



### Do bins of 1-4.99cM ###
data_final$IBD_segment_round_4.99 <-   ifelse(data_final$IBD_segment_round   == 2 | data_final$IBD_segment_round   == 3 | data_final$IBD_segment_round   == 4, 1, data_final$IBD_segment_round)
data_final$IBD_segment_round_4.99 <-   ifelse(data_final$IBD_segment_round   == 6 | data_final$IBD_segment_round   == 7 | data_final$IBD_segment_round   == 8 , 5, data_final$IBD_segment_round_4.99)
data_final$IBD_segment_round_4.99 <-   ifelse(data_final$IBD_segment_round   == 10 | data_final$IBD_segment_round   == 11 | data_final$IBD_segment_round   == 12, 9, data_final$IBD_segment_round_4.99)
data_final$IBD_segment_round_4.99 <-   ifelse(data_final$IBD_segment_round   == 14 | data_final$IBD_segment_round   == 15 | data_final$IBD_segment_round   == 16, 13, data_final$IBD_segment_round_4.99)
data_final$IBD_segment_round_4.99 <-   ifelse(data_final$IBD_segment_round   == 18 | data_final$IBD_segment_round   == 19 | data_final$IBD_segment_round   == 20, 17, data_final$IBD_segment_round_4.99)
data_final$IBD_segment_round_4.99 <-   ifelse(data_final$IBD_segment_round   == 22 | data_final$IBD_segment_round   == 23 | data_final$IBD_segment_round   == 24, 21, data_final$IBD_segment_round_4.99)
data_final$IBD_segment_round_4.99 <-   ifelse(data_final$IBD_segment_round   == 26 | data_final$IBD_segment_round   == 27 | data_final$IBD_segment_round   == 28, 25, data_final$IBD_segment_round_4.99)
data_final$IBD_segment_round_4.99 <-   ifelse(data_final$IBD_segment_round   == 30 | data_final$IBD_segment_round   == 31 | data_final$IBD_segment_round   == 32, 29, data_final$IBD_segment_round_4.99)
data_final$IBD_segment_round_4.99 <-   ifelse(data_final$IBD_segment_round   == 34 | data_final$IBD_segment_round   == 35 | data_final$IBD_segment_round   == 36, 33, data_final$IBD_segment_round_4.99)
data_final$IBD_segment_round_4.99 <-   ifelse(data_final$IBD_segment_round   == 38 | data_final$IBD_segment_round   == 39 | data_final$IBD_segment_round   == 40, 37, data_final$IBD_segment_round_4.99)
data_final$IBD_segment_round_4.99 <-   ifelse(data_final$IBD_segment_round   == 42 | data_final$IBD_segment_round   == 43 | data_final$IBD_segment_round   == 44, 41, data_final$IBD_segment_round_4.99)
data_final$IBD_segment_round_4.99 <-   ifelse(data_final$IBD_segment_round   == 46 | data_final$IBD_segment_round   == 47 | data_final$IBD_segment_round   == 48, 45, data_final$IBD_segment_round_4.99)
data_final$IBD_segment_round_4.99 <-   ifelse(data_final$IBD_segment_round   == 50 | data_final$IBD_segment_round   == 51 | data_final$IBD_segment_round   == 52, 49, data_final$IBD_segment_round_4.99)


### Do the table fct ###
data2use <- data.frame(unclass(table(data_final$pairs, data_final$IBD_segment_round_4.99)), check.names=F)


### Do modification to get a good DataFrame ###
data2use$reg <- substr(row.names(data2use), 1, 3)

meanvalue <- aggregate(data2use[, 1:(ncol(data2use) - 1)], list(data2use$reg), mean)

meanvalue2 <- t(meanvalue)
meanvalue2 <- as.data.frame(meanvalue2)
colnames(meanvalue2) <- c("GAC", "QUE", "GCI", "NSH", "GFC", "GLO", "SAG", "MTL")
meanvalue2 <- meanvalue2[-1,]
meanvalue2$length <- row.names(meanvalue2)
meanvalue2 <- meanvalue2[-1,]

meanvalue3 <- melt(meanvalue2, id=c("length"))
meanvalue3$length <- as.numeric(meanvalue3$length)
meanvalue3$value <- as.numeric(meanvalue3$value)
str(meanvalue3)


### Saving the file with binds ###
out_file <- args[2] ## Output file of the bins data 
write.table(meanvalue3, sep="\t", quote=FALSE, row.names=F, out_file)



