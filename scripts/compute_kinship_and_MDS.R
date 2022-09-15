####
# Laurence Gagnon
# 11 juin 2021
# Do MDS graph with kinship every 25 year (year of parents mariage)
####


### Packages ###
rm(list = ls());
args <- commandArgs(trailingOnly = TRUE) 
library("GENLIB")
library("ggplot2")
library("plyr")
library("dplyr")
library("extraoperators")
library("tidyverse")
library("pals")
library("matlab")


### Open data ###
## Reading the genealogical data in .asc table
print ("Reading .asc table")
geneal_file <- args[1]  ## A genealogy file with 4 columns including the individuals, fathers, mothers and sex
anc_df <- read.table(geneal_file, header=TRUE, sep="\t") 

## Reading the mariage file 
print ("Open mariage file")
mariage_file <- args[2] ## This file contain localisation and year of mariage of the parents of each individuals
mariage <- read.table(mariage_file, header=TRUE, sep="\t")
colnames(mariage) <- c("ind", "Region", "Region_Id", "DateMariage")



### Open the genealogy in GENLIB ###
my_gen <- gen.genealogy(anc_df)



### Add the parents mariage year to the .asc table ###
anc_df_year <- join(anc_df, mariage, by = c("ind"), type="left")



### Choose the probands within a 25-year intervals ###
start <- as.numeric(args[3]) ## The start year interval
interval <- as.numeric(args[4]) ## 25-year interval in our case
end <- start - interval 

print(paste("This interval is between :", start, "to", end))
anc_df_year_subset <- subset(anc_df_year, DateMariage <= start & DateMariage > end )
probands <- anc_df_year_subset$ind



### Compute kinship ###
kinship_file <- args[5] ## The output of the kinship file

kinship <- gen.phi(my_gen, probands)
write.table(kinship, file=kinship_file, quote=FALSE, sep=" ") 



### Open kinship file ###
kinship_matrix <- read.table(kinship_file, header=T, sep=" ", check.names=F)

## Cleaning : remove ind with kinship = 0 across all the matrix (lack of completeness)
kinship_matrix_tmp <- as.matrix(kinship_matrix)
diag(kinship_matrix_tmp) <- 0  

mean_kinship_matrix_tmp <- colMeans(kinship_matrix_tmp)
ind2remove_0 <- names(mmean_kinship_matrix_tmp[mean_kinship_matrix_tmp == 0])

ifelse(length(ind2remove_0) == 0, kinship_matrix_clean <- kinship_matrix, kinship_matrix_clean <- kinship_matrix[-which(names(kinship_matrix) %in% ind2remove_0) , -which(names(kinship_matrix) %in%  ind2remove_0)])



### Compute MDS ###
## Create a dissimilarity matrix
kinship_matrix_dissimilarity <- as.dist(1 - kinship_matrix_clean)

## Compute with cmdscale()
kinship_cmdscale <- cmdscale(kinship_matrix_dissimilarity, k=2)
kinship_cmdscale <- as.data.frame(kinship_cmdscale)
kinship_cmdscale <- cbind(rownames(kinship_cmdscale), data.frame(kinship_cmdscale, row.names=NULL))
colnames(kinship_cmdscale) <- c("ind", "axe_x", "axe_y")

## Add mariage informatioms
kinship_cmdscale <- join(kinship_cmdscale, mariage, by = c("ind"), type="left")

## Add the interval year
kinship_cmdscale$start <- start
kinship_cmdscale$end <- end



### Saving the MDS file ###
save_file <- args[6] ## The output of the MDS file
write.table(kinship_cmdscale, file=save_file, quote=FALSE, row.names=F, sep="\t") 





