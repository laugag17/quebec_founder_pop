#### 
# Laurence Gagnon
# May 2022
# Compute the MRCA for all pairs
####


### Packages ###
rm(list = ls());
args <- commandArgs(trailingOnly = TRUE) 
library("GENLIB")
library("plyr")
library("dplyr")
library("ggplot2")


### Data ###
## Which region we are at now
region <- args[1] ## The acronyms of the region

## Genealogical file
datafile <- args[2] ## A genealogy file with 4 columns including the individuals, fathers, mothers and sex
  
## Probands info file
reg_file  <- args[3] ## File with the correspondence ID of the probands (genetic & genealogy) and their home region
reg_data <- read.table(reg_file, header=T)
reg_data$reg <- substr(reg_data$final_id, 1, 3)


### Open the object genealogy ###
data <- read.table(datafile, header = T, sep = "\t")
my_gen <- gen.genealogy(data)




### Get the data of 1 region only ###
data2use <- reg_data[reg_data$reg == region, ]

  
### Get all the combinaison to use in the genlib fonction ###
all_combinaison <- combn(data2use$geneal_num , 2, simplify=T)
all_combinaison <- as.data.frame(t(all_combinaison ))


### Stat the loop that compitae all the MRCA of each pair ###
for (i in 1:nrow(all_combinaison)){     # Pour toutes les paires d'une région en 1 même fichier
  print(paste0("Combinaison ", i, " on ", nrow(all_combinaison)))
  
  ## Use the 2 fct for MRCA + change the result into a DataFrame
  fct_1 <- gen.findMRCA(my_gen, individuals=as.numeric(all_combinaison[i,]))
  
  if(is.null(fct_1)){
    print("fct_1 == NULL")
  }
  
  else{
    fct_2 <- gen.find.Min.Distance.MRCA(fct_1)
    fct_2 <- as.data.frame(fct_2)
    
    ## Add the region 
    fct_2$reg <- region
    
    ## Do the inbreeding coefficient + a clean DataFrame 
    inbreeding <- gen.f(my_gen, pro=fct_2$founder)
    inbreeding <- as.data.frame(inbreeding)
    inbreeding$founder <- as.numeric(row.names(inbreeding))
    colnames(inbreeding) <- c("inbreeding", "founder")
    
    ## Join inbreeding ans MRCA
    data_join <- join(fct_2, inbreeding, by = c("founder"), type="left")
   
    ## Save the data in a uniq variable 
    assign(paste("data_clean2use", i, sep="_"), data_join)
     
  }
}


### Put in one variable ###
all_vars <- as.list(mget(ls(pattern = "data_clean2use",)))
final_data <- bind_rows(all_vars) 
final_data$distance <- as.character(final_data$distance)


### Save the file ###
output_file <-  args[4]  ## The MRCA output data
write.table(final_data, file=output_file, sep="\t", quote=F, row.names=F)
print(paste0("All done for ", region, " !"))


