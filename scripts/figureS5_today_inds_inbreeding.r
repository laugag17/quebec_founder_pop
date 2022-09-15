#### 
# Laurence Gagnon
# July 2022
# Compute todays ind inbreeding
####


### Packages
rm(list = ls());
args <- commandArgs(trailingOnly = TRUE) 
library("GENLIB")
library("ggplot2")



### Get the genealogy file
geneal_file <- args[1]  ## A genealogy file with 4 columns including the individuals, fathers, mothers and sex
anc_df <- read.table(geneal_file,header=TRUE,sep="\t")



## Open the genealogy object
my_gen <- gen.genealogy(anc_df)
length(gen.pro(my_gen))


## Get the important information about the probands
reg_data_file <- args[2] ## File with the correspondence ID of the probands (genetic & genealogy) and their home region
reg_data <- read.table(reg_data_file, header=T, sep="\t")
reg_data$reg <- substr(reg_data$real_final_ID, 1,3)



### Calculate mean inbreeding per generation ###
j<-0
all_regions <- c()
for (i in unique(reg_data$reg)){
    anc_df_year_subset <- subset(reg_data, reg_data$reg == i)
    inbreeding <- gen.f(my_gen, depthmin=1,pro = anc_df_year_subset$geneal_num)
    # print (inbreeding)
    if (j!=0){
        print (i)
        print (colMeans(inbreeding))
        all_regions <- rbind(all_regions,cbind(as.numeric(colMeans(inbreeding)),rep(i,18), as.numeric(c(1:18))))
        print (all_regions)
        
    }else{
        print (i)
        all_regions <- cbind(as.numeric(colMeans(inbreeding)),rep(i,18), as.numeric(c(1:18)))
         print (all_regions)
        j<-1
    }
}

all_regions <- data.frame(Inbreeding=as.numeric(all_regions[,1]),Subpopulation=all_regions[,2],Generation=as.numeric(all_regions[,3]))



### Saving the file ###
my_file <- args[3] ## Ouput file 
write.table(all_regions, my_file,  quote=FALSE, row.names = FALSE)



### Do the graphic ###
## Grahic parameters
official_palete <- c("chocolate1", "green", "blue", "burlywood4", "darkorchid1", "yellow", "cyan", "red")
level_order = c("MTL", "QUE", "SAG", "NSH", "GFC", "GCI", "GLO", "GAC")



## Graph
plt_com <- ggplot() + geom_line(data=all_regions, aes(x=Generation, y=Inbreeding, col=factor(Subpopulation, level=level_order)), size=0.75) +
  scale_color_manual(name = "Group", values = official_palete) +
  theme(text=element_text(face="bold", size=11), plot.title = element_text(hjust = 0.5)) +
  labs(title=" ", x="Generation", y="Mean inbreeding")



## Savingthe graph
output_file_com <-  args[4] ## Ouput file for the graph
pdf(file = output_file_com, width = 15/2.54, height = 11/2.54)
    plt_com
dev.off()







