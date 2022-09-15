#### 
# Laurence Gagnon
# 2021
# Compute and graph the completude 
####



### Packages
rm(list = ls());
library("GENLIB")
library("ggplot2")


### Open the genealogy file ###
geneal_file <- args[1]  ## A genealogy file with 4 columns including the individuals, fathers, mothers and sex
anc_df <- read.table(geneal_file,header=TRUE,sep="\t")



### Open the genealogy object
my_gen <- gen.genealogy(anc_df)



### Open the information file about the probands
reg_data_file <- args[2] ## File with the correspondence ID of the probands (genetic & genealogy) and their home region
reg_data <- read.table(reg_data_file, header=T, sep="\t")
reg_data$reg <- substr(reg_data$real_final_ID, 1,3)



### Compute the completude by region ###
for (i in unique(reg_data$reg)) {
  anc_df_year_subset <- subset(reg_data, reg_data$reg == i)
  completude <- gen.completeness(my_gen, pro = anc_df_year_subset$geneal_num)
  completude <- as.data.frame(completude)
  completude$reg <- i
  completude$generation <- rownames(completude)
  
  assign(paste("completude", i, sep="_"), completude)
}

completude_dataset <- rbind(completude_SAG, completude_MTL, completude_QUE, completude_NSH, completude_GAC, completude_GLO, completude_GCI, completude_GFC)
completude_dataset$generation <- as.numeric(completude_dataset$generation)

### Save completude file ###
outputfile_completude <- args[3] ## Output file for the completude
write.table(completude_dataset, outputfile_completude,  quote=FALSE, row.names = FALSE)



### Do the Graphic ###
## Graph parameters
official_palete <- c("chocolate1", "green", "blue", "burlywood4", "darkorchid1", "yellow", "cyan", "red")
level_order = c("MTL", "QUE", "SAG", "NSH", "GFC", "GCI", "GLO", "GAC")

## Graph
plt_com <- ggplot() + geom_line(data=completude_dataset, aes(x=generation, y=completude, col=factor(reg, level=level_order)), size=0.75) +
  scale_color_manual(name = "Group", values = official_palete) +
  theme(text=element_text(face="bold", size=15), plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(ncol=1, override.aes = list(size=1))) +
  labs(title=" ", x="Generation", y="Completeness")
plt_com

## Saving the graph
output_file_com <- args[4] ## Output file for the graph
pdf(file = output_file_com, width = 15/2.54, height = 11/2.54)
plt_com
dev.off()







