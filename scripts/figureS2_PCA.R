####
# Laurence Gagnon
# 23 juin 2020
# Graph  PCA
####

### Packages
rm(list = ls());
args <- commandArgs(trailingOnly = TRUE) 
library("ggplot2")
library("plotly")
library("plyr")
library("dplyr")
library("htmlwidgets")
library("tidyverse")
library("ggpubr")


### Get the PCA data ###
pca_data_file <- args[1] ## The PCA data creted by PLINK (.eigenvec)
pca_data <- read.table(pca_data_file, sep=" ", dec=".")

colnames(pca_data)[colnames(pca_data) == "V1"] <- "sample"
colnames(pca_data)[colnames(pca_data) == "V3"] <- "PC1"
colnames(pca_data)[colnames(pca_data) == "V4"] <- "PC2"
colnames(pca_data)[colnames(pca_data) == "V5"] <- "PC3"



### Get the information about the good probands ###
good_ind_file <- args[2] ## File with the correspondence ID of the probands (genetic & genealogy) and their home region
good_ind <- read.table(good_ind_file, sep="\t", header=T)



### Join the info ###
data <- join(pca_data, good_ind, by = c("sample"), type="left")
data$graph_reg <- substr(data$final_id, 1, 3)



### Do the graphic ###
## Graphic parameters
official_palete <- c("chocolate1", "green", "blue", "burlywood4", "darkorchid1", "yellow", "cyan", "red")
level_order = c("MTL", "QUE", "SAG", "NSH", "GFC", "GCI", "GLO", "GAC")


## Graph PC1-PC2
graphic_1 <- ggplot(data, aes(x=PC1, y=PC2, col=factor(graph_reg, levels=level_order))) + geom_point(size=0.85, alpha=0.75) + scale_colour_manual(values=official_palete, "Group") + labs(y="PC2", x = "PC1", tag="A") + theme(text=element_text(face="bold", size=12))
graphic_1

## Saving the graph PC1-PC2
ouput_file_1 <- args[3] ## Output for graph 1
pdf(file = ouput_file_1 ,width = 15/2.54, height = 11/2.54)
print(graphic_1)
dev.off()



## Graph PC1-PC23
graphic_1.5 <- ggplot(data, aes(x=PC1, y=PC3,  col=factor(graph_reg, levels=level_order))) + geom_point(size=0.85, alpha=0.75) + scale_colour_manual(values=official_palete, "Group") + labs(y="PC3", x = "PC1", tag="B") + theme(text=element_text(face="bold", size=12))
graphic_1.5

## Saving the graph PC1-PC3
ouput_file_2 <- args[4] ## Output for graph 2
pdf(file = ouput_file_2,width = 15/2.54, height = 11/2.54)
print(graphic_1.5)
dev.off()



## Plot together
plot_together <- ggarrange(graphic_1, graphic_1.5,common.legend =T, legend="right")
plot_together

## Saving
ouput_file_12 <- args[5] ## Output for graph 1 and 2
pdf(file = ouput_file_12 ,width = 17.5/2.54, height = 7/2.54)
print(plot_together)
dev.off()




### Interactove graphic
## Graphic parameters
official_palete_int <- c("red", "yellow","darkorchid1", "cyan","chocolate1", "burlywood4","green", "blue")

## Graph interactif PC1-PC2
graph_1 <- data %>% plot_ly(
  x = ~PC1,
  y = ~PC2,
  color = I(~graph_reg),
  sizes= c(50,50),
  colors = official_palete_int,
  type = 'scatter',
  mode = 'markers'
) 
graph_1

## Saving 
ouput_file_interactive_1 <- args[6] ## Output for graph interactive 1
saveWidget(graph_1, ouput_file_interactive_1)


##  Graph interactif PC1-PC2
graph_2 <- data %>% plot_ly(
  x = ~PC1,
  y = ~PC3,
  color = I(~graph_reg),
  sizes= c(50,50),
  colors = official_palete_int,
  type = 'scatter',
  mode = 'markers'
) 
graph_2

## Saving 
ouput_file_interactive_2 <- args[7] ## Output for graph interactive 2
saveWidget(graph_2, output_file_2)


## Graph 3D - PC1, PC2 and PC3
fig_3D <- plot_ly(data, x = ~PC1, y = ~PC2, z = ~PC3, color = ~graph_reg, colors = official_palete_int) %>% 
  add_markers(opacity = 0.74, marker = list(size = 4)) %>% 
  layout(title= list(text = "3D PCA",font =  list(size = 15)), 
       font = list(size = 15), 
       legend=list(title=list(text='Subpopulation',font =  list(size = 20))), 
       xaxis = list(title = list(text ='PC1', font =  list(size = 20))),
       xaxis = list(title = list(text ='PC2', font =  list(size = 20))),
       xaxis = list(title = list(text ='PC3', font =  list(size = 20))))
fig_3D

## Saving
ouput_file_3D <- args[8] ## Output for graph 3D
saveWidget(fig_3D, ouput_file_3D)






