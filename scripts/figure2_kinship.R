#### 
# Laurence Gagnon
# 31 mai 2021
# Graphic of MDS 
####


### Packages ###
rm(list = ls());
args <- commandArgs(trailingOnly = TRUE) 
library("ggplot2")
library("plotly")
library("pals")
library("matlab")
library("htmltools")
library("htmlwidgets")
library("gtools")
library("plyr")
library("ggpubr")



### Open MDS file ###
setwd("/set/in/your/working/directory")

## With the output created in : compute_kinship_and_MDS.R
files = intersect(list.files(pattern= "your/file/pattern"), list.files(pattern="your/file/pattern/if/needed"))
all_file <- as.data.frame(files)
files_sort <- mixedsort(all_file)
data_list = lapply(files_sort, read.table, dec="." , header = T, sep="\t")
mds <- do.call(rbind, data_list) 



### Add origin data ###
origin_file <- args[1] ## This file contain the origin of the individuals that the parents were not married in Quebec
origin <- read.table(origin_file, header=T, sep="\t")
origin <- origin[,-2]
colnames(origin) <- c("ID", "origin")
mds <- join(mds, origin, by = c("ID"), type="left")




### Create junk data to put on the graph because ploty won't work if they are not there...
df <- data.frame(ID = rep(NA, 252),
                       axe_x = rep(NA, 252),
                       axe_y = rep(NA, 252),
                       reg_mariage = rep(c("Unknown", "Quebec City", "Mauricie", "Montreal", "Cote-de-Beaupre", "Richelieu", "Bois-Francs", "Lanaudiere",  "North Shore", "Cote-du-Sud", "Gaspe",  "Laurentides", "Charlevoix", "Bas-Saint-Laurent", "Beauce", "Iles-de-la-Madeleine", "Saguenay-Lac-Saint-Jean", "Outaouais", "Estrie", "Abitibi-Temiscamingue", "Nord du Quebec" ), each=12),
                       reg_mariage_number= rep(NA, 252),
                       start = rep(c(1675, 1700, 1725, 1750, 1775, 1800, 1825, 1850, 1875, 1900, 1925, 1950), 21),
                       end = rep(c( 1650, 1675, 1700, 1725, 1750, 1775, 1800, 1825, 1850, 1875, 1900, 1925), 21),
                       origin = rep(" ", 252),
                       good_x = rep(0, 252),
                       good_y = rep(0, 252)
)


df_2 <- data.frame(ID = rep(NA, 180),
                         axe_x = rep(NA, 180),
                         axe_y = rep(NA, 180),
                         reg_mariage = rep(c("Unknown"), 180),
                         reg_mariage_number= rep(NA, 180),
                         start = rep(c(1675, 1700, 1725, 1750, 1775, 1800, 1825, 1850, 1875, 1900, 1925, 1950), 15),
                         end = rep(c( 1650, 1675, 1700, 1725, 1750, 1775, 1800, 1825, 1850, 1875, 1900, 1925), 15),
                         origin = rep(c("France", " ",  "USA", "Acadie", "Quebec", "New Brunswick", "Swiss", "Newfoundland and labrador", "New Scotland", "Scotland", "UK", "Ireland",  "Channel Islands", "Ontario", "British Columbia"), each=12),
                         good_x = rep(0, 180),
                         good_y = rep(0, 180)
)


mds_allreg <- rbind(df, mds)
mds_allregion <- rbind(df_2, mds_allreg)


### The vector of color ###
COL_21 <- c("#00ffad","#005e8d","#e9b6ee","#5f6742","#349800","#9e39f3", "#fad914","#5800a8","red","#de9c00","#baeaab","#9c87ca","#9f1269", "chocolate1","#220d0c","burlywood4", "#00e4ff","green","#ff0076","blue","#7994ae")



### Interactive graoh with origin data ###
symboles <- c("circle","star", "circle-open",  "square", "square-open", "diamond-open", "pentagon","pentagon-open",  "cross", "cross-open","hexagon", "hexagon-open","hexagram", "bowtie", "bowtie-open")


graph_shapeorigin <- mds_allregion %>% plot_ly(
  x = ~good_x,
  y = ~good_y,
  color = I(~reg_mariage),
  sizes= c(50,50),
  colors = COL_21,
  frame = ~start,
  type = 'scatter',
  mode = 'markers',
  symbol= ~origin,
  symbols = symboles , # 'square' 'x',
  text = ~paste(reg_mariage, " - ", origin) ,
  hoverinfo = "text"
) %>% layout(title="Interactif MDS across 25 years interval",
             legend=list(title=list(text="Region")),
             xaxis=list(title=""),
             yaxis=list(title="")
)  %>% animation_slider(currentvalue = list(prefix = "Year : ", font = list(color="black")))



## Saving the file 
output_file <- args[2] ## Output file of the interactive graph
saveWidget(graph_shapeorigin, output_file)








#### Non interactive graph 

mds1700 <- mds_allregion[mds_allregion$start == 1700,]
mds1725 <- mds_allregion[mds_allregion$start == 1725,]
mds1750 <- mds_allregion[mds_allregion$start == 1750,]
mds1775 <- mds_allregion[mds_allregion$start == 1775,]
mds1800 <- mds_allregion[mds_allregion$start == 1800,]
mds1825 <- mds_allregion[mds_allregion$start == 1825,]
mds1850 <- mds_allregion[mds_allregion$start == 1850,]
mds1875 <- mds_allregion[mds_allregion$start == 1875,]
mds1900 <- mds_allregion[mds_allregion$start == 1900,]
mds1925 <- mds_allregion[mds_allregion$start == 1925,]

graph_1 <- ggplot(mds1700, aes(x=good_x, y=good_y, color=reg_mariage)) +
  geom_point(size=0.75)+
  scale_color_manual(values=COL_21)+
  #                  theme(legend.direction ="vertical", legend.position = "bottom")+
  theme(text=element_text(face="bold", size=10), plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(ncol=1, override.aes = list(size=3))) +
  labs(title="1676-1700", x=element_blank(), y=element_blank(), col="Parents' marriage region")+
  xlim(-0.15, 0.25)+
  ylim(-0.30,0.2)

graph_2 <- ggplot(mds1725, aes(x=good_x, y=good_y, color=reg_mariage)) +
  geom_point(size=0.75)+
  scale_color_manual(values=COL_21)+
  #                  theme(legend.direction ="vertical", legend.position = "bottom")+
  theme(text=element_text(face="bold", size=10), plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(ncol=1, override.aes = list(size=3))) +
  labs(title="1701-1725", x=element_blank(), y=element_blank(), col="Parents' marriage region")+
  xlim(-0.15, 0.25)+
  ylim(-0.3,0.2)

graph_3 <- ggplot(mds1750, aes(x=good_x, y=good_y, color=reg_mariage)) +
  geom_point(size=0.75)+
  scale_color_manual(values=COL_21)+
  #                  theme(legend.direction ="vertical", legend.position = "bottom")+
  theme(text=element_text(face="bold", size=10), plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(ncol=1, override.aes = list(size=3))) +
  labs(title="1726-1750", x=element_blank(), y=element_blank(), col="Parents' marriage region")+
  xlim(-0.15, 0.25)+
  ylim(-0.3,0.2)

graph_4 <- ggplot(mds1775, aes(x=good_x, y=good_y, color=reg_mariage)) +
  geom_point(size=0.75)+
  scale_color_manual(values=COL_21)+
  #                  theme(legend.direction ="vertical", legend.position = "bottom")+
  theme(text=element_text(face="bold", size=10), plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(ncol=1, override.aes = list(size=3))) +
  labs(title="1751-1775", x=element_blank(), y=element_blank(), col="Parents' marriage region")+
  xlim(-0.15, 0.25)+
  ylim(-0.3,0.2)

graph_5 <- ggplot(mds1800, aes(x=good_x, y=good_y, color=reg_mariage)) +
  geom_point(size=0.75)+
  scale_color_manual(values=COL_21)+
  #                  theme(legend.direction ="vertical", legend.position = "bottom")+
  theme(text=element_text(face="bold", size=10), plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(ncol=1, override.aes = list(size=3))) +
  labs(title="1776-1800", x=element_blank(), y=element_blank(), col="Parents' marriage region")+
  xlim(-0.15, 0.25)+
  ylim(-0.3,0.2)

graph_6 <- ggplot(mds1825, aes(x=good_x, y=good_y, color=reg_mariage)) +
  geom_point(size=0.75)+
  scale_color_manual(values=COL_21)+
  #                  theme(legend.direction ="vertical", legend.position = "bottom")+
  theme(text=element_text(face="bold", size=10), plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(ncol=1, override.aes = list(size=3))) +
  labs(title="1801-1825", x=element_blank(), y=element_blank(), col="Parents' marriage region")+
  xlim(-0.15, 0.25)+
  ylim(-0.3,0.2)

graph_7 <- ggplot(mds1850, aes(x=good_x, y=good_y, color=reg_mariage)) +
  geom_point(size=0.75)+
  scale_color_manual(values=COL_21)+
  #                  theme(legend.direction ="vertical", legend.position = "bottom")+
  theme(text=element_text(face="bold", size=10), plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(ncol=1, override.aes = list(size=3))) +
  labs(title="1826-1850", x=element_blank(), y=element_blank(), col="Parents' marriage region")+
  xlim(-0.15, 0.25)+
  ylim(-0.3,0.2)

graph_8 <- ggplot(mds1875, aes(x=good_x, y=good_y, color=reg_mariage)) +
  geom_point(size=0.75)+
  scale_color_manual(values=COL_21)+
  #                  theme(legend.direction ="vertical", legend.position = "bottom")+
  theme(text=element_text(face="bold", size=10), plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(ncol=1, override.aes = list(size=3))) +
  labs(title="1851-1875", x=element_blank(), y=element_blank(), col="Parents' marriage region")+
  xlim(-0.15, 0.25)+
  ylim(-0.3,0.2)

graph_9 <- ggplot(mds1900, aes(x=good_x, y=good_y, color=reg_mariage)) +
  geom_point(size=0.75)+
  scale_color_manual(values=COL_21)+
  #                  theme(legend.direction ="vertical", legend.position = "bottom")+
  theme(text=element_text(face="bold", size=10), plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(ncol=1, override.aes = list(size=3))) +
  labs(title="1876-1900", x=element_blank(), y=element_blank(), col="Parents' marriage region")+
  xlim(-0.15, 0.25)+
  ylim(-0.3,0.2)

graph_10 <- ggplot(mds1925, aes(x=good_x, y=good_y, color=reg_mariage)) +
  geom_point(size=0.75)+
  scale_color_manual(values=COL_21)+
  #                  theme(legend.direction ="vertical", legend.position = "bottom")+
  theme(text=element_text(face="bold", size=10), plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(ncol=1, override.aes = list(size=3))) +
 # guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(title="1901-1925", x=element_blank(), y=element_blank(), col="Parents' marriage region")+
  xlim(-0.15, 0.25)+
  ylim(-0.3,0.2)


plot_all <- ggarrange(graph_1,  graph_2,  graph_3,  graph_4,  graph_5,  graph_6,  graph_7,  graph_8,  graph_9,  graph_10, common.legend = T, legend="right", nrow=5, ncol=2)
plot_all <- annotate_figure(plot_all, 
                        left = text_grob("Dimension 2",  rot = 90, size=14),
                        bottom = text_grob("Dimension 1", size=14, x = 0.37))


graph_out_file_3.3 <- args[3] ## Output file of the graph
pdf(file = graph_out_file_3.3,  width = 17.4/2.54, height = 34/2.54)
plot_all
dev.off()












