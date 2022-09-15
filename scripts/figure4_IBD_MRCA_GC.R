####
# Laurence Gagnon
# August 2022
# Do figure 3
####


### Packages ###
rm(list = ls());
args <- commandArgs(trailingOnly = TRUE) 
library("ggplot2")
library("ggpubr")
library("plyr")
library("dplyr")
library("tidyr")



### Graph parameters ###
official_palete <- c("chocolate1", "green", "blue", "burlywood4", "darkorchid1", "yellow", "cyan", "red")
level_order = c("MTL", "QUE", "SAG", "NSH", "GFC", "GCI", "GLO", "GAC")
x_levels=c("[49,53[", "[45,49[", "[41,45[", "[37,41[", "[33,37[", "[29,33[", "[25,29[","[21,25[", "[17,21[" ,"[13,17[" ,"[9,13[", "[5,9[","[1,5[")


### Panel A ###
## Data
data_ibd_file <- args[1] ## output file of : compute_IBD_segments_number_by_pairs.R
data_ibd <- read.table(data_ibd_file, header=T)
data_ibd <- data_ibd[!(data_ibd$length > 50),]
data_ibd$interval <- paste0("[", data_ibd$length, ",", data_ibd$length + 4, "[")

## Graph
graph_A <- ggplot(data_ibd, aes(x=factor(interval, levels = x_levels), y=log10(value), col=factor(variable, levels=level_order), group = factor(variable, levels=level_order))) +
  geom_line(size=0.25) +
  scale_color_manual(values=official_palete) +
  theme(text=element_text(face="bold", size=8), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust=1, size=6)) +
  guides(color=guide_legend(ncol=4, override.aes = list(size=0.8))) +
  labs(x="IBD segment length (cM)", y="Mean number of segments \nshared per pairs (log10)", col="Group", tag="A") #+

graph_A



### Panel B ##
## Data
data_MRCA_file <- args[2] ## output file of : compute_MRCA_bootstrap.R
data_AV2 <- read.table(data_MRCA_file, header=T)
data_AV2$reg <-   ifelse(data_AV2$reg == "CNO", "NSH",data_AV2$reg)

data_AV2_order <- data_AV2 %>% group_by(reg) %>% arrange(reg, meiose)
data_AV2_order_cum <- data_AV2_order %>% group_by(reg) %>% mutate(value_cum = cumsum(mean))
data_AV2_order_cum_min <- data_AV2_order_cum %>% group_by(reg) %>% mutate(min_cum = cumsum(min))
data_AV2_order_cum_min_max <- data_AV2_order_cum_min %>% group_by(reg) %>% mutate(max_cum = cumsum(max))

graph_B <-  ggplot(data_AV2_order_cum, aes(x=meiose, y=log10(value_cum), col=factor(reg, levels=level_order))) +
  geom_line(size=0.25) +
  scale_color_manual(name = "Group", values=official_palete)+
  theme(text=element_text(face="bold", size=8), plot.title = element_text(hjust = 0.5)) +
  labs(x="Meiosis", y="MRCA cumulative number (log10)", col="Region", tag="B")+
  guides(color=guide_legend(ncol=4, override.aes = list(size=0.8))) +
  xlim(5,16)
graph_B



### Pannel C ###
## Data
data_cg_sum_file <- args[3] ## output file of : compute_CG_bootstrap.R
cg_sum  <- read.table( data_cg_sum_file, header=T)

## Graph 
graph_c <-  ggplot(cg_sum, aes(x=meiose, y=mean, col=factor(reg, levels=level_order))) +
  geom_line(size=0.25) +
  #  geom_ribbon(aes(ymin=min, ymax=max,  fill=factor(reg, levels=level_order)),linetype=2,alpha=0.1, size=0.25) +
  scale_color_manual(name = "Group", values=official_palete)+
  scale_fill_manual(name = "Group", values=official_palete)+
  theme(text=element_text(face="bold", size=8), plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(ncol=4, override.aes = list(size=0.8))) +
  labs(x="Meiosis", y="Sum of MRCAs GC product\nto both individuals", col="Subpopulation", tag="C") +
  xlim(5,16)
graph_c


### Group the graph ###
plot <-  ggarrange(graph_A, graph_B, graph_c, ncol=3, nrow=1, common.legend = T, legend="bottom")

## Saving the graph
outfile_plot <- args[4] ## The output of the graph
pdf(file = outfile_plot,  width = 17.4/2.54, height =8/2.54)
plot
dev.off()



### Supplementary fig 4 ###
graph_B2 <-  ggplot(data_AV2_order_cum_min_max, aes(x=meiose, y=log10(value_cum), col=factor(reg, levels=level_order))) +
  geom_line(size=0.25) +
  geom_ribbon(aes(ymin=log10(min_cum), ymax=log10(max_cum),  fill=factor(reg, levels=level_order)),linetype=2,alpha=0.1, size=0.25) +
  scale_color_manual(name = "Group", values=official_palete)+
  scale_fill_manual(name = "Group", values=official_palete)+
  theme(text=element_text(face="bold", size=8), plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(ncol=4, override.aes = list(size=0.4))) +
  labs(x="Meiosis", y="MRCA cumulative number (log10)", col="Region", tag="A")# +
# scale_y_sqrt() 
graph_B2


graph_c2 <-  ggplot(cg_sum, aes(x=meiose, y=mean, col=factor(reg, levels=level_order))) +
  geom_line(size=0.25) +
  geom_ribbon(aes(ymin=min, ymax=max,  fill=factor(reg, levels=level_order)),linetype=2,alpha=0.1, size=0.25) +
  scale_color_manual(name = "Group", values=official_palete)+
  scale_fill_manual(name = "Group", values=official_palete)+
  theme(text=element_text(face="bold", size=8), plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(ncol=4, override.aes = list(size=0.4))) +
  labs(x="Meiosis", y="Mean sum of product of the \nMRCA genetic contribution", col="Subpopulation", tag="B") 
graph_c2

fig_complete_V2 <- ggarrange(graph_B2,graph_c2, ncol=2, nrow=1, common.legend = T, legend="bottom")
fig_complete_V2


outfile_V2 <- args[5] ## The output of the supplementary graph
pdf(file = outfile_V2,  width = 17.4/2.54, height =11/2.54)
fig_complete_V2
dev.off()









