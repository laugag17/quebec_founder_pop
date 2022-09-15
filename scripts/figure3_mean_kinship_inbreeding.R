#### 
# Laurence Gagnon
# Summer 2022
# Graphic of mean kinship and inbreeding
####

### Packages
rm(list = ls());
library("ggplot2")

### Open the file 
## Kinship data
data_kinship_file <- args[1] ## File created in : compute_mean_kinship.R 
data_kinship <- read.table(data_kinship_file, header=T)
data_kinship$real_end <- data$end + 1
data_kinship$interval <- paste0(data_kinship$real_end, "-", data_kinship$start)

## Inbreeding data 
data_inbreeding_file <- args[2] ## File created in : compute_mean_inbreeding.R 
data_inbreeding <- read.table(data_inbreeding_file, header=T)
data_inbreeding $real_end <- data$end + 1
data_inbreeding $interval <- paste0(data_inbreeding$real_end, "-", data_inbreeding$start)

### Graphic info ###
official_palete <- c("chocolate1", "green", "blue", "burlywood4", "darkorchid1", "yellow", "cyan", "red")
level_order = c("MTL", "QUE", "SAG", "NSH", "GFC", "GCI", "GLO", "GAC")


### Do the graphics
plt_kinship <- ggplot(data=data_inbreeding, aes(x=interval, y=mean_inbreed, col=factor(Region, level=level_order), group=factor(Region, level=level_order))) +
  # geom_point(size=0.25)+
  geom_line(size=0.25) +
  #  geom_ribbon(aes(ymin=Min_inbreed, ymax=Max_inbreed ,fill=factor(Region, level=level_order)),linetype=2,alpha=0.1,size=0.25) +
  scale_color_manual(name = "Group", values = official_palete) +
  # scale_fill_manual(name = "Group", values = official_palete) +
  theme(text=element_text(face="bold", size=10), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust=1, size=7)) +
  guides(color=guide_legend(ncol=1, override.aes = list(size=0.8))) +
  labs(title=" ", x="Year interval", y="Ancestors' mean inbreeding", tag="B")


plt_inbreeding <- ggplot(data=data_kinship, aes(x=interval, y=mean, col=factor(Region, level=level_order), group=factor(Region, level=level_order))) +
  #  geom_point(size=0.25)+
  geom_line(size=0.25) +
  #  geom_ribbon(aes(ymin=Min, ymax=Max ,fill=factor(Region, level=level_order)),linetype=2,alpha=0.1,size=0.25) +
  scale_color_manual(name = "Group", values = official_palete) +
  # scale_fill_manual(name = "Group", values = official_palete) +
  theme(text=element_text(face="bold", size=10), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust=1, size=7)) +
  guides(color=guide_legend(ncol=1, override.aes = list(size=0.8))) +
  labs(title=" ", x="Year interval", y="Ancestors' mean kinship", tag="A")


### Put graphic together ###
plt_kinship_and_inbreeding <- ggarrange(plt_kinship, plt_inbreeding , ncol=2, nrow=1,common.legend = TRUE, legend = "right")

outfile_kinship_and_inbreeding <- args[3] ## Graphic output
pdf(file = outfile_kinship_and_inbreeding,  width = 17.4/2.54, height = 9/2.54)
plt_kinship_and_inbreeding 
dev.off()





