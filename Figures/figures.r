install.packages("ggplot2")
install.packages("tidyr")
install.packages("forcats")
library(ggplot2)
library(tidyr)
library(dplyr)
library(forcats)

cat("\014")  
rm(list = ls())

# prep data
proj_dir <- "D:/OneDrive - University Of Cambridge/Work/PhD Psychology/Experiment 1 - Cross-Modal Integration across Declarative Memory/"
fig_data <- read.delim(paste0(proj_dir, "/fig_data.csv"))
sum_data <- read.delim(paste0(proj_dir, "/Summary_data.csv"), header = TRUE, sep = ",")

dat.g <- gather(fig_data, type, value, -Site)
dat.g$Site <- factor(dat.g$Site, levels = c("Vertex", "AnG"))
dat.g$Task <- c("Intactness", "Intactness", "Intactness", "Intactness", "Relatedness", "Relatedness", "Relatedness", "Relatedness")
dat.g$Task <- factor(dat.g$Task, levels = c("Intactness", "Relatedness"))
dat.sum <- sum_data[match(dat.g$value, sum_data$RT), ]


# plot 
cols <- c("Vertex" = "steelblue", "AnG" = "red3")
plot.dat <- ggplot(dat.g, aes(type, value)) + geom_bar(aes(fill = Site), stat = "identity", = position_dodge(width = 0.9)) + facet_grid(. ~ Task, labeller = label_parsed, scales = "free_x")
plot.dat <- plot.dat + scale_fill_manual(values = cols)
plot.dat <- plot.dat + scale_x_discrete(labels = c("Within-Modal", "Cross-Modal"))
plot.dat <- plot.dat + aes(x = fct_inorder(type)) 
plot.dat <- plot.dat + theme(axis.title.x = element_blank(), 
                             axis.line = element_line(colour = "black"), 
                             panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(),
                             panel.background = element_blank())
plot.dat <- plot.dat + theme(axis.line = element_line(colour = "black"), 
                             panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(),
                             panel.background = element_blank())
plot.dat <- plot.dat + labs(y = "Median RT", fill = "TMS Site")
plot.dat <- plot.dat + geom_errorbar(aes(ymin = dat.sum$RT - dat.sum$moe, ymax = dat.sum$RT + dat.sum$moe), colour = "black", width = .1, position=position_dodge(width=0.9)) 

ggsave(paste0(proj_dir, "/median RTs plot.jpg"), plot.dat)