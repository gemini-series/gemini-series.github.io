#install packages  ---- 

#install packages
install.packages("ggplot2")
install.packages("readxl")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("RColorBrewer")
install.packages("viridis")
install.packages("cowplot")
install.packages("patchwork")
install.packages("tidyr")
install.packages("stringr")
install.packages("ggsci")
install.packages("magrittr")
install.packages("pandas")
install.packages("mblm")
install.packages("rstatix")
install.packages("spdep")
install.packages("psych")
install.packages("ggbeeswarm")
install.packages("ggbreak")



#load packages ----
library(ggplot2)
library(readxl)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(viridis)
library(cowplot)
library(patchwork)
library(tidyr)
library(stringr)
library(ggsci)
library(magrittr)
library(pandas)
library(mblm)
library(rstatix)
library(spdep)
library(psych)
library(ggbeeswarm)
library(ggbreak)


#set directories ----
#set working directory
wd<-"/Users/markmpainter/Desktop/Gemini" #define working directory
setwd(wd) #set working directory

#establish directories for saving plots
#you will change version directory each time you make substantial changes to your script
#this will save an archive of old versions of every figure you generated
#the figure directory will never change
#this will allow new figures to instantly replace old figures in your illustrator files

figure_directory <-"/Users/markmpainter/Desktop/Gemini/Exports/Figures" 
version_directory <- "/Users/markmpainter/Desktop/Gemini/Exports/Versions/20230425"

#create dataframes by reading in files containing data 
Vaccination_dataframe <- read.csv("Gemini.2.0.csv")

Longitudinal_dataframe <- read.csv("Gemini.1.0.csv")


#building an aesthetically pleasing ggplot ----

#create a basic boxplot
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_boxplot()
D614G_neuts

#great a basic scatter plot
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_point()
D614G_neuts


#add jittering and other specifications to the plot
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_point(pch=21, alpha = 0.7, size = 2, position = "jitter") 
D614G_neuts


#use the geom_quasirandom() function from the ggbeeswarm package to make a nicer plot
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom()
D614G_neuts


#add specifications to jittered plot
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.2)
D614G_neuts


#modify width to fit desired aesthetics
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4)
D614G_neuts

#change color for all points
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, color = "red")
D614G_neuts

#change color based on a variable
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))
D614G_neuts



#layer multiple geoms using the +
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine)) +
  geom_boxplot()
D614G_neuts


#improve the boxplot aesthetics
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine)) +
  geom_boxplot(alpha = 0.2, outlier.size = -1, aes(group = Vaccine))
D614G_neuts


#improve the boxplot aesthetics
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine)) +
  geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine))
D614G_neuts

#log scale 
#keep using the + to add more details to the plot
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine)) +
  geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10() 
D614G_neuts

  

#log scale with defined limits
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine)) +
  geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) 
D614G_neuts


#use stat_summary instead of boxplot
#use hashtags to remove elements you don't want
#can still save them there in case you want to use it later
#hashtags allow you to toggle on/off various aesthetic elements of your plot
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))+
  #geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) +
  stat_summary()
D614G_neuts


#use stat_summary with geometric mean

D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))+
  #geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) +
  stat_summary(fun = geometric.mean)
D614G_neuts


#make it look nice
#use hexcodes to manually assign colors
#idea: use the eyedropper tool in illustrator to pick your colors
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))+
  #geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) +
  stat_summary(fun = geometric.mean, geom = "crossbar", color = "#182B54", size = 0.5, width = 0.9)
D614G_neuts


#add log ticks
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))+
  #geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) +
  stat_summary (fun = geometric.mean, geom = "crossbar", color = "#182B54", size = 0.5, width = 0.9) +
  annotation_logticks(sides = "l") 
D614G_neuts

#make it less ugly using themes!
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))+
  #geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) +
  stat_summary (fun = geometric.mean, geom = "crossbar", color = "#182B54", size = 0.5, width = 0.9) +
  annotation_logticks(sides = "l") +  
  theme_bw()
D614G_neuts

#make it even less ugly!
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))+
  #geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) +
  stat_summary (fun = geometric.mean, geom = "crossbar", color = "#182B54", size = 0.5, width = 0.9) +
  annotation_logticks(sides = "l") +  
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
D614G_neuts

#manually set axis labels
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))+
  #geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) +
  stat_summary (fun = geometric.mean, geom = "crossbar", color = "#182B54", size = 0.5, width = 0.9) +
  annotation_logticks(sides = "l") +  
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = "FRNT50") +
  labs(x= "Vaccinated") 
D614G_neuts

#rename and move the legend
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))+
  #geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) +
  stat_summary (fun = geometric.mean, geom = "crossbar", color = "#182B54", size = 0.5, width = 0.9) +
  annotation_logticks(sides = "l") +  
  theme_bw() +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = "FRNT50") +
  labs(x= "Vaccinated") +
  labs(color = "Prior Vaccination") +
  theme(legend.position = "bottom") 
D614G_neuts

#adjust axis text
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))+
  #geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) +
  stat_summary (fun = geometric.mean, geom = "crossbar", color = "#182B54", size = 0.5, width = 0.9) +
  annotation_logticks(sides = "l") +  
  theme_bw() +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = "FRNT50") +
  labs(x= "Vaccinated") +
  labs(color = "Prior Vaccination") +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5)) 
D614G_neuts

#add a title
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))+
  #geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) +
  stat_summary (fun = geometric.mean, geom = "crossbar", color = "#182B54", size = 0.5, width = 0.9) +
  annotation_logticks(sides = "l") +  
  theme_bw() + theme(legend.position = "bottom") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = "FRNT50") +
  labs(x= "Vaccinated") +
  labs(color = "Prior Vaccination") +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  ggtitle("D614G")
D614G_neuts

#center the title
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))+
  #geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) +
  stat_summary (fun = geometric.mean, geom = "crossbar", color = "#182B54", size = 0.5, width = 0.9) +
  annotation_logticks(sides = "l") +  
  theme_bw() + theme(legend.position = "bottom") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = "FRNT50") +
  labs(x= "Vaccinated") +
  labs(color = "Prior Vaccination") +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  ggtitle("D614G") +
  theme(plot.title = element_text(hjust = 0.5))  
D614G_neuts

#manually control the colors using hexcodes
#only applies to colors in the aes()
#the color in stat_summary is outside the aes(), so it overrules the scale_color_manual
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))+
  #geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) +
  stat_summary (fun = geometric.mean, geom = "crossbar", color = "#182B54", size = 0.5, width = 0.9) +
  annotation_logticks(sides = "l") +  
  theme_bw() + theme(legend.position = "bottom") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = "FRNT50") +
  labs(x= "Vaccinated") +
  labs(color = "Prior Vaccination") +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  ggtitle("D614G") +
  theme(plot.title = element_text(hjust = 0.5)) +
  #theme(axis.text.y = element_blank()) +
  scale_color_manual(values = c("#E26E45", "#47598A")) #only applies to colors in the aes()
D614G_neuts


#run statistics
stat.test_D614G_neuts <- Vaccination_dataframe %>% #use the same dataframe and store as a new object
  #filter(infection_count < 2) %>% #filter if necessary (i.e. if subsetting in your ggplot)
  #group_by(Vaccine) %>% #group if necessary (i.e. if fascetting in your ggplot)
  wilcox_test(neut_d614g ~ Vaccine) %>% #wilcoxon test
  adjust_pvalue(method = "BH") %>% #correct for multiple comparisons
  add_significance() %>% #add significance so you can display it on the plot
  add_y_position(fun = "max", step.increase = 0)  %>% #add a y position so it displays where you want
  mutate(y.position = 5) #mutate the y position to set it exactly where you want
stat.test_D614G_neuts #check the statistical test results


# final plots ----
#add statistics to the plot!
D614G_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))+
  #geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) +
  stat_summary (fun = geometric.mean, geom = "crossbar", color = "#182B54", size = 0.5, width = 0.9) +
  stat_pvalue_manual(stat.test_D614G_neuts, hide.ns = TRUE, label = "p.adj.signif", size = 5,remove.bracket = FALSE) +
  annotation_logticks(sides = "l") +  
  theme_bw() + theme(legend.position = "bottom") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = "FRNT50") +
  labs(x= "Vaccinated") +
  labs(color = "Prior Vaccination") +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  ggtitle("D614G") +
  theme(plot.title = element_text(hjust = 0.5)) +
  #theme(axis.text.y = element_blank()) +
  scale_color_manual(values = c("#E26E45", "#47598A")) 
D614G_neuts



## Copy-paste to make the same plot for BA.1.1 instead of D614G

stat.test_BA11_Neuts <- Vaccination_dataframe %>%
  #filter(infection_count < 2) %>%
  #group_by(Vaccine) %>%
  wilcox_test(neut_BA11 ~ Vaccine) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance() %>%
  add_y_position(fun = "max", step.increase = 0)  %>%
  mutate(y.position = y.position * 3)
stat.test_BA11_Neuts

BA11_neuts <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_BA11)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))+
  #geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) +
  stat_summary (fun = geometric.mean, geom = "crossbar", color = "#CB2026", size = 0.5, width = 0.9) +
  stat_pvalue_manual(stat.test_D614G_neuts, hide.ns = TRUE, label = "p.adj.signif", size = 5,remove.bracket = FALSE) +
  annotation_logticks(sides = "l") +  
  theme_bw() + theme(legend.position = "bottom") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = "FRNT50") +
  labs(x= "Vaccinated") +
  labs(color = "Prior Vaccination") +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  ggtitle("BA.1.1") +
  theme(plot.title = element_text(hjust = 0.5)) +
  #theme(axis.text.y = element_blank()) +
  scale_color_manual(values = c("#E26E45", "#47598A")) 
BA11_neuts



#use patchwork to combine the two plots into a single figure

Neut_graphs <- (D614G_neuts | BA11_neuts)
Neut_graphs

#add an annotation to give the entire figure a shared title
Neut_graphs <- (D614G_neuts | BA11_neuts) +
  plot_annotation(title = 'Neutralization') 
Neut_graphs

#make the title centered over the figures
#this may take some optimization on a case-by-case basis
Neut_graphs <- (D614G_neuts | BA11_neuts) +
  plot_annotation(title = 'Neutralization', theme = theme(plot.title = element_text(hjust = 0.65))) 
Neut_graphs

##make optimized plots for combining into a figure
#remove legend from plot on the left
D614G_neuts_for_combining <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))+
  #geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) +
  stat_summary (fun = geometric.mean, geom = "crossbar", color = "#182B54", size = 0.5, width = 0.9) +
  stat_pvalue_manual(stat.test_D614G_neuts, hide.ns = TRUE, label = "p.adj.signif", size = 5,remove.bracket = FALSE) +
  annotation_logticks(sides = "l") +  
  theme_bw() + theme(legend.position = "bottom") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = "FRNT50") +
  labs(x= "Vaccinated") +
  labs(color = "Prior Vaccination") +
  theme(legend.position = "none") + #no legend in this one!
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  ggtitle("D614G") +
  theme(plot.title = element_text(hjust = 0.5)) +
  #theme(axis.text.y = element_blank()) +
  scale_color_manual(values = c("#E26E45", "#47598A")) 
D614G_neuts_for_combining


#remove y-axis info for plot on the right

BA11_neuts_for_combining <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_BA11)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))+
  #geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) +
  stat_summary (fun = geometric.mean, geom = "crossbar", color = "#CB2026", size = 0.5, width = 0.9) +
  stat_pvalue_manual(stat.test_D614G_neuts, hide.ns = TRUE, label = "p.adj.signif", size = 5,remove.bracket = FALSE) +
  annotation_logticks(sides = "l") +  
  theme_bw() + theme(legend.position = "bottom") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = element_blank()) + #no y-axis label for this one!
  labs(x= "Vaccinated") +
  labs(color = "Prior Vaccination") +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  theme(axis.text.y = element_blank()) + #no y-axis text for this one!
  ggtitle("BA.1.1") +
  theme(plot.title = element_text(hjust = 0.5)) +
  #theme(axis.text.y = element_blank()) +
  scale_color_manual(values = c("#E26E45", "#47598A")) 
BA11_neuts_for_combining






Neut_graphs <- (D614G_neuts_for_combining | BA11_neuts_for_combining) +
  plot_annotation(title = 'Neutralization', theme = theme(plot.title = element_text(hjust = 0.65))) 
Neut_graphs


Neut_graphs <- (D614G_neuts_for_combining | BA11_neuts_for_combining) +
  plot_annotation(title = 'Neutralization', theme = theme(plot.title = element_text(hjust = 0.45))) 
Neut_graphs



#realize I don't want the legend at all
#the information is already in my schematic and my axis titles

BA11_neuts_for_combining_no_legend <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_BA11)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))+
  #geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) +
  stat_summary (fun = geometric.mean, geom = "crossbar", color = "#CB2026", size = 0.5, width = 0.9) +
  stat_pvalue_manual(stat.test_D614G_neuts, hide.ns = TRUE, label = "p.adj.signif", size = 5,remove.bracket = FALSE) +
  annotation_logticks(sides = "l") +  
  theme_bw() + theme(legend.position = "bottom") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = element_blank()) + #no y-axis label for this one!
  labs(x= "Vaccinated") +
  labs(color = "Prior Vaccination") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  theme(axis.text.y = element_blank()) + #no y-axis text for this one!
  ggtitle("BA.1.1") +
  theme(plot.title = element_text(hjust = 0.5)) +
  #theme(axis.text.y = element_blank()) +
  scale_color_manual(values = c("#E26E45", "#47598A")) 
BA11_neuts_for_combining_no_legend


Neut_graphs <- (D614G_neuts_for_combining | BA11_neuts_for_combining_no_legend) +
  plot_annotation(title = 'Neutralization', theme = theme(plot.title = element_text(hjust = 0.55))) 
Neut_graphs


#use ggsave to save final figures as PDFs
#specify the new file name and plot
#set the path using one of the directories we used above- first the figure directory
#set scale = 2 for nice pairing of font size with plot size
#set width and height as desired - might take some optimization

ggsave(
  "Neut_graphs.pdf",
  plot = Neut_graphs,
  device = NULL,
  path = figure_directory,
  scale = 2,
  width = 1.5,
  height = 1.5,
  dpi = 300,
  limitsize = TRUE,)


#do the same ggsave but with the version directory
#you will change version directory each time you make substantial changes to your script
#this will save an archive of old versions of every figure you generated
#the figure directory will never change
#this will allow new figures to instantly replace old figures in your illustrator files

ggsave(
  "Neut_graphs.pdf",
  plot = Neut_graphs,
  device = NULL,
  path = version_directory,
  scale = 2,
  width = 1.5,
  height = 1.5,
  dpi = 300,
  limitsize = TRUE,)


#Now, the moment of truth
#Drag the pdf saved in the figure directory into illustrator
#This creates a linked figure in illustrator, which will use whatever file is in that folder with that name
#If you re-write the file with the same name in the same location, Illustator will automatically update
#If you take the file from the version directory, it will not be re-written when the version changes
#In that case, Illustrator would not update






#Make plots from the second dataframe ----
#I am not walking you through these, just making a few more plots
#This will help demonstrate the value of linking ggsave to illustrator


#plot the longitudinal data with statistics

stat.test_Antibodies_D614G <- Longitudinal_dataframe %>%
  #filter(A2_stain == "Y") %>%
  #group_by(Vaccine) %>%
  wilcox_test(RBD_D614G ~ timepoint_infection_numeric, ref.group = "0") %>%
  adjust_pvalue(method = "BH") %>%
  add_significance() %>%
  add_y_position(fun = "max", step.increase = 0) %>%
  mutate(y.position = y.position * 1.5)
stat.test_Antibodies_D614G

Antibodies_D614G <- ggplot(Longitudinal_dataframe, aes(x = as.factor(timepoint_infection_numeric), y = RBD_D614G)) + 
  #geom_point(pch=16, alpha = 0.7, size = 2, color = "#1887AB") +
  geom_quasirandom(pch=16, alpha = 0.7, size = 1.5, color = "#1887AB", varwidth = TRUE, width = 0.2)+
  geom_line(alpha = 0.3, size = 0.5, color = "#1887AB", aes(group = participant_ID)) + 
  stat_summary (fun = geometric.mean, geom = "crossbar", color = "#182B54", size = 0.5, width = 0.5) +
  scale_y_log10(limits = c(100, 3000)) +
  stat_pvalue_manual(stat.test_Antibodies_D614G, hide.ns = TRUE, label = "p.adj.signif", size = 5,remove.bracket = TRUE) +
  annotation_logticks(sides = "l") +  
  geom_vline(xintercept=1, linetype="dotted", color = "grey50") +
  theme_bw() + theme(legend.position = "bottom") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = "anti-D614G RBD titer") +
  labs(x= "Days post-symptom onset") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  ggtitle("Anti-RBD IgG (D614G)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#1887AB")) 
Antibodies_D614G

#save the plot directly (not using patchwork for this one)
ggsave(
  "Antibodies_D614G.pdf",
  plot = Antibodies_D614G,
  device = NULL,
  path = figure_directory,
  scale = 2,
  width = 1.5,
  height = 1.5,
  dpi = 300,
  limitsize = TRUE,)


ggsave(
  "Antibodies_D614G.pdf",
  plot = Antibodies_D614G,
  device = NULL,
  path = version_directory,
  scale = 2,
  width = 1.5,
  height = 1.5,
  dpi = 300,
  limitsize = TRUE,)


#plot a correlation between D614G binding and neutralizing antibodies
#can run the statistics directly within the ggplot

D614G_neuts_binding <- ggplot(Longitudinal_dataframe, aes(x = RBD_D614G, y = neut_d614g)) +
  geom_smooth(method = lm, size = 1, alpha = 0.2, se = FALSE, color = "#182B54") +
  geom_point(alpha = 0.7, size = 2.5, color = "#1887AB", aes(pch = as.factor(timepoint_infection_numeric))) +
  #geom_quasirandom(pch=21, colour = "black", alpha = 0.8, size = 2, varwidth = TRUE, width = 0.2)+
  #geom_boxplot(alpha = 0.2, width = 0.15, lwd = 0.2, outlier.size = -1, aes(group = months_factor, color = recovered)) +
  stat_cor(method = "spearman",  cor.coef.name = "rho",aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size= 5) +
  scale_y_log10() +
  scale_x_log10() +
  theme_bw() + theme(legend.position = "bottom") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = "anti-D614G RBD IgG") +
  labs(x= "FRNT50 D614G") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  ggtitle("D614G") +
  theme(plot.title = element_text(hjust = 0.5)) 
#facet_wrap(~timepoint_infection_numeric, ncol = 3) +
#theme(strip.background = element_blank())
D614G_neuts_binding

#do the same for BA.1.1

BA11_neuts_binding <- ggplot(Longitudinal_dataframe, aes(x = RBD_BA.1.1, y = neut_BA11)) +
  geom_smooth(method = lm, size = 1, alpha = 0.2, se = FALSE, color = "#CB2026") +
  geom_point(alpha = 0.7, size = 2.5, color = "#1887AB", aes(pch = as.factor(timepoint_infection_numeric))) +
  #geom_quasirandom(pch=21, colour = "black", alpha = 0.8, size = 2, varwidth = TRUE, width = 0.2)+
  #geom_boxplot(alpha = 0.2, width = 0.15, lwd = 0.2, outlier.size = -1, aes(group = months_factor, color = recovered)) +
  stat_cor(method = "spearman",  cor.coef.name = "rho",aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size= 5) +
  scale_y_log10() +
  scale_x_log10() +
  theme_bw() + theme(legend.position = "bottom") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = "anti-BA.1.1 RBD IgG") +
  labs(x= "FRNT50 BA.1.1") +
  guides(pch=guide_legend(title= "timepoint"))+
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  ggtitle("BA.1.1") +
  theme(plot.title = element_text(hjust = 0.5)) 
#facet_wrap(~timepoint_infection_numeric, ncol = 3) +
#theme(strip.background = element_blank())
BA11_neuts_binding


#save using patchwork
neut_binding_correlations <- D614G_neuts_binding | BA11_neuts_binding
neut_binding_correlations


ggsave(
  "neut_binding_correlations.pdf",
  plot = neut_binding_correlations,
  device = NULL,
  path = figure_directory,
  scale = 2,
  width = 4.5,
  height = 1.5,
  dpi = 300,
  limitsize = TRUE,)


ggsave(
  "neut_binding_correlations.pdf",
  plot = neut_binding_correlations,
  device = NULL,
  path = version_directory,
  scale = 2,
  width = 4.5,
  height = 1.5,
  dpi = 300,
  limitsize = TRUE,)



#create a long dataframe to analyze all specificities simultaneously
Longitudinal_dataframe_long <- Longitudinal_dataframe %>%
  rename(D614G = RBD_D614G, BA.5 = RBD_BA.5, BA.1 = RBD_BA.1, BA.1.1 = RBD_BA.1.1) %>%
  gather(variant, titer, -c(1:sex))

Longitudinal_dataframe_long$variant <- factor(Longitudinal_dataframe_long$variant, levels = c("D614G", "BA.1.1", "BA.5"))

#filter out an undesired variant
Longitudinal_dataframe_long_filtered <- Longitudinal_dataframe_long %>%
  filter(variant != "BA.1")


#do statistics grouped by variant
#this tests whether antibodies binding each variant are significantly increasing over time
stat.test_Antibodies_variants <- Longitudinal_dataframe_long_filtered %>%
  group_by(variant) %>%
  wilcox_test(titer ~ timepoint_infection_numeric, ref.group = "0") %>%
  adjust_pvalue(method = "BH") %>%
  add_significance() %>%
  add_y_position(fun = "max", step.increase = 0.1) %>%
  mutate(y.position = y.position * 1.2)
stat.test_Antibodies_variants

Antibodies_variants <- ggplot(Longitudinal_dataframe_long_filtered, aes(x = as.factor(timepoint_infection_numeric), y = titer)) + 
  #geom_point(pch=16, alpha = 0.7, size = 2, color = "#1887AB") +
  geom_quasirandom(pch=16, alpha = 0.7, size = 1.5, color = "#1887AB", varwidth = TRUE, width = 0.2)+
  geom_line(alpha = 0.3, size = 0.5, color = "#1887AB", aes(group = participant_ID)) + 
  stat_summary (fun = geometric.mean, geom = "crossbar", aes(color = variant), size = 0.5, width = 0.75) +
  scale_y_log10(limits = c(100,3000)) +
  stat_pvalue_manual(stat.test_Antibodies_variants, hide.ns = TRUE, label = "p.adj.signif", size = 5,remove.bracket = TRUE) +
  #scale_x_break(c(20, 40), ticklabels = c(40,50), scales = 0.25, space = 0.25)+
  #xlim(-0.5,50)+
  annotation_logticks(sides = "l") +  
  geom_vline(xintercept=0, linetype="dotted", color = "grey50") +
  theme_bw() + theme(legend.position = "bottom") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = "Endpoint Titer") +
  labs(x= "Days post-symptom onset") +
  #labs(fill = "Prior SARS-CoV2+") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  ggtitle(element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  #theme(axis.text.y = element_blank()) +
  #scale_fill_manual(values = c("#3182BD", "firebrick"))+
  scale_color_manual(values = c("#182B54", "#CB2026", "#BF6C9F")) +
  facet_wrap(~variant) + 
  theme(strip.background = element_blank())  
Antibodies_variants

Antibodies_variants_plot <- (Antibodies_variants) +
  plot_annotation(title = 'Anti-RBD IgG by variant', theme = theme(plot.title = element_text(hjust = 0.6))) 
Antibodies_variants_plot

ggsave(
  "Antibodies_variants.pdf",
  plot = Antibodies_variants_plot,
  device = NULL,
  path = figure_directory,
  scale = 2,
  width = 2,
  height = 1.5,
  dpi = 300,
  limitsize = TRUE,)


ggsave(
  "Antibodies_variants.pdf",
  plot = Antibodies_variants_plot,
  device = NULL,
  path = version_directory,
  scale = 2,
  width = 2,
  height = 1.5,
  dpi = 300,
  limitsize = TRUE,)






#new data - automatically update your figures in seconds! ----
#Now we will run the same script as before, but using files containing additional data
#re-make the same plots using the same exact script
#this is a demo, so I have re-pasted the identical script below so we can run in a linear fashion
#usually, I would just re-run the same script but edit the files being imported at the very beginning


wd<-"/Users/markmpainter/Desktop/Gemini" #define working directory
setwd(wd) #set working directory

#establish directories for saving plots
figure_directory <-"/Users/markmpainter/Desktop/Gemini/Exports/Figures" 

#change version directory whenever you have made a substantial change to your plots
#need to create the new folder and change the path name here
version_directory <- "/Users/markmpainter/Desktop/Gemini/Exports/Versions/20230426"  #used to be /20230425

#read in updated dataframes containing newly collected data
Vaccination_dataframe <- read.csv("Gemini.2.1.csv") #new file

Longitudinal_dataframe <- read.csv("Gemini.1.1.csv") #new file




#run statistics
stat.test_D614G_neuts <- Vaccination_dataframe %>% #use the same dataframe and store as a new object
  #filter(infection_count < 2) %>% #filter if necessary (i.e. if subsetting in your ggplot)
  #group_by(Vaccine) %>% #group if necessary (i.e. if fascetting in your ggplot)
  wilcox_test(neut_d614g ~ Vaccine) %>% #wilcoxon test
  adjust_pvalue(method = "BH") %>% #correct for multiple comparisons
  add_significance() %>% #add significance so you can display it on the plot
  add_y_position(fun = "max", step.increase = 0)  %>% #add a y position so it displays where you want
  mutate(y.position = 5) #mutate the y position to set it exactly where you want
stat.test_D614G_neuts #check the statistical test results

##make optimized plots for combining into a figure
D614G_neuts_for_combining <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_d614g)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))+
  #geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) +
  stat_summary (fun = geometric.mean, geom = "crossbar", color = "#182B54", size = 0.5, width = 0.9) +
  stat_pvalue_manual(stat.test_D614G_neuts, hide.ns = TRUE, label = "p.adj.signif", size = 5,remove.bracket = FALSE) +
  annotation_logticks(sides = "l") +  
  theme_bw() + theme(legend.position = "bottom") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = "FRNT50") +
  labs(x= "Vaccinated") +
  labs(color = "Prior Vaccination") +
  theme(legend.position = "none") + #no legend in this one!
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  ggtitle("D614G") +
  theme(plot.title = element_text(hjust = 0.5)) +
  #theme(axis.text.y = element_blank()) +
  scale_color_manual(values = c("#E26E45", "#47598A")) 
D614G_neuts_for_combining



## Copy-paste to make the same plot for BA.1.1 instead of D614G
stat.test_BA11_Neuts <- Vaccination_dataframe %>%
  #filter(infection_count < 2) %>%
  #group_by(Vaccine) %>%
  wilcox_test(neut_BA11 ~ Vaccine) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance() %>%
  add_y_position(fun = "max", step.increase = 0)  %>%
  mutate(y.position = y.position * 3)
stat.test_BA11_Neuts


BA11_neuts_for_combining_no_legend <- ggplot(Vaccination_dataframe, aes(x = Vaccine, y = neut_BA11)) + 
  geom_quasirandom(pch=21, alpha = 0.7, size = 1.5, varwidth = TRUE, width = 0.4, aes(color = Vaccine))+
  #geom_boxplot(alpha = 0.2, width = 0.5, lwd = 0.3, outlier.size = -1, aes(group = Vaccine)) +
  scale_y_log10(limits = c(5,200000)) +
  stat_summary (fun = geometric.mean, geom = "crossbar", color = "#CB2026", size = 0.5, width = 0.9) +
  stat_pvalue_manual(stat.test_D614G_neuts, hide.ns = TRUE, label = "p.adj.signif", size = 5,remove.bracket = FALSE) +
  annotation_logticks(sides = "l") +  
  theme_bw() + theme(legend.position = "bottom") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = element_blank()) + #no y-axis label for this one!
  labs(x= "Vaccinated") +
  labs(color = "Prior Vaccination") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  theme(axis.text.y = element_blank()) + #no y-axis text for this one!
  ggtitle("BA.1.1") +
  theme(plot.title = element_text(hjust = 0.5)) +
  #theme(axis.text.y = element_blank()) +
  scale_color_manual(values = c("#E26E45", "#47598A")) 
BA11_neuts_for_combining_no_legend


#use patchwork to combine the two plots into a single figure
#add an annotation to give the entire figure a shared title
Neut_graphs <- (D614G_neuts_for_combining | BA11_neuts_for_combining_no_legend) +
  plot_annotation(title = 'Neutralization', theme = theme(plot.title = element_text(hjust = 0.55))) 
Neut_graphs

#save plots using ggsave
#saving in the figure directory will now RE-WRITE the original file with the new figure
#this will automatically update in illustrator with the new data
ggsave(
  "Neut_graphs.pdf",
  plot = Neut_graphs,
  device = NULL,
  path = figure_directory,
  scale = 2,
  width = 1.5,
  height = 1.5,
  dpi = 300,
  limitsize = TRUE,)


#saving in the version directory will NOT re-write the original file
#since the directory has changed, you create a new version while retaining the old version
#this allows you to go back to your original figure in case something is ruined in your script
#hopefully this is never an issue, but it is good practice to save your version history!

ggsave(
  "Neut_graphs.pdf",
  plot = Neut_graphs,
  device = NULL,
  path = version_directory,
  scale = 2,
  width = 1.5,
  height = 1.5,
  dpi = 300,
  limitsize = TRUE,)


##second dataframe 

stat.test_Antibodies_D614G <- Longitudinal_dataframe %>%
  #filter(A2_stain == "Y") %>%
  #group_by(Vaccine) %>%
  wilcox_test(RBD_D614G ~ timepoint_infection_numeric, ref.group = "0") %>%
  adjust_pvalue(method = "BH") %>%
  add_significance() %>%
  add_y_position(fun = "max", step.increase = 0) %>%
  mutate(y.position = y.position * 1.5)
stat.test_Antibodies_D614G

Antibodies_D614G <- ggplot(Longitudinal_dataframe, aes(x = as.factor(timepoint_infection_numeric), y = RBD_D614G)) + 
  #geom_point(pch=16, alpha = 0.7, size = 2, color = "#1887AB") +
  geom_quasirandom(pch=16, alpha = 0.7, size = 1.5, color = "#1887AB", varwidth = TRUE, width = 0.2)+
  geom_line(alpha = 0.3, size = 0.5, color = "#1887AB", aes(group = participant_ID)) + 
  stat_summary (fun = geometric.mean, geom = "crossbar", color = "#182B54", size = 0.5, width = 0.5) +
  scale_y_log10(limits = c(100, 3000)) +
  stat_pvalue_manual(stat.test_Antibodies_D614G, hide.ns = TRUE, label = "p.adj.signif", size = 5,remove.bracket = TRUE) +
  annotation_logticks(sides = "l") +  
  geom_vline(xintercept=1, linetype="dotted", color = "grey50") +
  theme_bw() + theme(legend.position = "bottom") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = "anti-D614G RBD titer") +
  labs(x= "Days post-symptom onset") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  ggtitle("Anti-RBD IgG (D614G)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("#1887AB")) 
Antibodies_D614G


ggsave(
  "Antibodies_D614G.pdf",
  plot = Antibodies_D614G,
  device = NULL,
  path = figure_directory,
  scale = 2,
  width = 1.5,
  height = 1.5,
  dpi = 300,
  limitsize = TRUE,)


ggsave(
  "Antibodies_D614G.pdf",
  plot = Antibodies_D614G,
  device = NULL,
  path = version_directory,
  scale = 2,
  width = 1.5,
  height = 1.5,
  dpi = 300,
  limitsize = TRUE,)



D614G_neuts_binding <- ggplot(Longitudinal_dataframe, aes(x = RBD_D614G, y = neut_d614g)) +
  geom_smooth(method = lm, size = 1, alpha = 0.2, se = FALSE, color = "#182B54") +
  geom_point(alpha = 0.7, size = 2.5, color = "#1887AB", aes(pch = as.factor(timepoint_infection_numeric))) +
  #geom_quasirandom(pch=21, colour = "black", alpha = 0.8, size = 2, varwidth = TRUE, width = 0.2)+
  #geom_boxplot(alpha = 0.2, width = 0.15, lwd = 0.2, outlier.size = -1, aes(group = months_factor, color = recovered)) +
  stat_cor(method = "spearman",  cor.coef.name = "rho",aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size= 5) +
  scale_y_log10() +
  scale_x_log10() +
  theme_bw() + theme(legend.position = "bottom") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = "anti-D614G RBD IgG") +
  labs(x= "FRNT50 D614G") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  ggtitle("D614G") +
  theme(plot.title = element_text(hjust = 0.5)) 
#facet_wrap(~timepoint_infection_numeric, ncol = 3) +
#theme(strip.background = element_blank())
D614G_neuts_binding


BA11_neuts_binding <- ggplot(Longitudinal_dataframe, aes(x = RBD_BA.1.1, y = neut_BA11)) +
  geom_smooth(method = lm, size = 1, alpha = 0.2, se = FALSE, color = "#CB2026") +
  geom_point(alpha = 0.7, size = 2.5, color = "#1887AB", aes(pch = as.factor(timepoint_infection_numeric))) +
  #geom_quasirandom(pch=21, colour = "black", alpha = 0.8, size = 2, varwidth = TRUE, width = 0.2)+
  #geom_boxplot(alpha = 0.2, width = 0.15, lwd = 0.2, outlier.size = -1, aes(group = months_factor, color = recovered)) +
  stat_cor(method = "spearman",  cor.coef.name = "rho",aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),size= 5) +
  scale_y_log10() +
  scale_x_log10() +
  theme_bw() + theme(legend.position = "bottom") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = "anti-BA.1.1 RBD IgG") +
  labs(x= "FRNT50 BA.1.1") +
  guides(pch=guide_legend(title= "timepoint"))+
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  ggtitle("BA.1.1") +
  theme(plot.title = element_text(hjust = 0.5)) 
#facet_wrap(~timepoint_infection_numeric, ncol = 3) +
#theme(strip.background = element_blank())
BA11_neuts_binding

neut_binding_correlations <- D614G_neuts_binding | BA11_neuts_binding
neut_binding_correlations


ggsave(
  "neut_binding_correlations.pdf",
  plot = neut_binding_correlations,
  device = NULL,
  path = figure_directory,
  scale = 2,
  width = 4.5,
  height = 1.5,
  dpi = 300,
  limitsize = TRUE,)


ggsave(
  "neut_binding_correlations.pdf",
  plot = neut_binding_correlations,
  device = NULL,
  path = version_directory,
  scale = 2,
  width = 4.5,
  height = 1.5,
  dpi = 300,
  limitsize = TRUE,)











Longitudinal_dataframe_long <- Longitudinal_dataframe %>%
  rename(D614G = RBD_D614G, BA.5 = RBD_BA.5, BA.1 = RBD_BA.1, BA.1.1 = RBD_BA.1.1) %>%
  gather(variant, titer, -c(1:sex))

Longitudinal_dataframe_long$variant <- factor(Longitudinal_dataframe_long$variant, levels = c("D614G", "BA.1.1", "BA.5"))


Longitudinal_dataframe_long_filtered <- Longitudinal_dataframe_long %>%
  filter(variant != "BA.1")



stat.test_Antibodies_variants <- Longitudinal_dataframe_long_filtered %>%
  group_by(variant) %>%
  wilcox_test(titer ~ timepoint_infection_numeric, ref.group = "0") %>%
  adjust_pvalue(method = "BH") %>%
  add_significance() %>%
  add_y_position(fun = "max", step.increase = 0.1) %>%
  mutate(y.position = y.position * 1.2)
stat.test_Antibodies_variants

Antibodies_variants <- ggplot(Longitudinal_dataframe_long_filtered, aes(x = as.factor(timepoint_infection_numeric), y = titer)) + 
  #geom_point(pch=16, alpha = 0.7, size = 2, color = "#1887AB") +
  geom_quasirandom(pch=16, alpha = 0.7, size = 1.5, color = "#1887AB", varwidth = TRUE, width = 0.2)+
  geom_line(alpha = 0.3, size = 0.5, color = "#1887AB", aes(group = participant_ID)) + 
  stat_summary (fun = geometric.mean, geom = "crossbar", aes(color = variant), size = 0.5, width = 0.75) +
  scale_y_log10(limits = c(100,3000)) +
  stat_pvalue_manual(stat.test_Antibodies_variants, hide.ns = TRUE, label = "p.adj.signif", size = 5,remove.bracket = TRUE) +
  #scale_x_break(c(20, 40), ticklabels = c(40,50), scales = 0.25, space = 0.25)+
  #xlim(-0.5,50)+
  annotation_logticks(sides = "l") +  
  geom_vline(xintercept=0, linetype="dotted", color = "grey50") +
  theme_bw() + theme(legend.position = "bottom") +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(y = "Endpoint Titer") +
  labs(x= "Days post-symptom onset") +
  #labs(fill = "Prior SARS-CoV2+") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + 
  ggtitle(element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  #theme(axis.text.y = element_blank()) +
  #scale_fill_manual(values = c("#3182BD", "firebrick"))+
  scale_color_manual(values = c("#182B54", "#CB2026", "#BF6C9F")) +
  facet_wrap(~variant) + 
  theme(strip.background = element_blank())  
Antibodies_variants

Antibodies_variants_plot <- (Antibodies_variants) +
  plot_annotation(title = 'Anti-RBD IgG by variant', theme = theme(plot.title = element_text(hjust = 0.6))) 
Antibodies_variants_plot

ggsave(
  "Antibodies_variants.pdf",
  plot = Antibodies_variants_plot,
  device = NULL,
  path = figure_directory,
  scale = 2,
  width = 2,
  height = 1.5,
  dpi = 300,
  limitsize = TRUE,)


ggsave(
  "Antibodies_variants.pdf",
  plot = Antibodies_variants_plot,
  device = NULL,
  path = version_directory,
  scale = 2,
  width = 2,
  height = 1.5,
  dpi = 300,
  limitsize = TRUE,)




