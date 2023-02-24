setwd("~/Documents/UMBC/MDSG /qPCR data TransCh")

library(readxl)

qPCR<- read_xlsx("~/Documents/UMBC/MDSG /qPCR data TransCh/qPCR_Transmission2.xlsx")
qPCR

View(qPCR)

qPCR2<- na.omit(qPCR)
qPCR2

##### GRAPH ####

### log transform data ###

qPCR2$LogData <- log10(qPCR2$copies.mg+1)

head(qPCR2)
View(qPCR2)

library(Rmisc)
library(ggplot2)

qPCR_sum <- summarySE(qPCR2, "LogData", groupvars=c("Species", "Virus.Status"))
View(qPCR_sum)


qPCR_sum_raw <- summarySE(qPCR2, "copies.mg", groupvars=c("Species", "Virus.Status"))
View(qPCR_sum_raw)

qPCR_sum2 <- qPCR_sum

qPCR_sum2$Exposure <-factor(qPCR_sum2$Exposure)

qPCR_sum2

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


transch_plot <- ggplot(qPCR_sum2, aes(Virus.Status, LogData, fill = Species)) + geom_bar(position=position_dodge(), stat="identity") + 
  theme(axis.text.x = element_text(angle = 0)) + 
  labs(y= "Mean OsHV-1 Copies/mg Tissue across all doses", x = "Exposure")+
  scale_fill_manual(values=c ("grey83","black", "grey60")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1),
        strip.background = element_rect(color= NA, fill= NA),
        axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"), legend.text = element_text(face='italic')) +          # Remove the background, grid lines, and add a boarder
  labs(y = "Log 10 OsHV-1 copies per mg of tissue") + ylim(0,10) +
  geom_errorbar(aes(ymin=LogData-se, ymax=LogData+se),width=.2, position=position_dodge(.9))+
  theme(axis.text = element_text(size = 13),text = element_text(size = 15),axis.title = element_text(size = 20))
  

transch_plot


#### Mean copies/mg of tissue ######
transch_plot2 <- ggplot(qPCR_sum_raw, aes(Virus.Status, copies.mg, fill = Species)) + geom_bar(position=position_dodge(), stat="identity") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(y= "Mean OsHV-1 Copies/mg Tissue across all doses", x = NULL, title = "Transmission Challenge")+
  ylim(-1, 1.5e+08) + 
  geom_errorbar(aes(ymin=copies.mg-se, ymax=copies.mg+se),width=.2, position=position_dodge(.9))
transch_plot2



##### STATS - Nonparametric ########

library(readr)

Tissue <- read_xlsx("~/Documents/UMBC/MDSG /qPCR data TransCh/qPCR_Transmission_Stats.xlsx")
Tissue

Tissue$Virus <- factor(Tissue$Virus) 
Tissue$Species <- factor(Tissue$Species)
Tissue$Status <- factor(Tissue$Status)
Tissue$Dose <- factor(Tissue$Dose)

View(Tissue)

Tissue$LogData <- log10(Tissue$copies.mg+1)
head(Tissue)

#H: Is there a significant difference in viral copies between viruses?
Virus <- kruskal.test(LogData ~ Virus, data = Tissue)
Virus
#Kruskal-Wallis chi-squared = 0.0016461, df = 1, p-value = 0.9676
#Not significant


#H: Is there a significant difference in viral copies between speciess?
Species <- kruskal.test(LogData ~ Species, data = Tissue)
Species
#Kruskal-Wallis chi-squared = 98.675, df = 2, p-value < 2.2e-16
# Species is significant

#H: Is there a significant difference in viral copies between status - survivior , mortality, control?
Status <- kruskal.test(LogData ~ Status, data = Tissue)
Status
#Kruskal-Wallis chi-squared = 130.59, df = 3, p-value < 2.2e-16
#status is significant

#H: Is there a significant difference in viral copies between doses?
Dose <- kruskal.test(LogData ~ Dose, data = Tissue)
Dose
#Kruskal-Wallis chi-squared = 71.274, df = 3, p-value = 2.277e-15
#Dose is significant


### Need to look at this more ###
source("http://www.statmethods.net/RiA/wmc.txt")

wmc(LogData ~Virus, data=Tissue, method="holm")

wmc(LogData ~Species, data=Tissue, method="holm")

wmc(LogData ~Dose, data=Tissue, method="holm")


