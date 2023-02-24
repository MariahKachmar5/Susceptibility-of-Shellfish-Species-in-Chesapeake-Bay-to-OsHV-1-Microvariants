setwd("~/Documents/UMBC/MDSG /Vector Study")

library(readxl)


########## WATER #############

Water<- read_xlsx("~/Documents/UMBC/MDSG /Vector Study/WaterSamplesVS.xlsx")
Water


Water2<- na.omit(qPCR)
Water2


### log transform data ###

qPCR2$LogData <- log10(qPCR2$Copies.mL+1)

head(qPCR2)

library(Rmisc)
library(ggplot2)

qPCR_sum <- summarySE(Water2, "LogData", groupvars=c("Species", "Virus"))
qPCR_sum

qPCR_sum_raw <- summarySE(Water2, "Copies.mL", groupvars=c("Species", "Virus"))
View(qPCR_sum_raw)

qPCR_sum2 <- qPCR_sum

qPCR_sum2$Exposure <-factor(qPCR_sum2$Exposure)

qPCR_sum2

VS_Water_plot <- ggplot(qPCR_sum2, aes(Virus, LogData, fill = Species)) + geom_bar(position=position_dodge(), stat="identity") + 
  theme(axis.text.x = element_text(angle = 0)) + 
  labs(y= "Mean OsHV-1 Copies/mg Tissue across all doses", x = "Viral Injection")+
  scale_fill_manual(values=c("gray82", "gray16", "grey40")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1),
        strip.background = element_rect(color= NA, fill= NA),
        axis.text=element_text(size=12), axis.title=element_text(size=15,face="bold"), legend.text=element_text(face="italic")) +          # Remove the background, grid lines, and add a boarder
  labs(y = "Log 10 OsHV-1 copies per mL of water") + ylim(0,10) +
  geom_errorbar(aes(ymin=LogData-se, ymax=LogData+se),width=.2, position=position_dodge(.9))
  

VS_Water_plot


#### Mean copies/mg of tissue ######
transch_plot2 <- ggplot(qPCR_sum_raw, aes(Virus.Status, copies.mg, fill = Species)) + geom_bar(position=position_dodge(), stat="identity") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(y= "Mean OsHV-1 Copies/mg Tissue across all doses", x = NULL, title = "Transmission Challenge")+
  ylim(-1, 1.5e+08) + 
  geom_errorbar(aes(ymin=copies.mg-se, ymax=copies.mg+se),width=.2, position=position_dodge(.9))
transch_plot2


############ PLATES ##################


qPCR_p<- read_xlsx("~/Documents/UMBC/MDSG /Vector Study/qPCR_VS.xlsx")
qPCR_p

View(qPCR_p)
qPCR_p2<- na.omit(qPCR_p)
qPCR_p2


### log transform data ###

qPCR_p2$LogData <- log10(qPCR_p2$Copies.mL+1)

head(qPCR_p2)

View(qPCR_p2)
library(Rmisc)
library(ggplot2)

qPCR_sum1 <- summarySE(qPCR_p2, "LogData", groupvars=c("Exposure", "Virus"))
qPCR_sum1

qPCR_sum_raw1 <- summarySE(qPCR_p2, "Copies.mL", groupvars=c("Exposure", "Virus"))
View(qPCR_sum_raw1)

qPCR_sum3 <- qPCR_sum1

qPCR_sum3$Exposure <-factor(qPCR_sum3$Exposure)

qPCR_sum3

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

VS_plot1 <- ggplot(qPCR_sum3, aes(Virus, LogData, fill = Exposure)) + geom_bar(position=position_dodge(), stat="identity") + 
  theme(axis.text.x = element_text(angle = 0)) + 
  labs(y= "Mean OsHV-1 Copies/mg Tissue", x = "Expsoure")+
  scale_fill_manual(values=c("gray82", "gray16", "grey40")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1),
        strip.background = element_rect(color= NA, fill= NA),
        axis.text=element_text(size=12), axis.title=element_text(size=15,face="bold"), legend.text=element_text(face="italic", size=12)) +          # Remove the background, grid lines, and add a boarder
  labs(y = "Log 10 OsHV-1 copies per mg of tissue") + ylim(0,10) +
  geom_errorbar(aes(ymin=LogData-se, ymax=LogData+se),width=.2, position=position_dodge(.9))

VS_plot1

##### Non parametric Plates ########

library(readr)

Tissue <- read_xlsx("~/Documents/UMBC/MDSG /Vector Study/qPCR_VS_stats.xlsx")
Tissue

Tissue$Virus <- factor(Tissue$Virus, labels = c("SD", "FRA")) 
Tissue$Exposure <- factor(Tissue$Exposure, labels = c("C. gigas", "C. virginica", "M. mercenaria"))
Tissue$Status <- factor(Tissue$Status, labels = c("CON", "SURV", "MORT"))
                       

View(Tissue)

Tissue$LogData <- log10(Tissue$Copies.mL+1)
head(Tissue)

#H: Is there a significant difference in viral copies between viruses?
Virus <- kruskal.test(LogData ~ Virus, data = Tissue)
Virus

#results: Kruskal-Wallis chi-squared = 0.41893, df = 1, p-value = 0.5175
#Virus is not significant

#H: Is there a significant difference in viral copies between exposures?
Exposure <- kruskal.test(LogData ~ Exposure, data = Tissue)
Exposure
##Kruskal-Wallis chi-squared = 149.75, df = 2, p-value < 2.2e-16
##Exposure is significantly different

#H: Is there a significant difference in viral copies between status - suvivior , mortality, control?
Status <- kruskal.test(LogData ~ Status, data = Tissue)
Status
##Kruskal-Wallis chi-squared = 217.08, df = 2, p-value < 2.2e-16
##Status is significant 


### Need to look at this more ###
source("http://www.statmethods.net/RiA/wmc.txt")

wmc(LogData ~Virus, data=Tissue, method="holm")
wmc(LogData ~Exposure, data=Tissue, method="holm")
wmc(LogData ~Status, data=Tissue, method="holm")


