setwd("/Volumes/MKFILES/UMBC/MDSG /KaplanMeier")


# Load required packages

library(readxl)
library(survminer)
library(survival)
library(ggpubr)


# Upload data

FRA_TransChall <- read_excel("FRA_TransChall_KaplanMeier.xlsx")

Master <- FRA_TransChall

# All species, concentrations, FRA variant and control

# Species Labels
# CV = Eastern Oyster (NJ)
# CgB = Pacifics (BML)
# HC = Hard Clams
# CgH = Pacifics (Hog Island)
# B = Bay Scallops


Master$Species <- as.factor(Master$Species)
class(Master$Species)

########################## EASTERN OYSTER LD50 ########################################

Master_Easterns_NJ <- Master[Master$Species == "Easterns(NJ)", ]

Master_Easterns_NJ

## Create a survival object and plot
class(Master_Easterns_NJ$Day)
Master_Easterns_NJ$Day <- as.numeric(Master_Easterns_NJ$Day)

class(Master_Easterns_NJ$Dilution)
Master_Easterns_NJ$Dilution <- as.factor(Master_Easterns_NJ$Dilution)

Surv1 <- Surv(time = Master_Easterns_NJ$Day, event = Master_Easterns_NJ$Status)
fit.surv1 <- survfit(formula = Surv1 ~ Dilution, data = Master_Easterns_NJ)
CVplot<-ggsurvplot(fit.surv1, data = Master_Easterns_NJ, xlim = c(3,7), break.x.by = 1,
           linetype= c(1, 2, 3, 4),xlab = NULL, ylab= NULL, legend.labs= c("Control", "1x10^4",
                                                                 "1x10^5", "1x10^6"),
           legend.title = "Treatment:", legend = "top", risk.table =TRUE )+ theme_survminer(base_size = 20,font.legend = c(15))
CVplot
CVplot + labs(title= "Eastern Oyster FRA")




########################## HARD CLAM LD50 ########################################

Master_HardClams <- Master[Master$Species == "Hard Clams", ]

Master_HardClams


## Create a survival object and plot
class(Master_HardClams$Day)
Master_HardClams$Day <- as.numeric(Master_HardClams$Day)

class(Master_HardClams$Dilution)
Master_HardClams$Dilution <- as.factor(Master_HardClams$Dilution)

Surv2 <- Surv(time = Master_HardClams$Day, event = Master_HardClams$Status)
fit.surv2 <- survfit(formula = Surv2 ~ Dilution, data = Master_HardClams)
HCplot <- ggsurvplot(fit.surv2, data = Master_HardClams,  xlim = c(3,7), break.x.by = 1,
           linetype= c(1, 2, 3, 4),xlab = NULL, ylab= NULL, legend.labs= c("Control", "1x10^4",
                                                                "1x10^5", "1x10^6"),
           legend.title = "Treatment:", legend = "top", risk.table = TRUE)+ theme_survminer(base_size = 20,font.legend = c(15))
HCplot
HCplot + labs(title= "Hard Clam FRA")




########################## Pacifics(BML) LD50 ########################################

Master_Pacifics_BML <- Master[Master$Species == "Pacifics(BML)", ]

Master_Pacifics_BML

## Create a survival object and plot
class(Master_Pacifics_BML$Day)
Master_Pacifics_BML$Day <- as.numeric(Master_Pacifics_BML$Day)

class(Master_Pacifics_BML$Dilution)
Master_Pacifics_BML$Dilution <- as.factor(Master_Pacifics_BML$Dilution)

Surv3 <- Surv(time = Master_Pacifics_BML$Day, event = Master_Pacifics_BML$Status)
fit.surv3 <- survfit(formula = Surv3 ~ Dilution, data = Master_Pacifics_BML)
CgBplot <- ggsurvplot(fit.surv3, data = Master_Pacifics_BML,  xlim = c(3,7), break.x.by = 1,
                     linetype= c(1, 2, 3, 4),xlab = NULL, ylab= NULL, legend.labs= c("Control", "1x10^4",
                                                                          "1x10^5", "1x10^6"),
                     legend.title = "Treatment:", legend = "top", risk.table = TRUE)+ theme_survminer(base_size = 20,font.legend = c(15))
CgBplot
CgBplot + labs(title= "Pacifics(BML) FRA")


################# GRID LD50 ##################
gall<- ggarrange(CgBplot$plot, CVplot$plot, HCplot$plot,
                  ncol = 3, nrow = 1, common.legend = TRUE )
gall

annotate_figure(gall, left = text_grob("Survival Probability", color = "black",
                                       rot = 90, size = 15),
                bottom = text_grob("Day", color = "black", size = 15))




#################### Pacifics (HI) and Bay Scallops exposures ####################
Master_Exposures <- Master[Master$Species == "Pacifics(HI)"|Master$Species == "Bay Scallops"|Master$Species == "C Pacifics(HI)"|Master$Species == "C Bay Scallops",]

Master_Exposures


## Create a survival object and plot
class(Master_Exposures$Day)
Master_Exposures$Day <- as.numeric(Master_Exposures$Day)

class(Master_Exposures$Dilution)
Master_Easterns_NJ$Dilution <- as.factor(Master_Exposures$Dilution)

Surv4 <- Surv(time = Master_Exposures$Day, event = Master_Exposures$Status)
fit.surv4 <- survfit(formula = Surv4 ~ Species, data = Master_Exposures)

Exp<-ggsurvplot(fit.surv4, data = Master_Exposures, xlim = c(3,7), break.x.by = 1,
                   linetype= c(1, 2, 3, 4),xlab = "Day", legend.labs= c("A.irradians", "A. irradians Control",
                                                                        "C. gigas (HI)", "C.gigas(HI) Control"),
                legend.title = "Species:", legend = "right" )
Exp

Exp + labs(title= "FRA Exposures")



###### SD exposure #########
# Upload data

SD_TransChall <- read_excel("SD_TransChall_KaplanMeier.xlsx")

Master2 <- SD_TransChall



# All species, concentrations, FRA variant and control

# Species Labels
# CV = Eastern Oyster (NJ)
# CgB = Pacifics (BML)
# HC = Hard Clams
# CgH = Pacifics (Hog Island)
# B = Bay Scallops


Master2$Species <- as.factor(Master2$Species)

class(Master2$Species)

########################## EASTERN OYSTER LD50 ########################################

Master2_Easterns_NJ <- Master2[Master2$Species == "Easterns(NJ)", ]

Master2_Easterns_NJ


## Create a survival object and plot
class(Master2_Easterns_NJ$Day)
Master2_Easterns_NJ$Day <- as.numeric(Master2_Easterns_NJ$Day)

class(Master2_Easterns_NJ$Dilution)
Master2_Easterns_NJ$Dilution <- as.factor(Master2_Easterns_NJ$Dilution)

Surv4 <- Surv(time = Master2_Easterns_NJ$Day, event = Master2_Easterns_NJ$Status)
fit.surv4 <- survfit(formula = Surv4 ~ Dilution, data = Master2_Easterns_NJ)
CVplot2 <-ggsurvplot(fit.surv4, data = Master2_Easterns_NJ, xlim = c(3,7), break.x.by = 1,
                    linetype= c(1, 2, 3, 4),xlab = NULL, ylab= NULL, legend.labs= c("Control", "1x10^4",
                                                                                    "1x10^5", "1x10^6"),
                    legend.title = "Treatment:", legend = "top", risk.table = TRUE ) + theme_survminer(base_size = 20,font.legend = c(15))
CVplot2
CVplot2 + labs(title= "Eastern Oyster SD")




########################## HARD CLAM LD50 ########################################

Master_HardClams2 <- Master2[Master2$Species == "Hard Clams", ]

Master_HardClams2


## Create a survival object and plot
class(Master_HardClams2$Day)
Master_HardClams2$Day <- as.numeric(Master_HardClams2$Day)

class(Master_HardClams2$Dilution)
Master_HardClams2$Dilution <- as.factor(Master_HardClams2$Dilution)

Surv5 <- Surv(time = Master_HardClams2$Day, event = Master_HardClams2$Status)
fit.surv5 <- survfit(formula = Surv5 ~ Dilution, data = Master_HardClams2)
HCplot2 <- ggsurvplot(fit.surv5, data = Master_HardClams2,  xlim = c(3,7), break.x.by = 1,
                     linetype= c(1, 2, 3, 4),xlab = NULL, ylab= NULL, legend.labs= c("Control", "1x10^4",
                                                                                     "1x10^5", "1x10^6"),
                     legend.title = "Treatment:", legend = "top", risk.table = TRUE) + theme_survminer(base_size = 20,font.legend = c(15))
HCplot2
HCplot2 + labs(title= "Hard Clam SD")




########################## Pacifics(BML) LD50 ########################################

Master_Pacifics_BML2 <- Master2[Master2$Species == "Pacifics(BML)", ]

Master_Pacifics_BML2


## Create a survival object and plot
class(Master_Pacifics_BML2$Day)
Master_Pacifics_BML2$Day <- as.numeric(Master_Pacifics_BML2$Day)

class(Master_Pacifics_BML2$Dilution)
Master_Pacifics_BML2$Dilution <- as.factor(Master_Pacifics_BML2$Dilution)

Surv6 <- Surv(time = Master_Pacifics_BML2$Day, event = Master_Pacifics_BML2$Status)
fit.surv6 <- survfit(formula = Surv6 ~ Dilution, data = Master_Pacifics_BML2)
CgBplot2 <- ggsurvplot(fit.surv6, data = Master_Pacifics_BML2,  xlim = c(3,7), break.x.by = 1,
                      linetype= c(1, 2, 3, 4),xlab = NULL, ylab= NULL, legend.labs= c("Control", "1x10^4",
                                                                                      "1x10^5", "1x10^6"),
                      legend.title = "Treatment:", legend = "top", risk.table = TRUE)+ theme_survminer(base_size = 20,font.legend = c(15))

CgBplot2
CgBplot2 + labs(title= "Pacifics(BML) SD")


################# GRID LD50 ##################
gall<- ggarrange(CgBplot2$plot, CVplot2$plot, HCplot2$plot,CgBplot$plot, CVplot$plot,
                 HCplot$plot,ncol = 3,nrow = 2, common.legend = TRUE,
                 labels = c("a)", "b)", "c)", "d)", "e)", "f)"), font.label = list(size= 12, face="bold"))

gall

annotate_figure(gall, left = text_grob("Survival Probability", color = "black",
                                       rot = 90, size = 15,face= "bold"),
                bottom = text_grob("Day", color = "black",face= "bold", size = 15))




