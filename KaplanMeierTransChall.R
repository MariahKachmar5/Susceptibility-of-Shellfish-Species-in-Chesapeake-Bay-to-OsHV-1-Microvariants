setwd("/Volumes/MKFILES/UMBC/MDSG /KaplanMeier")

# Load required packages

library(readxl)
library(survminer)
library(survival)
library(ggpubr)


# Upload data

TransChall_All <- read_excel("TransChall_All_KaplanMeier.xlsx")

View(TransChall_All)
Master <- TransChall_All

MasterLD50 <- Master[Master$Species == "Easterns(NJ)"| Master$Species == "Pacifics(BML)"|
                       Master$Species == "Hard Clams" ,]
View(MasterLD50)
View(Master)

####################### Data for all species ################################
### Pariwise comparisons using Log Rank by species ###
### H: There is a signicicant difference between species. ###
pc50sp <- pairwise_survdiff(Surv(Day, Status) ~ Species, data = MasterLD50)
pc50sp

surv_diff_spp <- survdiff(Surv(Day, Status) ~ Species, data = MasterLD50)
surv_diff_spp

### Pairwise comparisons using log rank by virus ###
### H: There is a significant difference between virus ###
pc50virus <- pairwise_survdiff(Surv(Day, Status) ~ Virus, data = MasterLD50)
pc50virus

surv_diff_spp <- survdiff(Surv(Day, Status) ~ Virus, data = MasterLD50)
surv_diff_spp

### pairwise comparisons using log rank by Dilution ###
### There is a significant difference between dose. ###
pc50dil <- pairwise_survdiff(Surv(Day, Status) ~ Dilution, data = MasterLD50)
pc50dil

surv_diff_spp <- survdiff(Surv(Day, Status) ~ Dilution, data = MasterLD50)
surv_diff_spp

### Pairwise comparison by species, virus, and dilution ###
### H: There is a significant difference between species + virus + dose.###
pc50svd <- pairwise_survdiff(Surv(Day, Status) ~ Species + Virus + Dilution, data = MasterLD50)
pc50svd

surv_diff_spp <- survdiff(Surv(Day, Status) ~ Dilution + Virus + Species, data = MasterLD50)
surv_diff_spp

## create a survival object ##
Surv50 <- Surv(time = MasterLD50$Day, event = MasterLD50$Status)
fit.surv50 <- survfit(formula = Surv50 ~ Virus, data = MasterLD50)

## Get Chi square values for survival ##
## H: There is a significant difference in survival between virus. ##
cs50 <- survdiff(Surv(Day, Status) ~ Virus, data = MasterLD50)
cs50
##H: There is a significant difference between survival of virus combined with dose.##
csLD50 <- survdiff(Surv(Day, Status) ~ Dilution + Virus, data = MasterLD50)
csLD50


## Survival probability for all of the species by species, virus, and dose
summary(survfit(Surv(Day, Status) ~ Virus, data = MasterLD50), times = 7)
summary(survfit(Surv(Day, Status) ~ Species, data = MasterLD50), times = 7)
summary(survfit(Surv(Day, Status) ~ Dilution, data = MasterLD50), times = 7)


######################## Survival probablility for all specicies ###############
########################### exposed to SD & FRA ##############################

## Subset the data

MasterFRA_SD_21 <- MasterLD50[MasterLD50$Virus == "FRA"|MasterLD50$Virus == "SD",]

summary(survfit(Surv(Day, Status) ~ Species + Virus, data = MasterFRA_SD_21), times = 7)

## H: There is a significant difference in survival between species. ##
pcfrasdsp <- pairwise_survdiff(Surv(Day, Status) ~ Species, data = MasterFRA_SD_21)
pcfrasdsp

### coxph values ###

MasterLD50$Virus <- factor(MasterLD50$Virus, levels = c("CON", "FRA", "SD"),
                         labels = c("Control", "FRA µvar", "SD µvar"))
MasterLD50$Dilution <- factor(MasterLD50$Dilution, levels= c("1x10^0","1x10^4","1x10^5","1x10^6"),
                              )

## H: Both FRA + SD had a significant effect on survival ##
fit.coxph <- coxph(Surv50 ~ Virus, data = MasterLD50)
fit.coxph
ggforest(fit.coxph, data = MasterLD50, fontsize = 1.5)

## H: Dilution/Dose had a significant effect on surival. ##
fit.coxph2 <- coxph(Surv50 ~ Dilution, data = MasterLD50)
fit.coxph2
ggforest(fit.coxph2, data= MasterLD50, fontsize= 1.5)


## NOTE: Results show that the higher the dose the higher the death and lower survival

################ data for species by virus and dilution #################

## Subset individual species and remove the controls

Master$Virus <- factor(Master$Virus, levels = c("CON","SD", "FRA"))

Easterns <- Master[Master$Species =='Easterns(NJ)',]
HardClam <- Master[Master$Species =='Hard Clams',]
PacificBML <- Master[Master$Species == 'Pacifics(BML)',]

### Easterns ####

## H: There is a significant difference between viral dose on the species. ##
pcEasterns <- pairwise_survdiff(Surv(Day, Status) ~ Virus + Dilution, data = Easterns)
pcEasterns

## H: Virus + Dilution have significant effect on surival
csEasterns <- survdiff(Surv(Day, Status) ~ Virus + Dilution, data = Easterns)
csEasterns

## Surival
EasternsSurv <- Surv(time = Easterns$Day, event = Easterns$Status)
fit.Easterns <- survfit(formula = EasternsSurv ~ Virus + Dilution, data = Easterns)
summary(survfit(Surv(Day, Status) ~ Virus + Dilution, data = Easterns), times = 7)

### Hard Clams ###

## H: There is a significant difference between viral dose on the species. ##
pcHardClam <- pairwise_survdiff(Surv(Day, Status) ~ Virus + Dilution, data = HardClam)
pcHardClam
## H: Virus + Dilution have significant effect on surival
csHardClam <- survdiff(Surv(Day, Status) ~ Virus + Dilution, data = HardClam)
csHardClam
## Surival
HardClamSurv <- Surv(time = HardClam$Day, event = HardClam$Status)
fit.HardClam <- survfit(formula = HardClamSurv ~ Virus + Dilution, data = HardClam)
summary(survfit(Surv(Day, Status) ~ Virus + Dilution, data = HardClam), times = 7)

### Pacifics (BML) ###

## H: There is a significant difference between viral dose on the species. ##
pcPacificBML <- pairwise_survdiff(Surv(Day, Status) ~ Virus+ Dilution, data = PacificBML)
pcPacificBML

## H: Virus + Dilution have significant effect on surival
csPacificBML <- survdiff(Surv(Day, Status) ~ Virus + Dilution, data = PacificBML)
csPacificBML

## Surival
PacificBMLSurv <- Surv(time = PacificBML$Day, event = PacificBML$Status)
fit.PacificBML <- survfit(formula = PacificBMLSurv ~ Virus + Dilution, data = PacificBML)
summary(survfit(Surv(Day, Status) ~ Virus + Dilution, data = PacificBML), times = 7)



##### Pacific oysters to determine diff between Virus & Dose  ######

pcPacific <- pairwise_survdiff(Surv(Day, Status) ~ Virus, data = PacificBML)
pcPacific

surv_diff_Pacific <- survdiff(Surv(Day, Status) ~ Virus, data = PacificBML)
surv_diff_Pacific

### pairwise comparisons using log rank by Dilution ###
### There is a significant difference between dose. ###
pcPacificdil <- pairwise_survdiff(Surv(Day, Status) ~ Dilution, data = PacificBML)
pcPacificdil

surv_diff_PD<- survdiff(Surv(Day, Status) ~ Dilution, data = PacificBML)
surv_diff_PD




