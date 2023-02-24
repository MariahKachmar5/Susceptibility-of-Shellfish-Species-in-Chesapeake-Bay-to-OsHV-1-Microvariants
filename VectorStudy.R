
setwd("~/Documents/UMBC/MDSG /KaplanMeier")

# Load required packages

library(readxl)

library(survminer)
library(survival)
library(ggpubr)


#### open data PETRI DISHES ###

library(readxl)

Petri <- read_xlsx("VectorStudy_petri.xlsx")
Petri


##### Create survival object and plot SD ###

PetriSD <- Petri[Petri$Virus == "SD µvar"|Petri$Virus =="Control",]
View(PetriSD)

Surv1 <- Surv(time = PetriSD$Day, event = PetriSD$Status)
fit.surv1 <- survfit(formula = Surv1 ~ Treatment + Virus, data = PetriSD)
SDplot <- ggsurvplot(fit.surv1, data = PetriSD, xlim = c(3,7), xlab = "Day", risk.table = TRUE, legend.title= "Treatment",
                     legend.labs = c("C.gigas Control", "C. gigas", "C.virginica Control", "C.virginica", "M. mercenaria Control", "M. mercenaria"))

SDplot

SDplot$plot <- SDplot$plot +
    theme(legend.text = element_text(size = 12, face = "italic"))
SDplot
##### Create survival object and plot FRA ###

PetriFRA <- Petri[Petri$Virus == "FRA µvar"|Petri$Virus =="Control",]
View(PetriFRA)

Surv2 <- Surv(time = PetriFRA$Day, event = PetriFRA$Status)
fit.surv2 <- survfit(formula = Surv2 ~ Treatment + Virus, data = PetriFRA)
FRAplot <- ggsurvplot(fit.surv2, data = PetriFRA, xlim = c(3,7), xlab = "Day",risk.table = TRUE, legend.title= "Treatment",
                      legend.labs = c("C.gigas Control", "C. gigas", "C.virginica Control", "C.virginica", "M. mercenaria Control", "M. mercenaria"))


FRAplot

FRAplot$plot <- FRAplot$plot +
    theme(legend.text = element_text(size = 12, face = "italic"))
FRAplot

library(ggplot2)
library(gridExtra)
library(ggpubr)

FRAplot+SDplot

plot1<- ggarrange(FRAplot$plot+ rremove("ylab") + rremove("xlab"), SDplot$plot+ rremove("ylab") + rremove("xlab"), ncol = 2, nrow = 1, common.legend = TRUE, labels= c("a)", "b)"))
plot1
annotate_figure(plot1, left = text_grob("Survival Probability", color = "black",
                                       rot = 90, size = 20),
                bottom = text_grob("Day", color = "black", size = 20))

## Create Survival Object & Plot both virus ##

Surv3 <- Surv(time = Petri$Day, event = Petri$Status)
fit.surv3 <- survfit(formula = Surv3 ~ Treatment + Virus, data = Petri)
ALLplot <- ggsurvplot(fit.surv3, data = Petri, xlim = c(3,7), xlab = "Day", risk.table = TRUE, legend.title= "Treatment",
                      legend.labs = c("C.gigas CTL", "C. gigas FRA ", "C. gigas SD","C.virginica CTL", "C.virginica FRA", "C. virginica SD", "M. mercenaria CTL", "M. mercenaria FRA", "M. mercenaria SD"),
                      title = "Kaplan Meier Survivorship Exposed spat FRA & SD")


ALLplot

### pairwise comparisons by treatment- petri dishes ###
## H: There is a significant difference between treatment (exposed seawater) ##

surv_diff_T <- survdiff(Surv(Day, Status) ~ Treatment, data = Petri)
surv_diff_T
## result : TRUE,Chisq= 731  on 2 degrees of freedom, p= <2e-16

#Pairwise with log-rank test
pcT <- pairwise_survdiff(Surv(Day, Status) ~ Treatment, data = Petri)
pcT

## H: There is a significant difference between treatment and virus ###
surv_diff_T <- survdiff(Surv(Day, Status) ~ Treatment + Virus, data = Petri)
surv_diff_T
## result: TRUE, Chisq= 1991  on 8 degrees of freedom, p= <2e-16


#Pairwise with log-rank test
pcT <- pairwise_survdiff(Surv(Day, Status) ~ Treatment + Virus, data = Petri)
pcT
## results : There is a significant difference between treatments and viruses . There is no significant difference between any controls.
##There is a signficant difference between treatmeants C. gigas and C. virginica for both viruses when compared to controls.
##There is no significant difference between M. mercenaria treatments for either virus compared to controls.

## H: There is a significant difference between virus ###
surv_diff_T <- survdiff(Surv(Day, Status) ~ Virus, data = Petri)
surv_diff_T
## results: TRUE  Chisq= 276  on 2 degrees of freedom, p= <2e-16


#Pairwise with log-rank test
pcT <- pairwise_survdiff(Surv(Day, Status) ~ Virus, data = Petri)
pcT
## results: there is a significant difference in virus compared to  controls and between viruses.


## Get Chi square values for survival ##

## H: There is a significant difference in survival between virus. ##
cs1 <- survdiff(Surv(Day, Status) ~ Virus, data = Petri)
cs1
## Results: TRUE

##H: There is a significant difference between survival of virus combined with treatment.##
cs2 <- survdiff(Surv(Day, Status) ~ Treatment + Virus, data = Petri)
cs2
## Results: TRUE

View(Petri)

## Survival probability by treatment and virus
summary(survfit(Surv(Day, Status) ~ Virus, data = Petri), times = 7)
summary(survfit(Surv(Day, Status) ~ Treatment, data = Petri2), times = 7)

######################## Survival probablility for all treatments ###############
########################### exposed to SD & FRA ##############################

## subset data ##

View(Petri)

Petri2<- Petri[Petri$Virus == "SD"|Petri$Virus =="FRA",]
View(Petri2)

summary(survfit(Surv(Day, Status) ~ Treatment + Virus, data = Petri2), times = 7, extend = TRUE)

## H: There is a significant difference in survival between species. ##
pcfrasd <- pairwise_survdiff(Surv(Day, Status) ~ Treatment, data = Petri2)
pcfrasd





###### coxph values - Semi parametric ##########
View(Petri)

## Change virus order


Petri$Virus <- factor(Petri$Virus, levels = c("CTL", "FRA", "SD"),labels = c("Control", "FRA µvar", "SD µvar"))
Petri$Treatment <- factor(Petri$Treatment, levels = c( "C. gigas","C. virginica", "M. mercenaria"))
View(Petri)

## H:Both FRA + SD variant had a significant effect on survival ##
fit.coxph <- coxph(Surv(time = Petri$Day, event = Petri$Status) ~ Virus, data = Petri)
fit.coxph
summary(fit.coxph)
test<- cox.zph(fit.coxph)
test
ggcoxzph(test)
ggforest(fit.coxph, data = Petri)


## H: Treatment had a significant effect on survival. ##
fit.coxph2 <- coxph(Surv(time = Petri$Day, event = Petri$Status) ~ Treatment, data = Petri)
fit.coxph2
summary(fit.coxph2)
ggforest(fit.coxph2, data= Petri2)
test2<-cox.zph(fit.coxph2)
test2
ggcoxzph(test2)




