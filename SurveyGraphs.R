setwd("~/Documents/UMBC/MDSG /Surveys")

library(readxl)


survey<-read_xlsx("~/Documents/UMBC/MDSG /Surveys/qPCR Survey.xlsx")
survey

### log transform data ###

survey$LogData <- log10(survey$'2ul'+1)

head(survey)
View(survey)

library(Rmisc)
library(ggplot2)

qPCR_sum <- summarySE(survey, "LogData", groupvars=c("month", "virus"))
View(qPCR_sum)


qPCR_sum_raw <- summarySE(survey, "2ul", groupvars=c("month", "virus"))
View(qPCR_sum_raw)

qPCR_sum2 <- qPCR_sum


qPCR_sum2$Exposure <-factor(qPCR_sum2$Exposure)

qPCR_sum2

survey_plot <- ggplot(qPCR_sum2, aes(month, LogData, fill = virus)) + geom_bar(position=position_dodge(), stat="identity") + 
  theme(axis.text.x = element_text(angle = 0)) + 
  labs(y= "Mean OsHV-1 Copies/mg Tissue across all doses", x = "Month", title = "Survey")+
  scale_fill_manual(values=c("gray82", "gray16", "grey40")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1),
        strip.background = element_rect(color= NA, fill= NA),
        axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold")) +          # Remove the background, grid lines, and add a boarder
  labs(y = "Log 10 OsHV-1 copies per DNA sample") + ylim(0,10) +
  geom_errorbar(aes(ymin=LogData-se, ymax=LogData+se),width=.2, position=position_dodge(.9))


survey_plot




library(Rmisc)
library(ggplot2)
library(dplyr)
library(lubridate)

Means<-survey %>%
  group_by(month, virus) %>%
  summarize(LogData = mean(LogData))
View(Means)

surv_plot <- ggplot(survey, aes(month, LogData), fill = virus) + geom_bar(position=position_dodge(), stat="identity") + 
  theme(axis.text.x = element_text(angle = 0)) + 
  labs(y= "Mean Log 10 OsHV-1 Copies/ DNA sample ", x = "Sample month")+
  scale_fill_manual(values=c("gray82", "gray16", "grey40")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1),strip.background = element_rect(color= NA, fill= NA),
        axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold")) + ylim(0,10)
 


surv_plot


library(readxl)
control<-read_xlsx("~/Documents/UMBC/MDSG /Surveys/SurveyControls.xlsx")
control

Means2<-control %>%
  group_by(Month, site) %>%
  summarize(copies.2ul = mean(copies.2ul))
View(Means2)



c_plot <- ggplot(control, aes(Month, copies.2ul), fill = site) + geom_bar(position=position_dodge(),stat="identity", width= .5) + 
  theme(axis.text.x = element_text(angle = 0)) + 
  labs(y= "Mean OsHV-1 Copies/2ul DNA sample ", x = "Innoculated Sample")+
  scale_fill_manual(values=c("gray82", "gray16", "grey40")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1),strip.background = element_rect(color= NA, fill= NA),
        axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold")) + geom_text(aes(label = Means), vjust= -.25)


c_plot


plot<-ggplot(Means2, aes(Month, copies.2ul)) + geom_bar()
plot

library (gridExtra)
grid <- grid.arrange(surv_plot, c_plot) 




