#### Sports Analytics Survey Project

setwd("C:/Users/14702/OneDrive/Desktop/Emory/Sports Analytic/Sports Survey Project")

library(data.table)


# import data
Braves <- data.frame(read_excel("Braves.xlsx"))
Falcons <- read_excel("Falcons.xlsx")
Hawks <- read_excel("Hawks.xlsx")
AUFC <- read_excel("Atlanta United F.C..xlsx")

# Braves
names(Braves)[1] <- "rate"
Braves=as.data.frame(lapply(Braves,as.numeric))
Braves= Braves[complete.cases(Braves[,1]),]
Braves= Braves[,names(Braves)!='Family_Oriented']
Braves[is.na(Braves)]=0
lm_b = lm(Braves$rate ~.,data = Braves)
summary(lm_b)
step_b<-step(lm_b)
summary(step_b)

# Falcons
names(Falcons)[1] <- "rate"
Falcons=as.data.frame(lapply(Falcons,as.numeric))
Falcons= Falcons[complete.cases(Falcons[,1]),]
Falcons= Falcons[,names(Falcons)!='Family_Oriented']
Falcons[is.na(Falcons)]=0
lm_f = lm(Falcons$rate ~.,data = Falcons)
summary(lm_f)
step_f<-step(lm_f)
summary(step_f)

# Hawks
names(Hawks)[1] <- "rate"
Hawks=as.data.frame(lapply(Hawks,as.numeric))
Hawks= Hawks[complete.cases(Hawks[,1]),]
Hawks= Hawks[,names(Hawks)!='Family_Oriented']
Hawks[is.na(Hawks)]=0
lm_h = lm(Hawks$rate ~.,data = Hawks)
summary(lm_h)
step_h<-step(lm_h)
summary(step_h)

# AUFC
names(AUFC)[1] <- "rate"
AUFC=as.data.frame(lapply(AUFC,as.numeric))
AUFC= AUFC[complete.cases(AUFC[,1]),]
AUFC= AUFC[,names(AUFC)!='Family_Oriented']
AUFC[is.na(AUFC)]=0
lm_a = lm(AUFC$rate ~.,data = AUFC)
summary(lm_a)
step_a<-step(lm_a)
summary(step_a)






