# NBA DATA
# PLAYOFFS POINTS

rm(list=ls())
ls()

library(dplyr)      # rename
library(tidyverse)  # separate

setwd("/Users/macbook/Sync/temp/TidyTuesday/NBA")

# data
MJ <- read.csv('data/Jordan.csv')      #23
SP <- read.csv('data/Pippen.csv')      #33
SO <- read.csv('data/Oneal.csv')       #34
KB <- read.csv('data/Bryant.csv')      #8
KD <- read.csv('data/Durant.csv')      #35
RW <- read.csv('data/Westbrook.csv')   #0
SC <- read.csv('data/Curry.csv')       #30
KT <- read.csv('data/Thompson.csv')    #11

TD <- read.csv('data/Duncan.csv')     #21
TP <- read.csv('data/Parker.csv')     #9
LJ <- read.csv('data/James.csv')      #6+23
DW <- read.csv('data/Wade.csv')       #3
PP <- read.csv('data/Pierce.csv')     #34
KG <- read.csv('data/Garnett.csv')    #5+2
KI <- read.csv('data/Irving.csv')     #2
PG <- read.csv('data/Gasol.csv')     #16


# rename variables
MJ <- MJ %>% rename(Date = X1985.Playoffs)
SP <- SP %>% rename(Date = X1988.Playoffs)
SO <- SO %>% rename(Date = X1994.Playoffs)
KB <- KB %>% rename(Date = X1997.Playoffs)
KD <- KD %>% rename(Date = X2010.Playoffs)
RW <- RW %>% rename(Date = X2010.Playoffs)
SC <- SC %>% rename(Date = X2013.Playoffs)
KT <- KT %>% rename(Date = X2013.Playoffs)

TD <- TD %>% rename(Date = X1998.Playoffs)
TP <- TP %>% rename(Date = X2002.Playoffs)
LJ <- LJ %>% rename(Date = X2006.Playoffs)
DW <- DW %>% rename(Date = X2004.Playoffs)
PP <- PP %>% rename(Date = X2002.Playoffs)
KG <- KG %>% rename(Date = X1997.Playoffs)
KI <- KI %>% rename(Date = X2015.Playoffs)
PG <- PG %>% rename(Date = X2004.Playoffs)

# additional
MJ <- MJ %>% rename(At = X) 
SP <- SP %>% rename(At = X)

SP <- SP %>% rename(PlusMinus = X...)

MJ <- MJ %>% rename(Win = X.1) 
SP <- SP %>% rename(Win = X.1) 
KB <- KB %>% rename(Win = X.1) 
SO <- SO %>% rename(Win = X.1) 
RW <- RW %>% rename(Win = X.1) 
KT <- KT %>% rename(Win = X.1)
KD <- KD %>% rename(Win = X.1)
SC <- SC %>% rename(Win = X.1)
TD <- TD %>% rename(Win = X.1)
TP <- TP %>% rename(Win = X.1)
LJ <- LJ %>% rename(Win = X.1)
DW <- DW %>% rename(Win = X.1)
PP <- PP %>% rename(Win = X.1)
KG <- KG %>% rename(Win = X.1)
KI <- KI %>% rename(Win = X.1)
PG <- PG %>% rename(Win = X.1)

# separate one teammate
MJ <- separate(MJ, Win, into = c("Result", "Dif"), sep=" ", remove=T)
SP <- separate(SP, Win, into = c("Result", "Dif"), sep=" ", remove=T)
KB <- separate(KB, Win, into = c("Result", "Dif"), sep=" ", remove=T)
SO <- separate(SO, Win, into = c("Result", "Dif"), sep=" ", remove=T)
RW <- separate(RW, Win, into = c("Result", "Dif"), sep=" ", remove=T)
KD <- separate(KD, Win, into = c("Result", "Dif"), sep=" ", remove=T)
SC <- separate(SC, Win, into = c("Result", "Dif"), sep=" ", remove=T)
KT <- separate(KT, Win, into = c("Result", "Dif"), sep=" ", remove=T)
TD <- separate(TD, Win, into = c("Result", "Dif"), sep=" ", remove=T)
TP <- separate(TP, Win, into = c("Result", "Dif"), sep=" ", remove=T)
LJ <- separate(LJ, Win, into = c("Result", "Dif"), sep=" ", remove=T)
PP <- separate(PP, Win, into = c("Result", "Dif"), sep=" ", remove=T)
PG <- separate(PG, Win, into = c("Result", "Dif"), sep=" ", remove=T)
KI <- separate(KI, Win, into = c("Result", "Dif"), sep=" ", remove=T)
KG <- separate(KG, Win, into = c("Result", "Dif"), sep=" ", remove=T)
DW <- separate(DW, Win, into = c("Result", "Dif"), sep=" ", remove=T)

# merge
# BULLS
SP2 <- subset(SP, Tm == "CHI")[, c("Date", "PTS")]
SP2 <- SP2 %>% rename(Pippen = PTS) 
MJ2 <- MJ[,c("Date", "Series", "Result", "PTS")]
MJ2 <- MJ2 %>% rename(Jordan = PTS) 
PO_BULLS <- merge(MJ2, SP2, by="Date")
PO_BULLS$Series = factor(PO_BULLS$Series, c("EC1", "ECS", "ECF", "FIN"))
# LAKERS
SO2 <- subset(SO, Tm == "LAL")[, c("Date", "PTS")]
SO2 <- SO2 %>% rename(Oneal = PTS) 
KB2 <- KB[,c("Date", "Series", "Result", "PTS")]
KB2 <- KB2 %>% rename(Bryant = PTS) 
PO_LAKERS <- merge(KB2, SO2, by="Date")
PO_LAKERS$Series = factor(PO_LAKERS$Series, c("WC1", "WCS", "WCF", "FIN"))
# THUNDER
KD2 <- subset(KD, Tm == "OKC")[, c("Date", "PTS")]
KD2 <- KD2 %>% rename(Durant = PTS) 
RW2 <- subset(RW, Tm == "OKC")
RW2 <- RW2[,c("Date", "Series", "Result", "PTS")]
RW2 <- RW2 %>% rename(Westbrook = PTS) 
PO_THUNDER <- merge(RW2, KD2, by="Date")
PO_THUNDER$Series = factor(PO_THUNDER$Series, c("WC1", "WCS", "WCF", "FIN"))
# WARRIORS
SC2 <- SC[, c("Date", "PTS")]
SC2 <- SC2 %>% rename(Curry = PTS) 
KT2 <- KT[,c("Date", "Series", "Result", "PTS")]
KT2 <- KT2 %>% rename(Thompson = PTS) 
PO_WARRIORS <- merge(KT2, SC2, by="Date")
PO_WARRIORS$Series = factor(PO_WARRIORS$Series, c("WC1", "WCS", "WCF", "FIN"))

# LAKERS 2
PG2 <- subset(PG, Tm == "LAL")[, c("Date", "PTS")]
PG2 <- PG2 %>% rename(Gasol = PTS) 
KB2 <- KB[,c("Date", "Series", "Result", "PTS")]
KB2 <- KB2 %>% rename(Bryant = PTS) 
PO_LAKERS2 <- merge(KB2, PG2, by="Date")
PO_LAKERS2$Series = factor(PO_LAKERS2$Series, c("WC1", "WCS", "WCF", "FIN"))
# SPURS
TP2 <- subset(TP, Tm == "SAS")[, c("Date", "PTS")]
TP2 <- TP2 %>% rename(Parker = PTS) 
TD2 <- TD[,c("Date", "Series", "Result", "PTS")]
TD2 <- TD2 %>% rename(Duncan = PTS) 
PO_SPURS <- merge(TD2, TP2, by="Date")
PO_SPURS$Series = factor(PO_SPURS$Series, c("WC1", "WCS", "WCF", "FIN"))
# HEAT
DW2 <- subset(DW, Tm == "MIA")[, c("Date", "PTS")]
DW2 <- DW2 %>% rename(Wade = PTS) 
LJ2 <- subset(LJ, Tm == "MIA")[,c("Date", "Series", "Result", "PTS")]
LJ2 <- LJ2 %>% rename(James = PTS) 
PO_HEAT <- merge(LJ2, DW2, by="Date")
PO_HEAT$Series = factor(PO_HEAT$Series, c("EC1", "ECS", "ECF", "FIN"))
# CELTICS
KG2 <- subset(KG, Tm == "BOS" | Tm == "BRK")[, c("Date", "PTS", "Tm")]
KG2 <- KG2 %>% rename(Garnett = PTS) 
PP2 <- subset(PP, Tm == "BOS" | Tm == "BRK")[,c("Date", "Series", "Result", "PTS", "Tm")]
PP2 <- PP2 %>% rename(Pierce = PTS) 
PO_CELTICS <- merge(KG2, PP2, by=c("Date"))
PO_CELTICS$Series = factor(PO_BOSTON$Series, c("EC1", "ECS", "ECF", "FIN"))
# CAVALIERS
KI2 <- subset(KI, Tm == "CLE")[, c("Date", "PTS")]
KI2 <- KI2 %>% rename(Irving = PTS) 
LJ3 <- subset(LJ, Tm == "CLE")[,c("Date", "Series", "Result", "PTS")]
LJ3 <- LJ3 %>% rename(James = PTS) 
PO_CAVALIERS <- merge(LJ3, KI2, by="Date")
PO_CAVALIERS$Series = factor(PO_CAVALIERS$Series, c("EC1", "ECS", "ECF", "FIN"))

rm(MJ)
rm(SP)
rm(SO)
rm(KB)
rm(KD)
rm(RW)
rm(SC)
rm(KT)
rm(MJ2)
rm(SP2)
rm(SO2)
rm(KB2)
rm(KD2)
rm(RW2)
rm(SC2)
rm(KT2)
rm(LJ)
rm(DW)
rm(PG)
rm(KG)
rm(PP)
rm(TP)
rm(TD)
rm(KI)
rm(LJ2)
rm(DW2)
rm(PG2)
rm(KG2)
rm(PP2)
rm(TP2)
rm(TD2)
rm(KI2)
rm(LJ3)

source <- "basketball-reference.com"

save.image("data/PO.RData")
