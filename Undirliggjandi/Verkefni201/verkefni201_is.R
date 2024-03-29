rm(list = ls()) 
graphics.off() 
cat("\014") 

library(mirt)
library(tidyverse)
library(parallel)
library(MASS)
library(mice)
library(tictoc)

#Verkefni 201 - Svarferlalíkan fyrir RAASI gögn
gagnarammi <- read.delim("./Documents/GitHub/Megindleg.github.io/Undirliggjandi/gogninn.dat", header=F, row.names = 1)
colnames(gagnarammi) <- c("kyn", "ald_hop", "skolstig", "JS_01", "SS_02", "AH_03",
                       "JS_04", "SS_05", "JS_06", "SS_07", "JS_08", "SS_09", 
                       "SS_10", "SS_11", "SS_12", "AH_13", "AH_14", "SS_15", 
                       "JS_16", "AH_17", "TA_18", "JS_19", "TA_20", "AH_21",
                       "TA_22", "TA_23", "AH_24", "AH_25", "TA_26", "TA_27",
                       "TA_28", "TA_29", "TA_30", "TA_31", "AH_32")

sum(is.na(gagnarammi)) #ATH á N/A-gildum
rowSums(gagnarammi) #Telja hversu mörg gildi ery í hverri röð
describe(gagnarammi) #Lýsandi tölfræði


#-----Svarferlalíkan-----
#ATH AÐEINS KEYRA EINA BAKGRUNSBREYTU Í SENN!
#Skoða kynjahlutföll 
gagnarammi <- filter(gagnarammi, kyn == 1) #Filter by KK
gagnarammi <- filter(gagnarammi, kyn == 2) #Filter by KVK

#Skoða skólahlutföll
gagnarammi<- filter(gagnarammi, skolstig == 1) #Yngrastig
gagnarammi<- filter(gagnarammi, skolstig == 2) #Eldrastig

#Skoða yngra skólastig með yngri og eldri 
gagnarammi<- filter(gagnarammi, ald_hop == 11) #Yngri á yngrastigi
gagnarammi<- filter(gagnarammi, ald_hop == 12) #Eldri á yngrastigi

#Skoða eldra skólastig með yngri og eldri 
gagnarammi<- filter(gagnarammi, ald_hop == 21) #Yngri á eldrastigi
gagnarammi<- filter(gagnarammi, ald_hop == 22) #Eldri á eldrastigi

#---Tilreikningur----
sum(is.na(gagnarammi)) #ATH á N/A-gildum
set.seed(7)
m <- mice(gagnarammi, method="pmm", m = 5 )
m <- m$data
sum(is.na(gagnarammi)) #ATH á N/A-gildum

#----Svarferlalíkan fyrir Jákvæðasjálfsímynd ------
jakvedsjalfsimynd <- gagnarammi[c("JS_01", "JS_04", "JS_06", "JS_08", "JS_16", "JS_19")]

JS.likan <- mirt(jakvedsjalfsimynd, itemtype = "graded")
coef(JS.likan, simplify = T)
summary(JS.likan)
itemfit(JS.likan)
itemplot(JS.likan, 3)
itemplot(JS.likan, 3, type = 'threshold', )

#----Svarferlalíkan fyrir Skapstjórn ------
skapstjorn <- gagnarammi[c("SS_02", "SS_05", "SS_10", "SS_11", "SS_12", "SS_15")]
SS.likan <- mirt(skapstjorn, itemtype = "graded")
coef(SS.likan, simplify = T)
summary(SS.likan)
itemfit(SS.likan)
itemplot(SS.likan, 3)
itemplot(SS.likan, 3, type = 'threshold')

#----Svarferlalíkan fyrir Tilfinninga álag ------
tilfinningaalag <- gagnarammi[c("TA_18", "TA_20", "TA_22", "TA_23", "TA_26", "TA_27", "TA_28", "TA_29", "TA_30", "TA_31")]
TA.likan <- mirt(tilfinningaalag, itemtype = "graded")
coef(TA.likan, simplify = T)
summary(TA.likan)
itemfit(TA.likan)
itemplot(TA.likan, 3)
itemplot(TA.likan, 3, type = 'threshold')

#----Svarferlalíkan fyrir Andfélagslega hegðun ------
andfelagsleghegdun <- gagnarammi[c("AH_03", "AH_13", "AH_14", "AH_17", "AH_21", "AH_24", "AH_25", "AH_32")]
AH.likan <- mirt(andfelagsleghegdun, itemtype = "graded")
coef(AH.likan, simplify = T)
summary(AH.likan)
itemfit(AH.likan)
itemplot(AH.likan, 3)
itemplot(AH.likan, 3, type = 'threshold')

#----Svarferlalíkan fyrir RAASI ------
#WHY DONT YOU WORK??
heildarskor <- gagnarammi[!c("kyn","ald_hop", "skolstig")]


heildarskor <- gagnarammi[c("JS_01", "SS_02", "AH_03",
"JS_04", "SS_05", "JS_06", "SS_07", "JS_08", "SS_09", 
"SS_10", "SS_11", "SS_12", "AH_13", "AH_14", "SS_15", 
"JS_16", "AH_17", "TA_18", "JS_19", "TA_20", "AH_21",
"TA_22", "TA_23", "AH_24", "AH_25", "TA_26", "TA_27",
"TA_28", "TA_29", "TA_30", "TA_31", "AH_32")]

Heild.likan <- mirt(heildarskor, itemtype = "graded")
coef(Heild.likan, simplify = T)
summary(Heild.likan)
itemfit(Heild.likan)
itemplot(Heild.likan, 3)
itemplot(Heild.likan, 3, type = 'threshold')

mirtCluster(remove = TRUE)

#---- Önnur plot ---- ATH með Sigurgrím
itemplot(JS.likan, 3, CE = TRUE)
itemplot(JS.likan, 3, type = 'score')
itemplot(JS.likan, 3, type = 'score', CE = TRUE)
itemplot(JS.likan, 3, type = 'infotrace')
itemplot(JS.likan, 3, type = 'infocat')

