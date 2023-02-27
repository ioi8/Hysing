
## -----------------------------Sækja pakka--------------------------

library(psych) ## fyrir lýsandi tölfræði
library(mirt) ## fyrir IRT
library(tidyverse) ## fyrir filter og select
library(mice) ## fyrir brottfallsgögn

##------------------------------------Lesa inn nýju gögnin----------------
library(readr)
gogn <- read_delim("DATA_SAL138F_IRT_Skulason.2022_Vidhorf.Til.Namsmats_v28n3404.binay.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

bakgrunnur <- read_delim("DATA_SAL138F_IRT_Skulason.2022_Vidhorf.Til.Namsmats_BAKGRUNNSBR.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

setwd("C:/Users/Aðalheiður/Documents/GitHub/Hysing/Undirliggjandi")

##-------------------------------Meðhöndla brotfallsgögn-------------------------

gogn[gogn == -999] <- NA
bakgrunnur[bakgrunnur == -999] <- NA
gogn2[gogn2 == -999] <- NA


imp_multimean <- mice(gogn2[,-1], m = 5, method = "logreg", maxit = 20)
summary(imp_multimean)

gogn2 <- complete(imp_multimean, 4)

##-------------------------------Flokka----------------------------------

gogn2_foreldrar <- select(gogn)

hlutverk_namsmats <- select(gogn2, c(sp_b01, sp_b02, sp_b03, sp_b04, sp_b05, sp_b06, sp_b07,
                                    sp_b08, sp_b09, sp_b10, sp_b11, sp_b12, sp_b13, sp_b14))

upplys_mat <- select(gogn2, c(sp_a01, sp_a02, sp_a03, sp_a04, sp_a05, sp_a06, sp_a07, sp_a08,
                             sp_a09, sp_a10))

lykilhaefni <- select(gogn2, c(sp_c01, sp_c02, sp_c03, sp_c04))


##---------------------------------Lýsandi tölfræði--------------------------

describe(gogn2)
describe(hlutverk_namsmats)
describe(upplys_mat)
describe(lykilhaefni)

psych::alpha(gogn2)
psych::alpha(hlutverk_namsmats)
psych::alpha(upplys_mat)
psych::alpha(lykilhaefni)

#---------------------------------Filter----------------------------

#Starfsaldur
filter(gogn, Strf_ald == 1) # 0-5 ár 
filter(gogn, Strf_ald == 2) # 6-10
filter(gogn, Strf_ald == 3) # 11 + 

#Stærð skóla
filter(gogn, stard_sk == 1) # Innan við 10
filter(gogn, stard_sk == 11) # 11 - 20 
filter(gogn, stard_sk == 21) # 21 - 30
filter(gogn, stard_sk == 31) # 31 - 50
filter(gogn, stard_sk == 51) # 50 + 

#Staðsetning
filter(gogn, Ladhl == 1) # Höfuðborgarsvæði
filter(gogn, Ladhl == 3) # NV kjördæmi
filter(gogn, Ladhl == 6) # NA kjördæmi
filter(gogn, Ladhl == 8) # Suðurkjördæmi

#Staðsetning 3
filter(gogn, Hof_land == 1) #Höfuðborgarsvæði
filter(gogn, Hof_land == 2) # Utan höfuðborgarsvæðis


------------------------------------------------------------------------------------
  


## -----------------------------Lesa inn gögnin--------------------------
  
gognin <- read.delim("gogninn.dat", 
                      header=FALSE, row.names=1)

colnames(gogninn) <- c("kyn", "ald_hop", "skolstig", "JS_01", "SS_02", "AH_03",
                       "JS_04", "SS_05", "JS_06", "SS_07", "JS_08", "SS_09", 
                       "SS_10", "SS_11", "SS_12", "AH_13", "AH_14", "SS_15", 
                       "JS_16", "AH_17", "TA_18", "JS_19", "TA_20", "AH_21",
                       "TA_22", "TA_23", "AH_24", "AH_25", "TA_26", "TA_27",
                       "TA_28", "TA_29", "TA_30", "TA_31", "AH_32")


##-------------------------------Flokka------------------------------
JS <- select(gogninn, c(JS_01, JS_04, JS_06, JS_08, JS_16, JS_19))
SS <- select(gogninn, c(SS_02, SS_05, SS_07, SS_09, SS_10, SS_11, SS_12, SS_15))
TA <- select(gogninn, c(TA_18, TA_20, TA_22, TA_23, TA_26, TA_27, TA_28, TA_29, TA_30, TA_31))
AH <- select(gogninn, c(AH_03, AH_13, AH_14, AH_17, AH_21, AH_24, AH_25, AH_32))

##------------------------------Taka fyrstu þrjar---------------------

gogn2 <- select(gogninn, !c(kyn, ald_hop, skolstig))

## -----------------------------Breyta missing-------------------------

#gogninn[gogninn == 99] <- NA

## -----------------------------Lýsandi tölfræði-------------------------

describe(gogn2)

describe(JS)
describe(SS)
describe(TA)
describe(AH)

psych::alpha(gogn2)
psych::alpha(AH)
psych::alpha(JS)
psych::alpha(SS)
psych::alpha(TA)

## -----------------------------Svarferlalíkan--------------------------

filter(gogninn, kyn == 1) #Filter by KK
filter(gogninn, kyn == 2) #Filter by KVK

#Skoða skólahlutföll
filter(gogninn, skolastig == 1) #Yngrastig
filter(gogninn, skolastig == 2) #Eldrastig

#Skoða yngra skólastig með yngri og eldri 
filter(gogninn, ald_hop == 11) #Yngri á yngrastigi
filter(gogninn, ald_hop == 12) #Eldri á yngrastigi

#Skoða eldra skólastig með yngri og eldri 
filter(gagnarammi, ald_hop == 21) #Yngri á eldrastigi
filter(gagnarammi, ald_hop == 22) #Eldri á eldrastigi


#----Svarferlalíkan fyrir Jákvæðasjálfsímynd ------
JS.likan <- mirt(JS, itemtype = "graded", method = "MCEM", iter = 1000)
coef(JS.likan, simplify = T)
summary(JS.likan)
itemfit(JS.likan)
itemplot(JS.likan, 3)
itemplot(JS.likan, 3, type = 'threshold')

#----Svarferlalíkan fyrir Skapstjórn ------
SS.likan <- mirt(SS, itemtype = "graded", method = "MCEM", iter = 1000)
coef(SS.likan, simplify = T)
summary(SS.likan)
itemfit(SS.likan)
itemplot(SS.likan, 3)
itemplot(SS.likan, 3, type = 'threshold')

#----Svarferlalíkan fyrir Tilfinninga álag ------
TA.likan <- mirt(TA, itemtype = "graded", method = "MCEM", iter = 1000)
coef(TA.likan, simplify = T)
summary(TA.likan)
itemfit(TA.likan)
itemplot(TA.likan, 3)
itemplot(TA.likan, 3, type = 'threshold')

#----Svarferlalíkan fyrir Andfélagslega hegðun ------
AH.likan <- mirt(AH, itemtype = "graded", method = "MCEM", iter = 1000)
coef(AH.likan, simplify = T)
summary(AH.likan)
itemfit(AH.likan)
itemplot(AH.likan, 3)
itemplot(AH.likan, 3, type = 'threshold')

#----Svarferlalíkan fyrir RAASI ------
Heild.likan <- mirt(gogn2, itemtype = "graded", method = "MCEM", iter = 1000)
coef(AH.likan, simplify = T)
summary(Heild.likan)
itemfit(Heild.likan)
itemplot(Heild.likan, 3)
itemplot(Heild.likan, 3, type = 'threshold')


#---- Önnur plot ---- ATH með Sigurgrím
itemplot(JS.likan, 3, CE = TRUE)
itemplot(JS.likan, 3, type = 'score')
itemplot(JS.likan, 3, type = 'score', CE = TRUE)
itemplot(JS.likan, 3, type = 'infotrace')
itemplot(JS.likan, 3, type = 'infocat')



## -----------------------------Mat á gæðum líkans--------------------------

