
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

