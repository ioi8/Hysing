
## -----------------------------Sækja pakka--------------------------

library(psych) ## fyrir lýsandi tölfræði
library(tidyverse) 
library(mirt) ## fyrir IRT

## -----------------------------Lesa inn gögnin--------------------------
  
gogninn <- read.delim("gogninn.dat", 
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

gogninn[gogninn == 99] <- NA

## -----------------------------Lýsandi tölfræði-------------------------

describe(gogn2)

describe(JS)
describe(SS)
describe(TA)
describe(AH)

psych::alpha(gogn2)

## -----------------------------Svarferlalíkan--------------------------



## -----------------------------Mat á gæðum líkans--------------------------

