library(psych)
library(mirt)

#Verkefni 201 - 2 stika svarferlalíkan
gagnarammi <- read.delim("./Documents/GitHub/Megindleg.github.io/Undirliggjandi/gogninn.dat", header=F, row.names = 1)
colnames(gagnarammi) <- c("kyn", "ald_hop", "skolstig", "JS_01", "SS_02", "AH_03",
                       "JS_04", "SS_05", "JS_06", "SS_07", "JS_08", "SS_09", 
                       "SS_10", "SS_11", "SS_12", "AH_13", "AH_14", "SS_15", 
                       "JS_16", "AH_17", "TA_18", "JS_19", "TA_20", "AH_21",
                       "TA_22", "TA_23", "AH_24", "AH_25", "TA_26", "TA_27",
                       "TA_28", "TA_29", "TA_30", "TA_31", "AH_32")
jakvedsjalfsimynd <- select(gagnarammi, c(JS_01, JS_04, JS_06, JS_08, JS_16, JS_19))

skapstjorn <- select(gagnarammi, c("SS_02", "SS_05", "SS_10", "SS_11", "SS_12", "SS_15"))
tilfinningaalag
andfelagsleghegdun


sum(is.na(gagnarammi)) #ATH á N/A-gildum
lysandiGogn <- rowSums(gagnarammi)
describe(gagnarammi) 

#setja upp 2PL líkan og 3PL líkan, skoða S.E. milli aldurshópa (10,11 og 20,21) 
Tveggjastika <- mirt(gagnarammi, itemtype = "2PL")
coef(Tveggjastika, simplify=TRUE)

Þriggjastika <- mirt(gagnarammi, itemtype = "3PL" )
coef(Þriggjastika, simplify=TRUE)
