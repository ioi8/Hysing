
## -----------------------------Sækja pakka--------------------------

library(psych) ## fyrir lýsandi tölfræði
library(mirt) ## fyrir IRT
library(tidyverse) ## fyrir filter og select
library(mice) ## fyrir brottfallsgögn
library(readr) ## fyrir að lesa inn gögn

##------------------------------------Lesa inn nýju gögnin----------------
gagnarammi <- read_delim("DATA_SAL138F_IRT_Skulason.2022_Vidhorf.Til.Namsmats_v28n3404.binay.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE, na = c("-999"))

bakgrunnur <- read_delim("DATA_SAL138F_IRT_Skulason.2022_Vidhorf.Til.Namsmats_BAKGRUNNSBR.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE, na = c("-999"))

## Snúa við breytum
gagnarammi$sp_a02 = car::recode(gagnarammi$sp_a02, "0=1; 1=0")
gagnarammi$sp_a03 = car::recode(gagnarammi$sp_a03, "0=1; 1=0")


#---------------------------------Filter----------------------------
#ATH AÐEINS KEYRA EINA BAKGRUNSBREYTU Í SENN!

#Stærð skóla
StorSkoli <- filter(gagnarammi, stard_sk == 51)
litillSkoli <- filter(gagnarammi,stard_sk == 1 & 11 & 21 & 31 & 41)

#Staðsetning
HBS <- filter(gagnarammi, Ladhl == 1) # Höfuðborgarsvæði
NVK <- filter(gagnarammi, Ladhl == 3) # NV kjördæmi
NAK <- filter(gagnarammi, Ladhl == 6) # NA kjördæmi
SK <- filter(gagnarammi, Ladhl == 8) # Suðurkjördæmi
UtanHBS <- filter(gagnarammi, Hof_land == 2) # Utan höfuðborgarsvæðis

# Kennarar  vs foreldrar
VidhorfKennara <- filter(gagnarammi,hopur == 2)
VidhorfForeldra <- filter(gagnarammi, hopur == 1)
VidhorfForeldra <- dplyr::select(VidhorfForeldra, !c(sp_a10, sp_b13, sp_b14)) #!spr fyrir kennara

#Starfsaldur
"11lengur" <- filter(VidhorfKennara, Strf_ald == 3)
"6til10" <- filter(VidhorfKennara,Strf_ald == 2)
"0til5" <- filter(VidhorfKennara,Strf_ald == 1)


##-------------------------------Flokka spurningarnar----------------------------------


hlutverk_namsmats <- select(gagnarammi, c(sp_b01, sp_b02, sp_b03, sp_b04, sp_b05, sp_b06, sp_b07,
                                          sp_b08, sp_b09, sp_b10, sp_b11, sp_b12, sp_b13, sp_b14))

upplys_mat <- select(gagnarammi, c(sp_a01, sp_a02, sp_a03, sp_a04, sp_a05, sp_a06, sp_a07, sp_a08,
                                   sp_a09, sp_a10))

lykilhaefni <- select(gagnarammi, c(sp_c01, sp_c02, sp_c03, sp_c04))


#-------------------Taka burt raðir þá sem svöruðu engu------------------------
NAgildi <- rowSums(is.na(VidhorfKennara[,-1:-8])) >= 30
tilbuinnGogn <- VidhorfKennara[!NAgildi, ]
sum(is.na(tilbuinnGogn))

#----------------------------------------Tilreikningur----------------------------------------
imputationgogn <- mice(tilbuinnGogn, m=3, method="pmm", maxit = 3)
imputationgogn <- complete(imputationgogn, 3)
sum(is.na(imputationgogn))

describe(imputationgogn)
psych::alpha(imputationgogn)


#----------------------------------------Svarferlalíkan----------------------------------------
Likan <- mirt(imputationgogn[,-1:-8], itemtype = "graded")
coef(Likan, simplify = T)
summary(Likan)
itemfit(Likan)
itemplot(Likan, 3)
itemplot(Likan, 3, type = 'threshold', )

