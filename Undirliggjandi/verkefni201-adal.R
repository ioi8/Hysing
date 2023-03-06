
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

#Snúa við breytum
gagnarammi$sp_a02 = car::recode(gagnarammi$sp_a02, "0=1; 1=0")
gagnarammi$sp_a03 = car::recode(gagnarammi$sp_a03, "0=1; 1=0")


#---------------------------------Filter----------------------------
#ATH AÐEINS KEYRA EINA BAKGRUNSBREYTU Í SENN!

# Gögn án bakgrunnsbreyta
Baraspurningar <- dplyr::select(gagnarammi, !c(id, ID2, hopur, stard_sk, Strf_ald, stadsetn, Hof_land, Ladhl, nmiss, Nmiss_f))

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
VidhorfStarfsfolks <- filter(gagnarammi,hopur == 2)
VidhorfStarfsfolks <- dplyr::select(VidhorfStarfsfolks, !c(id, ID2, hopur, stard_sk, Strf_ald, stadsetn, Hof_land, Ladhl, nmiss, Nmiss_f))
VidhorfStarfsfolksHBS <- filter(gagnarammi, hopur == 2, Ladhl == 1)
VidhorfStarfsfolksUtanHBS <- filter(gagnarammi, hopur == 2, Hof_land == 2)

VidhorfForeldra <- filter(gagnarammi, hopur == 1)
VidhorfForeldra <- dplyr::select(VidhorfForeldra, !c(id, ID2, hopur, stard_sk, Strf_ald, stadsetn, Hof_land, Ladhl, nmiss, Nmiss_f, sp_a10, sp_b13, sp_b14)) #!spr fyrir kennara
VidhorfForeldraHBS <- filter(gagnarammi, hopur == 1, Ladhl == 1)
VidhorfForeldraHBS <- dplyr::select(VidhorfForeldraHBS, !c(id, ID2, hopur, stard_sk, Strf_ald, stadsetn, Hof_land, Ladhl, nmiss, Nmiss_f, sp_a10, sp_b13, sp_b14))
VidhorfForeldraUtanHBS <- filter(gagnarammi, hopur == 1, Hof_land == 2)
VidhorfForeldraUtanHBS <- dplyr::select(VidhorfForeldraUtanHBS, !c(id, ID2, hopur, stard_sk, Strf_ald, stadsetn, Hof_land, Ladhl, nmiss, Nmiss_f, sp_a10, sp_b13, sp_b14))


#Starfsaldur
ellefu_plus <- filter(VidhorfStarfsfolks, Strf_ald == 3)
sex_til_tiu <- filter(VidhorfStarfsfolks, Strf_ald == 2)
null_til_fimm <- filter(VidhorfStarfsfolks, Strf_ald == 1)


##-------------------------------Flokka spurningarnar----------------------------------


hlutverk_namsmats <- select(imputationgogn, c(sp_b01, sp_b02, sp_b03, sp_b04, sp_b05, sp_b06, sp_b07,
                                          sp_b08, sp_b09, sp_b10, sp_b11, sp_b12))
describe(rowSums(hlutverk_namsmats))


upplys_mat <- select(imputationgogn, c(sp_a01, sp_a02, sp_a03, sp_a04, sp_a05, sp_a06, sp_a07, sp_a08,
                                   sp_a09, sp_a10))
describe(rowSums(upplys_mat))

lykilhaefni <- select(imputationgogn, c(sp_c01, sp_c02, sp_c03, sp_c04))
describe(rowSums(lykilhaefni))


#-------------------Taka burt raðir þá sem svöruðu engu------------------------
NAgildi <- rowSums(is.na(VidhorfForeldraHBS)) >=25
tilbuinnGogn_ForeldrarHBS <- gagnarammi[!NAgildi, ]
sum(is.na(tilbuinnGogn_ForeldrarHBS))

NAgildi <- rowSums(is.na(VidhorfForeldraUtanHBS)) >=25
tilbuinnGogn_ForeldrarUtanHBS <- gagnarammi[!NAgildi, ]
sum(is.na(tilbuinnGogn_ForeldrarUtanHBS))


#----------------------------------------Tilreikningur----------------------------------------
imputationgogn_kennararHBS <- mice(VidhorfStarfsfolksHBS, m=3, method="pmm", maxit = 3)
imputationgogn_kennarar <- complete(imputationgogn_kennarar, 3)
sum(is.na(imputationgogn_kennarar))

imputationgogn_kennararUtanHBS <- mice(VidhorfStarfsfolksUtanHBS, m=3, method="pmm", maxit = 3)
imputationgogn_kennararUtanHBS <- complete(imputationgogn_kennararUtanHBS, 3)
sum(is.na(imputationgogn_kennararUtanHBS))

imputationgogn_foreldrarHBS <- mice(VidhorfForeldraHBS, m=3, method="pmm", maxit = 3)
imputationgogn_foreldrarHBS <- complete(imputationgogn_foreldrarHBS, 3)
sum(is.na(imputationgogn_foreldrarHBS))

imputationgogn_foreldrarUtanHBS <- mice(VidhorfForeldraUtanHBS, m=3, method="pmm", maxit = 3)
imputationgogn_foreldrarUtanHBS <- complete(imputationgogn_foreldrarUtanHBS, 3)
sum(is.na(imputationgogn_foreldrarUtanHBS))


#----------------------------------------Svarferlalíkan----------------------------------------
VidhorfForeldraHBS_likan <- mirt(upplys_mat, itemtype = "graded")
coef(VidhorfForeldraHBS_likan, simplify = T)
summary(VidhorfForeldraHBS_likan)
itemfit(VidhorfForeldraHBS_likan)
itemplot(VidhorfForeldraHBS_likan, 3)
itemplot(VidhorfForeldraHBS_likan, 3, type = 'threshold', )

VidhorfForeldraUtanHBS_likan <- mirt(imputationgogn, itemtype = "graded")
coef(Likan, simplify = T)
summary(Likan)
itemfit(Likan)
itemplot(Likan, 3)
itemplot(Likan, 3, type = 'threshold', )


VidhorfStarfsfolksHBS_likan <- mirt(imputationgogn, itemtype = "graded")
coef(Likan, simplify = T)
summary(Likan)
itemfit(Likan)
itemplot(Likan, 3)
itemplot(Likan, 3, type = 'threshold', )


VidhorfStarfsfolksUtanHBS_likan <- mirt(imputationgogn, itemtype = "graded")
coef(Likan, simplify = T)
summary(Likan)
itemfit(Likan)
itemplot(Likan, 3)
itemplot(Likan, 3, type = 'threshold', )



