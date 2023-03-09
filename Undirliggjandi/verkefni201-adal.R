
rm(list =  ls()) 
graphics.off() 
cat("\014") 

## -----------------------------Sækja pakka--------------------------

library(psych) ## fyrir lýsandi tölfræði
library(mirt)## fyrir IRT
library(tidyverse) ## fyrir filter og select
library(mice) ## fyrir brottfallsgögn
library(readr) ## fyrir að lesa inn gögn
library(ggplot2) ## fyrir myndir
library(latticeExtra) ## fyrir myndir  í svarferlalíkönum

library(gridExtra)
library(parallel)
library(lattice)

##------------------------------------Lesa inn nýju gögnin----------------
gagnarammi <- read_delim("DATA_SAL138F_IRT_Skulason.2022_Vidhorf.Til.Namsmats_v28n3404.binay.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE, na = c("-999"))

#bakgrunnur <- read_delim("DATA_SAL138F_IRT_Skulason.2022_Vidhorf.Til.Namsmats_BAKGRUNNSBR.csv", 
                         #delim = ";", escape_double = FALSE, trim_ws = TRUE, na = c("-999"))

#Snúa við breytum
#gagnarammi$sp_a02 = car::recode(gagnarammi$sp_a02, "0=1; 1=0")
#gagnarammi$sp_a03 = car::recode(gagnarammi$sp_a03, "0=1; 1=0")


#---------------------------------Filter----------------------------
#ATH AÐEINS KEYRA EINA BAKGRUNSBREYTU Í SENN!

# Gögn án bakgrunnsbreyta
#Baraspurningar <- dplyr::select(gagnarammi, !c(id, ID2, hopur, stard_sk, Strf_ald, stadsetn, Hof_land, Ladhl, nmiss, Nmiss_f))

#Stærð skóla
#StorSkoli <- filter(gagnarammi, stard_sk == 51)
#litillSkoli <- filter(gagnarammi,stard_sk == 1 & 11 & 21 & 31 & 41)

#Staðsetning
#HBS <- filter(gagnarammi, Ladhl == 1) # Höfuðborgarsvæði
#NVK <- filter(gagnarammi, Ladhl == 3) # NV kjördæmi
#NAK <- filter(gagnarammi, Ladhl == 6) # NA kjördæmi
#SK <- filter(gagnarammi, Ladhl == 8) # Suðurkjördæmi
#UtanHBS <- filter(gagnarammi, Hof_land == 2) # Utan höfuðborgarsvæðis

# Kennarar  vs foreldrar
VidhorfStarfsfolks <- filter(gagnarammi,hopur == 2)
VidhorfStarfsfolks <- dplyr::select(VidhorfStarfsfolks, !c(id, ID2, hopur, stard_sk, 
                                                           Strf_ald, stadsetn, Hof_land, 
                                                           Ladhl, nmiss, Nmiss_f))


VidhorfForeldra <- filter(gagnarammi, hopur == 1)
VidhorfForeldra <- dplyr::select(VidhorfForeldra, !c(id, ID2, hopur, stard_sk, Strf_ald, 
                                                     stadsetn, Hof_land, Ladhl, nmiss, 
                                                     Nmiss_f, sp_a10, sp_b13, sp_b14)) #!spr fyrir kennara


#VidhorfStarfsfolksHBS <- filter(gagnarammi, hopur == 2, Ladhl == 1)
#VidhorfStarfsfolksUtanHBS <- filter(gagnarammi, hopur == 2, Hof_land == 2)
#VidhorfForeldraHBS <- filter(gagnarammi, hopur == 1, Ladhl == 1)
#VidhorfForeldraHBS <- dplyr::select(VidhorfForeldraHBS, !c(id, ID2, hopur, stard_sk, Strf_ald, stadsetn, Hof_land, Ladhl, nmiss, Nmiss_f, sp_a10, sp_b13, sp_b14))
#VidhorfForeldraUtanHBS <- filter(gagnarammi, hopur == 1, Hof_land == 2)
#VidhorfForeldraUtanHBS <- dplyr::select(VidhorfForeldraUtanHBS, !c(id, ID2, hopur, stard_sk, Strf_ald, stadsetn, Hof_land, Ladhl, nmiss, Nmiss_f, sp_a10, sp_b13, sp_b14))


#Starfsaldur
#ellefu_plus <- filter(VidhorfStarfsfolks, Strf_ald == 3)
#sex_til_tiu <- filter(VidhorfStarfsfolks, Strf_ald == 2)
#null_til_fimm <- filter(VidhorfStarfsfolks, Strf_ald == 1)

#-------------------Taka burt raðir þá sem svöruðu engu------------------------
NAgildi <- rowSums(is.na(VidhorfForeldra)) >=25
tilbuinnGogn_Foreldrar <- VidhorfForeldra[!NAgildi, ]
sum(is.na(tilbuinnGogn_Foreldrar))

#NAgildi <- rowSums(is.na(VidhorfForeldraUtanHBS)) >=25
#tilbuinnGogn_ForeldrarUtanHBS <- gagnarammi[!NAgildi, ]
#sum(is.na(tilbuinnGogn_ForeldrarUtanHBS))


#----------------------------------------Tilreikningur----------------------------------------
imputationgogn_kennarar <- mice(VidhorfStarfsfolks, m=3, method="pmm", maxit = 3)
imputationgogn_kennarar <- complete(imputationgogn_kennarar, 3)
sum(is.na(imputationgogn_kennarar))


imputationgogn_foreldrar <- mice(tilbuinnGogn_Foreldrar, m=3, method="pmm", maxit = 3)
imputationgogn_foreldrar <- complete(imputationgogn_foreldrar, 3)
sum(is.na(imputationgogn_foreldrar))


##-------------------------------Flokka spurningarnar----------------------------------

# Hlutverk námsmats foreldrar 
hlutverk_namsmats_foreldrar <- select(imputationgogn_foreldrar, c(sp_b01, sp_b02, sp_b03, 
                                                                  sp_b04, sp_b05, sp_b06, 
                                                                  sp_b07, sp_b08, sp_b09, 
                                                                  sp_b10, sp_b11, sp_b12))
# Heild foreldrar
heild_hlutverk_namsmats_foreldar <- rowSums(hlutverk_namsmats_foreldrar)
describe(heild_hlutverk_namsmats_foreldar)

psych::alpha(hlutverk_namsmats_foreldrar)
psych::alpha(heild_hlutverk_namsmats_foreldar)

# Hlutverk námsmats kennarar
hlutverk_namsmats_kennarar <- select(imputationgogn_kennarar, c(sp_b01, sp_b02, sp_b03, sp_b04, 
                                                                sp_b05, sp_b06, sp_b07,
                                                                sp_b08, sp_b09, sp_b10, 
                                                                sp_b11, sp_b12, sp_b13, sp_b14))
## Heildartala kennarar
heild_hlutverk_namsmats_kennarar <- rowSums(hlutverk_namsmats_kennarar)
describe(heild_hlutverk_namsmats_kennarar)

# Mikllvægi upplýsinga og um framkvæmd mats í skólum foreldrar 
upplys_mat_foreldrar <- select(imputationgogn_foreldrar, c(sp_a01, sp_a02, sp_a03, 
                                                           sp_a04, sp_a05, sp_a06, 
                                                           sp_a07, sp_a08, sp_a09))
heild_upplys_mat_foreldrar <- rowSums(upplys_mat_foreldrar)
describe(heild_upplys_mat_foreldrar)


# Mikilvægi upplýsinga og um framkvæmd mat í skólum kennarar
upplys_mat_kennarar <- select(imputationgogn_kennarar, c(sp_a01, sp_a02, sp_a03, 
                                                         sp_a04, sp_a05, sp_a06, 
                                                         sp_a07, sp_a08,
                                                         sp_a09, sp_a10))
heild_upplys_mat_kennarar <- rowSums(upplys_mat_kennarar)
describe(heild_upplys_mat_kennarar)

# Lykilhæfni foreldrar
lykilhaefni_foreldrar <- select(imputationgogn_foreldrar, c(sp_c01, sp_c02, sp_c03, sp_c04))

heild_lykilhaefni_foreldrar <- rowSums(lykilhaefni_foreldrar)
describe(heild_lykilhaefni_foreldrar)

# Lykilhæfni kennarar
lykilhaefni_kennarar <- select(imputationgogn_kennarar, c(sp_c01, sp_c02, sp_c03, sp_c04))

heild_lykilhaefni_kennarar <- rowSums(lykilhaefni_kennarar)
describe(heild_lykilhaefni_kennarar)


#----------------------------------------Svarferlalíkan----------------------------------------
# Foreldrar 
Hlutverk_nams_foreldrar_likan <- mirt(hlutverk_namsmats_foreldrar, itemtype = "2PL")
mirt::coef(Hlutverk_nams_foreldrar_likan, simplify = T)
summary(Hlutverk_nams_foreldrar_likan)
itemfit(Hlutverk_nams_foreldrar_likan)
itemplot(Hlutverk_nams_foreldrar_likan, 3)
itemplot(Hlutverk_nams_foreldrar_likan, 3, type = 'threshold', )

# Kennarar 
Hlutverk_nams_kennarar_likan <- mirt(hlutverk_namsmats_kennarar, itemtype = "2PL")
coef(Hlutverk_nams_kennarar_likan, simplify = T, IRTpar=T)
summary(Hlutverk_nams_kennarar_likan)
itemfit(Hlutverk_nams_kennarar_likan)
itemplot(Hlutverk_nams_kennarar_likan, 3)
itemplot(Hlutverk_nams_kennarar_likan, 3, type = 'threshold', )


# Sameiginlegt plot 
key=list(columns=1,text=list(lab=c("Starfsfólk skóla"," Forsjáraðilar")), 
         lines=list(lwd=2, col=c("blue","red")), space="right")
HN1 = plot(Hlutverk_nams_kennarar_likan,key=key, main="")
HN2 = update(plot(Hlutverk_nams_foreldrar_likan),col="red")
HN1+HN2


# Dreifing í færni próftaka (stuðlarit)
faerni_hlutverk_nams_foreldrar <- fscores(Hlutverk_nams_foreldrar_likan, full.scores = T, full.scores.SE = T)
hist(faerni_hlutverk_nams_foreldrar, xlim = c(-3, 3), ylim = c(0, 1000), breaks = 10, ylab = "Tíðni",
     xlab = "Færni", main="", col = "lightgrey", border = "black")


h_foreldrar <- hist(faerni_hlutverk_nams_foreldrar,border = "white", col= "grey", 
                   xlab = "Færni", ylab = "Tíðni", main = NULL, xlim = c(-3, 3), 
                   breaks = 10, ylim = c(0, 1000)) 
xfit <- seq(min(faerni_hlutverk_nams_foreldrar), max(faerni_hlutverk_nams_foreldrar), length = 6) 
yfit <- dnorm(xfit_h_foreldrar, mean = mean(faerni_hlutverk_nams_foreldrar), sd = sd(faerni_hlutverk_nams_foreldrar) + 0.5) 
yfit <- yfit_h_foreldrar * diff(h$mids[2:1]) * (length(faerni_hlutverk_nams_foreldrar))
lines(xfit, yfit, col = "black", lwd = 2)


faerni_hlutverk_nams_kennarar <- fscores(Hlutverk_nams_kennarar_likan, full.scores = T, full.scores.SE = T)
hist(faerni_hlutverk_nams_kennarar, xlim = c(-3, 3), xlab = "Færni", ylab = "Tíðni", main="")

h_kennarar <- hist(faerni_hlutverk_nams_kennarar,border = "white", col= "grey", 
          xlab = "Færni", ylab = "Tíðni", main = NULL, xlim = c(-3, 3), 
          breaks = 10, ylim = c(0, 100)) 
xfit_h_kennarar <- seq(min(faerni_hlutverk_nams_kennarar), max(faerni_hlutverk_nams_kennarar), length = 6) 
yfit_h_kennarar <- dnorm(xfit, mean = mean(faerni_hlutverk_nams_kennarar), sd = sd(faerni_hlutverk_nams_kennarar) + 0.5) 
yfit_h_kennarar <- yfit * diff(h$mids[2:1]) * (length(faerni_hlutverk_nams_kennarar))
lines(xfit, yfit, col = "black", lwd = 2)

# C liður

# Fylgni og samdreifing fyrir færni og summu (scatterplot)

# Tengsl færni og staðalvillu færni 

key=list(columns=1,text=list(lab=c("Starfsfólk skóla"," Forsjáraðilar")), 
         lines=list(lwd=2, col=c("blue","red")), space="right")
HNF1 = plot(faerni_hlutverk_nams_kennarar,key=key, main="")
HNF2 = update(plot(faerni_hlutverk_nams_foreldrar),col="red")
HNF1+HNF2

plot(faerni_hlutverk_nams_kennarar)
plot(faerni_hlutverk_nams_foreldrar)

# Mat á stikum líkans 
M2(Hlutverk_nams_foreldrar_likan)
M2(Hlutverk_nams_kennarar_likan)

# Leifar líkans og kí-kvaðratpróf

# Mat á stikum og staðalvillur (scatterplot)

# Mat á færni og staðalvillu (scatterplot)

#-------------------------Svarferlalíkan 2 ---------------------------------

Upplys_mat_foreldrar_likan <- mirt(upplys_mat_foreldrar, itemtype = "2PL")
mirt::coef(Upplys_mat_foreldrar_likan, simplify = T, IRTpar = T)
summary(Upplys_mat_foreldrar_likan)
itemfit(Upplys_mat_foreldrar_likan)
itemplot(Upplys_mat_foreldrar_likan, 3)
itemplot(Upplys_mat_foreldrar_likan, 3, type = 'threshold', )

Upplys_mat_kennarar_likan <- mirt(upplys_mat_kennarar, itemtype = "2PL")
coef(Upplys_mat_kennarar_likan, simplify = T, IRTpar = T)
summary(Upplys_mat_kennarar_likan)
itemfit(Upplys_mat_kennarar_likan)
itemplot(Upplys_mat_kennarar_likan, 3)
itemplot(Upplys_mat_kennarar_likan, 3, type = 'threshold', )

# Sameiginlegt plot 
key=list(columns=1,text=list(lab=c("Starfsfólk skóla"," Forsjáraðilar")), lines=list(lwd=2, col=c("blue","red")), space="right")
HN1 = plot(Upplys_mat_kennarar_likan,key=key, main="")
HN2 = update(plot(Upplys_mat_foreldrar_likan),col="red")
HN1+HN2


# Dreifing í færni próftaka (stuðlarit)
faerni_upplys_mat_foreldrar <- fscores(Upplys_mat_foreldrar_likan, full.scores = T, full.scores.SE = T)
hist(faerni_upplys_mat_foreldrar, xlim = c(-3.0, 3.0))

faerni_upplys_mat_kennara <- fscores(Upplys_mat_kennarar_likan, full.scores = T, full.scores.SE = T)
hist(faerni_upplys_mat_kennara, breaks=10, xlim = c(-2.0, 2.0))


# C liður

# Fylgni og samdreifing fyrir færni og summu (scatterplot)

# Tengsl færni og staðalvillu færni 


# Mat á stikum líkans 
M2(Upplys_mat_foreldrar_likan)
M2(Upplys_mat_kennarar_likan)


# Leifar líkans og kí-kvaðratpróf

# Mat á stikum og staðalvillur (scatterplot)

# Mat á færni og staðalvillu (scatterplot)

#-----------------------------Svarferlalíkan 3-------------------------

Lykilhaefni_foreldrar_likan <- mirt(lykilhaefni_foreldrar, itemtype = "2PL")
mirt::coef(Lykilhaefni_foreldrar_likan, simplify = T, IRTpar=T)
summary(Lykilhaefni_foreldrar_likan)
itemfit(Lykilhaefni_foreldrar_likan)
itemplot(Lykilhaefni_foreldrar_likan, 3)
itemplot(Lykilhaefni_foreldrar_likan, 3, type = 'threshold', )


Lykilhaefni_kennarar_likan <- mirt(lykilhaefni_kennarar, itemtype = "2PL")
coef(Lykilhaefni_kennarar_likan, simplify = T)
summary(Lykilhaefni_kennarar_likan)
itemfit(Lykilhaefni_kennarar_likan)
itemplot(Lykilhaefni_kennarar_likan, 3)
itemplot(Lykilhaefni_kennarar_likan, 3, type = 'threshold', )

# Sameiginlegt plot 
key=list(columns=1,text=list(lab=c("Starfsfólk skóla","Forsjáraðila")), lines=list(lwd=2, col=c("blue","red")), space="right")
HN1 = plot(Lykilhaefni_kennarar_likan,key=key, main="")
HN2 = update(plot(Lykilhaefni_foreldrar_likan),col="red")
HN1+HN2


# Dreifing í færni próftaka (stuðlarit)
faerni_lykilhaefni_kennara <- fscores(Lykilhaefni_kennarar_likan, full.scores = T, full.scores.SE = T)
hist(faerni_lykilhaefni_kennara, breaks = 10, xlim = c(-2.0, 2.0))
mean(faerni_lykilhaefni_kennara)


faerni_lykilhaefni_foreldra <- fscores(Lykilhaefni_foreldrar_likan, full.scores = T, full.scores.SE = T)
hist(faerni_lykilhaefni_foreldra, breaks = 10, xlim = c(-2.0, 2.0))
summary(faerni_lykilhaefni_foreldra)

plot(faerni_lykilhaefni_foreldra)
plot(faerni_lykilhaefni_kennara)

# C liður

# Fylgni og samdreifing fyrir færni og summu (scatterplot)

# Tengsl færni og staðalvillu færni 



# Mat á stikum líkans 
M2(Lykilhaefni_foreldrar_likan)
M2(Lykilhaefni_kennarar_likan)

# Leifar líkans og kí-kvaðratpróf

# Mat á stikum og staðalvillur (scatterplot)

# Mat á færni og staðalvillu (scatterplot)




