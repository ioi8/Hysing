
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


colnames(gagnarammi) <- c("id", 'ID2', 'hopur', 'stard_sk', 'Strf_ald',
                          'stadsetn', 'Hof_land', 'Ladhl', 'A10', 'B13', 'B14',
                          'A1', 'C4', 'B1', 'B2', 'B3', 'B4', 'A2', 'A3', 
                          'A4', 'A5', 'A6', 'A7', 'A8', 'A9', 'B5', 'B6',
                          'B7', 'B8', 'B9', 'B10', 'B11', 'B12', 'C1', 'C2', 'C3',
                          'nmiss', 'Nmiss_f')

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
                                                     Nmiss_f, A10, B13, B14)) #!spr fyrir kennara


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

#NAgildi <- rowSums(is.na(Baraspurningar)) >=25
#tilbuinnGogn_heild <- Baraspurningar[!NAgildi, ]
#sum(is.na(tilbuinnGogn_heild))


#----------------------------------------Tilreikningur----------------------------------------
imputationgogn_kennarar <- mice(VidhorfStarfsfolks, m=3, method="pmm", maxit = 3)
imputationgogn_kennarar <- complete(imputationgogn_kennarar, 3)
sum(is.na(imputationgogn_kennarar))


imputationgogn_foreldrar <- mice(tilbuinnGogn_Foreldrar, m=3, method="pmm", maxit = 3)
imputationgogn_foreldrar <- complete(imputationgogn_foreldrar, 3)
sum(is.na(imputationgogn_foreldrar))


#imputationgogn_heild <- mice(tilbuinnGogn_heild, m=3, method="pmm", maxit = 3)
#imputationgogn_heild <- complete(imputationgogn_heild, 3)
#sum(is.na(imputationgogn_heild))

##-------------------------------Flokka spurningarnar----------------------------------

# Hlutverk námsmats foreldrar 
hlutverk_namsmats_foreldrar <- select(imputationgogn_foreldrar, c(B1, B2, B3, 
                                                                  B4, B5, 
                                                                  B7, B8, B9, 
                                                                  B10, B11, B12))
# Heild foreldrar
heild_hlutverk_namsmats_foreldar <- rowSums(hlutverk_namsmats_foreldrar)
describe(heild_hlutverk_namsmats_foreldar)

psych::alpha(hlutverk_namsmats_foreldrar)
psych::alpha(heild_hlutverk_namsmats_foreldar)

# Hlutverk námsmats kennarar
hlutverk_namsmats_kennarar <- select(imputationgogn_kennarar, c(B1, B2, B3, 
                                                                B4, B5, 
                                                                B7, B8, B9, 
                                                                B10, B11, B12, B13, B14))
## Heildartala kennarar
heild_hlutverk_namsmats_kennarar <- rowSums(hlutverk_namsmats_kennarar)
describe(heild_hlutverk_namsmats_kennarar)

# Mikllvægi upplýsinga og um framkvæmd mats í skólum foreldrar 
upplys_mat_foreldrar <- select(imputationgogn_foreldrar, c(A1, A2, A3, 
                                                           A4, A5, A6, 
                                                           A7, A8, A9))
heild_upplys_mat_foreldrar <- rowSums(upplys_mat_foreldrar)
describe(heild_upplys_mat_foreldrar)


# Mikilvægi upplýsinga og um framkvæmd mat í skólum kennarar
upplys_mat_kennarar <- select(imputationgogn_kennarar, c(A1, A2, A3, 
                                                         A4, A5, A6, 
                                                         A7, A8, A9, A10))
heild_upplys_mat_kennarar <- rowSums(upplys_mat_kennarar)
describe(heild_upplys_mat_kennarar)

# Lykilhæfni foreldrar
lykilhaefni_foreldrar <- select(imputationgogn_foreldrar, c(C1, C2, C3, C4, B6))

heild_lykilhaefni_foreldrar <- rowSums(lykilhaefni_foreldrar)
describe(heild_lykilhaefni_foreldrar)

# Lykilhæfni kennarar
lykilhaefni_kennarar <- select(imputationgogn_kennarar, c(C1, C2, C3, C4, B6))

heild_lykilhaefni_kennarar <- rowSums(lykilhaefni_kennarar)
describe(heild_lykilhaefni_kennarar)

#----------------------------------------Svarferlalíkan----------------------------------------
# Foreldrar 
Hlutverk_nams_foreldrar_likan <- mirt(hlutverk_namsmats_foreldrar, itemtype = "2PL")
mirt::coef(Hlutverk_nams_foreldrar_likan, simplify = T, IRTpars=T)
summary(Hlutverk_nams_foreldrar_likan)
itemfit(Hlutverk_nams_foreldrar_likan)
itemplot(Hlutverk_nams_foreldrar_likan, 3)
itemplot(Hlutverk_nams_foreldrar_likan, 3, type = 'threshold', )
plot(Hlutverk_nams_foreldrar_likan, which.items=1:11, facet_items=F, type='trace', 
     main="Forsjáraðilar")

# Kennarar 
Hlutverk_nams_kennarar_likan <- mirt(hlutverk_namsmats_kennarar, itemtype = "2PL")
coef(Hlutverk_nams_kennarar_likan, simplify = T, IRTpars=T)
summary(Hlutverk_nams_kennarar_likan)
itemfit(Hlutverk_nams_kennarar_likan)
itemplot(Hlutverk_nams_kennarar_likan, 3)
itemplot(Hlutverk_nams_kennarar_likan, 3, type = 'threshold', )
plot(Hlutverk_nams_kennarar_likan, which.items=1:13, facet_items=F, type='trace', 
     main="Starfsfólk skóla")

# Sameiginlegt plot 
key=list(columns=2,text=list(lab=c("Starfsfólk skóla"," Forsjáraðilar")), 
         lines=list(lwd=2, col=c("blue","red")), space="top")
HN1 = plot(Hlutverk_nams_kennarar_likan,key=key, main="")
HN2 = update(plot(Hlutverk_nams_foreldrar_likan),col="red")
HN1+HN2


# Dreifing í færni próftaka
faerni_hlutverk_nams_foreldrar <- fscores(Hlutverk_nams_foreldrar_likan, full.scores = T)
p1 <- hist(faerni_hlutverk_nams_foreldrar, xlim = c(-3, 3), breaks = 10, ylab = "Tíðni",
     xlab = "Færni", main="", col = "lightgrey", border = "black")
heild_faerni_HN_f <- rowSums(faerni_hlutverk_nams_foreldrar)


faerni_hlutverk_nams_kennarar <- fscores(Hlutverk_nams_kennarar_likan, full.scores=T)
p2 <- hist(faerni_hlutverk_nams_kennarar, xlim = c(-3, 3), xlab = "Færni", ylab = "Tíðni", main="")
heild_faerni_HN_k <- rowSums(faerni_hlutverk_nams_kennarar)


plot(p1, col="#e9ecef", xlim=c(-3.0,3.0), main="",xlab="θ", ylab="Tíðni")
plot(p2, col= "lightblue", xlim=c(-3.0,3.0), add=T)
par(xpd=TRUE)
legend(x='top', c('Forsjáraðilar', 'Starfsfólk skóla'), fill=c('#e9ecef', 'lightblue'), 
       border="black", box.lty = 0, bty = "n", ncol=2, inset=-0.1, cex=1)


# Fylgni og samdreifing fyrir færni og summu (scatterplot)
plot(faerni_hlutverk_nams_foreldrar, heild_hlutverk_namsmats_foreldar, col="blue", ylim=c(0, 15), xlim=c(-3,3), xlab="θ", ylab="Summutala")
points(faerni_hlutverk_nams_kennarar, heild_hlutverk_namsmats_kennarar, col="red", pch=1)
legend(x='top', c('Forsjáraðilar', 'Starfsfólk skóla'), fill=c('blue', 'red'), 
       border="black", box.lty = 0, bty = "n", ncol=2, inset=-0.15, cex=1)


cor(faerni_hlutverk_nams_foreldrar, heild_hlutverk_namsmats_foreldar)
cor(faerni_hlutverk_nams_kennarar, heild_hlutverk_namsmats_kennarar)

# Tengsl færni og staðalvillu færni 
faerni_hlutverk_nams_foreldrar_stadalvilla <- fscores(Hlutverk_nams_foreldrar_likan, full.scores = T, full.scores.SE = T)
faerni_hlutverk_nams_kennarar_stadalvilla <- fscores(Hlutverk_nams_kennarar_likan, full.scores=T, full.scores.SE = T)

heild_faerni_HN_f_stadalvilla <- rowSums(faerni_hlutverk_nams_foreldrar_stadalvilla)
describe(heild_faerni_HN_f_stadalvilla)

heild_faerni_HN_k_stadalvilla <- rowSums(faerni_hlutverk_nams_kennarar_stadalvilla)
describe(heild_faerni_HN_k_stadalvilla)

plot(faerni_hlutverk_nams_foreldrar_stadalvilla,col="blue", xlim=c(-3.0,3.0), main="", xlab="θ", ylab="Staðalvilla θ")
points(faerni_hlutverk_nams_kennarar_stadalvilla, col='red', pch=1)
legend(x='top', c('Forsjáraðilar', 'Starfsfólk skóla'), fill=c('blue', 'red'), 
       border="black", box.lty = 0, bty = "n", ncol=2, inset=-0.15, cex=1)


# Mat á stikum líkans 

# Leifar líkans og kí-kvaðratpróf
M2(Hlutverk_nams_foreldrar_likan)
M2(Hlutverk_nams_kennarar_likan)

# Mat á stikum og staðalvillur (scatterplot)

# Mat á færni og staðalvillu (scatterplot)

#-------------------------Svarferlalíkan 2 ---------------------------------

Upplys_mat_foreldrar_likan <- mirt(upplys_mat_foreldrar, itemtype = "graded")
mirt::coef(Upplys_mat_foreldrar_likan, simplify = T, IRTpars=T)
summary(Upplys_mat_foreldrar_likan)
itemfit(Upplys_mat_foreldrar_likan)
itemplot(Upplys_mat_foreldrar_likan, 3)
itemplot(Upplys_mat_foreldrar_likan, 3, type = 'threshold', )
plot(Upplys_mat_foreldrar_likan, which.items=1:9, facet_items=F, type='trace', 
     main="Forsjáraðilar")

Upplys_mat_kennarar_likan <- mirt(upplys_mat_kennarar, itemtype = "graded")
coef(Upplys_mat_kennarar_likan, simplify = T, IRTpars=T)
summary(Upplys_mat_kennarar_likan)
itemfit(Upplys_mat_kennarar_likan)
itemplot(Upplys_mat_kennarar_likan, 3)
itemplot(Upplys_mat_kennarar_likan, 3, type = 'threshold', )
plot(Upplys_mat_kennarar_likan, which.items=1:10, facet_items=F, type='trace', 
     main="Forsjáraðilar")

# Sameiginlegt plot 
key=list(columns=2,text=list(lab=c("Starfsfólk skóla"," Forsjáraðilar")), lines=list(lwd=2, col=c("blue","red")), space="top")
HN1 = plot(Upplys_mat_kennarar_likan,key=key, main="")
HN2 = update(plot(Upplys_mat_foreldrar_likan),col="red")
HN1+HN2


# Dreifing í færni próftaka (stuðlarit)

faerni_upplys_mat_foreldrar <- fscores(Upplys_mat_foreldrar_likan, full.scores = T)
p3 <- hist(faerni_upplys_mat_foreldrar,breaks = 10, xlim = c(-3.0, 3.0), main="", xlab="θ", ylab="Tíðni", col = "#e9ecef")

faerni_upplys_mat_kennara <- fscores(Upplys_mat_kennarar_likan, full.scores = T)
p4 <- hist(faerni_upplys_mat_kennara, breaks=10, xlim = c(-3.0, 3.0), main="", xlab="θ", ylab="Tíðni", col = "lightblue", add=T)

plot(p3, col="#e9ecef", xlim=c(-3.0,3.0), main="", xlab="θ", ylab="Tíðni")
plot(p4, col= "lightblue", xlim=c(-3.0,3.0), add=T)
par(xpd=TRUE)
legend(x='top', c('Forsjáraðilar', 'Starfsfólk skóla'), fill=c('#e9ecef', 'lightblue'), 
       border="black", box.lty = 0, bty = "n", ncol=2, inset=-0.1, cex=1)


# Fylgni og samdreifing fyrir færni og summu (scatterplot)
plot(faerni_upplys_mat_foreldrar, heild_upplys_mat_foreldrar, col="blue", ylim=c(0, 15), xlim=c(-3,3), xlab="θ", ylab="Summutala")
points(faerni_upplys_mat_kennara, heild_upplys_mat_kennarar, col="red", pch=1)
legend(x='top', c('Forsjáraðilar', 'Starfsfólk skóla'), fill=c('blue', 'red'), 
       border="black", box.lty = 0, bty = "n", ncol=2, inset=-0.15, cex=1)

cor(faerni_upplys_mat_foreldrar, heild_upplys_mat_foreldrar)
cor(faerni_upplys_mat_kennara, heild_upplys_mat_kennarar)

# Tengsl færni og staðalvillu færni 

faerni_upplys_mat_foreldrar_stadalvilla <- fscores(Upplys_mat_foreldrar_likan, full.scores.SE = T)
faerni_upplys_mat_kennara_stadalvilla <- fscores(Upplys_mat_kennarar_likan, full.scores.SE = T)

heild_faerni_UM_f_stadalvilla <- rowSums(faerni_upplys_mat_foreldrar_stadalvilla)
describe(heild_faerni_UM_f_stadalvilla)

plot(faerni_upplys_mat_foreldrar_stadalvilla, col="red", xlim=c(-3.0,3.0), ylim=c(0.0, 0.7), main="", xlab="θ", ylab="Staðalvilla θ")
points(faerni_upplys_mat_kennara_stadalvilla, col='blue', pch=1)
legend(x='top', c('Forsjáraðilar', 'Starfsfólk skóla'), fill=c('red', 'blue'), 
       border="black", box.lty = 0, bty = "n", ncol=2, inset=-0.15, cex=1)


# Mat á stikum líkans 

# Leifar líkans og kí-kvaðratpróf
M2(Upplys_mat_foreldrar_likan)
M2(Upplys_mat_kennarar_likan)


# Mat á stikum og staðalvillur (scatterplot)

# Mat á færni og staðalvillu (scatterplot)

#-----------------------------Svarferlalíkan 3-------------------------

Lykilhaefni_foreldrar_likan <- mirt(lykilhaefni_foreldrar, itemtype = "2PL")
mirt::coef(Lykilhaefni_foreldrar_likan, simplify = T, IRTpars=T)
summary(Lykilhaefni_foreldrar_likan)
itemfit(Lykilhaefni_foreldrar_likan)
itemplot(Lykilhaefni_foreldrar_likan, 3)
itemplot(Lykilhaefni_foreldrar_likan, 3, type = 'threshold', )
plot(Lykilhaefni_foreldrar_likan, which.items=1:5, facet_items=F, type='trace', 
     main="Forsjáraðilar")

Lykilhaefni_kennarar_likan <- mirt(lykilhaefni_kennarar, itemtype = "2PL")
coef(Lykilhaefni_kennarar_likan, simplify = T, IRTpars=T)
summary(Lykilhaefni_kennarar_likan)
itemfit(Lykilhaefni_kennarar_likan)
itemplot(Lykilhaefni_kennarar_likan, 3)
itemplot(Lykilhaefni_kennarar_likan, 3, type = 'threshold', )
plot(Lykilhaefni_kennarar_likan, which.items=1:5, facet_items=F, type='trace', 
     main="Starfsfólk skóla")

# Sameiginlegt plot 
key=list(columns=2,text=list(lab=c("Starfsfólk skóla","Forsjáraðila")), lines=list(lwd=2, col=c("blue","red")), space="top")
HN1 = plot(Lykilhaefni_kennarar_likan,key=key, main="")
HN2 = update(plot(Lykilhaefni_foreldrar_likan),col="red")
HN1+HN2


# Dreifing í færni próftaka (stuðlarit)
faerni_lykilhaefni_kennara <- fscores(Lykilhaefni_kennarar_likan, full.scores = T)
p6 <- hist(faerni_lykilhaefni_kennara, breaks=5, xlim = c(-3.0, 3.0), main="", xlab="θ", ylab="Tíðni", col = "lightblue")
heild_faerni_lykilhaefni_kennara <- rowSums(faerni_lykilhaefni_kennara)

plot(faerni_lykilhaefni_kennara, heild_lykilhaefni_kennarar)

faerni_lykilhaefni_foreldra <- fscores(Lykilhaefni_foreldrar_likan, full.scores = T)
p5 <- hist(faerni_lykilhaefni_foreldra,breaks = 5, xlim = c(-3.0, 3.0), main="", xlab="θ", ylab="Tíðni", col = "#e9ecef")
summary(faerni_lykilhaefni_foreldra)

plot(p5, col="#e9ecef", xlim=c(-3.0,3.0), main="", xlab="θ", ylab="Tíðni")
plot(p6, col= "lightblue", xlim=c(-3.0,3.0), add=T)
legend(x='top', c('Forsjáraðilar', 'Starfsfólk skóla'), fill=c('#e9ecef', 'lightblue'), 
       border="black", box.lty = 0, bty = "n", ncol=2, inset=-0.1, cex=1)


# Fylgni og samdreifing fyrir færni og summu (scatterplot)
plot(faerni_lykilhaefni_foreldra, heild_lykilhaefni_foreldrar, col="blue", ylim=c(0, 5), xlim=c(-3,3), xlab="θ", ylab="Summutala")
points(faerni_lykilhaefni_kennara, heild_lykilhaefni_kennarar, col="red", pch=1)
legend(x='top', c('Forsjáraðilar', 'Starfsfólk skóla'), fill=c('blue', 'red'), 
       border="black", box.lty = 0, bty = "n", ncol=2, inset=-0.15, cex=1)

cor(faerni_lykilhaefni_foreldra, heild_lykilhaefni_foreldrar)
cor(faerni_lykilhaefni_kennara, heild_lykilhaefni_kennarar)

# Tengsl færni og staðalvillu færni 
faerni_lykilhaefni_kennara_stadalvilla <- fscores(Lykilhaefni_kennarar_likan, full.scores = T, full.scores.SE = T)
faerni_lykilhaefni_foreldra_stadalvilla <- fscores(Lykilhaefni_foreldrar_likan, full.scores = T, full.scores.SE = T)

heild_faerni_LH_K_stadalvilla <- rowSums(faerni_lykilhaefni_kennara_stadalvilla)
describe(heild_faerni_LH_K_stadalvilla)

heild_faerni_LH_f_stadalvilla <- rowSums(faerni_lykilhaefni_foreldra_stadalvilla)
describe(heild_faerni_LH_f_stadalvilla)

plot(faerni_lykilhaefni_foreldra_stadalvilla, col="red", xlim=c(-3.0,3.0), main="", xlab="θ", ylab="Staðalvilla θ", pch=1)
points(faerni_lykilhaefni_kennara_stadalvilla, col='blue', pch=1)
legend(x='top', c('Forsjáraðilar', 'Starfsfólk skóla'), fill=c('red', 'blue'), 
       border="black", box.lty = 0, bty = "n", ncol=2, inset=-0.15, cex=1)


# Mat á stikum líkans 
# Leifar líkans og kí-kvaðratpróf
M2(Lykilhaefni_foreldrar_likan)
M2(Lykilhaefni_kennarar_likan)


# Mat á stikum og staðalvillur (scatterplot)

# Mat á færni og staðalvillu (scatterplot)




