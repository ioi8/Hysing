"Verkefni 201
A: Spurningar um  mikilvægi upplýsigna og um framkvæmd mats í skólum 10 atriði
B: Spurningar um hlutverk námsmats 14  atriði
C: Spurningar um  mat tengt lykilhæfni menntunar í aðalnámsskrá 4 atriði

Tveim breytum var snúið við:
- Mér finnst að skólar ættu einir að sjá um mat á námsárangri nemenda sinna.
- Mér finnst hlutur námsmats í skólastarfi of mikill.

tilgátur:
  - Hvernig er viðhorf smærri skóla til náms vs. stærri
- Viðhorf kennara til náms m.t.t. starfsaldur
- Viðhorf til náms eftir landhluta (Norðvesturkjör, Norðaustur, Suðurkjördæmi, HBS)"

rm(list =  ls()) 
graphics.off() 
cat("\014") 
#----------------------------------------Pakkar----------------------------------------
library(mirt)
library(tidyverse)
library(parallel)
library(readr)
library(mice)
library(mirt)
library(tidyverse)
#----------------------------------------Gögn----------------------------------------
gagnarammi <- read_delim("./Documents/GitHub/Megindleg.github.io/Undirliggjandi/DATA_SAL138F_IRT_Skulason.2022_Vidhorf.Til.Namsmats_v28n3404.binay.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE, na = c("-999"))

#-------------------Taka burt raðir með fleiri en 4 N/A gildum í spurningum-------------------------
NAgildi <- rowSums(is.na(gagnarammi)) >= 28
tilbuinnGogn <- gagnarammi[!NAgildi, ]
sum(is.na(tilbuinnGogn))

#----------------------------------------Breytur----------------------------------------
#ATH AÐEINS KEYRA EINA BAKGRUNSBREYTU Í SENN!
##---------------------------------------Stærð skóla------------------------------------

#StorSkoli <- filter(gagnarammi, stard_sk == 51)
#litillSkoli <- filter(gagnarammi,stard_sk == 1 & 11 & 21 & 31 & 41)
##----------------------------------------Staðsetning------------------------------------
HBS <- filter(gagnarammi, Ladhl == 1)
#NordVest <- filter(gagnarammi, Ladhl == 3)
#NordAust <- filter(gagnarammi, Ladhl == 6)
#Sud <- filter(gagnarammi, Ladhl == 8)
UtanHBS <- filter(gagnarammi, Hof_land == 2)

##----------------------------------------Viðhorf------------------------------------------
VidhorStarfsfólks<- filter(gagnarammi,hopur == 2)
VidhorfForsjá <- filter(gagnarammi, hopur == 1)
VidhorfForsjá <- dplyr::select(gagnarammi, !c(sp_a10, sp_b05, sp_b14)) #!spr fyrir kennara
###----------------------------------------Starfsaldur--------------------------------------
#"11lengur" <- filter(VidhorfKennara, Strf_ald == 3)
#"6til10" <- filter(VidhorfKennara,Strf_ald == 2)
#"0til5" <- filter(VidhorfKennara,Strf_ald == 1)


#----------------------------------------Veljum spurningar-----------------------------------
##---Spurningar um  mikilvægi upplýsigna og um framkvæmd mats í skólum-----------------------


#Settu hér inn í stað "gagnarammi" breytuheiti á viðeigandi hópi, t.d. viðhorfkennara
hlutverk_namsmats1 <- dplyr::select(VidhorStarfsfólks, c(sp_b01, #hve vel nemendur eru búnir undir daglegt líf og starf á fullorðinsárum
                                                 sp_b02, #hve vel nemendur eru búnir undir nám í framhaldsskóla.
                                                 sp_b03, #hvort nemendur þurfi á sérstökum stuðningi að halda.
                                                 sp_b04, #hvort nemendur þurfi fleiri áskoranir í námi.
                                                 sp_b05, #námsframvindu nemenda milli skólaára.
                                                 sp_b06, #skilning nemenda á sjálfbærni.
                                                 sp_b07, #styrk- og veikleika nemenda við upphaf grunnskóla.
                                                 sp_b08, #stöðu nemenda í einstökum námsgreinum.
                                                 sp_b09, #stöðu nemenda í því námsefni sem unnið er með í kennslu hverju sinni.
                                                 sp_b10, #stöðu nemenda miðað við jafnaldra.
                                                 sp_b11, #stöðu nemenda út frá viðmiðum aðalnámskrár.
                                                 sp_b12, #stöðu nemenda þegar kennari eða forsjáraðili telur þörf á.
                                                 sp_b13, #námsframvindu nemenda á sama skólaári.
                                                 sp_b14  #hve vel nemendur eru búnir undir nám á næsta stigi grunnskólans
                                                 ))

hlutverk_namsmats2 <- dplyr::select(VidhorfForsjá, c(sp_b01, #hve vel nemendur eru búnir undir daglegt líf og starf á fullorðinsárum
                                                        sp_b02, #hve vel nemendur eru búnir undir nám í framhaldsskóla.
                                                        sp_b03, #hvort nemendur þurfi á sérstökum stuðningi að halda.
                                                        sp_b04, #hvort nemendur þurfi fleiri áskoranir í námi.
                                                        sp_b06, #skilning nemenda á sjálfbærni.
                                                        sp_b07, #styrk- og veikleika nemenda við upphaf grunnskóla.
                                                        sp_b08, #stöðu nemenda í einstökum námsgreinum.
                                                        sp_b09, #stöðu nemenda í því námsefni sem unnið er með í kennslu hverju sinni.
                                                        sp_b10, #stöðu nemenda miðað við jafnaldra.
                                                        sp_b11, #stöðu nemenda út frá viðmiðum aðalnámskrár.
                                                        sp_b12, #stöðu nemenda þegar kennari eða forsjáraðili telur þörf á.
                                                        sp_b13, #námsframvindu nemenda á sama skólaári.
))
##---------------------------------Spurningar um hlutverk námsmats------------------------------
upplys_mat1 <- dplyr::select(VidhorStarfsfólks, c(sp_a01, #Ég tel mikilvægt að utanaðkomandi aðili meti námsárangur nemenda til viðbótar við námsmat skólans.
                                          sp_a02, #Mér finnst að skólar ættu einir að sjá um mat á námsárangri nemenda sinna.
                                          sp_a03, #Mér finnst hlutur námsmats í skólastarfi of mikill.
                                          sp_a04, #Mér finnst námsmat nauðsynlegur hluti skólastarfs.
                                          sp_a05, #Mikilvægt er að framhaldsskólar sem það kjósa geti notað eigin próf við inntöku nýnema.
                                          sp_a06, #Mikilvægt er að kennarar í öllum skólum landsins hafi aðgang að sömu prófum og matstækjum.
                                          sp_a07, #Mikilvægt er að til séu upplýsingar um stöðu einstakra skóla.
                                          sp_a08, #Mikilvægt er að til séu upplýsingar um stöðu skóla í einstökum landshlutum eða sveitarfélögum.
                                          sp_a09, #Mikilvægt er að til séu upplýsingar um stöðu skólakerfisins almennt.
                                          sp_a10  #Ég á auðvelt með að meta frammistöðu nemenda samkvæmt viðmiðum aðalnámskrár.
                                          ))
upplys_mat2 <- dplyr::select(VidhorfForsjá, c(sp_a01, #Ég tel mikilvægt að utanaðkomandi aðili meti námsárangur nemenda til viðbótar við námsmat skólans.
                                           sp_a02, #Mér finnst að skólar ættu einir að sjá um mat á námsárangri nemenda sinna.
                                           sp_a03, #Mér finnst hlutur námsmats í skólastarfi of mikill.
                                           sp_a04, #Mér finnst námsmat nauðsynlegur hluti skólastarfs.
                                           sp_a05, #Mikilvægt er að framhaldsskólar sem það kjósa geti notað eigin próf við inntöku nýnema.
                                           sp_a06, #Mikilvægt er að kennarar í öllum skólum landsins hafi aðgang að sömu prófum og matstækjum.
                                           sp_a07, #Mikilvægt er að til séu upplýsingar um stöðu einstakra skóla.
                                           sp_a08, #Mikilvægt er að til séu upplýsingar um stöðu skóla í einstökum landshlutum eða sveitarfélögum.
                                           sp_a09, #Mikilvægt er að til séu upplýsingar um stöðu skólakerfisins almennt.
                                           sp_a10  #Ég á auðvelt með að meta frammistöðu nemenda samkvæmt viðmiðum aðalnámskrár.
))
##-----------------------------Spurningar um  mat tengt lykilhæfni manntunar í aðalnámsskrá-------
lykilhaefni1 <- dplyr::select(VidhorStarfsfólks, c(sp_c01, #velferð nemenda í skóla.
                                           sp_c02, #vinnubrögð nemenda og námstækni.
                                           sp_c03, #virkni nemenda í lýðræði og jafnrétti.
                                           sp_c04  #getu nemenda til sköpunar.
                                           ))
lykilhaefni2 <- dplyr::select(VidhorfForsjá, c(sp_c01, #velferð nemenda í skóla.
                                            sp_c02, #vinnubrögð nemenda og námstækni.
                                            sp_c03, #virkni nemenda í lýðræði og jafnrétti.
                                            sp_c04  #getu nemenda til sköpunar.
))


#----------------------------------------Svarferlalíkan----------------------------------------

hlutverk_namsmats1 <- mice(hlutverk_namsmats1, m=3, method="pmm", maxit = 3) #Tilreikningur
hlutverk_namsmats1 <- complete(hlutverk_namsmats1, 3)
hlutverk_namsmats1_Likan <- mirt(hlutverk_namsmats1, itemtype = "graded")
coef(hlutverk_namsmats1_Likan, simplify = T)
summary(hlutverk_namsmats1_Likan)
itemfit(hlutverk_namsmats1_Likan)
itemplot(hlutverk_namsmats1_Likan, 3)
itemplot(hlutverk_namsmats1_Likan, 3, type = 'threshold', )

hlutverk_namsmats2 <- mice(hlutverk_namsmats2, m=3, method="pmm", maxit = 3) #Tilreikningur
hlutverk_namsmats2 <- complete(hlutverk_namsmats2, 3)
hlutverk_namsmats2_Likan <- mirt(hlutverk_namsmats2, itemtype = "graded")
coef(hlutverk_namsmats2_Likan, simplify = T)
summary(hlutverk_namsmats2_Likan)
itemfit(hlutverk_namsmats2_Likan)
itemplot(hlutverk_namsmats2_Likan, 3)
itemplot(hlutverk_namsmats2_Likan, 3, type = 'threshold', )

key=list(columns=1, 
         text=list(lab=c("Kennarar"," Forráðamenn")), 
         lines=list(lwd=1, col=c("blue","red")))
HN1 = plot(hlutverk_namsmats1_Likan,key=key, main="Viðhorf kennara")
HN2 = update(plot(hlutverk_namsmats2_Likan, col="red"))
HN1+HN2

#----------------------------------------Svarferlalíkan----------------------------------------

upplys_mat1 <- mice(upplys_mat1, m=3, method="pmm", maxit = 3) #Tilreikningur
upplys_mat1 <- complete(upplys_mat1, 3)
upplys_mat1_Likan <- mirt(upplys_mat1, itemtype = "graded")
coef(upplys_mat1_Likan, simplify = T)
summary(upplys_mat1_Likan)
itemfit(upplys_mat1_Likan)
itemplot(upplys_mat1_Likan, 3)
itemplot(upplys_mat1_Likan, 3, type = 'threshold', )

upplys_mat2 <- mice(upplys_mat2, m=3, method="pmm", maxit = 3) #Tilreikningur
upplys_mat2 <- complete(upplys_mat2, 3)
upplys_mat2_Likan <- mirt(upplys_mat2, itemtype = "graded")
coef(hlutverk_namsmats2_Likan, simplify = T)
summary(hlutverk_namsmats2_Likan)
itemfit(hlutverk_namsmats2_Likan)
itemplot(hlutverk_namsmats2_Likan, 3)
itemplot(hlutverk_namsmats2_Likan, 3, type = 'threshold', )

key=list(columns=1, 
         text=list(lab=c("Kennarar"," Forráðamenn")), 
         lines=list(lwd=1, col=c("blue","red")))
UM1 = plot(upplys_mat1,key=key, main="Viðhorf kennara")
UM2 = update(plot(upplys_mat2,col="red"))
UM1+UM2

#----------------------------------------Svarferlalíkan----------------------------------------

lykilhaefni1 <- mice(lykilhaefni1, m=3, method="pmm", maxit = 3) #Tilreikningur
lykilhaefni1 <- complete(lykilhaefni1, 3)
lykilhaefni1_likan <- mirt(lykilhaefni1, itemtype = "graded")
coef(lykilhaefni1_likan, simplify = T)
summary(lykilhaefni1_likan)
itemfit(lykilhaefni1_likan)
itemplot(lykilhaefni1_likan, 3)
itemplot(lykilhaefni1_likan, 3, type = 'threshold', )

lykilhaefni2 <- mice(lykilhaefni2, m=3, method="pmm", maxit = 3) #Tilreikningur
lykilhaefni2 <- complete(lykilhaefni2, 3)
lykilhaefni2_likan <- mirt(lykilhaefni2, itemtype = "graded")
coef(lykilhaefni2_likan, simplify = T)
summary(lykilhaefni2_likan)
itemfit(lykilhaefni2_likan)
itemplot(lykilhaefni2_likan, 3)
itemplot(lykilhaefni2_likan, 3, type = 'threshold', )

key=list(columns=1, 
         text=list(lab=c("Kennarar"," Forráðamenn")), 
         lines=list(lwd=1, col=c("blue","red")))
LH1 = plot(lykilhaefni1_likan,key=key, main="Viðhorf kennara")
LH2 = update(plot(lykilhaefni2_likan,col="red"))
LH1+LH2
