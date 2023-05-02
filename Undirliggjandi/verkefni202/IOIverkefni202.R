#Verkefni 202 - krosstengslalíkan  

rm(list = ls()) 
graphics.off() 
cat("\014") 

library(psych)
library(lavaan)
library(tidySEM)
library(tidyverse)
library(mice)

#---- sækja gögn ----

gagnarammi <- read.delim("./Documents/GitHub/Megindleg.github.io/Undirliggjandi/verkefni202/curranGogn.dat", na = c("-999"),
col.names = c(
  "Id", # Auðkenni þátttakenda
  "andf1", 
  "andf2", 
  "andf3", 
  "andf4", # Mælingar á andfélagslegri hegðun á 4 tímapunktum
  "lestur1", 
  "lestur2", 
  "lestur3", 
  "lestur4", # Mælingar á lestri á 4 tímapunktum
  "kynBarns", # kyn barns
  "aldurMom", # aldur móður
  "aldurBarns", # aldur barns á tímapunkti 1
  "HugUpp", # hugræn hvatning
  "TilfUpp", # tilfinningalegur stuðningur
  "nmis" # fjöldi brottfallsgilda hjá þátttakanda
))

Rawdata <- gagnarammi
sum(is.na(gagnarammi)) #ATH á N/A-gildum
KMO(gagnarammi) #Kaiser-Mayer-Olkin factor adequacy - 0.9+ er talið frábært
cortest.bartlett(gagnarammi)
describe(gagnarammi)

#------------------Marghliða tilreikningur--------------------------
gagnarammi <- mice(gagnarammi, m=3, method="pmm", maxit = 3) #Tilreikningur
gagnarammi <- complete(gagnarammi, 3)
describe(gagnarammi)
describe(Rawdata)

#------------------Skoða dreifingu--------------------------

hist(gagnarammi$HugUpp)
describe(gagnarammi$HugUpp)

hist(gagnarammi$TilfUpp)
describe(gagnarammi$TilfUpp)


#------------------Breytuval--------------------------

MikillStudningur <- filter(gagnarammi,HugUpp >= 9 &  TilfUpp < 10 )
LitillStudningur <- filter(gagnarammi,HugUpp < 9 & TilfUpp < 10)
#------------------ CLPM --------------------------
gagnarammi.mod1 <- " 
lestur4 ~ AR21 * lestur3 + CL21 * andf3 
lestur3 ~ AR22 * lestur2 + CL22 * andf2
lestur2 ~ AR23 * lestur1 + CL23 * andf1 

andf4 ~ AR11 * andf3 + CL11 * lestur3 
andf3 ~ AR12 * andf2 + CL12 * lestur2
andf2 ~ AR13 * andf1 + CL13 * lestur1

#Samdreifni
andf1 ~~ lestur1
"
MikillStudningur_likan <- sem(data = MikillStudningur, model = gagnarammi.mod1)
LitillStudningur_likan <- sem(data = LitillStudningur, model = gagnarammi.mod1)

layout <- get_layout("lestur1", "lestur2", "lestur3", "lestur4",
                          "andf1", "andf2", "andf3", "andf4", rows=2)

graph_sem(model = MikillStudningur_likan, layout=layout)
summary(MikillStudningur_likan, fit.measures = TRUE)

graph_sem(model = LitillStudningur_likan, layout=layout)
summary(LitillStudningur_likan, fit.measures = TRUE)



