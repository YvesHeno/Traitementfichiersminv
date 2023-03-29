rm(list = ls())
install.packages("tidyverse")
library(tidyverse)
# poids <- 81
# taille <- 1.80
# imc <- poids/taille^2
# print(imc)
# install.packages('remotes')
# remotes::install_github("MTES-MCT/savoirfR") 
# library(tidyverse)
# install.packages("GGally")
# library(GGally) 
# install.packages("plotly")
# library(plotly)di
# ase <- read.csv(file = "extdata/Base_synth_territoires.csv",
#                 header = TRUE, sep = ";", dec = ",")
setwd('~/Prog_R/Lire_Donnees')
Database <- readLines("data/donnees_ofb/echange_liste_minvCE_2021_DREAL_BZH_RCA.txt")
#remarque pas d'antislash au debut, on ecrit data et non /data
#Database <- readLines('/data/donnees_ofb/echange_liste_minvCE_2021_DREAL_BZH_RCA.txt')

onde <- read.csv("data/donnees_ofb/onde_france_2022.csv",header=TRUE,sep=",")
onde[2,1]
onde[2,]
summary(onde)
onde2 <- data.table::fread("data/donnees_ofb/onde_france_2022.csv",header=TRUE)# fast reading
#pas besoin d'indiquer les separateurs
summary(onde2)
dim(onde)
entete <- names(onde2)
class(onde2)
class(onde)
typeof(onde2)
typeof(onde)

#les noms d'entete commencent par X.
#on va les enlever

nouveau_nom <- str_replace(string=names(onde),"X.","")
nouveau_nom <- str_replace(string=nouveau_nom,"\\.","")# remplacer le point par rien
nouveau_nom2 <- str_replace(string=names(onde2),"<","")
nouveau_nom2 <- str_replace(string=nouveau_nom2,">","")
print(nouveau_nom)
print(nouveau_nom2)
names(onde) <- nouveau_nom
names(onde2) <- nouveau_nom2

onde[1:8,1:6]
onde[1:8,c("CdSiteHydro","Annee")]#c pour "combine"
dimtab <- dim(onde)
nrow(onde)
rm(onde2)
onde$LbRsObservationDpt
summary(onde$LbRsObservationDpt)
str(onde$LbRsObservationDpt)
hist(onde$RsObservationNat) 
sd(onde$RsObservationNat)#ecart type
mean(onde$RsObservationNat)
ggplot(data=onde,aes(x=LbRegion),name="nb occurences")+geom_bar()+coord_flip()
#nombre d'observations par region
ggplot(data=onde, aes(x=RsObservationNat))+geom_histogram()
ondes2 <- onde %>% 
rename(x,CoorditeHydro,
         y,CoordYSiteHydro)

install.packages("readODS")
invertebres <- readODS::read_ods("data/donnees_ofb/AFB_Saisie_MinvCE_DREAL_Bzh_RRP_2021.ods",
                                 sheet="04187500",skip=86) %>% select(CODE_STATION:C)

str(invertebres)
print(invertebres)


#lecture fichier xls
chemin <- "data/donnees_ofb/aspe_vivarmor.xlsx"
file.exists(chemin)
stations_vivarmor <- readxl::read_xlsx(path=chemin,sheet="liste_stations")
synthese_vivarmor <- readxl::read_xlsx(path=chemin,sheet="synthese")
str(stations_vivarmor)
str(synthese_vivarmor)
ggplot(data=synthese_vivarmor,aes(x=effectif),name="nb occurences")+geom_histogram()+coord_flip()
max(synthese_vivarmor$effectif)


#--------------------------- donnée geographique
install.packages("sf")
depts <- sf::read_sf("data/donnees_ofb/DEPARTEMENT_CARTO.shp")
install.packages("mapview")
Bvs <- sf::read_sf("data/donnees_ofb/BVGP5_shape.shp")
mapview::mapview(depts,col.regions="green")+mapview::mapview(x=Bvs,col.regions="red",col="black")




