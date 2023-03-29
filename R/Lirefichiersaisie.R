rm(list = ls())
install.packages("tidyverse")
library(tidyverse)
setwd('~/Prog_R/Traitementfichiersminv')

#Database <- readLines("data/donnees_ofb/echange_liste_minvCE_2021_DREAL_BZH_RCA.txt")
#remarque pas d'antislash au debut, on ecrit data et non /data

install.packages("readODS")

invertebres <- readODS::read_ods("data/Saisieexemple.ods",
                                 sheet="04187500",skip=86) %>% select(CODE_STATION:C)

str(invertebres)
print(invertebres)



