rm(list = ls())
#install.packages("tidyverse")
library(tidyverse)

#Database <- readLines("data/donnees_ofb/echange_liste_minvCE_2021_DREAL_BZH_RCA.txt")
#remarque pas d'antislash au debut, on ecrit data et non /data

#install.packages("readODS")
operation_see <- c()
invertebres <- readODS::read_ods("data/Saisieexemple.ods",
                                 sheet="04180900")
cdstation <- invertebres[22,2]
nom_station <- invertebres[22,4]
Date_ope <- invertebres[25,4]
code_prod <- invertebres[22,1]
nom_producteur<- invertebres[25,1]
code_determ <- invertebres[25,5]#=code preleveur
nom_determ <- invertebres[25,6]#=nom preleveur
typo <- invertebres[25,7]
nom_cours_eau <- invertebres[38,3]
code_point=""
x_op<- invertebres[22,11]
y_op<- invertebres[22,12]
x_aval<- invertebres[22,13]
y_aval<- invertebres[22,14]
code_operation=paste(cdstation,"MINV",sep="-")
invertebres <- invertebres %>%
  slice(-(1:86)) %>%
  select(1:19)
invertebres[is.na(invertebres)] <- 0 # pour eviter les comparaisons de na


i <- 1
fin <- FALSE
while (fin==FALSE){
  if (invertebres[i,5]!=0) { #phaseA<>0
   print(i)
   prel_elem=which(invertebres[i,8:11]!=0) #chercher le num duprel elem
   prel_elem <- prel_elem+7 #pour retrouver les bonnes positions dans le tableau macroinvertebres
   nb <- length(prel_elem)
  for (j in prel_elem){
    #écrire dans le fichier
    ligne <- c(code_operation,cdstation,nom_station,
               code_point,Date_ope,code_prod,nom_producteur,
               code_determ,nom_determ, #en fait c'est le preleveur ici mais on met determ c est le même
               code_determ,nom_determ,x_op,y_op,x_aval,y_aval,
               typo,"A",j-7,invertebres[i,4],invertebres[i,3],
               invertebres[i,j],"1")
    operation_see <- rbind(operation_see,ligne)
  }
  }
  else if (invertebres[i,6]!=0) { #phaseB<>0
    prel_elem=which(invertebres[i,12:15]!=0) #chercher le num duprel elem
    prel_elem <- prel_elem+11 #pour retrouver les bonnes positions dans le tableau macroinvertebres
    nb <- length(prel_elem)
    for (j in prel_elem){
      #écrire dans le fichier
      ligne <- c(code_operation,cdstation,nom_station,
                 code_point,Date_ope,code_prod,nom_producteur,
                 code_determ,nom_determ, #en fait c'est le preleveur ici mais on met determ c est le même
                 code_determ,nom_determ,x_op,y_op,x_aval,y_aval,
                 typo,"B",j-7,invertebres[i,4],invertebres[i,3],
                 invertebres[i,j],"1")
      operation_see <- rbind(operation_see,ligne)
    }
  }
  else if (invertebres[i,7]!=0) { #phaseB<>0
    prel_elem=which(invertebres[i,16:19]!=0) #chercher le num duprel elem
    prel_elem <- prel_elem+15 #pour retrouver les bonnes positions dans le tableau macroinvertebres
    nb <- length(prel_elem)
    for (j in prel_elem){
      #écrire dans le fichier
      ligne <- c(code_operation,cdstation,nom_station,
                 code_point,Date_ope,code_prod,nom_producteur,
                 code_determ,nom_determ, #en fait c'est le preleveur ici mais on met determ c est le même
                 code_determ,nom_determ,x_op,y_op,x_aval,y_aval,
                 typo,"C",j-7,invertebres[i,4],invertebres[i,3],
                 invertebres[i,j],"1")
      operation_see <- rbind(operation_see,ligne)
    }
  }
  ifelse(invertebres[i+1,3]==0,fin <- TRUE,i <- i+1)
}




