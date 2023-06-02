#Saisie_minv_to_seee
#' Title
#'
#' @Data
#'
#' @return
#' @export
#'
#' @examples
#extrait un dataframe de tous les fichiers de saisie
Seee_to_dataframe<- function(chemin_data){
source(file="Lire_formulaire_irstea.R", encoding ="UTF-8")

  premiereligne <- c("CODE_OPERATION","CODE_STATION",	"LB_STATION",
                         "CODE_POINT",	"DATE",	"CODE_PRODUCTEUR",
                         "NOM_PRODUCTEUR",	"CODE_PRELEVEUR",
                         "NOM_PRELEVEUR",	"CODE_DETERMINATEUR",
                         "NOM_DETERMINATEUR",	"COORD_X_OP",	"COORD_Y_OP",
                         "COORD_X_OP_AVAL",	"COORD_Y_OP_AVAL",
                         "TYPO_NATIONALE",	"CODE_PHASE",	"CODE_PREL_ELEM",
                         "CODE_TAXON",	"NOM_LATIN_TAXON",
                         "RESULTAT",	"CODE_REMARQUE")

  chemin_data <- paste(chemin_data,"/",sep="")
  #Df_entree_seee <- matrix(ncol = 22,nrow = 0) %>%     as.data.frame()

  Df_entree_seee <- data.frame()

  Liste_fichiers <-
    list.files(chemin_data,pattern=".ods")
  ifelse(!dir.exists("Exports"),dir.create("Exports"),""
         )

for (fichier_unitaire in Liste_fichiers){
 #print(paste("Fichier en cours: ", fichier_unitaire))
  Fichier <- paste(chemin_data,fichier_unitaire,sep="")
  feuilles <-readODS::list_ods_sheets(Fichier)
  nb_feuilles <- length(feuilles)


  for (i in(1:nb_feuilles)){
    if (nchar(feuilles[i])<9){
      #il y a qqfois des feuilles annexes
      print(paste("station :",feuilles[i]))
      data <- lire_formulaire_saisie(Fichier,i) %>%  as.data.frame()
      Df_entree_seee <- rbind(data,Df_entree_seee)
    }
   }


  }
  print("Traitement fini, export(s) dans le dossier Exports")


  colnames(Df_entree_seee) <- premiereligne
  return(Df_entree_seee)


}
#library(tidyverse)
Dfexemple <- Seee_to_dataframe("Fichiers_saisie")

