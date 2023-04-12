#Saisie_minv_to_seee
#' Title
#'
#' @Data
#'
#' @return
#' @export
#'
#' @examples
Saisie_minv_to_seee<- function(chemin_data){
library(tidyverse)
library(purrr)
source(file="R/Lire_formulaire_irstea.R", encoding ="UTF-8")

  premiereligne <- paste("CODE_OPERATION","CODE_STATION",	"LB_STATION",
                         "CODE_POINT",	"DATE",	"CODE_PRODUCTEUR",
                         "NOM_PRODUCTEUR",	"CODE_PRELEVEUR",
                         "NOM_PRELEVEUR",	"CODE_DETERMINATEUR",
                         "NOM_DETERMINATEUR",	"COORD_X_OP",	"COORD_Y_OP",
                         "COORD_X_OP_AVAL",	"COORD_Y_OP_AVAL",
                         "TYPO_NATIONALE",	"CODE_PHASE",	"CODE_PREL_ELEM",
                         "CODE_TAXON",	"NOM_LATIN_TAXON",
                         "RESULTAT",	"CODE_REMARQUE",sep="\t")

  chemin_data <- paste(chemin_data,"/",sep="")
  Liste_fichiers <-
    list.files(chemin_data,pattern=".ods")
  ifelse(!dir.exists("Exports"),dir.create("Exports"),""
         )
  for (fichier_unitaire in Liste_fichiers){
    print(paste("Fichier en cours: ", fichier_unitaire))
    if (length(grep("RCS",fichier_unitaire))!=0){
      fichier_export <- "Exports/exportseeeRCS.txt"
    }
    else if (length(grep("RCO",fichier_unitaire))!=0){
      fichier_export <- "Exports/exportseeeRCO.txt"
    }  else{fichier_export <- "Exports/exportseee_noreseau.txt"}

    write.table(x=premiereligne,file=fichier_export,
                sep="\t",append=FALSE,col.names = FALSE,
                row.names = FALSE,quote=FALSE)

      Fichier <- paste(chemin_data,fichier_unitaire,sep="")
      nb_feuilles <-
        readODS::list_ods_sheets(Fichier) %>% length() %>% -2
      for (i in(1:nb_feuilles)){
        data <- lire_formulaire_saisie(Fichier,i)
        write.table(data,file=fichier_export,
                    sep="\t",append=TRUE,col.names = FALSE,
                    row.names = FALSE,quote=FALSE)
      }
  }
  print("Traitement fini, export(s) dans le dossier Exports")
}
Saisie_minv_to_seee("Data")

