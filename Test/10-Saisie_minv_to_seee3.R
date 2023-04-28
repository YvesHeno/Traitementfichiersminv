#Saisie_minv_to_seee
#' Title
#'
#' @Data
#'
#' @return
#' @export
#'
#' @examples
Saisie_minv_to_seee3<- function(chemin_data){
source(file="Test/Lire_formulaire_irstea.R", encoding ="UTF-8")

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
  Df_entree_seee <- matrix(ncol = 22,nrow = 0) %>%     as.data.frame()


  Liste_fichiers <-
    list.files(chemin_data,pattern=".ods")
  ifelse(!dir.exists("Exports"),dir.create("Exports"),""
         )
  for (fichier_unitaire in Liste_fichiers){
    print(paste("Fichier en cours: ", fichier_unitaire))
    if (length(grep("RCS",fichier_unitaire))!=0){
      fichier_export <- "Exports/Echange_liste_RCS.txt"
    }
    else if (length(grep("RCO",fichier_unitaire))!=0){
      fichier_export <- "Exports/Echange_liste_RCO.txt"
    }
    else if (length(grep("RCR",fichier_unitaire))!=0){
      fichier_export <- "Exports/Echange_liste_RCR.txt"
    }
      else if (length(grep("RRP",fichier_unitaire))!=0){
        fichier_export <- "Exports/Echange_liste_RRP.txt"
      }
        else{fichier_export <- "Exports/Echange_liste_noreseau.txt"}

    write.table(x=premiereligne,file=fichier_export,
                sep="\t",append=FALSE,col.names = FALSE,
                row.names = FALSE,quote=FALSE)

      Fichier <- paste(chemin_data,fichier_unitaire,sep="")
      nb_feuilles <-readODS::list_ods_sheets(Fichier) %>%
        length() %>% -2

       for (i in(1:nb_feuilles)){
         data <- lire_formulaire_saisie(Fichier,i)
         Df_entree_seee <- rbind(Df_entree_seee,data)
         write.table(data,file=fichier_export,
                     sep="\t",append=TRUE,col.names = F,
                     row.names = FALSE,quote=FALSE)
       }

  }





  print("Traitement fini, export(s) dans le dossier Exports")


  return(Df_entree_seee)


}
# Dfexemple <- Saisie_minv_to_seee3("Fichiers_saisie")

