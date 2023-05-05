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
  #Df_entree_seee <- matrix(ncol = 22,nrow = 0) %>%     as.data.frame()

  Df_entree_seee <- NULL

  Liste_fichiers <-
    list.files(chemin_data,pattern=".ods")
  ifelse(!dir.exists("Exports"),dir.create("Exports"),""
         )

  for (fichier_unitaire in Liste_fichiers){
    print(paste("Fichier en cours: ", fichier_unitaire))
    Fichier <- paste(chemin_data,fichier_unitaire,sep="")
    nb_feuilles <-
      readODS::list_ods_sheets(Fichier) %>% length() %>% -2
    for (i in(1:nb_feuilles)){
      data <- lire_formulaire_saisie(Fichier,i)
      Df_entree_seee <- rbind(Df_entree_seee,data)

    }

  }
  print("Traitement fini, export(s) dans le dossier Exports")


  return(Df_entree_seee)


}
Dfexemple <- Seee_to_dataframe("Fichiers_saisie")

