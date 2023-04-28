#' Regroupe_traitement
#'
#' @return
#' pas de retour
#' @export
#'fichiers d'export: entree SEEE
#'sortie i2m2 +métriques
#'carte i2m2 (à améliorer)
#'à venir sortie outil diag
#' @examples
Regroupe_traitement<- function(){

  #dependances de fonctions----
  source(file="R/10-Saisie_minv_to_seee2.R", encoding ="UTF-8")
  #source(file="R/30-seee_to_i2m2.R", encoding ="UTF-8")
  source(file="R/30-calcule_SEEE_I2M2_eptb.R", encoding ="UTF-8")
  source(file="R/31-Chargement_packages.R", encoding ="UTF-8")
  source(file="R/35-Carto_i2m2.R", encoding ="UTF-8")
  #source(file="R/40-outil diagODInvertebres_v1.0.2.R", encoding ="UTF-8")
  source(file="R/41-calcule_SEEE_ODinvertebres_eptb.R", encoding ="UTF-8")


  #chargement des packages
  ## CHARGEMENT DES PACKAGES ----
  dependencies <- c("dplyr", "sfsmisc", "tidyr","purrr","stringr",
                    "vegan", "ade4", "ranger", "mlr","sf","tidyverse","httr")
  loadDependencies(dependencies)


  print("***************** transformation des fichiers de saisie en fichiers entrée SEEE")
  #on regarde si les fichiers n'on pas dejà été transformés, sinon
  # pas la peine de retraiter les fichiers de saisie irstea
  Liste_fichiers <-  list.files("Exports/",pattern="*.txt",full.names=T)
  if(length(Liste_fichiers)==0){
    Saisie_minv_to_seee2("Fichiers_saisie")
  }
    else{
      print("fichiers déjà traités")
    }





  #Calcul de i2m2 en fct des fichier entrée SEEE
  Liste_fichiers <-  list.files("Exports/",pattern="*.txt",full.names=T)
  n <- length(Liste_fichiers)
  print(Liste_fichiers)

  data_entree <- NULL
  for (i in 1:n){
    data_temp <- read.table(Liste_fichiers[i], header = TRUE, sep = "\t",
                              stringsAsFactors = FALSE, quote = "\"",
                              colClasses = c(CODE_OPERATION = "character",
                                             CODE_STATION   = "character",
                                             CODE_TAXON     = "character")) %>%
      filter(RESULTAT > 0)
    data_entree <- rbind(data_entree,data_temp)

  }

  print("******************  calcul I2M2")
  Tableau_i2m2_metriques <- calcule_SEEE_I2M2(data_entree)

  print("******************  calcul métriques de l'outil diagnostic")
  Tableau_i2m2_diagnostic <- calcule_SEEE_ODinvertebres(data_entree)

  #mise en carto
  print("******************  Cartographie")
  Carte_i2m2(Tableau_i2m2_metriques)

  return(data_entree)
}
 data <- Regroupe_traitement()
