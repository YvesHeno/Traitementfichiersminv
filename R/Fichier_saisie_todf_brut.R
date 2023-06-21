Fichier_saisie_todf_brut <- function(){
  invertebres <- c()

  Liste_fichiers <- list.files("Fichiers_saisie/",pattern=".ods",full.names = T)
  for (fichier_unitaire in Liste_fichiers){
    feuilles <-readODS::list_ods_sheets(fichier_unitaire)
    i <- 1
    nb_feuilles <- 0
    while (i<length(feuilles)){
      if (nchar(feuilles[i])<9){
        nb_feuilles <- nb_feuilles+1
      }
      i <- i+1
    }
    for (i in(1:nb_feuilles)){

        temp <- readODS::read_ods(fichier_unitaire,sheet=feuilles[i]) %>%
        slice(-(1:86)) %>%
        select(1:6)
        j <- which(is.na(temp[,3]))
        invertebres <- rbind(invertebres,temp[1:j[1]-1,])

    }

  }
invertebres[,4] <- as.numeric(invertebres[,4])
invertebres[,5] <- as.numeric(invertebres[,5])
invertebres[,6] <- as.numeric(invertebres[,6])
# invertebres %>% select(A,B,C) %>%  mutate(Total=A+B+C)

return(invertebres)
}
inv <- Fichier_saisie_todf_brut()
