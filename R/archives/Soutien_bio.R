Soutien_bio<- function(Fichier,Feuille){

  invertebres <- readODS::read_ods(Fichier,
                                   sheet=Feuille) %>%
  slice((1:86)) %>% replace(is.na(.),"")
  Export <- "Data/soutienbioex.ods"
  write_ods(invertebres,Export)

return(invertebres)
}
library(tidyverse)
a <- Soutien_bio("Data/SaisieexempleRCO.ods",1)
