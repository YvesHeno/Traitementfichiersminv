Carte_i2m2 <- function(Tableau_i2m2_metriques){
  #test2 <- test %>% reduce(rbind)
  Extracti2m2<-filter(Tableau_i2m2_metriques,CODE_PAR=='7613')%>%
    distinct() #distinct pour enlever les doublons

  colnames(Extracti2m2)[2] <- "cdstation" #pour la jointure future



  stations <- sf::st_read("Shape/stations_qualite_eau_surf.shp",options = "ENCODING=WINDOWS-1252") %>% filter(y>687000)# il y a une station en
  #afrique, on la vire
  #mapview::mapview(stations)
  indicesgeom <-dplyr::left_join(Extracti2m2,stations,by="cdstation") %>% st_as_sf()
  #création de carte
  #les seuils sont ceux des HER Bretagne 12A et 12B
  #couleurs à changer
  mapview::mapview(indicesgeom,zcol="RESULTAT",
                   col.regions=c('red','orange','yellow','green','blue'),
                   at = c(0,0.1523,0.3047,0.457,0.7078,1),
                   legend=T)
  #mapview::mapview(indicesgeom,at = c(0,0.1523,0.3047,0.457,0.7078,1))
}
