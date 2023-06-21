Carte_i2m2 <- function(Tableau_i2m2_metriques){
  #test2 <- test %>% reduce(rbind)
  Extracti2m2<-filter(Tableau_i2m2_metriques,CODE_PAR=='7613')%>%
    distinct() #distinct pour enlever les doublons

  colnames(Extracti2m2)[2] <- "cdstation" #pour la jointure future



  stations <- sf::st_read("Shape/Stations_surf_V2.shp",options = "ENCODING=UTF-8")
  #afrique, on la vire
  #mapview::mapview(stations)
  indicesgeom <-dplyr::left_join(Extracti2m2,stations,by="cdstation") %>% st_as_sf()
  #sf::st_write(indicesgeom,"Cartographie/i2m2.shp")
  #création de carte
  #les seuils sont ceux des HER Bretagne 12A et 12B
  #couleurs à changer
  # mapview::mapview(indicesgeom,zcol="RESULTAT",
  #                  col.regions=c('red','orange','yellow','green','blue'),
  #                  at = c(0,0.1523,0.3047,0.457,0.7078,1),
  #                  legend=TRUE)

 # mapview::mapview(indicesgeom,at = c(0,0.1523,0.3047,0.457,0.7078,1))
 m <- mapview::mapview(indicesgeom,zcol="RESULTAT",
                                   col.regions=c('red','orange','yellow','green','blue'),
                  at = c(0,0.148,0.295,0.443,0.665,1),
                                   legend=TRUE,na.color = "black")
 mapshot(m, url = paste0(getwd(), "/Cartographie/map.html"))
}
