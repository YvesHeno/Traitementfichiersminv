Graphes_outil_diag <- function(Tableau_i2m2_diagnostic){
  library(fmsb)
  #il y a "trop" de colonnes dans le tableau (colonnes annexes, exploitables peut etre par la suite?)
  Tableau_restreint <-  select(Tableau_i2m2_diagnostic, CODE_STATION:ANTHROPISATION_BV)
  nbstations <- nrow(Tableau_i2m2_diagnostic)
  #lignes de max min, nécéssaires pour les routines d'affichage
  max <- rep(1,11)
  min <- rep(0,11)
  opar <- par()
  # Définir les paramètres graphiques dans une grille 3x4, avec des marges appropriées:
  x11()
  par(mar = rep(0.8,4))
  par(mfrow = c(3,2))


  # Produire un graphique radar pour chaque station
   for (i in 1:nbstations) {
    if (i%%3==0) {
      x11()
      par(mar = rep(0.8,4))
      par(mfrow = c(3,2))

    }
    T_temp <- rbind(max,min,Tableau_restreint[i,])
    radarchart(
     T_temp[,4:9 ],
      pfcol = c(scales::alpha("green", 0.5),NA),
      pcol= c(NA,2), plty = 1, plwd = 2,
      title = T_temp[3,1],vclex=0.7
    )
    radarchart(
      T_temp[,10:15 ],

      pfcol = c(scales::alpha("red",0.5),NA),
      pcol= c(NA,2), plty = 1, plwd = 2,
      title = T_temp[3,1]
    )
  }
  # Restaurer les paramètres standard de par()
  par <- par(opar)
}
Graphes_outil_diag(Tableau_i2m2_diagnostic)



