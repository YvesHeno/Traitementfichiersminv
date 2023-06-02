data_entree <- Seee_to_df("Exports/") %>% data.frame()

Station <- ""
n <- length(data_entree[,1])
i <- 1
nbstations <- 1
ivaleursaffichees <- c(2,17,19,20,21)
# print(data_entree[,2])
while (i<n)  {
  Station2 <- data_entree[i,2]
  if (Station2!=Station){
    #nelle sttaion
    nbstations <- nbstations+1
    print("---------------------------")
    print(paste("Station :",Station2))
    Station <- Station2
    colnames(data_entree[,ivaleursaffichees]) %>%
      toString() %>%
      print()
  }

    affiche <- data_entree[i,ivaleursaffichees] %>% toString() %>% print()
    i <- i+1
  }

  # knitr::kable(data_entree[indices,17:22], caption = "essai dataframe.",
  #floating.environment="sidewaystable")



  #print(data_entree[i,ivaleursaffichees])


