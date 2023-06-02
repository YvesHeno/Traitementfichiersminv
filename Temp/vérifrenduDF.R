n <- length(Dftri[,1])
print(n)
i <- 1

while (i<n){
  Codeop <- Dftri[i,1]
  indices <- which(Dftri[,1]==Codeop)
  print(indices)
  print("----------------------------")
  i <- i+length(indices)-1 #on passe à la station suivante
  print(i)
}
