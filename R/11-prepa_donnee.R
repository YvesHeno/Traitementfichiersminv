prepa_donnee <- function(data){
n <- nrow(data)
for (i in 1:n){
  if (data[i,1]==""){
    data[i,1] <- paste0(data[1,2],"-",data[1,5])
  }
}
data[is.na(data)] <- 0
return(data)
}
