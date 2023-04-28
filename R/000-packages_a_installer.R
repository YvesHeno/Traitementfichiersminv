packages_necessaires <- function(){
 a_installer <-  c("dplyr", "sfsmisc", "tidyr","purrr","stringr",
    "vegan", "ade4", "ranger", "mlr","sf")
 n=length(a_installer)
 for (i in 1:n){
   print(a_installer[i])
   install.packages(a_installer[i])

 }
}
packages_necessaires()
