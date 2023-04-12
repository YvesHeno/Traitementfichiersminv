#chargement traits eco
chemin <- "Data/TBioEco.xls"
file.exists(chemin)
Traits_bio <- readxl::read_xls(path=chemin,sheet=1,skip=3)
