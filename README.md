# Traitement de fichiers de saisie macro invertébrés
traitement de données brutes macro invertébrés
but : editer rapport d'essai, fichiers entree seee  et calcul indicateurs
préalable: 
+ placer les fichiers de saisie en format ODS dans le dossier "Fichiers_saisie"
+ le fichier shape des stations sur votre territoire dans "Shape/".j'ai appelé mon fichier Stations_surf_V2.shp
+ la colonne correpondant au code station s'appelle cdstation
  
Lancer la fonction "regroupe_traitement"
en sortie:
- les fichiers d'entrée SEEE dans "Exports/"
- les fichiers de sortie i2m2 metriques et diag dans "Exports/"
- Les graphiques radar liés à l'outil diagnostic
- le rapport d'essai à la racine du projet (je n'ai pas réussi à changer)
- la carte avec les indices i2m2 dans 'Cartographie/"
