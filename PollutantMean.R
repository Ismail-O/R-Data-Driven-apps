


                            ##-------------------##IMPORTANT##-------------------##
## le chemin du dossier doit être spécifié  avec "\\" comme séparateur, exemple : C:\\Dossier1\\Dossier2\\etc....

pollutantmean<-function(directory,pollutant,id){ ##Creation de la fonction pollutantmean
  
##On ouvre le dossier
  
chem2<-dir(path = directory,all.files = TRUE,   
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = TRUE)
##chem2 est le chemin du dossier,chemf reenvoie aux fichiers contenu dans le dossier
chemf<-paste (directory,chem2, sep = "\\")

x=0
count=0 

for(i in id){ ## id est le vecteur qui nous indiquera les moniteurs que l'on veut étudier aka les fichiers à ouvrir
  
  j <- read.csv(chemf[i], header=TRUE, sep=",")##On lis les fichiers 1 par 1 
  
  nb<-which( colnames(j)==pollutant ) ##Identifier la colonne du polluant choisi
  m<-mean(j[[nb]],na.rm=TRUE) #calcul du mean de la colonne
 
  ##On ignore les cases NaN car cela fausse les calculs
  if(is.nan(m)==FALSE){
  x=x+m
  count=count+1
  
  }
  
  
}

return(x/count) ##Moyenne de tout les moniteurs selectionnés
}