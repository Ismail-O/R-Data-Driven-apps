
                      ##-------------------##IMPORTANT##-------------------##
## le chemin du dossier doit être spécifié  avec "\\" comme séparateur, exemple : C:\\Dossier1\\Dossier2\\etc....


complete<-function(directory,cid){ #Creation de la fonction pollutantmean

  ##On ouvre le dossier
  chem2<-dir(path = directory,all.files = TRUE,   
             full.names = FALSE, recursive = FALSE,
             ignore.case = FALSE, include.dirs = FALSE, no.. = TRUE)
  chemf<-paste (directory,chem2, sep = "\\")
  
  x=0
  count=0 
  indexcount=1 ## index de la colonne 'nobs'
  
  ##Création d'une data frame df contenant 2 colonnes 'id' et 'nobs' tous deux de la meme taille et initialisés à cid
  df<-data.frame(id=cid,nobs=cid) 
  
  for(i in cid){ 
    
    j <- read.csv(chemf[i], header=TRUE, sep=",") 
    cc<-complete.cases(j) ## Calcul des complete cases du fichier j du moniteur i
    l<-length(cc) ##calcul de la longeur des complete cases
    count=0
    for(k in 1:l){ ##On parcours les cc
     if(cc[k]==TRUE){ ## On incrémente count lorsqu'on detecte une complete case
       count=count+1
     }
    }
    df$nobs[indexcount]=count ## On met le nombre de complete cases dans la colonne nobs de le data frame
    indexcount=indexcount+1
  }
 
  return(df)
}
  

