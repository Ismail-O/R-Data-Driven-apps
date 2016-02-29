                
                      ##-------------------##IMPORTANT##-------------------##
## le chemin du dossier doit être spécifié  avec "\\" comme séparateur, exemple : C:\\Dossier1\\Dossier2\\etc....

corr<-function(directory,treshold){ 
  chem2<-dir(path = directory,all.files = TRUE,   
             full.names = FALSE, recursive = FALSE,
             ignore.case = FALSE, include.dirs = FALSE, no.. = TRUE)
  chemf<-paste (directory,chem2, sep = "\\")
  
  v<-c(correlation<-double()) ##Creation du vecteur de correlation
  h<-1

  for(i in 1:332){ ##On parcour tous les moniteurs
    
    j <- read.csv(chemf[i], header=TRUE, sep=",") 
    cc<-complete.cases(j) ## Calcul des complete cases du fichier j du moniteur i
    l<-length(cc) ##calcul de la longeur des complete cases
    count=0
    for(k in 1:l){ ##On parcour les cc
      if(cc[k]==TRUE){ ## On incrémente count lorsqu'on detecte une complete case
        count=count+1
      }
    }
    if(count>treshold){ ##On compare le nombre de cc dans j au treshold
      v[h]<-cor(j[2],j[3],use="pairwise.complete.obs") ## L'argument use="pairwise.complete.obs" nous permet d'ignorer les missing values
      h<-h+1
    }
   
    
  }

    return(v)
  
  
}
