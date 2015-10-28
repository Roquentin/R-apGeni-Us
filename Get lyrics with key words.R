###############################################################################
#                                                                             #
#                                                                             #
#                   RECUPERER DES DONNES SUR RAPGENIUS                        #
#                                                                             #
#                                                                             #
###############################################################################


#Les paquets dont on a besoin
library(rvest)
library(utils)
library(rjson)
library(RCurl)


#token
token<-"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" #le token d'autorisation de l'API rapgenius
urlr<-"https://api.genius.com/search?q="   #l'URL qui permet de faire des recherches sur rapgenius
nresult<- "&per_page=117" # le nombre de résultats qu'on veut (sinon limité à 20), là j'ai mis le max de ce qui m'intéresse (après que du rap conscient dans les annotations)
request<-URLencode(("rap+conscient"))  #la recherche qu'on veut mener ici "rap conscient"

urlf<-paste(urlr,request,nresult,"&access_token=",token,sep="") # on met ensemble pour avoir le vrai URL
json<- getURL(urlf) # on récupère la requête sous format json
resul<- fromJSON(json) # on la décode


TEXT<-data.frame() #on crée une base de données vide pour le moment

# boucle pour remplir la dataframe
for (i in 1:length(resul$response$hits)) { #length : le nombre de résultats qu'on veut (si ça galère faire la boucle en plusieurs fois)
  url<-(resul$response$hits[[i]]$result$url) #pour chaque résultat, on récupère l'URL du morceau
  html<- read_html(url, encoding="UTF-8") #on le lit en UTF-8
  lyricshtml<-html_node(html, "div.lyrics") #on recherche le bon "noeud" celui où il y a les paroles
  txt<-html_text(lyricshtml) # on transforme ce qu'il y a dans le noeud pour que ce soit lisible
  TEXT[i,1]<- resul$response$hits[[i]]$result$primary_artist$name #le nom de l'artiste dans la première colonne
  TEXT[i,2]<- resul$response$hits[[i]]$result$title #le titre du morceau dans la deuxième
  TEXT[i,3]<-txt #le texte dans la troisième colonne
  TEXT[i,3]<-iconv(TEXT[i,3], from="UTF-8", to="ASCII//TRANSLIT") #on enlève les accents
  TEXT[i,2]<-iconv(TEXT[i,2], from="UTF-8", to="ASCII//TRANSLIT") # idem
  TEXT[i,1]<-iconv(TEXT[i,1], from="UTF-8", to="ASCII//TRANSLIT") # idem
} 


#On nomme les colonnes de notre dataframe
colnames(TEXT)<- c("Artiste", "Titre", "Texte")


#Fonction pour extraire les mots autour de la requête
trouver_positions = function(mots,mot1,mot2){
  position=matrix(ncol=1,nrow=0)
  occurrences=grep(mot1,mots[[1]]) # trouver les positions des occurrences de "rap" dans le texte
  for(i in 1:length(occurrences)){ # pour chaque occurrence de rap
    if(grepl(mot2,mots[[1]][occurrences[i]+1])){ # on regarde si le mot suivant est "conscient" (mot2)
      position=rbind(occurrences[i],position) # si oui, on ajoute la position de rap dans la liste
    }
  }
  return(position)
}


for (i in 1:length(resul$response$hits)) { #idem : changer la longueur si ça galère (faire en plusieurs fois)
  mots=tolower(TEXT[i,3]) #en minuscule pour pas avoir de pb
  mots=strsplit(mots," ") # dès qu'il y a un espace c'est un nouveau mot
  mot1="rap"
  mot2="conscient"
  position=trouver_positions(mots,mot1,mot2) #donne la position de l'expression "rap conscient" (si elle existe)
  
  if (is.na(position[1])) { #si la position n'existe pas (NA), c'est à dire qu'il n'y a pas l'expression "rap conscient"
    TEXT [i,3]<- NA  #alors on met NA dans la case (en vrai il faudrait la virer à la fin)
  } else { #sinon
    
    if (position>20) { #sinon rap conscient n'est pas dans les vingts premiers mots
      avant<- paste(mots[[1]][(position[1]-20):(position[1])], collapse=" ") #20 mots avant
    }   else { #Si c'est dans les vingt premiers mots on lui demande de partir du premier mot du morceau
      avant<- paste(mots[[1]][(position[1]-position[1]+1):(position[1])], collapse=" ")
    }
    
    apres<- paste(mots[[1]][(position[1]+1):(position[1]+20)], collapse=" ") #20 mots après (on peut modifier 20 et 20 pour avoir plus ou moins large)
    Phrase<- paste(avant, apres, collapse= " ") #on colle ce qu'il y a avant et après
    TEXT[i,3]<- Phrase # et on le met dans la base de données
  }
  
  TEXT[i,3]<-gsub("[\r\n]", " / ", TEXT[i,3])  # on remplace les sauts de ligne par des / (dépend de comment on le lit mais sur excell c'est plus lisible) (changer en fonction)
}


setwd("C:/Users/User-pc/Documents/Sociologie/Quanti/Rapgenius") #le répertoire de sauvegarde
write.csv2(TEXT, file="rapconscient.csv")  #on l'écrit ici en CSV (peut-être qu'il y a mieux)
#FIN!!!!!!!!!!!





