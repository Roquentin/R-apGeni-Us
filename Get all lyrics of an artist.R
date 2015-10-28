###############################################################################
#                                                                             #
#                                                                             #
#                   RECUPERER DES DONNES SUR RAPGENIUS                        #
#                                                                             #
#                                                                             #
###############################################################################


#Les paquets dont on a besoin : si on ne les a pas install.packages("nomdupaquet")
library(rvest)
library(utils)
library(rjson)
library(RCurl)


#token
token<-"XXXXXXXXXXXXXXXXXXXXXXXXXX" #le token d'autorisation de l'API rapgenius
urlr<-"https://api.genius.com/search?q="   #l'URL qui permet de faire des recherches sur rapgenius
nresult<- "&per_page=959" # le nombre de résultats qu'on veut (sinon limité à 20), là j'ai mis le max de ce qui m'intéresse (après que du rap conscient dans les annotations)
request<-URLencode(("Booba"))  #la recherche qu'on veut mener ici "rap conscient"

urlf<-paste(urlr,request,nresult,"&access_token=",token,sep="") # on met ensemble pour avoir le vrai URL
json<- getURL(urlf) # on récupère la requête sous format json
resul<- fromJSON(json) # on la décode




TEXT<-data.frame() #on crée une base de données vide pour le moment

# boucle pour remplir la dataframe
for (i in 1:length(resul$response$hits)) { #length : le nombre de résultats qu'on veut (si ça galère faire la boucle en plusieurs fois)
  
  if (resul$response$hits[[i]]$result$primary_artist$name=="Booba") {
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
    
  } else {
    TEXT[i,1]<- resul$response$hits[[i]]$result$primary_artist$name
    TEXT[i,2]<- resul$response$hits[[i]]$result$title
    TEXT[i,3]<- NA
    
  }
} 


#Lunatic maintenant
for (i in 1:length(resul$response$hits)) { #length : le nombre de résultats qu'on veut (si ça galère faire la boucle en plusieurs fois)
  
  if (resul$response$hits[[i]]$result$primary_artist$name=="Lunatic") {
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
    
  } else {
    print(url) #on peut lui dire de faire n'importe quoi tant que ça ne modifie pas la dataframe
  }
} 

table(is.na(TEXT[,1]))
#On nomme les colonnes de notre dataframe
colnames(TEXT)<- c("Artiste", "Titre", "Texte")


setwd("C:/Users/User-pc/Documents/Sociologie/Quanti/Rapgenius") #le répertoire de sauvegarde
write.table(TEXT, file="booba.txt")  #on l'écrit en texte avec les sauts de ligne
#Ici on a une base avec tous les textes de Booba + Lunatic + les artistes et morceaux qui ne sont d'aucun des deux (sans les textes, juste pour savoir ce qu'il faudrait rajouter)

f2 <- TEXT[ !is.na( TEXT$Artiste ) , ] #on crée une nouvelle base en ne gardant que Lunatic et Booba
f2 <- subset( TEXT, !is.na( Artiste ) )
f2 <- na.omit( TEXT )
write.table(f2, file="boobaluna.txt") #on a la base en texte

TEXT[,3]<-gsub("[\r\n]", " / ", TEXT[,3]) #on enlève les sauts de ligne
write.csv2(TEXT, file="booba.csv") #on a sous forme de tableau

f2[,3]<-gsub("[\r\n]", " / ", f2[,3]) #on enlève les sauts de ligne
write.csv2(f2, file="boobaluna.csv") #on a sous forme de tableau


# f2[,3]<-gsub("/", "\r\n", f2[,3]) #on remet les sauts de ligne : si on a fait une fausse manip
