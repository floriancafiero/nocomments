#Fonctions d’extraction des sections comments (rough slicing)
#Nécessite library : stringr
#Nécessite fichiers : data.frame htmls (id_site, label_site, id_url, url, html) - data.frame balises (id_site, label_site, comments, balise_ouverture, balise_fermeture)
#Produits : data.frame htmls_clean (id_site, label_site, id_url, url, html) - data.frame comments (id_site, label_site, id_url, url, html) - data.framr erreur (type, site, url)

comments <- c()
erreurs <- c()

extract_comment_sections <- function(htmls,balises,comments,erreurs)
{
htmls_clean <- htmls #possible de travailler directement sur htmls
comments <- data.frame(id_site = character(0), label_site = character(0), id_url = character(0), page_url  = character(0), html = character(0))
erreurs <- data.frame(type = character(0), site = character(0), url = character(0))

Sites <- unique(htmls$id_site)
load=txtProgressBar(min=0, max=length(Sites), style = 3)

for (x in 1:length(Sites)) #boucle : chaque site
	{
Site <- Sites[x]
n_site <- which(balises$id_site == Site)
if (balises$comments[n_site]==TRUE)	#test : présence de comments
{
Ouverture = as.character(Balises$balise_ouverture[n_site])
Fermeture = as.character(Balises$balise_fermeture[n_site])
Pages <- which(htmls$id_site == Site)
for (y in 1:length(Pages)) #boucle : chaque page du site
{
Txt = htmls$html[Pages[y]]
NOuverture = as.data.frame(str_locate_all(Txt,Ouverture))
LFermeture = as.data.frame(str_locate_all(Txt,Fermeture))
if (nrow(NOuverture)==0)
{erreurs=rbind(erreurs,data.frame(type = "Pas d'ouverture", site = Site, url = html$url[Pages[y]]))}
else	#test : une ouverture est trouvée
	{if (nrow(NOuverture)>1)
{erreurs=rbind(erreurs,data.frame(Type = "Plusieurs ouvertures", site = Site, url = html$url[Pages[y]]))}
else	#test : plusieurs ouvertures
{if (is.na(which(LFermeture[,1]>NOuverture[1,2])[1]))
{erreurs=rbind(erreurs,data.frame(type = "Pas de fermeture",  site = Site, url = html$url[Pages[y]]))}
else	#Test présence fermeture après ouverture
{n_F <- which(LFermeture[,1]>NOuverture[1,2])[1]
if (is.na(n_F))
{erreurs=rbind(erreurs,data.frame(type = "Pas de fermeture après ouverture",  site = Site, url = html$url[Pages[y]]))}
else
{
comments <- rbind(comments,data.frame(id_site = Site, 
label_site = htmls$label_site[Pages[y]], 
id_url = htmls$id_url[Pages[y]], 
page_url = htmlpages$url[Pages[y]],
html = str_sub(Txt,start = NOuverture[1,1], end = LFermeture[n_F,2])))
htmls_clean$html[Pages[y]] <- paste(str_sub(Txt,start = 1, end=NOuverture[1,2]), str_sub(Txt, start=LFermeture[n_F,1], end=nchar(Txt)))
}
}
}	    
}
}
}
setTxtProgressBar(load,x)
}
return(htmls_clean)
}

# le passage par des variables en entrée (comments, erreurs) peut être remplacé par une sortie sous forme de CSV (avec éventuellement en seconde variable d’entrée l’emplacement du dossier où sauvegarder les sorties).
write.csv(erreurs,file="Erreurs_tri_comments.csv")
write.csv(comments,file="comments.csv")
write.csv(htmls_clean,file="Nocomment.csv”)


