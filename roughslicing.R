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

#Fonctions d’identification des liens “internes”
# Remarque : l’utilisation des préfixes = liées à hyphe mais adaptable à d’autres corpus.

#Nécessite library : stringr
#Nécessite fichiers : data.frame htmls_edges (source, target, id) - data.frame prefixes (id_site, multi_prefixes, n_prefixes, name, prefixe 1… prefixe Nmax)
#Produits : data.frame htmls_edges_IN (source, target, weight)

sort_edges <- function(htmls_edges, prefixes)
{
htmls_edges_IN <- htmls_edges
sites = prefixes$id_site
for (x in 1:length(sites))
{
prefixe <- prefixes[which(prefixes$id_site == sites[x]),]
INx <- c()
for (y in 5:(4+prefixe$n_prefixes)) # passage par les indices de colonnes en raison de la structure du fichier
{
a <- grep(tolower(prefixe[y]),tolower(htmls_edges$target))
INx <- c(INx,a)
htmls_edges_IN[a,2] <- sites[x]
}
IN=c(IN,unique(INx))
}
htmls_edges_IN <- htmls_edges_IN[IN,]
htmls_edges_IN <- htmls_edges_IN[which(htmls_edges_IN$source != htmls_edges_IN$target),]
htmls_edges_IN <- as.data.frame(table(htmls_edges_IN[,1],htmls_edges_IN[,2]))
names(htmls_edges_IN) <- c(“source”, “target”, “weight”)
return(htmls_edges_IN)
}

#Fonctions d’individualisation des commentaires (precise slicing)
#Remarque : les balises regex sont mal stockées en fichier excel.

#Nécessite library : stringr
#Nécessite fichiers : data.frame comments (id_site, label_site, id_url, url, html) - data.frame balises (id_site, label_site, empty_size, reg_general, reg_time, reg_name, reg_depth, reg_site, reg_text)
#Produits : data.frame isolated_comments (id_site, label_site, id_url, url, Ncom, time_com, name_com, site_com, depth_com, text_com)

isolate_comments <- function(comments,balises)
{
isolated_comments <-  data.frame(id_site=character(0), label_site=character(0), 
id_url=character(0), url=character(0),
Ncom=character(0), time_com=character(0),
name_com=character(0), site_com=character(0),
depth_com=character(0), text_com=character(0),stringsAsFactors=FALSE)
sites <- unique(commentaires$id_site) 
labels <- unique(commentaires$label_site)
for (x in 1:length(SitesComID)) #site by site
{
if (balise$reg_general !='FALSE') #discard sites without comments
{
site <- sites[x] ; 
balise <- balises[which(balises$id == site),]
pages <- comments[which(comments$id_site == site),]
for(y in 1:nrow(pages)) #page by page
{
if (nchar(pages$html[y]) >= as.numeric(balise$empty_size)) #discard pages without comments 
{
coms <- as.list(str_extract_all(as.character(pages$html[y]), balise$reg_general)[[1]])
l=length(coms)
if (l>0)
{
if (balise$reg_time != 'FALSE') 
{time_com <- str_extract(coms,balise$reg_time)} else 
{time_com <- rep('NA',l)}
if (balise$reg_name != 'FALSE') 
{name_com <- str_extract(coms,balise$reg_name)} else 
{name_com <- rep('NA',l)}
if (balise$reg_site != 'FALSE') 
{site_com <- str_extract(coms,balise$reg_site)} else 
{site_com <- rep('NA',l)}
if (balise$reg_depth != 'FALSE') 
{depth_com <- str_extract(coms,balise$reg_depth)} else 
{depth_com <- rep('NA',l)}
isolated_comments=rbind(isolated_comments, data.frame(id_site=rep(site,l),
label_site = rep(labels[x],l),
id_url = rep(pages$id_url[y],l),
url = rep(pages$url[y],l),
Ncom=seq(1,l),
time_com = time_com,
name_com = name_com,
site_com = site_com,
depth_com = depth_com,
text_com <- str_extract(coms,balise$reg_text)))
}
}
}
}
}
return(isolated_comments)
}

#Possible d’ajouter une colonne d’identifiants pour les commentaires
#isolated_comments$id_com <- seq(1,nrow(isolated_comments))