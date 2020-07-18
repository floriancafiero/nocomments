#Extraction of each comment in the comment section (precise slicing)
#Requires library : stringr
#Requires files : data.frame comments (id_site, label_site, id_url, url, html) - data.frame balises (id_site, label_site, empty_size, reg_general, reg_time, reg_name, reg_depth, reg_site, reg_text)
#Products : data.frame isolated_comments (id_site, label_site, id_url, url, Ncom, time_com, name_com, site_com, depth_com, text_com)

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

#Possible to add an ID column for the comments
#isolated_comments$id_com <- seq(1,nrow(isolated_comments))
