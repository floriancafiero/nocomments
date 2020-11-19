#Link extraction
#Requires library : stringr
#Requires files : data.frame htmls (id_site, label_site, id_url, url, html)
#Products : data.frame htmls_edges (source, target, id)

extract_edges <- function(htmls)
{
sites <- unique(htmls$id_site)
htmls_edges <- data.frame(source = character(0), target = character(0), type = character(0), id = integer(0))
Id = 1 ; l = integer(0)
for (x in 1:length(sites))
{
pages_site <- which(htmls$id_site == sites[x])
edges <- unlist(str_match_all(htmls$html[pages_site], "<a[^>]*href\\s*=\\s*(\"[^\">]+[\">]|\'[^\'>]+[\'>]|[^\\s>]+[\\s>])"))
l=length(edges)
if (l != 0)
{
Id = Id + l
htmls_edges <- rbind(htmls_edges, data.frame(source=rep(sites[x],l), target=edges, id=seq(Id-l,Id-1)))
}
}
return(htmls_edges)
}
