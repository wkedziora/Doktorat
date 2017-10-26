rm(list=ls()) #clearing the memory

library(httr)
library(tidyverse)
library(rvest)
library(xml2)

# bulding query -----
#setting query parameters, all have to be provided (empty for styles means default)
server_old <- c("http://cbdgmapa.pgi.gov.pl/arcgis/services/kartografia/smgp50k/MapServer/WMSServer?")
server <- c("http://cbdgmapa.pgi.gov.pl/arcgis/services/kartografia/mgp500k/MapServer/WMSServer?")
request <- c("request=GetFeatureInfo&")
service <- c("service=WMS&")
version <- c("version=1.1.1&")
layers <- c("layers=0&") #layer to bbox
styles <- c("styles=&")
srs <- c("srs=EPSG%3A4326&")
bbox <- c("bbox=18.765420,52.341627,18.765421,52.341628&")
old_bbox <- c("bbox=19.49816426,52.49971120,19.49816427,52.49971121&")
width <- c("width=1&")
height <- c("height=1&")
query_layers <- c("query_layers=0&") #layer to query (?)
x <- c("x=0&") #x and y pixels in bbox to query; (0, 0) means top left
y <- c("y=0&") #in case of exteding no. of query parameteres remember to add "&" at the end of each line
# format <- c("info_format=text/plain")

# preapring query to send
query <- paste0(server, request, service, version, layers, styles, srs, bbox, width, height, query_layers, x, y)

#TODO: when query sends "null" response - omitt record

# testing query -----
#read query xml response, then axtract attributes and place them into a data frame horizontally
read_xml(query) %>% 
  xml_find_all(., "//d1:FIELDS", xml_ns(.)) %>% 
  xml_attrs(.) %>% 
  unlist(.) %>% 
  as.list(.) %>% 
  data.frame(., stringsAsFactors = FALSE) -> results

# looping in WMS for data -----
#loading resultsGPS file with GPS of every sample plot I need to investigate
gps_coord_sample <- read_tsv("resultGPS2.txt")

gps_coord <- gps_coord_sample
n <- nrow(gps_coord)
m <- ncol(gps_coord)
for (i in 1:n) {
bbox <- (paste0("bbox=", paste(gps_coord[i,4], gps_coord[i,3], gps_coord[i,4]+0.000001, gps_coord[i,3]+0.000001, sep = ","), "&"))
query <- paste0(server, request, service, version, layers, styles, srs, bbox, width, height, query_layers, x, y)
read_xml(query) %>% 
  xml_find_all(., "//d1:FIELDS", xml_ns(.)) %>% 
  xml_attrs(.) %>% 
  unlist(.) %>% 
  as.list(.) %>% 
  data.frame(., stringsAsFactors = FALSE) -> results
if (is.null(results[1,1]) == FALSE) {gps_coord[i,m+1] <- results[1,1]} else {gps_coord[i,m+1] <- NA}
}
names(gps_coord)[m+1] <- c("litography_500k")
gps_coord$litography <- factor(gps_coord$litography)
gps_coord$litography_500k <- factor(gps_coord$litography_500k)
summary(gps_coord)

# saving output -----
write_tsv(gps_coord, "litography_500k.txt")
