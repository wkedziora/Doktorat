rm(list=ls()) #clearing the memory
setwd("D:\\Praca\\Badania\\Doktorat\\") #setting working directory

#loading libraries
library(httr)
library(dplyr)
library(rvest)
library(xml2)

#setting query parameters, all have to be provided (empty for styles means default)
server <- c("http://cbdgmapa.pgi.gov.pl/arcgis/services/kartografia/smgp50k/MapServer/WMSServer?")
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

######################
### Original query ###
######################
# cdbg <- GET("http://cbdgmapa.pgi.gov.pl/arcgis/services/kartografia/smgp50k/MapServer/WMSServer?
#     request=GetFeatureInfo&service=WMS&version=1.1.1&layers=0&styles=&srs=EPSG%3A4326&bbox=18.765420,52.341627,18.765421,52.341628&
#     width=780&height=330&query_layers=0&x=0&y=0")

# cdbg <- GET(query) #sending query

#TODO: when query sends "null" response - omitt record

# #asking for content, parsing to xml and then to list, and unlisting to vector
# cdbg %>% content(., encoding = "UTF-8") %>% xmlParse(.) %>% xmlToList(.) -> results1
# results1[[1]] %>% as.list(.) %>% data.frame(., stringsAsFactors = FALSE) -> results2

#read query xml response, then axtract attributes and place them into a data frame horizontally
read_xml(query) %>% xml_find_all(., "//d1:FIELDS", xml_ns(.)) %>% xml_attrs(.) %>% unlist(.) %>% as.list(.) %>% data.frame(., stringsAsFactors = FALSE) -> results

#loading resultsGPS file with GPS of every sample plot I need to investigate
gps.coord.sample <- read_tsv("gps.txt")

gps.coord <- gps.coord.sample
n <- nrow(gps.coord)
m <- ncol(gps.coord)
for (i in 1:n) {
bbox <- (paste0("bbox=", paste(gps.coord[i,4], gps.coord[i,3], gps.coord[i,4]+0.000001, gps.coord[i,3]+0.000001, sep = ","), "&"))
query <- paste0(server, request, service, version, layers, styles, srs, bbox, width, height, query_layers, x, y)
read_xml(query) %>% xml_find_all(., "//d1:FIELDS", xml_ns(.)) %>% xml_attrs(.) %>% unlist(.) %>% as.list(.) %>% data.frame(., stringsAsFactors = FALSE) -> results
if (is.null(results[1,1]) == FALSE) {gps.coord[i,m+1] <- results[1,1]} else {gps.coord[i,m+1] <- NA}
}
names(gps.coord)[m+1] <- c("litography")
gps.coord$litography <- factor(gps.coord$litography)
summary(gps.coord)

write.table(gps.coord, "resultGPS2.txt", sep="\t", row.names=FALSE)
