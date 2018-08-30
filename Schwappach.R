library(odbc)
library(DBI)
library(tidyverse)
library(feather)

# database connection -----
testdb <- file.path("D:\\Praca\\Badania\\Doktorat\\Schwappach") #determining database filepath
con <- dbConnect(odbc::odbc(), dsn = "Schwappach", encoding = "Windows-1250")#connecting to db
# dbListTables(con) #listing all tables available in th database

# data loading -----
dbReadTable(con, "F_ACTUAL_FUNCTION") %>%
  as.tibble() %>%
  filter(SPECIES_CD == "SO") %>%
  mutate_if(is.character, as.factor) -> Schwappach

### Z PYTHONA
def getGompertz(gat, bon, wiek, gomp, fl):
  a, b, c, d, minAge, maxAge = gomp[gat, bon, fl]
if wiek > maxAge: wiek = maxAge
if wiek >= minAge:
  v = a * math.exp(-b * math.exp(-c * wiek)) + d
else: v = 0
return v

def oblicz_bon(gat, wiek, wys, gomp):
  if gat == "SO":
  lBon = ('IA', 'I', 'II', 'III', 'IV', 'V')
elif gat in ("BRZ", "DB", "OL", "OS"):
  lBon = ('I', 'II', 'III', 'IV')
elif gat == "JS":
  lBon = ('I', 'II')
elif gat == "MD":
  lBon = ('I', 'II', 'III')
else:
  lBon = ('I', 'II', 'III', 'IV', 'V')

hPop = 1000
oBon = ""

for bon in lBon:
  ageMin, ageMax = getGompertzMinMax(gat, bon, gomp, 'H')
if wiek > ageMax: wiek = ageMax
if (gat == "SO" and bon == "IA") or (gat <> "SO" and bon == "I"):
  if wiek < ageMin:
  oBon = ""
break
h = getGompertz(gat, bon, wiek, gomp, 'H')
if wys > (h + hPop) / 2.0:
  break
else:
  oBon = bon
hPop = h
return oBon