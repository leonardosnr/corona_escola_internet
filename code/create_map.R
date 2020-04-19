# load packages
library(tidyverse)
library(leaflet)

# load data
  # geographical data
db.geo <- read_csv2('./data/11_Escolas_coordenadas.csv') %>% 
  mutate(id_school = 35000000 + COD_ESC) %>% 
  select(MUN, id_school, NOMESC, NOMEDEP, DS_LONGITUDE, DS_LATITUDE)

names(db.geo) <- c('name_mun', 'id_school', 'name_sch', 'name_provider', 'long', 'lat')

db.stud <- read_csv('./data/stud_data_prova_brasil_2017.csv')

db.geo <- db.geo %>% 
  left_join(db.stud, by = 'id_school')


# prepare map
  ## define colors
db.geo <- db.geo %>% 
  mutate(colors = case_when(
    perc_computer < .5 ~ "red",
    between(perc_computer, .50, .75) ~ "yellow",
    perc_computer > .75 ~ "green",
    is.na(perc_computer) ~ "grey"
  ))
  
db.map <- db.geo %>% 
  filter(name_mun == "SAO PAULO")
  
leaflet::leaflet(data = db.map[1:1000, ]) %>% 
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
  addCircleMarkers(radius = 3, 
                   color = ~colors,
                   fillOpacity = 0.5)
