# load packages
library(tidyverse)
library(leaflet)

# load data
  # geographical data
db.geo <- read_csv('./data/escolas_enderecos_0.csv')
names(db.geo)
db.geo <- db.geo %>% 
  mutate(id_school = 35000000 + CD_ESCOLA) %>% 
  select(mun, id_school, NM_COMPLETO_ESCOLA, depadm, nomedep, LONGITUDE, LATITUDE)

names(db.geo) <- c('name_mun', 'id_school', 'name_sch', 'id_provider', 'name_provider', 'long', 'lat')

db.geo %>% 
  filter(grepl("CASAIS", name_sch))

db.stud <- read_csv('./data/stud_data_prova_brasil_2017.csv')

db.geo <- db.geo %>% 
  left_join(db.stud, by = 'id_school') 

# prepare map
  ## define colors
pal <- colorFactor(palette = c("#d7191c", "#fec44f", "#1a9641"), 
             levels = c(1, 2, 3), na.color = "#808080", alpha = FALSE,
             reverse = FALSE)

db.geo <- db.geo %>% 
  mutate(colors = case_when(
    perc_computer < .5 ~ 1,
    between(perc_computer, .50, .75) ~ 2,
    perc_computer > .75 ~ 3,
    is.na(perc_computer) ~ as.numeric(NA)
  )) %>% 
  mutate(popup = paste(sep = '',
                       "<b><a ", "<br/>", name_sch, "<br/>", "</a></b> <br/>",
                       "<b>% of students w/ computer: </b>", round(perc_computer*100), "%",
                       "<br/>", "<b>Administração:</b> ", name_provider 
  )) %>% 
  # filter for NA in the computer number
  filter(!is.na(perc_computer))
  
db.map <- db.geo %>% 
  filter(name_mun == "SAO PAULO" & id_provider <= 2) %>% 
  # clean points out of the city
  filter(between(long, -47,  -46),
         between(lat, -24,  -23))

leaflet::leaflet(data = db.map[, ]) %>% 
  addProviderTiles(providers$Esri.WorldStreetMap) %>% 
  addCircleMarkers(radius = 7, 
                   color = ~pal(colors),
                   fillOpacity = .9, popup = ~popup, group = ~name_provider) %>% 
  addLegend(
    position = "bottomright",
    colors = c("#d7191c", "#fec44f", "#1a9641"),
    labels = c("Menos de 50%", "Entre 50 e 75%", "Mais do que 75%"), opacity = 1,
    title = "Percentual de estudantes <br/> com computador em casa"
  ) %>% 
  addLayersControl(
    overlayGroups = ~name_provider,
    options = layersControlOptions(collapsed = FALSE)
  )
