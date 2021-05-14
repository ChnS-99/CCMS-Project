library(ggmap)
locs = as.character(a$Address[1])
register_google(key = "AIzaSyD_esKrQqYTkbnLnaKLzwcPYBsVazCIzP4")
locs <- c('Jiron Cuzco 423, Magdalena del Mar', 'Av Nicolas Arriola 500, La Victoria')
z = geocode(locs)

library(leaflet)
library(htmltools)

leaflet(data = df) %>% addTiles() %>% addMarkers(~Longitude, ~Latitude, label = ~htmlEscape(Name))
labelOptions = labelOptions(noHide = T, direction = "bottom",
                            style = list(
                              "color" = "black",
                              "font-family" = "serif",
                              "font-style" = "bold",
                              "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                              "font-size" = "12px",
                              "border-color" = "rgba(0,0,0,0.5)"))

address_list <- unique(a$Address)

###CREATING A DATA FRAME TO APPEND VALUES TO ###
address <- c()
lat <- c()
long <- c()

###GEOCODE###
for (i in 1:length(address_list)){
  coord <- geocode(address_list[i])
  address <- c(address,address_list[i])
  lat <- c(lat,coord[2])
  long <- c(long,coord[1])
}