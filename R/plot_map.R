library(dplyr)
library(leaflet)

plot_map <- function(data, variable){
  
  # get lad names
  la <- data %>% pull(lad20nm)
  # get domain values
  domain <- data %>% pull(variable)
  
  # define palette
  pal <- colorNumeric(palette = "viridis", domain=domain, na.color="grey")
  
  # make the leaflet
  m <- leaflet(data) %>%
    #addTiles() %>%
    addPolygons(fillColor = ~pal(domain),
                stroke = TRUE, weight = 0.5, color = "black",
                label = ~lapply(paste0(la, "<br>", variable, ":", round(domain,2)), htmltools::HTML)
    ) %>%
    addLegend(pal = pal, 
              values = ~domain, position = "bottomleft", title = "",
    ) %>%
    
    
    
    addControl("Green space in London", position = "topright") %>%
    addProviderTiles(providers$CartoDB.Positron) 
  m
}