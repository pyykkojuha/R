# Based on: https://ggplot2tutor.com/streetmaps/streetmaps/

library(tidyverse)
library(osmdata)
library(ggplot2)

# data

streets <- getbb("Tampere Finland")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

small_streets <- getbb("Tampere Finland")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

river <- getbb("Tampere Finland")%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

lake <- getbb("Tampere Finland")%>%
  opq()%>%
  add_osm_feature(key = "water", value = "lake") %>%
  osmdata_sf()

railway <- getbb("Tampere Finland")%>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()

# plot

ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "#7fc0ff",
          size = .4,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .2,
          alpha = .6) +
  geom_sf(data = railway$osm_lines,
          inherit.aes = FALSE,
          color = "firebrick",
          size = .2,
          alpha = .5) +
  coord_sf(xlim = c(23.55, 24.1), 
           ylim = c(61.43, 61.85),
           expand = FALSE) +
  labs(title = "T  A  M  P  E  R  E", caption ="© OpenStreetMap contributors        @pyyxxo") +
  theme_void() +
  theme(plot.background = element_rect(fill = "gray10", color = NA),
        panel.background = element_rect(fill = "#282828", color = NA), 
        panel.border = element_rect(colour = "firebrick", fill=NA, size=4), 
        plot.title =    element_text(hjust=.5, size=60,
                                     family="Alata",
                                     colour = "firebrick",
                                     margin = unit(c(0, 0, 5, 0), "mm")),
        plot.caption =    element_text(hjust=1, size=10,
                                       family="Press Start 2P",
                                       colour = "firebrick",
                                       margin = unit(c(10, 0, 0, 0), "mm")),
        plot.margin=unit(c(10, 1, 10, 1),"mm")  )

ggsave("tampere.png", width = 12, height = 12, units = "in", bg = "gray10")
