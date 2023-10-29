# libraries
library(ggplot2)
library(ggtext)
library(geomtextpath)  # https://allancameron.github.io/geomtextpath/

# fonts ----
font1 <- "Oswald"
font2 <- "Roboto Mono"

# colors ----
palette1 <- c("#7f7f4e", "#f7f7f0",  "#cdcdda", "#303051" )

# data: https://ourworldindata.org/wild-mammal-decline ----
year <- c(-100000+2020, -10000+2020, 1900, 2020)
agriculturural_land_use_hectare <- c(0, 0.1, 2.5, 4.8)
biomass_carbon_tonne <- c(20, 15, 10, 3)
data <- as.data.frame(cbind(year, agriculturural_land_use_hectare, biomass_carbon_tonne))
data$a_prob <- data$agriculturural_land_use_hectare/max(data$agriculturural_land_use_hectare)
data$b_prob <- data$biomass_carbon_tonne/max(data$biomass_carbon_tonne)

# labels ----
lab_a <- paste("<span>agricultural land use</span>",
              "<span style='font-size:14pt'>(ha)</span>")

lab_b <- paste("<span>wild land mammals biomass</span>",
              "<span style='font-size:14pt'>(carbon t)</span>")

caption1 <- "**100,000 years ago till now** (lines are relative to their respective<br>maximum values) ···· source: **Our World in Data** ···· figure: **PYYXXO**"

#plot ----
FIGURE <- ggplot(data) +
  # LINES
  geom_line(aes(x = year, y = a_prob), linewidth = 6, linejoin = "mitre", alpha = 1,  col = palette1[2]) +
  geom_line(aes(x = year, y = b_prob), linewidth = 6, linejoin = "mitre", alpha = 1,  col = palette1[4]) +
  geom_line(aes(x = year, y = a_prob), linewidth = 6, linejoin = "mitre", alpha = .5, col = palette1[2]) +   # mix lines' intersection
  # LABELS
  geom_textline(aes(x = year, y = a_prob), label = lab_a, 
                size = 14, hjust=0.02, vjust = -0.75, color = palette1[2], rich=TRUE, 
                linewidth = 0, linecolor = "black", linetype=1) + # hide line, only use text
  geom_textline(aes(x = year, y = b_prob), label = lab_b, 
                size = 14, hjust=0.02, vjust = 1.5,  color = palette1[4], rich=TRUE, 
                linewidth = 0, linecolor = "black", linetype=1) + # hide line, only use text
  labs(caption = caption1) + 
  # THEME
  theme_void(base_family = font1) +
  theme( plot.background = element_rect(color = palette1[1], fill = palette1[1]),
         plot.caption.position = "plot",
         plot.caption = element_markdown(colour = palette1[3],  size = 17, family = font2,
                                            hjust = .5, vjust = .5,
                                            margin=unit(c(20, 0, 0, 0),"mm")), 
         plot.margin = unit(c(25, 10, 15, 10),"mm"))

ggsave(FIGURE, file="mammals_v_land.png", width = 16*(2/3), height = 16*(2/3), units = "in")
