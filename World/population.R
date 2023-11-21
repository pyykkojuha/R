# population
# libraries ----
library(ggplot2)
library(scales)
library(viridis)
library(gridExtra)
library(ggtext)

# data ----
pop <- read.csv("population.csv")
world <- subset(pop, Entity=="World")

## growth rate ----
world$growth <- NA
world$years <- NA
world$growth.y <- NA
world$billions <- NA

B <- 1
for(i in 2:length(world$Year)){
  if(world$Year[i]>world$Year[i-1]){
    world$growth[i]   <- world$Population..historical.estimates.[i] / world$Population..historical.estimates.[i-1]
    world$years[i]    <- world$Year[i]   - world$Year[i-1]
    world$growth.y[i] <- world$growth[i] / world$years[i]
    if(world$Population..historical.estimates.[i] >= B * 1000000000){
      world$billions[i] <- B
      B <- B+1
    }
  }
}
world$billions2 <- world$billions*0
world$billions3 <- paste0(world$billions, "B")

# fonts ----
FONT1 <- "Atkinson Hyperlegible"
FONT2 <- "Monaco"

# WORLD GROWTH PLOT ----
A <- ggplot(subset(world, years==1), aes(x=Year, y=growth-1, col=growth-1)) +
  scale_color_viridis(option="inferno", labels = percent, guide = guide_colorbar(), limits=c(0,.025)) +
  #scale_fill_continuous(guide = guide_colorbar())
  geom_line(linewidth=1.4) +  #1.35
 # geom_point(size=2) +
  #geom_hline(yintercept = 0) +
  labs(title="Planet Earth's Human Population Growth from 1801 to 2021",
       colour="Yearly population growth rate",
       y="Growth",
       x=NULL) +
 # guides(color=guide_legend(title="Yearly growth")) +
  scale_y_continuous(labels = scales::percent_format(accuracy=.1), 
                     limits=c(0,.03), 
                     breaks=seq(0,.03,.01),
                     expand=c(0,0)) +
  #scale_x_continuous(breaks=c(1801, seq(1850,2000,50),2021)) +
  scale_x_continuous(breaks=seq(1801,2021,20),
                     expand=c(0,0)) +
  theme_minimal(base_family = FONT1, 
                base_size = 17) +
  theme(legend.position = "none",
        plot.title.position = "plot",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(linewidth = .2),
        axis.text = element_text(family=FONT2, color="gray20"),
        axis.title.y = element_text(hjust=1.11, color="black", size=20),   
        plot.title = element_text(size=22.61, margin=unit(c(0, 0, 7.5, 0),"mm")),
        plot.background = element_rect(fill = "grey70", colour = NA), 
        plot.margin = unit(c(10, 13, 1.4, 7),"mm"))

B <- ggplot(subset(world, years==1)) +
  geom_col(aes(x=Year, y=Population..historical.estimates., color=growth-1,  fill=growth-1)) +
  #geom_line(aes(x=Year, y=Population..historical.estimates.), col="black", linewidth=3) +
  scale_color_viridis(option="inferno", labels = percent, limits=c(0,.025)) +
  scale_fill_viridis( option="inferno", labels = percent, limits=c(0,.025)) +
  #BILLIONS
  #geom_point(aes(x=Year, y=billions*1000000000), shape=23, col="white", fill=NA, size=5) +
  geom_point(aes(x=Year, y=billions2), shape=18, col="white", fill=NA, size=10) +
  geom_text(aes(x=Year, y=billions2+25000000, label=billions),  col="black", hjust=.5, vjust=0, size=3, family=FONT2) +
  labs(#title="Yearly population growth rate",
       #label="Yearly growth",
       x="Year",
       y="Population",
       caption="Graph: **PYYXXO** | Source: **OurWorldInData.org**") +
  guides(fill  = guide_legend(title="Yearly growth"),
         color = guide_legend(title="Yearly growth")) +
  scale_y_continuous(labels = unit_format(unit = "B ", scale = 1e-9), 
                     breaks=seq(0,8*10^9, 1*10^9), 
                     limits=c(0,8*10^9),
                     expand=c(0.0,0.09)) +
  #scale_x_continuous(breaks=c(1801, seq(1850,2000,50),2021)) +
  scale_x_continuous(breaks=seq(1801,2021,20),
                     expand=c(0,0)) +
  theme_minimal(base_family = FONT1, 
                base_size = 17) +
  theme(legend.position = "none",
        plot.caption.position = "plot",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(linewidth = .2),
        axis.text = element_text(family=FONT2, color="gray20"),
        axis.title.y = element_text(hjust=1.0235, color="black", size=20),
        axis.title.x = element_text(hjust=1.03, color="black", size=20), 
        plot.background = element_rect(fill = "grey70", colour = NA),
        plot.caption = element_markdown(size=10, hjust=1.041),
        plot.margin = unit(c(0, 13, 7, 7),"mm"))

AB <- grid.arrange(A, B, heights=c(1/3, 2/3), ncol=1)

ggsave(AB, file="population_world_1801_2021_point.png", width = 9, height = 9, units = "in")
