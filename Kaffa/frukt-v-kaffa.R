# KAFFA & FRUKT 2019
# https://www.kaffaroastery.fi/raakakahvien-hinnat-2019
# https://www.fruktcoffeeroasters.com/blogs/news/green-coffee-transparency-report-2019

library(openxlsx)
library(ggplot2)
library(scales)
library(extrafont)
library(plyr)
library(gridExtra)  # gridarrange

#DATA
fk <- read.xlsx("FruktKaffa.xlsx", sheet = 1)

#SUMMARIZE
yhteenveto1 <- ddply(fk, .(ORIGIN, ROASTERY), summarize,
                    kiloja = sum(KG),
                    usdkg  = sum(FOB),
                    rahaa  = sum(TOTAL))

yhteenveto2 <- ddply(fk, .(ROASTERY), summarize,
                    kiloja = sum(KG),
                    usdkg  = sum(FOB),
                    rahaa  = sum(TOTAL))

yhteenveto1$dollariakilo <- yhteenveto1$rahaa / yhteenveto1$kiloja
yhteenveto2$dollariakilo <- yhteenveto2$rahaa / yhteenveto2$kiloja

# ORDER
yhteenveto1$ORIGIN <- factor(yhteenveto1$ORIGIN, levels = unique(yhteenveto1$ORIGIN[order(yhteenveto1$dollariakilo)]) )

# UPPER PLOT
X1 <- ggplot(yhteenveto1, aes(x=ORIGIN, y=dollariakilo, fill=ROASTERY)) +
  geom_bar(stat="identity", colour="black", width = 0.8, position = position_dodge(width = .8)) +
  scale_fill_manual(name="", 
                    labels=c("Frukt Coffee Roasters                 ", 
                             "Kaffa Roastery"),
                    values=c("gray50", "darkgoldenrod2")) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
  xlab("") + ylab("FOB: $/kg") +
  labs(title = "Raakakahvi, FOB: Painotetut keskiarvot maittain 2019") +
  theme_minimal() +
  theme(legend.position = "top",
        panel.grid.major.y = element_line(color="gray70"), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(family="Staatliches", colour="black"),
        plot.title = element_text(size = 36, hjust=.5), 
        plot.subtitle = element_text(size = 32), 
        axis.text.y = element_text(size = 20, colour = "black", family="Press Start 2P"),
        axis.text.x = element_text(size = 16, colour = "black", angle=0, vjust=15),
        axis.title.y = element_text(size = 24, angle=90, vjust=.5, family="Press Start 2P"), 
        legend.text = element_text(size = 36),
        legend.key.size = unit(3,"line"),
        plot.background = element_rect(fill = "bisque2", colour = NA),
        plot.margin=unit(c(1,2,0,1),"cm"))

# LOWER PLOT
X2 <- ggplot(yhteenveto2, aes(x=ROASTERY, y=kiloja)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black", fill=c("gray50", "darkgoldenrod2")) +
  scale_x_discrete(labels=c("5 160 kg", "169 293 kg")) +
  coord_flip() +
  xlab("") + ylab("") +
  labs(title = "Ostetun kahvin määrä 2019",
       caption = "KOODI: @pyyxxo\n\nDATA: @fruktcoffeeroasters @kaffaroastery") +
  theme_minimal()+
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(family="Staatliches"),
        plot.title = element_text(size = 33, hjust=0.1), 
        plot.subtitle = element_text(size = 24), 
        axis.text.y = element_text(size = 24, family="Press Start 2P", colour = "black"),
        plot.background = element_rect(fill = "bisque2", colour = NA),
        plot.caption = element_text(color = "black", family="Press Start 2P", size=10, hjust=1),
        plot.margin=unit(c(0,2,1,1),"cm") )

# COMBINE PLOTS
layout2 <- rbind(1,1,1,2)
X3 <- grid.arrange(X1, X2, layout_matrix = layout2)
ggsave(X3, file="FRKA.jpg", width = 12, height = 12, units = "in") 
