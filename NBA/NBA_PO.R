# NBA PLAYOFFS POINTS BY DUOS

library(ggplot2)
library(extrafont)
library(gridExtra)

load("PO.Rdata")

# summarize
summary(PO_BULLS)
summary(PO_LAKERS)
summary(PO_THUNDER)
summary(PO_WARRIORS)

PO_BULLS$Date[1];PO_BULLS$Date[dim(PO_BULLS)[1]]
PO_LAKERS$Date[1];PO_LAKERS$Date[dim(PO_LAKERS)[1]]
PO_THUNDER$Date[1];PO_THUNDER$Date[dim(PO_THUNDER)[1]]
PO_WARRIORS$Date[1];PO_WARRIORS$Date[dim(PO_WARRIORS)[1]]

# layout for 16:9 combining plot and text
lay <- rbind(c(rep(1,9), rep(2,16-9)))

#
# # 
# # # BULLS

BULLS1 <- ggplot(PO_BULLS, aes(y=Jordan , x=Pippen))  + 
  coord_fixed(ratio=1)  +
  geom_abline(slope=1, color="black", size = 0.1) +    
  scale_x_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  scale_y_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .5 , colour="black") + 
  scale_fill_distiller(palette="YlOrRd", direction=1) + 
  xlab("SCOTTIE PIPPEN") +
  ylab("MICHAEL JORDAN") +
  annotate("text", x=10, y=50, label="23", hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  annotate("text", x=50, y=10, label="33", hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  geom_point(shape=4, size=3, colour="black", alpha=.6) +
  scale_shape_identity() + 
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#CE1141", colour = NA),
        text=element_text(family="Staatliches"),
        panel.grid.major.y = element_line(colour = "black", size = 0.2), 
        panel.grid.major.x = element_line(colour = "black", size = 0.2),
        panel.grid.minor  = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.ticks.x  = element_blank(),
        plot.title    = element_text(size = 48, colour = "black",  hjust=.5), 
        plot.subtitle = element_text(size = 44, colour = "ghostwhite", hjust=.5, margin = unit(c(0, 0, 5 , 0), "mm")), 
        plot.caption  = element_text(size = 12, colour = "black",  hjust=1, family="Press Start 2P"), 
        axis.text.x   = element_text(size = 32, colour = "black",  angle=0, hjust=.5),
        axis.text.y   = element_text(size = 32, colour = "black",  angle=0, vjust=.5),
        axis.title.x  = element_text(size = 64, colour = "ghostwhite", angle=0,   hjust=.5, margin = unit(c(10, 0, 0, 0), "mm")),
        axis.title.y  = element_text(size = 64, colour = "ghostwhite", angle=270, vjust=.5, margin = unit(c(0, 10, 0, 0), "mm")),
        legend.position = "none",
        plot.margin = unit(c(.1, .1, .5, .5),"cm") ) 

BULLS2 <- ggplot(PO_BULLS, aes(y=Jordan , x=Pippen))  + 
  coord_fixed(ratio=2/3)  + scale_x_continuous(limits=c(0,30)) + scale_y_continuous(limits=c(0,60)) +
  annotate("text", x=30, y=59, hjust=1, vjust=.5,size = 40, color="black",  family="Staatliches",    alpha=1,  label="CHICAGO BULLS") +
  annotate("text", x=30, y=50, hjust=1, vjust=.5,size = 30, color="ghostwhite", family="Staatliches",    alpha=1,  label="JORDAN + PIPPEN") +
  annotate("text", x=30, y=30, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="POINTS IN PLAYOFF GAMES TOGETHER") +
  annotate("text", x=30, y=26, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="1988-1998") +
  annotate("text", x=30, y=20, hjust=1, vjust=.5,size = 12, color="black",  family="Staatliches",    alpha=1, label="HEAT MAP + 168 GAMES (X)") +
  annotate("text", x=30, y=4,  hjust=1, vjust=.5,size =  8, color="black",  family="Press Start 2P", alpha=1, label="@pyyxxo") +
  annotate("text", x=30, y=0,  hjust=1, vjust=.5,size =  4, color="black",  family="Press Start 2P", alpha=1, label="SOURCE: basketball-reference.com") +
  geom_point(shape=4, size=3, colour="azure", alpha=.0) + theme_void() +
  theme(plot.background = element_rect(fill = "#CE1141", colour = NA), plot.margin = unit(c(.1, .1, .1, .1),"cm") ) 

BULLS <- grid.arrange(BULLS1, BULLS2, layout_matrix = lay)
ggsave(BULLS, file="fig/PO_BULLS.png", width = (16/9)*12, height = 12, units = "in", bg = "#CE1141") 

#
# # 
# # # LAKERS

LAKERS1 <- ggplot(PO_LAKERS, aes(y=Oneal , x=Bryant))  + 
  coord_fixed(ratio=1)  +
  geom_abline(slope=1, color="#FDB927", size = 0.1) +    
  scale_x_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  scale_y_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .5 , colour="black") + 
  scale_fill_distiller(palette="YlOrRd", direction=1) + 
  xlab("KOBE BRYANT") +
  ylab("SHAQUILLE O'NEAL") +
  annotate("text", x=10, y=50, label="34", hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  annotate("text", x=50, y=10, label="8",  hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  geom_point(shape=4, size=3, colour="#FDB927", alpha=.6) +
  scale_shape_identity() + 
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#552583", colour = NA),
        text=element_text(family="Staatliches"),
        panel.grid.major.y = element_line(colour = "#FDB927", size = 0.2), 
        panel.grid.major.x = element_line(colour = "#FDB927", size = 0.2),
        panel.grid.minor  = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.ticks.x  = element_blank(),
        plot.title    = element_text(size = 48, colour = "#FDB927",  hjust=.5), 
        plot.subtitle = element_text(size = 44, colour = "ghostwhite", hjust=.5, margin = unit(c(0, 0, 5 , 0), "mm")), 
        plot.caption  = element_text(size = 12, colour = "#FDB927",  hjust=1, family="Press Start 2P"), 
        axis.text.x   = element_text(size = 32, colour = "#FDB927",  angle=0, hjust=.5),
        axis.text.y   = element_text(size = 32, colour = "#FDB927",  angle=0, vjust=.5),
        axis.title.x  = element_text(size = 64, colour = "ghostwhite", angle=0,   hjust=.5, margin = unit(c(10, 0, 0, 0), "mm")),
        axis.title.y  = element_text(size = 64, colour = "ghostwhite", angle=270, vjust=.5, margin = unit(c(0, 10, 0, 0), "mm")),
        legend.position = "none",
        plot.margin = unit(c(.1, .1, .5, .5),"cm") ) 

LAKERS2 <- ggplot(PO_LAKERS, aes(y=Oneal , x=Bryant))  + 
  coord_fixed(ratio=2/3)  + scale_x_continuous(limits=c(0,30)) + scale_y_continuous(limits=c(0,60)) +
  annotate("text", x=30, y=59, hjust=1, vjust=.5,size = 48, color="#FDB927",  family="Staatliches",    alpha=1,  label="L.A. LAKERS") +
  annotate("text", x=30, y=50, hjust=1, vjust=.5,size = 30, color="ghostwhite", family="Staatliches",    alpha=1,  label="O'NEAL + BRYANT") +
  annotate("text", x=30, y=30, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="POINTS IN PLAYOFF GAMES TOGETHER") +
  annotate("text", x=30, y=26, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="1997-2004") +
  annotate("text", x=30, y=20, hjust=1, vjust=.5,size = 12, color="#FDB927",  family="Staatliches",    alpha=1, label="HEAT MAP + 119 GAMES (X)") +
  annotate("text", x=30, y=4,  hjust=1, vjust=.5,size =  8, color="#FDB927",  family="Press Start 2P", alpha=1, label="@pyyxxo") +
  annotate("text", x=30, y=0,  hjust=1, vjust=.5,size =  4, color="#FDB927",  family="Press Start 2P", alpha=1, label="SOURCE: basketball-reference.com") +
  geom_point(shape=4, size=3, colour="azure", alpha=.0) + theme_void() +
  theme(plot.background = element_rect(fill = "#552583", colour = NA), plot.margin = unit(c(.1, .1, .1, .1),"cm") ) 

LAKERS <- grid.arrange(LAKERS1, LAKERS2, layout_matrix = lay)
ggsave(LAKERS, file="fig/PO_LAKERS.png", width = (16/9)*12, height = 12, units = "in", bg = "#552583") 

#
# # 
# # # THUNDER

THUNDER1 <- ggplot(PO_THUNDER, aes(y=Durant , x=Westbrook))  + 
  coord_fixed(ratio=1)  +
  geom_abline(slope=1, color="#EF3B24", size = 0.1) +    
  scale_x_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  scale_y_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .5 , colour="black") + 
  scale_fill_distiller(palette="YlOrRd", direction=1) + 
  xlab("RUSSELL WESTBROOK") +
  ylab("KEVIN DURANT") +
  annotate("text", x=10, y=50, label="35", hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  annotate("text", x=50, y=10, label="0",  hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  geom_point(shape=4, size=3, colour="#EF3B24", alpha=.6) +
  scale_shape_identity() + 
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#007AC1", colour = NA),
        text=element_text(family="Staatliches"),
        panel.grid.major.y = element_line(colour = "#EF3B24", size = 0.2), 
        panel.grid.major.x = element_line(colour = "#EF3B24", size = 0.2),
        panel.grid.minor  = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.ticks.x  = element_blank(),
        plot.title    = element_text(size = 48, colour = "#EF3B24",  hjust=.5), 
        plot.subtitle = element_text(size = 44, colour = "ghostwhite", hjust=.5, margin = unit(c(0, 0, 5 , 0), "mm")), 
        plot.caption  = element_text(size = 12, colour = "#EF3B24",  hjust=1, family="Press Start 2P"), 
        axis.text.x   = element_text(size = 32, colour = "#EF3B24",  angle=0, hjust=.5),
        axis.text.y   = element_text(size = 32, colour = "#EF3B24",  angle=0, vjust=.5),
        axis.title.x  = element_text(size = 64, colour = "ghostwhite", angle=0,   hjust=.5, margin = unit(c(10, 0, 0, 0), "mm")),
        axis.title.y  = element_text(size = 64, colour = "ghostwhite", angle=270, vjust=.5, margin = unit(c(0, 10, 0, 0), "mm")),
        legend.position = "none",
        plot.margin = unit(c(.1, .1, .5, .5),"cm") ) 

THUNDER2 <- ggplot(PO_THUNDER, aes(y=Durant , x=Westbrook))  + 
  coord_fixed(ratio=2/3)  + scale_x_continuous(limits=c(0,30)) + scale_y_continuous(limits=c(0,60)) +
  annotate("text", x=30, y=59, hjust=1, vjust=.5,size = 46, color="#EF3B24",  family="Staatliches",    alpha=1,  label="OKC THUNDER") +
  annotate("text", x=30, y=50, hjust=1, vjust=.5,size = 26, color="ghostwhite", family="Staatliches",    alpha=1,  label="DURANT + WESTBROOK") +
  annotate("text", x=30, y=30, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="POINTS IN PLAYOFF GAMES TOGETHER") +
  annotate("text", x=30, y=26, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="2010-2016") +
  annotate("text", x=30, y=20, hjust=1, vjust=.5,size = 12, color="#EF3B24",  family="Staatliches",    alpha=1, label="HEAT MAP + 82 GAMES (X)") +
  annotate("text", x=30, y=4,  hjust=1, vjust=.5,size =  8, color="#EF3B24",  family="Press Start 2P", alpha=1, label="@pyyxxo") +
  annotate("text", x=30, y=0,  hjust=1, vjust=.5,size =  4, color="#EF3B24",  family="Press Start 2P", alpha=1, label="SOURCE: basketball-reference.com") +
  geom_point(shape=4, size=3, colour="azure", alpha=.0) + theme_void() +
  theme(plot.background = element_rect(fill = "#007AC1", colour = NA), plot.margin = unit(c(.1, .1, .1, .1),"cm") ) 

THUNDER <- grid.arrange(THUNDER1, THUNDER2, layout_matrix = lay)
ggsave(THUNDER, file="fig/PO_THUNDER.png", width = (16/9)*12, height = 12, units = "in", bg = "#007AC1") 

#
# # 
# # # WARRIORS

WARRIORS1 <- ggplot(PO_WARRIORS, aes(y=Curry , x=Thompson))  + 
  coord_fixed(ratio=1)  +
  geom_abline(slope=1, color="#FDB927", size = 0.1) +    
  scale_x_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  scale_y_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .5 , colour="black") + 
  scale_fill_distiller(palette="YlOrRd", direction=1) + 
  xlab("KLAY THOMPSON") +
  ylab("STEPHEN CURRY") +
  annotate("text", x=10, y=50, label="30", hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  annotate("text", x=50, y=10, label="11", hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  geom_point(shape=4, size=3, colour="#FDB927", alpha=.6) +
  scale_shape_identity() + 
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#006BB6", colour = NA),
        text=element_text(family="Staatliches"),
        panel.grid.major.y = element_line(colour = "#FDB927", size = 0.2), 
        panel.grid.major.x = element_line(colour = "#FDB927", size = 0.2),
        panel.grid.minor  = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.ticks.x  = element_blank(),
        plot.title    = element_text(size = 48, colour = "#FDB927",  hjust=.5), 
        plot.subtitle = element_text(size = 44, colour = "ghostwhite", hjust=.5, margin = unit(c(0, 0, 5 , 0), "mm")), 
        plot.caption  = element_text(size = 12, colour = "#FDB927",  hjust=1, family="Press Start 2P"), 
        axis.text.x   = element_text(size = 32, colour = "#FDB927",  angle=0, hjust=.5),
        axis.text.y   = element_text(size = 32, colour = "#FDB927",  angle=0, vjust=.5),
        axis.title.x  = element_text(size = 64, colour = "ghostwhite", angle=0,   hjust=.5, margin = unit(c(10, 0, 0, 0), "mm")),
        axis.title.y  = element_text(size = 64, colour = "ghostwhite", angle=270, vjust=.5, margin = unit(c(0, 10, 0, 0), "mm")),
        legend.position = "none",
        plot.margin = unit(c(.1, .1, .5, .5),"cm") ) 

WARRIORS2 <- ggplot(PO_WARRIORS, aes(y=Curry , x=Thompson))  + 
  coord_fixed(ratio=2/3)  + scale_x_continuous(limits=c(0,30)) + scale_y_continuous(limits=c(0,60)) +
  annotate("text", x=30, y=59, hjust=1, vjust=.5,size = 40, color="#FDB927",  family="Staatliches",    alpha=1,  label="G.S. WARRIORS") +
  annotate("text", x=30, y=50, hjust=1, vjust=.5,size = 30, color="ghostwhite", family="Staatliches",    alpha=1,  label="CURRY + THOMPSON") +
  annotate("text", x=30, y=30, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="POINTS IN PLAYOFF GAMES TOGETHER") +
  annotate("text", x=30, y=26, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="2013-2019") +
  annotate("text", x=30, y=20, hjust=1, vjust=.5,size = 12, color="#FDB927",  family="Staatliches",    alpha=1, label="HEAT MAP + 110 GAMES (X)") +
  annotate("text", x=30, y=4,  hjust=1, vjust=.5,size =  8, color="#FDB927",  family="Press Start 2P", alpha=1, label="@pyyxxo") +
  annotate("text", x=30, y=0,  hjust=1, vjust=.5,size =  4, color="#FDB927",  family="Press Start 2P", alpha=1, label="SOURCE: basketball-reference.com") +
  geom_point(shape=4, size=3, colour="azure", alpha=.0) + theme_void() +
  theme(plot.background = element_rect(fill = "#006BB6", colour = NA), plot.margin = unit(c(.1, .1, .1, .1),"cm") ) 

WARRIORS <- grid.arrange(WARRIORS1, WARRIORS2, layout_matrix = lay)
ggsave(WARRIORS, file="fig/PO_WARRIORS.png", width = (16/9)*12, height = 12, units = "in", bg = "#006BB6")

#end
