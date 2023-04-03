# EKV23
library(scales)
library(ggplot2)
library(extrafont)

# DATA & EDIT
EKV <- read.csv(paste0(DIRECTORY, "EKV23.csv"))
EKV$kannatus23 <- EKV$kannatus23/1000
EKV$kannatus19 <- EKV$kannatus19/1000 

# COLORS & FONTS
COLORS <- c("Kok."  = EKV$ylecol[1],
            "PS"    = EKV$ylecol[2],
            "SDP"   = EKV$ylecol[3],
            "Kesk." = EKV$ylecol[4],
            "Vas."  = EKV$ylecol[5],
            "Vihr." = EKV$ylecol[6],
            "RKP"   = EKV$ylecol[7],
            "KD"    = EKV$ylecol[8],
            "Liik." = EKV$ylecol[9],
            "Åland" = EKV$ylecol[10])

FONT1 = "Andada Pro"
FONT2 = "Inter"

# FIGURE
PLOT <- ggplot(EKV) +
  # EQUAL LINE
  geom_abline(intercept=0, slope=.005, col="gray80") +
  # DATA
  geom_segment(aes(x=paikkoja19, xend=paikkoja23, y=kannatus19, yend=kannatus23, col=puolue), alpha=.8, size=1.25) +
  geom_point(aes(x=paikkoja23, y=kannatus23, col=puolue), size=4) + 
  # LABELS
  geom_text(data=EKV[1,],  aes(x=paikkoja23,     y= kannatus23+.011, label=puolue, colour=puolue), angle=0, hjust=.5, vjust=.5, family=FONT2, size=5) +
  geom_text(data=EKV[2,],  aes(x=paikkoja23+0.5, y= kannatus23-.011, label=puolue, colour=puolue), angle=0, hjust=.5, vjust=.5, family=FONT2, size=5) +
  geom_text(data=EKV[3,],  aes(x=paikkoja23-1.5, y= kannatus23,      label=puolue, colour=puolue), angle=0, hjust=1,  vjust=.5, family=FONT2, size=5) +
  geom_text(data=EKV[4,],  aes(x=paikkoja23+1.2, y= kannatus23-.006, label=puolue, colour=puolue), angle=0, hjust=0,  vjust=.5, family=FONT2, size=5) +
  geom_text(data=EKV[5,],  aes(x=paikkoja23-1.3, y= kannatus23,      label=puolue, colour=puolue), angle=0, hjust=1,  vjust=.5, family=FONT2, size=5) +
  geom_text(data=EKV[6,],  aes(x=paikkoja23+1.7, y= kannatus23-.011, label=puolue, colour=puolue), angle=0, hjust=.5, vjust=.5, family=FONT2, size=5) +
  geom_text(data=EKV[7,],  aes(x=paikkoja23+1.2, y= kannatus23,      label=puolue, colour=puolue), angle=0, hjust=0,  vjust=.5, family=FONT2, size=5) +
  geom_text(data=EKV[8,],  aes(x=paikkoja23,     y= kannatus23+.011, label=puolue, colour=puolue), angle=0, hjust=.5, vjust=.5, family=FONT2, size=5) +
  geom_text(data=EKV[9,],  aes(x=paikkoja23+1.2, y= kannatus23,      label=puolue, colour=puolue), angle=0, hjust=0,  vjust=.5, family=FONT2, size=5) +
  geom_text(data=EKV[10,], aes(x=paikkoja23+1.5, y= kannatus23,      label=puolue, colour=puolue), angle=0, hjust=0,  vjust=.5, family=FONT2, size=5) +
  # EXPLAIN
  geom_segment(aes(x=3, xend=3, y=.215, yend=.235),               col="gray50", size=.75) +
  geom_point(  aes(x=3,                    y=.235),               col="gray50", size=3) + 
  geom_text(   aes(x=4.1,                  y=.235, label="2023"), col="gray50", size=3, hjust=0, vjust=.5, family=FONT2) +
  geom_text(   aes(x=4.1,       y=.215,            label="2019"), col="gray50", size=3, hjust=0, vjust=0,  family=FONT2) +
  geom_text(   aes(x=41,        y=.157,            label="Enemmän paikkoja\nkuin kannatusta"),  col="gray50", size=2.4, hjust=0, vjust=0, family=FONT2) +
  geom_text(   aes(x=1,         y=.096,            label="Vähemmän paikkoja\nkuin kannatusta"), col="gray50", size=2.4, hjust=0, vjust=1, family=FONT2) +
  # EXTRA  
  scale_color_manual(values=COLORS) +
  scale_x_continuous(limits=c(0,50), breaks=seq(0,50,10)) +
  scale_y_continuous(limits=c(0,.25), breaks=seq(0,.25,.05), labels = scales::label_percent(accuracy = 1L, suffix = " %")) +
  coord_fixed(ratio=200) +
  # LABS
  labs(title    = "Suomen eduskuntavaalit 2023",
       subtitle = "Kannatuksen ja paikkojen muutos vuodesta 2019 vuoteen 2023",
       x        = "Paikat eduskunnassa", 
       y        = "Kannatus vaaleissa",
       caption  = "Kuvio: PYYXXO | Lähde: Yle") +
  # THEME
  theme_minimal() +
  theme(legend.position = "none", 
        plot.background  = element_rect(colour = "white", fill = "white"),
        panel.background = element_rect(colour = "white", fill = "white"),
        plot.title    = element_text(hjust = 0, colour = "black", size = 21, family = FONT1),
        plot.subtitle = element_text(hjust = 0, colour = "black", size = 12, family = FONT1),
        plot.caption  = element_text(hjust = 1, colour = "black", size = 7,  family = FONT2),
        axis.title.y  = element_text(hjust=.96,  size = 14, family=FONT2, color="black"),
        axis.title.x  = element_text(hjust=.055, size = 14, family=FONT2, color="black"),
        axis.text     = element_text(size = 14, family=FONT2, color="black"),
        panel.grid.minor = element_line(color = "gray80"),
        panel.grid.major = element_line(color = "gray80"),
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        plot.margin = unit(c(2, 2, 2, 2),"mm"))

ggsave(PLOT, file=paste0(DIRECTORY, "30_2023_04.png"), width = 9*(2/3), height = 9*(2/3), units = "in")
