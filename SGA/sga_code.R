# SGA: Alexander vs Villar
# Juha Pyykkö 
# Twitter: @juha_pyykko @pyyxxo

rm(list=ls())

#LIBRARIES ----
library(openxlsx)   # read.xslx
library(ggplot2)    # ggplot
library(gridExtra)  # gridarrange
library(ggtext)     # markdown

#DATA ----
sgat <- read.xlsx("sga_tables.xlsx", sheet=1)
sgaA <- na.omit(sgat[, c("GA", "Alexander_Male", "Alexander_Female", "Alexander_avg")])
sgaI <- na.omit(sgat[, c("GA", "Intergrowth_Male", "Intergrowth_Female", "IGz2_Male", "IGz2_Female", "IG_avg", "IGz2_avg", 
                         "IG_A_03", "IG_A_05", "IG_A_10", "IG_A_50", "IG_A_90", "IG_A_95", "IG_A_97")])

#COLORS ----
COLI <- "navyblue"
COLA <- "tan3"

#PLOT AVERAGE ----
SGA3 <- ggplot(sgaI) +
  #TEXT CENTILE
  annotate(geom="text", y=895,  x=23.95, label="97th", alpha=1, hjust=1, vjust=.5, size=2, color=COLI) +
  annotate(geom="text", y=855,  x=23.95, label="95th", alpha=1, hjust=1, vjust=.5, size=2, color=COLI) +
  annotate(geom="text", y=795,  x=23.95, label="90th", alpha=1, hjust=1, vjust=.5, size=2, color=COLI) +
  annotate(geom="text", y=620,  x=23.95, label="50th", alpha=1, hjust=1, vjust=.5, size=2, color=COLI) +
  annotate(geom="text", y=3125, x=43.05, label="10th", alpha=1, hjust=0, vjust=.5, size=2, color=COLI) +
  annotate(geom="text", y=2980, x=43.05, label="5th",  alpha=1, hjust=0, vjust=.5, size=2, color=COLI) +
  annotate(geom="text", y=2880, x=43.05, label="3rd",  alpha=1, hjust=0, vjust=.5, size=2, color=COLI) +
  #LINES
  geom_line(aes(x=GA, y=IG_A_03),                  color=COLI, size= .4, alpha=.1, linetype = "solid") + 
  geom_line(aes(x=GA, y=IG_A_05),                  color=COLI, size= .4, alpha=.1, linetype = "solid") + 
  geom_line(aes(x=GA, y=IG_A_10),                  color=COLI, size= .4, alpha=.1, linetype = "solid") + 
  geom_line(aes(x=GA, y=IG_A_50),                  color=COLI, size= .5, alpha=.2, linetype = "solid") + 
  geom_line(aes(x=GA, y=IG_A_90),                  color=COLI, size= .4, alpha=.1, linetype = "solid") + 
  geom_line(aes(x=GA, y=IG_A_95),                  color=COLI, size= .4, alpha=.1, linetype = "solid") + 
  geom_line(aes(x=GA, y=IG_A_97),                  color=COLI, size= .4, alpha=.1, linetype = "solid") + 
  geom_line(aes(x=GA, y=IGz2_avg),                 color=COLI, size=1.0, alpha=.1, linetype = "solid") +
  geom_line(aes(x=GA, y=IGz2_avg),                 color=COLI, size=1.0, alpha=.8, linetype = "dashed") +
  geom_line(aes(x=GA, y=IG_avg),                   color=COLI, size=1.3, alpha=.8, linetype = "solid") +
  geom_line(data=sgaA, aes(x=GA, y=Alexander_avg), color=COLA, size=1.3, alpha=.8, linetype = "solid") +
  #LABELS
  labs(title="Small for Gestational Age",
       subtitle="Average of female and male",
       caption="**References:** Alexander *et al*. (1996) A United States National Reference for Fetal Growth, *Obstetrics & Gynecology* (Table 4)<br>
       Villar *et al*. (2014) International standards for newborn weight, length, and head circumference by gestational age and sex: the Newborn Cross-Sectional Study of the INTERGROWTH-21st Project, *Lancet*<br>
       Villar *et al*. (2016) INTERGROWTH-21st very preterm size at birth reference charts, *Lancet*<br>
       **Graphic:** Dr. Juha Pyykkö **Twitter:** @juha_pyykko **Code:** pyyxxo.fi/r",
       x="Gestation Age (Completed Weeks)",
       y="Birth Weigth (g)") +
  #TEXT TITLES
  annotate(geom="text", y=2350, x=21,   label="A United States National Reference", alpha=1, hjust=0, vjust=.5, size=9, color=COLA) +
  annotate(geom="text", y=2150, x=23.2, label="10th percentile",                    alpha=1, hjust=0, vjust=.5, size=6, color=COLA) +
  annotate(geom="text", y=1650, x=36,   label="INTERGROWTH-21st",                   alpha=1, hjust=0, vjust=.5, size=9, color=COLI) +
  annotate(geom="text", y=1450, x=38.2, label="10th percentile",                    alpha=1, hjust=0, vjust=.5, size=6, color=COLI) +
  annotate(geom="text", y=1250, x=38.2, label="z-score -2",                         alpha=1, hjust=0, vjust=.5, size=6, color=COLI) +
  annotate(geom="text", y=950,  x=23.95, label="INTERGROWTH-21st percentile",       alpha=1, hjust=1, vjust=.5, size=3, color=COLI) +
  #INFO LINES
  annotate("segment", x=21.2, xend=22.8, y=2150, yend=2150, color=COLA, linetype="solid",  alpha=.8, size=1.1) +
  annotate("segment", x=36.2, xend=37.8, y=1450, yend=1450, color=COLI, linetype="solid",  alpha=.8, size=1.1) +
  annotate("segment", x=36.2, xend=37.8, y=1250, yend=1250, color=COLI, linetype="solid",  alpha=.2, size=1.1) +
  annotate("segment", x=36.2, xend=37.8, y=1250, yend=1250, color=COLI, linetype="dashed", alpha=.8, size=1.1) +
  #SCALE
  scale_x_continuous(breaks=20:45) +
  scale_y_continuous(breaks=seq(500,3000,500), limits=c(100,3400), minor_breaks = seq(0,3500,100)) +
  coord_cartesian(expand = FALSE, clip = 'off') +  	
  #THEME
  theme_minimal() +
  theme(plot.title.position   = "plot",
        plot.caption.position = "plot",
        plot.background = element_rect(fill = "papayawhip", colour = NA),
        plot.title    = element_text(size=36, hjust=.0, color="black"),
        plot.subtitle = element_text(size=24, hjust=.0, color="black", face="bold"),
        plot.caption  = element_markdown(), 
        panel.grid.major   = element_line(colour="gray80", size=0.4), 
        panel.grid.minor.y = element_line(colour="gray80", size=0.1), 
        panel.grid.minor.x = element_blank(),
        axis.text   = element_text(size=18, color="black"),
        axis.title  = element_text(size=22, color="black"),
        plot.margin = margin(10, 20, 10, 20))

## PRINT ----
ggsave(SGA3, file="SGA_USA_v_INTERGROWTH_AVG.png", width = 12, height = 8, units = "in", bg="black") 

#PLOT SEPARATE ----
## MALE ----
SGA1 <- ggplot(sgaI) +
  geom_line(aes(x=GA, y=IGz2_Male),                 color=COLI, size=.8, alpha=.2, linetype = "solid") +
  geom_line(aes(x=GA, y=IGz2_Male),                 color=COLI, size=.8, alpha=.8, linetype = "dashed") +
  geom_line(aes(x=GA, y=Intergrowth_Male),          color=COLI, size=1,  alpha=.8, linetype = "solid") +
  geom_line(data=sgaA, aes(x=GA, y=Alexander_Male), color=COLA, size=1,  alpha=.8, linetype = "solid") +
  labs(subtitle="Male",
       caption="**References:** Alexander *et al*. (1996), A United States National Reference for Fetal Growth, *Obstetrics & Gynecology* (Table 4)<br>
       Villar *et al*. (2014), International standards for newborn weight, length, and head circumference by gestational age and sex: the Newborn Cross-Sectional Study of the INTERGROWTH-21st Project, *Lancet*<br>
       Villar *et al*. (2016), INTERGROWTH-21st very preterm size at birth reference charts, *Lancet*<br>
       **Graphic:** Dr. Juha Pyykkö **Twitter:** @juha_pyykko **Code:** pyyxxo.fi/r",
       x="Gestation Age (Completed Weeks)",
       y="Birth Weigth (g)") +
  annotate(geom="text", y=2250, x=21,   label="A United States National Reference", alpha=1, hjust=0, vjust=.5, size=9, color=COLA) +
  annotate(geom="text", y=2050, x=23.5, label="10th percentile",                    alpha=1, hjust=0, vjust=.5, size=6, color=COLA) +
  annotate(geom="text", y=1700, x=36,   label="INTERGROWTH-21st",                   alpha=1, hjust=0, vjust=.5, size=9, color=COLI) +
  annotate(geom="text", y=1500, x=38.5, label="10th percentile",                    alpha=1, hjust=0, vjust=.5, size=6, color=COLI) +
  annotate(geom="text", y=1350, x=38.5, label="z-score -2",                         alpha=1, hjust=0, vjust=.5, size=6, color=COLI) +
  annotate("segment", x = 21, xend = 23, y = 2050, yend = 2050, color=COLA, linetype ="solid",  alpha=.8) +
  annotate("segment", x = 36, xend = 38, y = 1500, yend = 1500, color=COLI, linetype ="solid",  alpha=.8) +
  annotate("segment", x = 36, xend = 38, y = 1350, yend = 1350, color=COLI, linetype ="solid",  alpha=.2) +
  annotate("segment", x = 36, xend = 38, y = 1350, yend = 1350, color=COLI, linetype ="dashed", alpha=.8) +
  scale_x_continuous(breaks=20:45) +
  scale_y_continuous(breaks=seq(500,3000,500), limits=c(0,3500), minor_breaks = seq(0,3500,100)) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  theme(plot.title.position = "plot",
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "lightskyblue1", colour = NA),
        plot.title   = element_text(size = 32, hjust=.0, color="black"),
        plot.subtitle   = element_text(size = 24, hjust=.0, color="black", face="bold"),
        plot.caption  = element_markdown(), 
        axis.text    = element_text(size = 16, color="black"),
        axis.title =element_text(size=20, color="black"),
        plot.margin = margin(10, 20, 10, 20))

## FEMALE ----
SGA2 <- ggplot(sgaI) +
  geom_line(aes(x=GA, y=IGz2_Female),                 color=COLI, size=.8, alpha=.2, linetype = "solid") +
  geom_line(aes(x=GA, y=IGz2_Female),                 color=COLI, size=.8, alpha=.8, linetype = "dashed") +
  geom_line(aes(x=GA, y=Intergrowth_Female),          color=COLI, size=1,  alpha=.8, linetype = "solid") +
  geom_line(data=sgaA, aes(x=GA, y=Alexander_Female), color=COLA, size=1,  alpha=.8, linetype = "solid") +
  labs(title="Small for Gestational Age",
       subtitle = "Female",
       x="Gestation Age (Completed Weeks)",
       y="Birth Weigth (g)") +
  annotate(geom="text", y=2250, x=21,   label="A United States National Reference", alpha=1, hjust=0, vjust=.5, size=9, color=COLA) +
  annotate(geom="text", y=2050, x=23.5, label="10th percentile",                    alpha=1, hjust=0, vjust=.5, size=6, color=COLA) +
  annotate(geom="text", y=1700, x=36,   label="INTERGROWTH-21st",                   alpha=1, hjust=0, vjust=.5, size=9, color=COLI) +
  annotate(geom="text", y=1500, x=38.5, label="10th percentile",                    alpha=1, hjust=0, vjust=.5, size=6, color=COLI) +
  annotate(geom="text", y=1350, x=38.5, label="z-score -2",                         alpha=1, hjust=0, vjust=.5, size=6, color=COLI) +
  annotate("segment", x = 21, xend = 23, y = 2050, yend = 2050, color=COLA, linetype ="solid",  alpha=.8) +
  annotate("segment", x = 36, xend = 38, y = 1500, yend = 1500, color=COLI, linetype ="solid",  alpha=.8) +
  annotate("segment", x = 36, xend = 38, y = 1350, yend = 1350, color=COLI, linetype ="solid",  alpha=.2) +
  annotate("segment", x = 36, xend = 38, y = 1350, yend = 1350, color=COLI, linetype ="dashed", alpha=.8) +
  scale_x_continuous(breaks=20:45) +
  scale_y_continuous(breaks=seq(500,3000,500), limits=c(0,3500), minor_breaks = seq(0,3500,100)) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  theme(plot.title.position = "plot",
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "rosybrown1", colour = NA),
        plot.title   = element_text(size = 36, hjust=.0, color="black"),
        plot.subtitle   = element_text(size = 24, hjust=.0, color="black", face="bold"),
        axis.text    = element_text(size = 16, color="black"),
        axis.title = element_text(size=20, color="black"),
        plot.margin = margin(10, 20, 10, 20))

## COMBINE ----
layout2 <- rbind(c(1),c(2))
SGA12 <- grid.arrange(SGA2, SGA1, layout_matrix = layout2)

## PRINT ----
ggsave(SGA12, file="SGA_USA_v_INTERGROWTH_BOTH.png", width = 12, height = 16, units = "in", bg="black") 
