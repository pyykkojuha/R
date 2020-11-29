# # # THE WEEKND - AFTER HOURS - TRACK LENGTHS

library(ggplot2)
ah <- read.csv("AfterHours.csv")
st <- subset(ah, Version=="Standard")

# # # 16:9

ggplot(ah) +
  geom_segment(aes(x=Track, xend=Track, y=Minusecs, yend=Secs), color="#f70102",  size=7) + 
  labs(title = "AFTER HOURS", 
       caption = "DELUXE EDITION TRACK LENGTHS") +
  annotate("text", y=0, x=ah$Track, hjust=.5, vjust=.5, angle=90, size = 6, color="black", family="AFTER HOURS", alpha=1, label=ah$Roman) +
  theme_void() +
  theme(plot.title   = element_text(hjust=.5, size=36),
        plot.caption = element_text(hjust=.5, size=16),
        text=element_text(family="AFTERHOURS", colour ="#f70102"),
        plot.background = element_rect(fill = "black", colour = NA),
        plot.margin=unit(c(1, 1, 1, 1),"cm")) 

# # # 1:1 (A)

ggplot(st) +
  geom_segment(aes(x=Track, xend=Track, y=Minusecs, yend=Secs), color="#f70102",  size=12.5) + 
  annotate("text", y=0,    x=mean(st$Track), hjust=.5, vjust=.5, angle=0, size = 20, color="black",   family="AFTER HOURS", label="AFTER HOURS") +
  annotate("text", y=-500, x=mean(st$Track), hjust=.5, vjust=.5, angle=0, size = 8,  color="#f70102", family="AFTER HOURS", label="TRACK LENGTHS") +
  theme_void() +
  theme(plot.background = element_rect(fill = "black", colour = NA),
        plot.margin=unit(c(4, 1, 2.5, 1),"cm")) 

# # # 1:1 (B)

ggplot(ah) +
  geom_segment(aes(y=Rev, yend=Rev, x=Minusecs, xend=Secs), color="#f70102",  size=6) + 
  labs(title = "AFTER HOURS", 
       caption = "DELUXE EDITION TRACK LENGTHS") +
  annotate("text", x=0, y=ah$Rev, hjust=.5, vjust=.5, angle=0, size = 5.9, color="black", family="AFTER HOURS", label=ah$Name) +
  theme_void() +
  theme(plot.title   = element_text(hjust=.5, size=46, margin=unit(c(0, 0, 7, 0),"mm")),
        plot.caption = element_text(hjust=.5, size=18, margin=unit(c(7, 0, 0, 0),"mm")),
        text=element_text(family="AFTER HOURS", colour ="#f70102"),
        plot.background = element_rect(fill = "black", colour = NA),
        plot.margin=unit(c(2, 1, 2, 1),"cm")) 
