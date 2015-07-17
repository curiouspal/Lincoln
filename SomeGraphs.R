allcities <- read.csv("landdata-msas-2015q1.csv")
library("ggplot2")
portland <- subset(allcities, MSA=="PORTLAND" | MSA=="DENVER" | MSA=="BOSTON" & as.character(Date)>"1985Q1")
ggplot(data=allcities, aes(Date, Land.Price.Index)) + geom_point(aes(color=MSA)) + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3, color="#000033"), 
    axis.text.x = element_text(size=12, angle = 90, hjust = 1, color="#000033"), 
    axis.text.y = element_text(size=18), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 18)
  ) + facet_wrap(~MSA)


ggplot(data=allcities, aes(Date, Home.Price.Index)) + geom_point(aes(color=MSA)) + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3, color="#000033"), 
    axis.text.x = element_text(size=12, angle = 90, hjust = 1, color="#000033"), 
    axis.text.y = element_text(size=18), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 18)
  ) + facet_wrap(~MSA)


ggplot(data=allcities, aes(Date, Land.Share..Pct.)) + geom_point(aes(color=MSA)) + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3, color="#000033"), 
    axis.text.x = element_text(size=12, angle = 90, hjust = 1, color="#000033"), 
    axis.text.y = element_text(size=8), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 18)
  ) + facet_wrap(~MSA)


ggplot(data=allcities, aes(Date, Home.Value)) + geom_point(aes(color=MSA)) + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3, color="#000033"), 
    axis.text.x = element_text(size=12, angle = 90, hjust = 1, color="#000033"), 
    axis.text.y = element_text(size=8), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 18)
  ) + facet_wrap(~MSA)

ggplot(data=portland, aes(Date, Home.Value)) + geom_point(aes(color=MSA)) + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3, color="#000033"), 
    axis.text.x = element_text(size=12, angle = 90, hjust = 1, color="#000033"), 
    axis.text.y = element_text(size=8), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 18)
  ) 