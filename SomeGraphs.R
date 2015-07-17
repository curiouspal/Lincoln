allcities <- read.csv("landdata-msas-2015q1.csv")
for(i in 1:length(allcities$Home.Value)) {
  b <- as.vector(strsplit(as.character(allcities$Home.Value[i]), "$", fixed = TRUE))
  allcities$Home.Value1[i] <- b[[1]][2]
}
for(i in 1:length(allcities$Home.Value1)) {
  b <- as.vector(strsplit(as.character(allcities$Home.Value1[i]), ",", fixed = TRUE))
  allcities$Home.Value1[i] <- paste(b[[1]][1], b[[1]][2], sep = "")
}
allcities$Home.Value1 <- as.numeric(allcities$Home.Value1)

library("ggplot2")
portland <- subset(allcities, MSA=="PORTLAND" | MSA=="DENVER" | MSA=="BOSTON" & as.character(Date)>"1985Q1")

ggplot(data=allcities, aes(Date, Land.Price.Index)) + geom_point() + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3), 
    axis.text.x = element_text(size=5, angle = 90, hjust = 1), 
    axis.text.y = element_text(size=18), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 18)
  ) + facet_wrap(~MSA)


ggplot(data=allcities, aes(Date, Home.Price.Index)) + geom_point() + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3), 
    axis.text.x = element_text(size=5, angle = 90, hjust = 1), 
    axis.text.y = element_text(size=18), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 18)
  ) + facet_wrap(~MSA)

ggplot(data=allcities, aes(Date, Land.Share..Pct.)) + geom_point() + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3), 
    axis.text.x = element_text(size=5, angle = 90, hjust = 1), 
    axis.text.y = element_text(size=18), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 18)
  ) + facet_wrap(~MSA)



ggplot(data=allcities, aes(Date, Home.Value1)) + geom_point() + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3), 
    axis.text.x = element_text(size=5, angle = 90, hjust = 1), 
    axis.text.y = element_text(size=18), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 18)
  ) + facet_wrap(~MSA)

ggplot(data=portland, aes(Date, Home.Value1)) + geom_point(aes(color=MSA)) + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3, color="#000033"), 
    axis.text.x = element_text(size=12, angle = 90, hjust = 1, color="#000033"), 
    axis.text.y = element_text(size=8), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 18)
  ) 



