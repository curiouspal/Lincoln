
library("ggplot2")
allcities <- read.csv("landdata-msas-2015q1.csv")

# Remove the "$" sign and the commas in between numbers and then converting the resulting number as numeric type data instead of a string.
for(i in 1:length(allcities$Home.Value)) {
  b <- as.vector(strsplit(as.character(allcities$Home.Value[i]), "$", fixed = TRUE))
  allcities$Home.Value1[i] <- b[[1]][2]
}
for(i in 1:length(allcities$Home.Value1)) {
  b <- as.vector(strsplit(as.character(allcities$Home.Value1[i]), ",", fixed = TRUE))
  allcities$Home.Value1[i] <- paste(b[[1]][1], b[[1]][2], sep = "")
}
allcities$Home.Value1 <- as.numeric(allcities$Home.Value1)

for(i in 1:length(allcities$Land.Value)) {
  b <- as.vector(strsplit(as.character(allcities$Land.Value[i]), "$", fixed = TRUE))
  allcities$Land.Value1[i] <- b[[1]][2]
}
for(i in 1:length(allcities$Land.Value1)) {
  b <- as.vector(strsplit(as.character(allcities$Land.Value1[i]), ",", fixed = TRUE))
  allcities$Land.Value1[i] <- paste(b[[1]][1], b[[1]][2], sep = "")
}
allcities$Land.Value1 <- as.numeric(allcities$Land.Value1)


for(i in 1:length(allcities$Date)) {
  b <- as.vector(strsplit(as.character(allcities$Date[i]), "Q", fixed = TRUE))
  allcities$Date1[i] <- as.numeric(b[[1]][1]) + (as.numeric(b[[1]][2])*0.25) 
}


for(i in 1:length(allcities$Land.Share..Pct.)) {
  b <- as.vector(strsplit(as.character(allcities$Land.Share..Pct.[i]), "%", fixed = TRUE))
  allcities$Land.Share1[i] <- b[[1]][1]
}
for(i in 1:length(allcities$Land.Share1)) {
  b <- as.vector(strsplit(as.character(allcities$Land.Share1[i]), ".", fixed = TRUE))
  allcities$Land.Share1[i] <- paste(b[[1]][1], b[[1]][2], sep = "")
}
allcities$Land.Share1 <- as.numeric(allcities$Land.Share1)/10

portland <- subset(allcities, MSA=="PORTLAND" | MSA=="DENVER" | MSA=="BOSTON" | MSA=="SANFRANCISCO" | MSA=="MIAMI" & as.character(Date)>"1985Q1")




ggplot(data=allcities, aes(Date1, as.numeric(Land.Price.Index))) + geom_line() + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3), 
    axis.text.x = element_text(size=15, angle = 90, hjust = 1), 
    axis.text.y = element_text(size=18), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 9)
  ) + facet_wrap(~MSA)


ggplot(data=allcities, aes(Date1, as.numeric(Home.Price.Index))) + geom_line() + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3), 
    axis.text.x = element_text(size=15, angle = 90, hjust = 1), 
    axis.text.y = element_text(size=18), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 9)
  ) + facet_wrap(~MSA)

ggplot(data=allcities, aes(Date1, Land.Share1)) + geom_line() + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3), 
    axis.text.x = element_text(size=5, angle = 90, hjust = 1), 
    axis.text.y = element_text(size=18), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 9)
  ) + facet_wrap(~MSA)



ggplot(data=allcities, aes(Date1, Home.Value1)) + geom_line() + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3), 
    axis.text.x = element_text(size=5, angle = 90, hjust = 1), 
    axis.text.y = element_text(size=18), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 9)
  ) + facet_wrap(~MSA)

ggplot(data=portland, aes(Date1, Home.Value1)) + geom_line(aes(color=MSA)) + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3, color="#000033"), 
    axis.text.x = element_text(size=12, angle = 90, hjust = 1, color="#000033"), 
    axis.text.y = element_text(size=8), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 9)
  ) 

ggplot(data=portland, aes(Date1, Land.Value1)) + geom_line(aes(color=MSA)) + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3, color="#000033"), 
    axis.text.x = element_text(size=12, angle = 90, hjust = 1, color="#000033"), 
    axis.text.y = element_text(size=8), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 9)
  ) 

ggplot(data=portland, aes((Date1), (Land.Share1))) + geom_line(stat="identity", aes(color=MSA)) + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3, color="#000033"), 
    axis.text.x = element_text(size=12, angle = 90, hjust = 1, color="#000033"), 
    axis.text.y = element_text(size=8), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 9)
  ) 


allcities1 <- subset(allcities, MSA!="SANFRANCISCO" & MSA!="SANJOSE")
g <- ggplot(data=allcities1, aes((Date1), (Home.Value1))) + geom_line(stat="identity", aes(color=MSA)) + 
  theme(
    plot.title = element_text(size=20, face="bold", vjust=3, color="#000033"), 
    axis.text.x = element_text(size=12, angle = 90, hjust = 1, color="#000033"), 
    axis.text.y = element_text(size=12), 
    axis.title.x = element_text(color="#000033", size=18), 
    axis.title.y = element_text(color="#000033", size=18, vjust=2),
    legend.title=element_blank(),
    legend.text = element_text(colour="#000033", size = 9)
  ) 

g + geom_line(data=portland, aes(color=MSA, size=3))

g + geom_text(data=subset(allcities1, Date1==2015.25), size = 3, aes(label=MSA))
