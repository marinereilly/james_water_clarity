library(dplyr)
library(ggplot2)

df<-read.csv("data/MASTER FILE - Synced SAV and WQ Data for ALL Segments - CBWQData_ANALYSISFILE.csv")

df<-mutate(df,SampleDate=lubridate::mdy(SampleDate))

a<-ggplot(df,aes(x=SampleDate))+
  geom_point(aes(y=Parameter))
a

unique(df$Station)

library(sf)

df2<-df %>% 
  filter(grepl("JMS|CHK|APP",CBSeg2003))
b<-ggplot(df2, aes(x=SampleDate, y=Parameter))+
  geom_point()
b
