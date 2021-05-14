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

df3<-st_as_sf(x = df2, 
              coords = c("Longitude", "Latitude"),
              crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

c<-ggplot(df3) +
  geom_sf()
c

library(tidyr)
library(purrr)

df4<-df2 %>% 
  group_by(Parameter) %>% 
  nest() %>% 
  mutate(plot=map2(data,Parameter,~ggplot(.x)+
                     geom_histogram(aes(x=MeasureValue),bins = 50)+
                     ggtitle(.y)+theme_bw()
                     ))
df4$plot[[2]]

if(!dir.exists("./parameter_histograms")){ #if a figures folder does not exist, create it.
  dir.create("./parameter_histograms")
}
#use the map function with ggsave to save named figures. 

map2(paste0("./parameter_histograms/", df4$Parameter, ".jpg"), df4$plot, ggsave)
