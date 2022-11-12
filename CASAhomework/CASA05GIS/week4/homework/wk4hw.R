library(sf)
library(here)
library(raster)
library(terra)#terra是用于空间数据分析的 R 包，是raster包的升级版，可用于栅格数据和矢量数据处理和空间分析。 此外在R中，还有sf、sp、raster、stars等包可用于处理空间数据。
library(fs)#文件操作包
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tmap)
#-------------#加载数据
shape <- st_read("D:/CASA/CASA0005_Geographic_Information_Systems_and_Science/week4/wk4homework/gisgithub/World_Countries_(Generalized)/World_Countries__Generalized_.shp")
csv<- read_csv("HDR21-22_Composite_indices_complete_time_series.csv",locale = locale(encoding = "latin1"),
                     na = c("","NULL"))  #na = "NULL"
#-------------#从csv中选取country列，以及从dhi2010到hdi2019所有列
csv1<-csv %>%
  dplyr::select("country"|hdi_2010:hdi_2019)
#-------------#以country为列，统计2019到2010的差值给到diff列
csv2 <- csv1 %>%
  group_by(country) %>%
  summarise(diff=hdi_2019-hdi_2010)
#-------------#此段merge暂时可不用，以leftjoin做连接
'''
shape <- shape%>%
  merge(.,
        csv,
        by.x="COUNTRY", 
        by.y="Country")
tmap_mode("plot")
shape %>%
  qtm(.,fill = "diff")+
  tm_layout(main.title  = "2010-2019差异图",main.title.position  = "center",title.size=2) 
'''
#-------------#出图
shape <- shape %>% 
  left_join(.,#左连接，以左边的共有属性为准，右边的单独的抛弃
            csv2,
            by = c("COUNTRY" = "country"))

qtm(shape,
    fill = "diff")+
  tm_layout(main.title  = "2010-2019 Global Gender Inequality Change Difference Chart",main.title.position  = "center",title.size=2,bg.color = "skyblue")+
  tm_compass(type = "4star",position = c("left","top"))
