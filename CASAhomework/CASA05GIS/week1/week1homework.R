library(sf)
library(tidyverse)
library(tmap)
library(readr)
library(RSQLite)
##加载包

shape <- st_read("D:/CASA/CASA0005_Geographic_Information_Systems_and_Science/week1/Course_Data/homework/week1homework/xinxilan.shp") ##加载shp文件
summary(shape) 
##显示shp基本信息

shape %>% 
  st_geometry() %>%
  plot() 
##显示shp基本轮廓
plot(shape)
##快速查看shp外观
mycsv <-  read_csv("D:/CASA/CASA0005_Geographic_Information_Systems_and_Science/week1/Course_Data/homework/week1homework/2018-SA1-dataset-individual-part-3a-total-NZ_updated_16-7-20-GTN.csv") ##加载csv  
mycsv 
##加载csv并显示CSV文件内容

shape <- shape%>%
  merge(.,
        mycsv,
        by.x="TA2018_V11", 
        by.y="DRE") 
##使用merge合并csv和shp文件 ,shp字段为by.x ,csv属性列为by.y  

shape%>%
  head(., n=10) 
##显示前十行查看结果


tmap_mode("plot")
# change the fill to your column name if different
shape %>%
  qtm(.,fill = "Paid employee.y") 
##以自定义fill输出图形

shape %>%
  st_write(.,"D:/CASA/CASA0005_Geographic_Information_Systems_and_Science/week1/Course_Data/homework/week1homework/wk1hw.gpkg",
           "Paid employee.y",
           delete_layer=TRUE)
##以指定图层生成gpkg包

library(readr)
library(RSQLite)
con <- dbConnect(RSQLite::SQLite(),dbname="D:/CASA/CASA0005_Geographic_Information_Systems_and_Science/week1/Course_Data/homework/week1homework/wk1hw.gpkg")
## SQLite连接到gpkg包

con %>%
  dbListTables()
##检查里面有什么.gpkg

con %>%
  dbWriteTable(.,
               "2018-SA1-dataset-individual-part-3a-total-NZ_updated_16-7-20-GTN.csv",
               mycsv,
               overwrite=TRUE)

con %>% 
  dbDisconnect()
##添加您的.csv并断开连接.gpkg


