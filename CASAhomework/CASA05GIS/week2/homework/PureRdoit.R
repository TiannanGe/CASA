library(sf)
library(tidyverse)
library(tmap)
library(dplyr)
library(janitor)
#--------------------#upload SHP and CSV
schoolcsv<- read_csv("Report_Card_Assessment_Data_2018-19_School_Year1.csv",locale = locale(encoding = "latin1"),
                     na = c("n/a","NULL"))  #na = "NULL"

shape <- st_read("D:/CASA/CASA0005_Geographic_Information_Systems_and_Science/week2/PureRdoit/Washington_Counties_with_Natural_Shoreline___washsh_area/Washington_Counties_with_Natural_Shoreline___washsh_area.shp")
#--------------------#CSV is defined as df data.frame
df <- data.frame(schoolcsv) 
#--------------------#Extraction of "science" disciplines
science<- df %>% 
  filter(str_detect(`TestSubject`, "Science")) 
#--------------------#Remove duplicate
science<-science %>%  
  distinct()
#--------------------#Extracting useful columns
science<-science %>%  
  dplyr::select(contains("County"),contains("Count.of.Students.Expected.to.Test"),contains("CountMetStandard")) 
#--------------------#Rename column and delete the No.3 column
science <- science %>% 
  dplyr::rename(allstudents=`Count.of.Students.Expected.to.Test`)%>%
  dplyr::rename(pass=`CountMetStandard`)%>%
  clean_names()
science=science[,-3]
#--------------------#Two columns convert a character to a numeric type , Delete the column with NA 
science <- science  %>%
  mutate(nupass = as.numeric(pass),nuallstudents = as.numeric(allstudents))
science <-na.omit(science) 
#--------------------# Remove Spaces and 'Multiple' (data cleaning)
science <- subset(science, county != 'Multiple')
science <- subset(science, county != '')
#--------------------# Summarise Total number of students by county
science <- science %>%  
  group_by(county)%>%
  summarise(allpass=sum(nupass),aallstudents=sum(nuallstudents))
#--------------------# Increase the pass percentage of each county by one column
science <- science %>% 
  mutate(countypercentage = (allpass/aallstudents)*100)
#--------------------# Increase the percentage of each county passing relative to the state ※
science <- science %>% 
  mutate(totalave= countypercentage /
           mean(countypercentage))
#--------------------# Merge table and shp , why join data?
shape <- shape%>%
  merge(.,
        science,
        by.x="COUNTYLABE", 
        by.y="county")

#--------------------# Draw countypercentage ※
library(tmap)
library(ggplot2)
tmap_mode("plot")
shape %>%
  qtm(.,fill = "countypercentage")+
tm_layout(main.title  = "ZXXXq",main.title.position  = "center",title.size=2) 

#hOW to take a
?tmap()
library(tmap)
tmap_mode("plot")
shape %>%
  qtm(.,fill = "totalave")

