ls()
rm(A)
function(object, argument1, argument2, argument3)
  X<-function(data, argument1, argument2, argument3)
ls()    
ls()
#create some datasets, first a vector（创建向量） of 1-100 and 101-200,
Data1 <- c(1:100)
Data2 <- c(101:200)
#Plot the data
plot(Data1, Data2, col="red")#第一个是x轴``

Data1 <- c(0:100)
Data2 <- c(100:200)
#Plot the data
plot(Data1, Data2, col="red")
#----------------------------------------------
#just for fun, create some more, this time some normally distributed
#vectors of 100 numbers R语言中rnorm函数用于产生服从正态分布的随机数。（r：random， norm： normal）
Data3 <- rnorm(1000, mean = 53, sd=34)#产生1000个，平均值为53，标准差为34的正态分布的平均数
Data4 <- rnorm(1000, mean = 64, sd=14)
#plot
plot(Data3, Data4, col="red")
#----------------------------------------------
length(c("aa", "bb", "cc", "dd", "ee")) #计算列表里有多少个值,c本身在这里应该是“combine”的首字母，用于合并一系列数字从而形成向量/数列。
n<-numeric(5) #创建一个五个值的数字列表，值为0
n
m<-character(5) #创建一个五个值的字符向量，空值
m 
m<-c(3,4,5,6,7)#创建一个数字向量，34567
m
v<-c(2:50) #创建一个2到50的数字向量
v
x<-v+m #把前两个向量加起来
x
newcustomer <- list("Benny",24,2,"M") #创建一个多类型的列表
newcustomer
#----------------------------------------------
a<- c(1,2,3,4,5,6,7) 
#matrix函数默认用col填充矩阵col ，nrow=2创建两行，自动生成V1,V2,V3，三列，一直用a循环填充
m <- matrix(a,nrow=2) 
m 
#----------------------------------------------
Name<- c("John","Thomas","Alice") #以下创建三个向量
Age<- c(20,25,35) 
Experience<- c(2,3,8) 
People<- data.frame(Name,Age,Experience)#创建一个数据框，把三个向量合起来
People              
#----------------------------------------------
Data1 <- c(0:100)
Data2 <- c(100:200)
df <- data.frame(Data1, Data2)#把两个向量创建为一个数据框
plot(df, col="green")#把数据框内容画出来
#----------------------------------------------
install.packages('tidyverse')
library(tidyverse)
#show the first 10 and then last 10 rows of data in df...
df %>%
  head()#数据框显示前6个值
#----------------------------------------------
df %>%
  tail()#数据框显示尾6个值
df
df[1:10, 1]#显示数据框中第一列的1到10个值
df[5:15,]#显示数据框中所有列的5到15个值
df[c(2,3,6),2]#显示第二列的第2,3,6个值
df[,1]#显示第一列的所有
#----------------------------------------------
library(dplyr)
df <- df %>%
  dplyr::rename(column1 = Data1, column2=Data2)#使用dplyr中的rename函数，为列改名
options(digits = 0)  #保留整数
set.seed(1)          #设置随机数的种子
#创建一个df的数据框，ID列是1到12，class列是使用rep重复函数，重复向量c，重复四次
df <- data.frame(ID = 1:12,                                 #ID
                Class = rep(c(1,2,3),4),                   #班级
                Chinese = runif(12,min = 0,max = 100),     #语文，使用 runif(...) 生成统一的随机数，12个，最小值最大值
                Math = runif(12,min = 0,max = 100),        #数学
                English = runif(12,min = 0,max = 100))     #英语
for (i in 1:ncol(df)) {
  +   df[,i] <- as.integer(df[,i])  #将每列类型变为integer型
  + }

#show the first 10 and then last 10 rows of data in df...
df %>%
  head()

df %>%
  tail()
#----------------------------------------------
df %>% 
  dplyr::select(ID)#选择df的Data1列
df
df$ID#也是选择Data1列
df[["ID"]]#也是选择Data1列
#----------------------------------------------
LondonDataOSK<- read.csv("D:/CASA/CASA0005_Geographic_Information_Systems_and_Science/week2/prac2/LondonData.csv")
header = TRUE
sep = ","                         
encoding = "latin1"
install.packages("here")
library(here)
here::here()
LondonDataOSK<- read.csv(here() )

LondonData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a")#加载csv，将“n/a定义为空值”
class(LondonData)#返回该内容是什么类型属性
class(LondonDataOSK)
Datatypelist <- LondonData %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), #宽表变成长表的函数
               names_to="All_variables", 
               values_to="Variable_class")

#----------------------------------------------
Datatypelist
summary(df)
LondonData%>%#取列的前六个
  colnames()%>%
  head()
#----------------------------------------------
class(LondonData)
Femalelifeexp<- LondonData %>% 
  filter(`Female life expectancy -2009-13`>90)#使用filter函数筛选出某一列大于90的行

LondonBoroughs<- LondonData %>% 
  filter(str_detect(`New code`, "^E09"))
#str_detect检测字符串中是否存在模式，str_detect(string, pattern, negate = FALSE)	
#如果TRUE，则返回不匹配的元素。
aaa <- LondonBoroughs$`Ward name`#列出所有ward name列的值
aaa <- data.frame(aaa)
#----------------------------------------------
#选择ward name列并打印
LondonBoroughs %>% 
  dplyr::select(`Ward name`) %>%
  print()

LondonBoroughs<-LondonBoroughs %>%
  distinct()#从数据框中仅选择唯一/不同的行
#select columns 1,19,20 and 21
LondonBoroughs_manualcols<-LondonBoroughs[,c(1,19,20,21)]#选择四列，成为新数据框

#select columns 1,19,20 and 21
LondonBoroughs_dplyrcols<-LondonBoroughs %>%
  dplyr::select(c(1,19,20,21))#使用dplyr的select函数也可以选择该几列

LondonBoroughs_contains<-LondonBoroughs %>% 
  dplyr::select(contains("expectancy"), #选择包含了expectancy所有的列
                contains("obese - 2011/12 to 2013/14"),#包含了obese的一个区间
                contains("Ward name")) 
#----------------------------------------------
install.packages("janitor")
library(janitor)

LondonBoroughs <- LondonBoroughs %>%
  dplyr::rename(Borough=`Ward name`)%>%#重命名列
  clean_names()

Life_expectancy <- LondonBoroughs %>% 
  #new column with average of male and female life expectancy
  #使用mutate函数新增一列，平均寿命的，使用男+女除2
  mutate(averagelifeexpectancy= (female_life_expectancy_2009_13 +
                                   male_life_expectancy_2009_13)/2)%>%
  #new column with normalised life expectancy
  #使用mutate函数再增加一列求归一化的寿命
  mutate(normalisedlifeepectancy= averagelifeexpectancy /
           mean(averagelifeexpectancy))%>%
  #select only columns we want
  dplyr::select(new_code,#选择四个列
                borough,
                averagelifeexpectancy, 
                normalisedlifeepectancy)%>%
  #arrange in descending order
  #ascending is the default and would be
  #arrange(normalisedlifeepectancy)
  arrange(desc(normalisedlifeepectancy))#arrange的主要功能是排序，desc是降序
#----------------------------------------------
#top of data
slice_head(Life_expectancy, n=5)#slice系列函数用于抽取样本
slice_tail(Life_expectancy,n=5)
slice_tail(Life_expectancy,n=5)
Life_expectancy2 <- Life_expectancy %>%#插入一列，UKcompare，内容是平均是大于81.16的是写above UK average，反之
  mutate(UKcompare = case_when(averagelifeexpectancy>81.16 ~ "above UK average",
                               TRUE ~ "below UK average"))
Life_expectancy2
#----------------------------------------------
Life_expectancy2_group <- Life_expectancy2 %>%
  mutate(UKdiff = averagelifeexpectancy-81.16) %>%#UKdiff为平均值与81.16的差值
  group_by(UKcompare)%>%#group_by()函数进行分组
  summarise(range=max(UKdiff)-min(UKdiff), count=n(), Average=mean(UKdiff))#新增三列，range，count，average

Life_expectancy2_group

Life_expectancy3 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(where(is.numeric), round, 3))%>%
  mutate(across(UKdiff, round, 0))%>%
  mutate(UKcompare = case_when(averagelifeexpectancy >= 81 ~ #当平均数大于等于81时，内容为..，UKdiff是年数
                                 str_c("equal or above UK average by",
                                       UKdiff, 
                                       "years", 
                                       sep=" "), 
                               TRUE ~ str_c("below UK average by",#否则，内容为..，UKdiff是年数
                                            UKdiff,
                                            "years",
                                            sep=" ")))%>%
  group_by(UKcompare)%>%
  summarise(count=n())

Life_expectancy3
#----------------------------------------------
Life_expectancy4 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(is.numeric, round, 3))%>%
  mutate(across(UKdiff, round, 0))#不太懂

plot(LondonBoroughs$male_life_expectancy_2009_13,
     LondonBoroughs$percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14,
     col='red')#从LondonBoroughs各拿一个数据来做XY出图
#----------------------------------------------
install.packages("plotly")
library(plotly)#专门画图的包
plot_ly(LondonBoroughs, 
        #data for x axis
        x = ~male_life_expectancy_2009_13, #设置X轴
        #data for y axis
        y = ~percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14 , #设置y轴
        #attribute to display when hovering #以下设置悬停显示的信息
        text = ~borough, 
        type = "scatter", 
        mode = "markers")
#----------------------------------------------
install.packages("maptools")
install.packages(c("classInt", "tmap"))
# might also need these ones
install.packages(c("RColorBrewer", "sp", "rgeos", 
                   "tmaptools", "sf", "downloader", "rgdal", 
                   "geojsonio"))

#Load Packages (ignore any error messages about being built under a 
#different R version):
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(maptools)
library(ggplot2)
#----------------------------------------------这段很多实现不了
EW <- st_read("D:/CASA/CASA0005_Geographic_Information_Systems_and_Science/week2/prac2/Local_Authority_Districts_(December_2015)_Boundaries.geojson")

EW1 <- st_read(here::here("Local_Authority_Districts_(December_2015)_Boundaries.shp"))

LondonMap<- EW %>%
  filter(str_detect(lad15cd,"^E09"))
EW
#plot it using the qtm function
qtm(EW)
plot(shape)

LondonMap<- EW %>%
  filter(str_detect(lad15cd,"^E09"))#报错，不知道如何解决
#plot it using the qtm function
qtm(LondonMap)

LondonData <- clean_names(LondonData)#做名字清理

#EW is the data we read in straight from the web
library(dplyr)
library(tidyverse)
BoroughDataMap <- EW %>%
  #clean_names() %>%#clean_names报错，已经加载了dplyr和tidy了
  # the . here just means use the data already loaded
  filter(str_detect(lad15cd, "^E09"))%>%
  merge(.,
        LondonData, 
        by.x="lad15cd", 
        by.y="new_code",
        no.dups = TRUE)%>%
  distinct(.,lad15cd,
           .keep_all = TRUE)
#----------------------------------------------
BoroughDataMap3 <- EW %>% 
  filter(str_detect(lad15cd, "^E09"))%>%#在EW的lad15cd列里匹配所有包含E09的数据中做连接
  left_join(.,#左连接，以左边的共有属性为准，右边的单独的抛弃
            LondonData,
            by = c("lad15cd" = "new_code"))#EW的lad15cd列与LondonData的new_code相连接
qtm(BoroughDataMap3,
    fill = "mean_age_2013")#按fill指按指定列做分类
class(BoroughDataMap3)#本质上作为shp，也是个数据框结构
#----------------------------------------------
library(tmap)
library(tmaptools)
tmap_mode("plot")
qtm(BoroughDataMap, 
    fill = "rate_of_job_seekers_allowance_jsa_claimants_2015")
#----------------------------------------------

tmaplondon <- BoroughDataMap %>%
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "osm", zoom = NULL)#使用tmaptools的read_osm函数
qtm(tmaplondon)#先打印出OSM底图看看
#----------------------------------------------
tmap_mode("plot")
#本段落为将OSM作为底图出图
tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(BoroughDataMap) + 
  tm_polygons("rate_of_job_seekers_allowance_jsa_claimants_2015", 
              style="jenks",
              palette="YlOrBr",
              midpoint=NA,
              title="Rate per 1,000 people",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Job seekers' Allowance Claimants", legend.position = c("right", "bottom"))

#----------------------------------------------
Life_expectancy4map <- EW %>%
  inner_join(., 
             Life_expectancy4,
             by = c("lad15cd" = "new_code"))%>%#lad15cd是Life_expectancy4的列，new_code是Life_expectancy4map的列，相连接
  distinct(.,lad15cd, 
           .keep_all = TRUE)

#----------------------------------------------
tmap_mode("plot")
#本段落为将OSM作为底图出图
tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(Life_expectancy4map) + 
  tm_polygons("UKdiff", 
              style="pretty",
              palette="Blues",
              midpoint=NA,
              title="Number of years",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Difference in life expectancy", legend.position = c("right", "bottom"))

#----------------------------------------------
#本段落为将OSM作为底图出图

EW <- st_read(here::here("Local_Authority_Districts_(December_2015)_Boundaries.shp"))

LondonMap <- EW %>%#EW是网上直接读到的数据，赋值到london，通过filter删选出E09就是伦敦
  filter(str_detect(lad15cd, "^E09"))
qtm(LondonMap)#把筛选出来的E09也就是LondonMap画出来
LondonData <- clean_names(LondonData)#做不了这条，报错
#EW is the data we read in straight from the web
#----------------------------------------------
BoroughDataMap <- EW %>%
  # the . here just means use the data already loaded
  filter(str_detect(lad15cd, "^E09"))%>%#通过E09提取出伦敦
  merge(.,#做合并，和londondata数据，x和y对应连接
        LondonData, 
        by.x="lad15cd", 
        by.y="new_code",
        no.dups = TRUE)%>%
  distinct(.,lad15cd,
           .keep_all = TRUE)
qtm(BoroughDataMap)
#----------------------------------------------
BoroughDataMap2 <- EW %>% 
  filter(str_detect(lad15cd, "^E09"))%>%
  left_join(., 
            LondonData,
            by = c("lad15cd" = "new_code"))


library(tmap)
library(tmaptools)
tmap_mode("plot")
qtm(BoroughDataMap, 
    fill = "rate_of_job_seekers_allowance_jsa_claimants_2015")
install.packages("OpenStreetMap")
#搞了个osm的背景底图
tmaplondon <- BoroughDataMap %>%
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "osm", zoom = NULL)
qtm(tmaplondon)
#----------------------------------------------
#用osm底图叠加数据作图，tmap工具包的名称是Thematic Maps的缩写，
#是R中专门绘制地图的工具包。该包语法与ggplot2包比较类似，都是通过符合+来进行图层叠加。
tmap_mode("plot")

tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(BoroughDataMap) + 
  tm_polygons("rate_of_job_seekers_allowance_jsa_claimants_2015", #使用数据
              style="jenks",
              palette="YlOrBr",
              midpoint=NA,
              title="Rate per 1,000 people",#图例名
              alpha = 0.5) + #透明度
  tm_compass(position = c("left", "bottom"),type = "arrow") + #指北针
  tm_scale_bar(position = c("left", "bottom")) +#比例尺
  tm_layout(title = "Job seekers' Allowance Claimants", legend.position = c("right", "bottom"))#图名
#----------------------------------------------
flytipping <- read_csv("https://data.london.gov.uk/download/fly-tipping-incidents/536278ff-a391-4f20-bc79-9e705c9b3ec0/fly-tipping-borough.csv")


flytipping1 <- read_csv("https://data.london.gov.uk/download/fly-tipping-incidents/536278ff-a391-4f20-bc79-9e705c9b3ec0/fly-tipping-borough.csv", 
                        col_types = cols(#以下都是创建列名并计数
                          code = col_character(),
                          area = col_character(),
                          year = col_character(),
                          total_incidents = col_number(),
                          total_action_taken = col_number(),
                          warning_letters = col_number(),
                          fixed_penalty_notices = col_number(),
                          statutory_notices = col_number(),
                          formal_cautions = col_number(),
                          injunctions = col_number(),
                          prosecutions = col_number()
                        ))
# view the data
view(flytipping1)#使用view预览

#convert the tibble into a tidy tibble
flytipping_long <- flytipping1 %>% 
  pivot_longer(#宽表变长表
    cols = 4:11,#不明白这个切片
    names_to = "tipping_type",
    values_to = "count"
  )

# view the data
view(flytipping_long)


#an alternative which just pulls everything out into a single table
flytipping2 <- flytipping1[,1:4]



#pivot the tidy tibble into one that is suitable for mapping
flytipping_wide <- flytipping_long %>% 
  pivot_wider(
    id_cols = 1:2,
    names_from = c(year,tipping_type),
    names_sep = "_",
    values_from = count
  )

view(flytipping_wide)


widefly <- flytipping2 %>% 
  pivot_wider(
    names_from = year, 
    values_from = total_incidents)
 installed.packages(RODBC)
data()
mtcars
installed.packages("maps")

