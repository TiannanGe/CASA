library(sf)
library(here)
library(raster)
library(terra)#terra是用于空间数据分析的 R 包，是raster包的升级版，可用于栅格数据和矢量数据处理和空间分析。 此外在R中，还有sf、sp、raster、stars等包可用于处理空间数据。
library(fs)#文件操作包
library(tidyverse)
library(ggplot2)
library(dplyr)
#-------------#查看有哪些图层st_layers()，st_layers是sf里的选择图层命令，使用here后,进入第一个子目录,选择gpkg包
st_layers(here("gadm36_AUS_gpkg", "gadm36_AUS.gpkg")) 

#-------------#使用st_read()读gpkg包里的第一个图层，layer=gadm36_AUS_0,以Ausoutline定义
Ausoutline <- st_read(here("gadm36_AUS_gpkg", "gadm36_AUS.gpkg"), 
                      layer='gadm36_AUS_0')
  st_set_crs(4326)#如没有没有空间参考系统，我们可以使用st_set_crs(),仅在加载数据时没有 CRS 时才有用。
'''
Ausoutline <- Ausoutline %>%
  st_set_crs(., 4326) 如已加载了没空间参考系统的数据,可通过此注释代码赋值.
'''
print(Ausoutline)
#-------------#使用st_crs函数来查看导入的sf对象是否含有投影信息。
st_crs(Ausoutline)$proj4string
#-------------#将WGS84转换为澳大利亚GDA94,使用st_transform(.,3112),3112是GDA94的EPSG
AusoutlinePROJECTED <- Ausoutline %>%
  st_transform(.,3112)
print(AusoutlinePROJECTED)
#-------------##C从SF变为SP
AusoutlineSP <- Ausoutline %>%
  as(., "Spatial")
#-------------##C从SP变为SF(一般常用SF)
AusoutlineSF <- AusoutlineSP %>%
  st_as_sf()
#-------------#使用terra包的rast功能读取tif栅格数据,并绘制数据，全球气候层（栅格）数据集
jan<-terra::rast(here("wc2.1_5m_tavg", "wc2.1_5m_tavg_01.tif"))#栅格数据使用rast读取
# have a look at the raster layer jan
jan
plot(jan)
st_crs(jan)$proj4string
#-------------#使用 Mollweide 投影保存到新对象的快速示例
pr1 <- terra::project(jan, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#使用terra包的project投影功能,投影jan栅格,投影为WGS84，投影到pr1
newproj<-"ESRI:54009"#定义新的投影代号到newproj
#获得Jan的光栅，并给它新的proj4。
pr1 <- jan %>%
  terra::project(., newproj)
plot(pr1)
#-------------#投影回WGS84
pr1 <- pr1 %>%
  terra::project(., "EPSG:4326")
plot(pr1)
#-------------#查看该目录下所有文件的信息
dir_info("wc2.1_5m_tavg") 
#-------------#提取所有tif文件到listfiles
library(dplyr)
listfiles<-dir_info("wc2.1_5m_tavg") %>%#wc2.1_5m_tavg目录下所有tif文件进去到listfiles，以下为固定格式
  filter(str_detect(path, ".tif"))%>%
  dplyr::select (path) %>%#select同时存在于dplyr和raster包中，使用：：准确引用
  pull()
view(listfiles)
listfiles
#-------------#将所有数据直接加载到 SpatRaster。SpatRaster 是具有相同空间范围和分辨率的栅格图层的集合。
worldclimtemp <- listfiles %>%
  terra::rast()#栅格数据用rast读取
worldclimtemp
plot(worldclimtemp)
plot(worldclimtemp[[1]])#选择第一个月的图
#-------------#访问栅格集的所有文件用[1],访问其中第一个用[[1]]
worldclimtemp[1]
worldclimtemp[[1]]
#-------------#重命名堆栈中的层
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(worldclimtemp) <- month
worldclimtemp$Jul
#-------------#定义了三个向量,全都放在一个列表中
site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
          "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
          "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)
#把以上信息放到一个列表中
samples <- data.frame(site, lon, lat, row.names="site")
# 从Rasterstack中提取所有点的数据 ,worldclimtemp是栅格，sanmoles是指定点，按指定点提取该点位置的栅格值
AUcitytemp<- terra::extract(worldclimtemp, samples)
plot(AUcitytemp[[10]])
plot(samples)
#-------------#将城市名添加到表中,site是第一个向量,添加到Site列中,在jan之前.
Aucitytemp2 <- AUcitytemp %>% 
  as_tibble()%>% 
  add_column(Site = site, .before = "Jan")
#-------------#取了perth城市的温度为单独一列,通过定位到城市名
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")
#-------------#取了perth城市的温度为单独一列,通过定位到列表行数
Perthtemp <- Aucitytemp2[3,]
#-------------#使用hist()函数绘制直方图,转为数字，x为温度区间，y为频率
hist(as.numeric(Perthtemp))
#-------------#定义断点位置
userbreak<-c(8,10,12,14,16,18,20,22,24,26)
#-------------# 在Aucitytemp2中提取了perth城市的信息行,重新赋值给Perthtemp，这与之前的操作完全一样
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")
#-------------# 在Perthtemp中选择出从Jan:Dec的列给到t，切片
t<-Perthtemp %>%
  dplyr::select(Jan:Dec)
#-------------# t转数字型,断点数据为之前设置好的userbreak,红色,标题等等,用hist()画直方图
hist((as.numeric(t)), 
     breaks=userbreak, 
     col="red", 
     main="Histogram of Perth Temperature", 
     xlab="Temperature", 
     ylab="Frequency")
#-------------#t转为数字型,并将值赋给histinfo,画个直方图,点代指了histinfo.
histinfo <- as.numeric(t) %>%
  hist(.,
       breaks=userbreak, 
       col="green", 
       main="Histogram of Perth Temperature", 
       xlab="Temperature", 
       ylab="Frequency")
#-------------#加载矢量并简化矢量
plot(Ausoutline$geom)#在Ausoutline中查找geom,并绘画

AusoutSIMPLE <- Ausoutline$geom %>%
  st_simplify(., dTolerance = 1000) %>%#使用st_simplify函数简化，dTolerance参数控制简化级别

  plot()

print(Ausoutline)
print(worldclimtemp)
crs(worldclimtemp)#查看投影
class(worldclimtemp)#"SpatRaster"格式
#-------------#裁剪,使用矢量裁剪栅格，Ausoutline矢量使用crop函数裁剪temp，使用mask蒙版这两个
Austemp <- Ausoutline %>%
  terra::crop(worldclimtemp,.)
  exactAus<-terra::mask(worldclimtemp, Ausoutline)
plot(exactAus)
#-------------#出直方图,exactaus中第三月的数据
#subset using the known location of the raster
hist(exactAus[[3]], col="red", main ="March temperature")
#-------------#使用ggplot2出图
#将exactAus是栅格，变为数据框格式,给到exactAusdf
exactAusdf <- exactAus %>%
  as.data.frame()
plot(exactAusdf[[3]])
#-------------#使用ggplot2出图,x =mar三月,设置柱状颜色,标题等
gghist <- ggplot(exactAusdf, 
                 aes(x=Mar)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Australian March temperatures", 
       x="Temperature", 
       y="Frequency")
gghist
#-------------#在表示平均温度的直方图上添加一条垂直线，geom_vline画线函数
gghist + geom_vline(aes(xintercept=mean(Mar, 
                                        na.rm=TRUE )),#rm是remove，指定na.rm=T，就会移除na数据
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))

#-------------#将每月的温度放入变量

squishdata<-exactAusdf%>%
  pivot_longer(#该函数从宽列表变为长列表
    cols = 1:12,
    names_to = "Month",#names_to是固定用法
    values_to = "Temp"#values_to是固定用法
  )

squishdata
#-------------#filter对数据进行子集化,选择两个月
twomonths <- squishdata %>%
  # | = OR
  filter(., Month=="Jan" | Month=="Jun")
#-------------#在这两个月中, 每个月的平均值,统计新增一列叫mean,以月为分组,计算Temp的平均值，赋值到meantwomonths
meantwomonths <- twomonths %>%
  group_by(Month) %>%
  summarise(mean=mean(Temp, na.rm=TRUE))

meantwomonths
#-------------#根据变量（这是我们的月份）选择颜色和填充。虚线的位置是我们刚刚计算的平均值，线条也基于列变量。
ggplot(twomonths, aes(x=Temp, color=Month, fill=Month)) + #aes函数,x=温度,颜色和填充都是月份
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=meantwomonths, 
             aes(xintercept=mean, 
                 color=Month),
             linetype="dashed")+
  labs(title="Ggplot2 histogram of Australian Jan and Jun
       temperatures",
       x="Temperature",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
#-------------#
data_complete_cases <- squishdata %>%#前后没变化啊搞毛啊
  drop_na()%>% 
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar",#mutate函数创建新列，列名month，因素为月份12个
                                          "Apr","May","Jun",
                                          "Jul","Aug","Sep",
                                          "Oct","Nov","Dec")))
#-------------#
# Plot faceted histogram绘制面状直方图
ggplot(data_complete_cases, aes(x=Temp, na.rm=TRUE))+#使用ggplot函数画图，数据是datacomplete，移除空值，使用hist直方图
  geom_histogram(color="black", binwidth = 5)+
  labs(title="Ggplot2 faceted histogram of Australian temperatures", 
       x="Temperature",
       y="Frequency")+
  facet_grid(Month ~ .)+#facet_grid一个月一个直方图
  theme(plot.title = element_text(hjust =0.5))#hiust是标题位置，0.5居中

#-------------#

library(plotly)#绘制交互式图表
# split the data for plotly based on month，使用plotly工具对月份数据做切割

jan <- squishdata %>%
  drop_na() %>%#删除缺失值
  filter(., Month=="Jan")#筛选吧一月份全部筛选出来，给到jan
jun <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jun")

# 给XY设置两个title，温度频率
x <- list (title = "Temperature")
y <- list (title = "Frequency")

# set the bin width
xbinsno<-list(start=0, end=40, size = 0.5)#设置了一个列表，里面三个标头，size是断点，数据颗粒度
xbinsno
# 绘制直方图
ihist<-plot_ly(alpha = 0.6) %>%
  add_histogram(x = jan$Temp,#x等于jan的温度列，xbins是断点设置
                xbins=xbinsno, name="January") %>%
  add_histogram(x = jun$Temp,#又添加了一个新要素在图中
                xbins=xbinsno, name="June") %>% 
  layout(barmode = "overlay", xaxis=x, yaxis=y)

ihist
xbinsno
#-------------#

# mean per month
meanofall <- squishdata %>%
  group_by(Month) %>%#按月份做合计
  summarise(mean = mean(Temp, na.rm=TRUE))#统计：mean列的内容是使用mean函数求temp的平均值，忽略空值
meanofall
# print the top 1
head(meanofall, n=1)
#-------------#
# standard deviation per month求标准差
sdofall <- squishdata %>%
  group_by(Month) %>%
  summarize(sd = sd(Temp, na.rm=TRUE))
sdofall
# maximum per month#求最大最小值
maxofall <- squishdata %>%
  group_by(Month) %>%
  summarize(max = max(Temp, na.rm=TRUE))

# minimum per month
minofall <- squishdata %>%
  group_by(Month) %>%
  summarize(min = min(Temp, na.rm=TRUE))

# Interquartlie range per month四分位点内距
IQRofall <- squishdata %>%
  group_by(Month) %>%
  summarize(IQR = IQR(Temp, na.rm=TRUE))
IQRofall

# 一个列表中存储多个输出。
lotsofstats <- squishdata %>%
  group_by(Month) %>%
  summarize(IQR = IQR(Temp, na.rm=TRUE), 
            max=max(Temp, na.rm=T),
            min=min(Temp,na.rm=T))
lotsofstats、

# 求全年平均数，没有groupby，就是求整年的

meanwholeyear=squishdata %>%
  summarize(meanyear = mean(Temp, na.rm=TRUE))
meanwholeyear
