library(sf)
library(here)
library(raster)
library(terra)#terra是用于空间数据分析的 R 包，是raster包的升级版，可用于栅格数据和矢量数据处理和空间分析。 此外在R中，还有sf、sp、raster、stars等包可用于处理空间数据。
library(fs)#文件操作包
library(tidyverse)
library(ggplot2)
#-------------#
shape <- st_read("D:/CASA/CASA0005_Geographic_Information_Systems_and_Science/week3/workdata/wk3hw/gadm41_GBR_shp/gadm41_GBR_2.shp")
qqsg<-terra::rast(here( "wc2.1_2.5m_tmin_ACCESS-CM2_ssp126_2081-2100.tif"))
plot(qqsg)#画出栅格
plot(shape)#画出shp
st_crs(shape)#查看shp坐标类型
st_crs(qqsg)#查看shp坐标类型
#-------------#裁剪,使用矢量裁剪栅格,画出裁剪后的栅格
ldsg <- shape %>%
  terra::crop(qqsg,.)
exactAus<-terra::mask(qqsg, shape)
plot(ldsg)
#-------------#出直方图,exactaus中第三月的数据
hist(exactAus[[3]], col="red", main ="names")
#-------------#
hist(exactAus[], col="green", main ="names")
#-------------#
ldsg1 <- exactAus %>%
  as.data.frame()
gghist <- ggplot(ldsg1, 
                 aes(x=tmin01)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Australian March temperatures", 
       x="Temperature", 
       y="Frequency")
#-------------#
gghist + geom_vline(aes(xintercept=mean(names, 
                                        na.rm= TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))
