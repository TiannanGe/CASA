library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)
# ------------------加载伦敦边界shp
Londonborough <- st_read(here::here("statistical-gis-boundaries-london",
                                    "London_Borough_Excluding_MHW.shp"))%>%
                           st_transform(., 27700)
# ------------------#加载osm的poi
OSM <- st_read(here::here("greater-london-latest-free",
                          "gis_osm_pois_a_free_1.shp")) %>%
  st_transform(., 27700) %>%#转化为同一个坐标系，即27700，这是针对英国地图的一个坐标系
  #仅选择酒店，dplyr包的filter选择函数
  dplyr::filter(fclass == 'hotel')
# ------------------
join_example <-  st_join(Londonborough, OSM)#SF包的st_join函数是空间连接
plot(join_example)
head(join_example)
# ------------------#加载世界主要城市poi
Worldcities <- st_read(here::here("World_Cities", 
                                  "World_Cities.shp")) %>%
  st_transform(., 27700)
# ------------------#加载英国全境边界shp
UK_outline <- st_read(here::here("gadm41_GBR_shp", 
                                 "gadm41_GBR_0.shp")) %>%
  st_transform(., 27700)
# ------------------爱彼迎加载csv，设置经纬度，投影，转为英国本土坐标，筛选365天营业的，整个家庭的
Airbnb <- read_csv("listings.csv") %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)%>%
  #select entire places that are available all year
  filter(room_type == 'Entire home/apt' & availability_365 =='365')
# ------------------定一个函数joinfun，两参数，功能是二参连接一参，添加计数，以GSS_CODE列为分类依据，列名为hotels_in_borough
Joinfun <- function(data1, data2){
  
  output<- data1%>%
    st_join(data2,.) %>%
    add_count(GSS_CODE, name="hotels_in_borough") 
  
  return(output)
}

# use the function for hotels
Hotels <- Joinfun(OSM, Londonborough)#使用函数，计算出osm酒店poi连接伦敦边界后赋值给新的面Hotels
plot(Hotels)
# then for airbnb
Airbnb <- Joinfun(Airbnb, Londonborough)#同理做爱彼迎

Worldcities2 <- Worldcities %>%#世界城市里筛选出英国里的三个城市
  filter(CNTRY_NAME=='United Kingdom'&
           Worldcities$CITY_NAME=='Birmingham'|
           Worldcities$CITY_NAME=='London'|
           Worldcities$CITY_NAME=='Edinburgh')

newbb <- c(xmin=-296000, ymin=5408, xmax=655696, ymax=1000000)#新建一个向量

UK_outlinecrop <- UK_outline$geometry %>%
  st_crop(., newbb)#st_crop用指定地理或投影坐标确定的范围截取矢量对象，裁剪范围为newbb向量范围

Hotels <- Hotels %>%
  #at the moment each hotel is a row for the borough
  #we just one one row that has number of airbnbs
  group_by(., GSS_CODE, NAME)%>%#以城市为组，统计酒店数（Accomodation count）等于hotels_in_borough列元素的唯一值
  summarise(`Accomodation count` = unique(hotels_in_borough))

Airbnb <- Airbnb %>%#爱彼迎同理
  group_by(., GSS_CODE, NAME)%>%
  summarise(`Accomodation count` = unique(hotels_in_borough))
# ------------------
tmap_mode("plot")

# set the breaks
# for our mapped data
breaks = c(0, 5, 12, 26, 57, 286) #设置一组打断点（图例间隔)

# plot each map
tm1 <- tm_shape(Hotels) +#设置tm1小图，使用hotels数据的Accomodation count列作图，设置打断点， 
  tm_polygons("Accomodation count", 
              breaks=breaks,
              palette="PuBu")+#设置样式
  tm_legend(show=TRUE)+#显示图例
  tm_layout(frame=TRUE)+#显示图框
  tm_credits("(a)", position=c(0,0.85), size=1.5)#添加标题

tm2 <- tm_shape(Airbnb) + 
  tm_polygons("Accomodation count",
              breaks=breaks, 
              palette="PuBu") + 
  tm_legend(show=TRUE)+
  tm_layout(frame=TRUE)+
  tm_credits("(b)", position=c(0,0.85), size=1.5)

tm3 <- tm_shape(UK_outlinecrop)+ 
  tm_polygons(col="darkslategray1")+
  tm_layout(frame=TRUE)+
  tm_shape(Worldcities2) +
  tm_symbols(col = "red", scale = .5)+
  tm_text("CITY_NAME", xmod=-1, ymod=-0.5)

legend <- tm_shape(Hotels) +#图例
  tm_polygons("Accomodation count",#以酒店数做图例
              palette="PuBu") +
  tm_scale_bar(position=c(0.2,0.04), text.size=0.6)+#设置比例尺
  tm_compass(north=0, position=c(0.65,0.6))+#设置指北针
  tm_layout(legend.only = TRUE, legend.position=c(0.2,0.25),asp=0.1)+#设置布局
  tm_credits("(c) OpenStreetMap contrbutors and Airbnb", position=c(0.0,0.0))

t=tmap_arrange(tm1, tm2, tm3, legend, ncol=2)#设置图顺序

t
# ------------------
library(grid)
grid.newpage()#grid包新建页面

pushViewport(viewport(layout=grid.layout(2,2)))#设置2行，2列
print(tm1, vp=viewport(layout.pos.col=1, layout.pos.row=1, height=5))
print(tm2, vp=viewport(layout.pos.col=2, layout.pos.row=1, height=5))
print(tm3, vp=viewport(layout.pos.col=1, layout.pos.row=2, height=5))
print(legend, vp=viewport(layout.pos.col=2, layout.pos.row=2, height=5))
# ------------------airbnb取出四周的一个边框，Londonbb 就是取得后的方框
Londonbb <- st_bbox(Airbnb,
                    crs = st_crs(Airbnb))%>%
  #we need this to convert it into a class of sf
  # otherwise it our bb won't have a class it will just be x and y coordinates for the box
  # this makes it into a polygon
  st_as_sfc()

# ------------------
main <- tm_shape(Airbnb, bbbox = Londonbb) + #使用Airbnb数据的Accomodation count列，设置断点
  tm_polygons("Accomodation count",
              breaks=breaks,
              palette="PuBu")+
  tm_scale_bar(position = c("left", "bottom"), text.size = .75)+#比例尺
  tm_layout(legend.position = c("right","top"), #图例位置
            legend.text.size=.75, #图例尺寸
            legend.title.size = 1.1,#图例标题
            frame=FALSE)+#不要框架
  tm_credits("(c) OpenStreetMap contrbutors and Air b n b", position=c(0.0,0.0))+#加一段文字
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.06, 0.1)) +#指北针
  
  #bottom left top right
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.2))#内边距（地图与边框四周的距离）
main
# ------------------画出英国三个城市的位置
inset = tm_shape(UK_outlinecrop) + tm_polygons() +#使用英国边界和Londonbb
  tm_shape(Londonbb)+ 
  tm_borders(col = "grey40", lwd = 5)+#此代码仅针对Londonbb起作用，tm_polygons既绘制边界又填充颜色。tm_fill只填充不边界，tm_borders只边界不填充
  tm_layout(frame=FALSE,#是否要画出外边框
            bg.color = "transparent")+#背景为透明
  tm_shape(Worldcities2) +#加载三个城市poi
  tm_symbols(col = "red", scale = .5)+#poi样式
  tm_text("CITY_NAME", xmod=-1.5, ymod=-0.5)#poi标注为city_name列
inset

# ------------------
library(grid)
main
print(inset, vp = viewport(0.86, 0.29, width = 0.5, height = 0.55))#不明白为什么vp能和inset放在一起出图
# ------------------
tmap_mode("view")#plot是静态，view是交互式

tm_shape(Airbnb) + 
  tm_polygons("Accomodation count", breaks=breaks) #图层是使用Accomodation count
# ------------------
# library for pop up boxes设置弹出框
library(leafpop)
library(leaflet)
library(raster)
#join data
Joined <- Airbnb%>%
  st_join(., Hotels, join = st_equals)%>%#sf包的 st_join，做空间连接Airbnb和Hotels
  dplyr::select(GSS_CODE.x, NAME.x, `Accomodation count.x`, `Accomodation count.y`)%>%#x指Airbnb，以Airbnb为主，仅加入Hotels的count，所以就一个y
  dplyr::rename(`GSS code` =`GSS_CODE.x`,
                `Borough` = `NAME.x`,
                `Airbnb count` = `Accomodation count.x`,
                `Hotel count`= `Accomodation count.y`)%>%
  st_transform(., 4326)#转换坐标指WGS84

tmap_mode("view")#plot是静态，view是交互式

tm_shape(Joined) + 
  tm_polygons("Hotel count", breaks=breaks) #图层是使用Accomodation count
# ------------------
#本段不明白
#remove the geometry for our pop up boxes to avoid
popupairbnb <-Joined %>%
  st_drop_geometry()%>%#删除对象的几何要素，成为数据框
  dplyr::select(`Airbnb count`, Borough)%>%
  popupTable()

popuphotel <-Joined %>%
  st_drop_geometry()%>%
  dplyr::select(`Hotel count`, Borough)%>%
  popupTable()
# ------------------
tmap_mode("view")

# set the colour palettes using our previously defined breaks

#colorBin()相当于对数据切片分箱，将数字性向量数据与固定数量的颜色输出相匹配。
pal1 <- Joined %>%
  colorBin(palette = "YlOrRd", domain=.$`Airbnb count`, bins=breaks)

pal1 <-colorBin(palette = "YlOrRd", domain=Joined$`Airbnb count`, bins=breaks)

pal2 <- Joined %>%
  colorBin(palette = "YlOrRd", domain=.$`Hotel count`, bins=breaks)


map<- leaflet(Joined) %>%#leaflet开源交互式地图包
  # add basemap options
  addTiles(group = "OSM (default)") %>%#瓦片是osm
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB")%>%
  
  #add our polygons, linking to the tables we just made
  addPolygons(color="white", 
              weight = 2,
              opacity = 1,
              dashArray = "3",
              popup = popupairbnb,
              fillOpacity = 0.7,
              fillColor = ~pal2(`Airbnb count`),
              group = "Airbnb")%>%
  
  addPolygons(fillColor = ~pal2(`Hotel count`), 
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              popup = popupairbnb,
              fillOpacity = 0.7,group = "Hotels")%>%
  # add a legend
  addLegend(pal = pal2, values = ~`Hotel count`, group = c("Airbnb","Hotel"), 
            position ="bottomleft", title = "Accomodation count") %>%
  # specify layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite", "CartoDB"),
    overlayGroups = c("Airbnb", "Hotels"),
    options = layersControlOptions(collapsed = FALSE)
  )

# plot the map
map