# library(ggmap)
library(ggplot2)
# library(raster)
# library(rgeos)
# library(maptools)
library(rgdal)

library(xlsx)
library(ggplot2)
library(dplyr)
library(rlang)
library(reshape2)
library(devtools)

#### 주택종류별 (동별) 엑셀
c_df = read.xlsx("./input_data/주택종류별 주택(동별).xls", sheetIndex = 1, encoding = "UTF-8", stringsAsFactors = F)

colnames(c_df) <- c_df[2,]
c_df <- c_df[c(-1,-2),]

c_df <- rename(c_df,  "주택(계)" = "계")

new_c_df <- c_df %>% filter(자치구 == "관악구") %>% select("자치구", "동", "합계",  "단독주택", "연립주택", "다세대주택", "비거주용건물내주택")

cols.num <- c("합계",  "단독주택", "연립주택", "다세대주택", "비거주용건물내주택")

new_c_df[cols.num] <- sapply(new_c_df[cols.num], as.numeric)

# sapply(new_c_df, class)

#new_c_df[c("합계",  "단독주택", "연립주택", "다세대주택", "비거주용건물내주택")] <- as.numeric(new_c_df[c("합계",  "단독주택", "연립주택", "다세대주택", "비거주용건물내주택")])


new_c_df <- new_c_df %>% mutate(SUMS = 단독주택 + 연립주택 + 다세대주택 +비거주용건물내주택) %>% mutate(RATIO = SUMS / 합계)

new_c_df["RATIO"] = lapply(new_c_df["RATIO"], round, 3) 

new_c_df2 <- rename(new_c_df, "dongNm" = "동")

new_c_df2 <- new_c_df2[-1,]

##### 지도를 그려보자자

list.dirs(path = "./input_data")

list.files(path = "./input_data/gwanak_gu_dong_00_2018")

# 관악구 sp 파일 객체를 만듬
gwanak_gu <- readOGR(
  dsn = "./input_data/gwanak_gu_dong_00_2018",
  layer = "bnd_dong_11210_2018_2018_2Q",
  encoding = "UTF-8"
)

class(gwanak_gu)

# 관악구 객체파일을 데이터프레임으로 변환
gwanakDf <- fortify(model = gwanak_gu)

str(gwanakDf)
head(x = gwanakDf, n = 10L)

# 관악구는 S4 클래스라서 @로 어세스 해야함
temp_gwanak_dataDf <- gwanak_gu@data

class(gwanak_gu@data)
str(gwanak_gu@data)

# id column을 data 데이터 프레임에 추가
gwanak_gu@data$id <- rownames(x = gwanak_gu@data)
gwanak_gu@data


# 데이터 프레임 병합
new_gwanakDf <- merge(x = gwanakDf,
                      y = gwanak_gu@data[, c("id", "base_year", "adm_dr_cd", "adm_dr_nm")],
                      by = 'id',
                      all.x = T)

# id 랑 order 기준 정렬??
new_gwanakDf <- new_gwanakDf[order(new_gwanakDf$id, new_gwanakDf$order),]

head(x = new_gwanakDf, n = 10L)

colnames(new_gwanakDf)[8:10] <- c('year', 'dongCd', 'dongNm')

#order 4로 나누었을 때 나머지가 1인 행만 남깁니다
# new_gwanakDf <- new_gwanakDf[new_gwanakDf$order %% 4 == 1, ]

nrow(new_gwanakDf)

my_theme <- theme(panel.background = element_blank(),
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  plot.title = element_text(hjust = 0.5,
                                            face = 'bold'))

# 지도 그리기
gwanakMap <- ggplot(data = new_gwanakDf,
                  mapping = aes(x = long, 
                                y = lat,
                                group = group
                  )) + 
  geom_polygon(fill = 'white',
               color = 'black') +
  my_theme

gwanakMap

new_gwanakDf2 <- new_gwanakDf
# all(is.na(new_gwanakDf$id))

# WGS84 좌표 -> GRS80 변환?

library(sp)
library(magrittr)
library(mapproj)

convertCoords <- function(lon, lat) {
  
  # 2개 벡터 입력 받아 데이터프레임 만들고 
  # Sp type 변환환
  xy <- data.frame(lon = lon, lat = lat)
  coordinates(obj = xy) <- ~ lon + lat
  
  # CRS (Coordinate Reference Syste) 설정
  fmCRS <- CRS('+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs')
  toCRS <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
  
  # GRS80 좌표 -> WGS84
  xy %>% 
    SpatialPoints(proj4string = fmCRS) %>% 
    spTransform(CRSobj = toCRS) %>% 
    SpatialPoints() %>% 
    as.data.frame() %>% 
    set_colnames(c('lonWGS84', 'latWGS84')) %>% 
    return()
}

changedCoords <- convertCoords(lon = new_gwanakDf2$long, lat = new_gwanakDf2$lat)

new_gwanakDf2 <- cbind(new_gwanakDf2, changedCoords)

gwanakMap <- ggplot(data = new_gwanakDf2,
                  mapping = aes(x = lonWGS84, 
                                y = latWGS84,
                                group = group
                  )) + 
  geom_polygon(fill = 'white',
               color = 'black') +
  my_theme +
  coord_map()

gwanakMap

# 선거결과 지역명과 경계데이터 areaNm이 서로 같은지 확인합니다.
intersect(x = new_c_df2$dongNm, y = new_gwanakDf2$dongNm %>% unique() %>% sort()) %>% length()

# 나만의 theme? 
my_theme <- 
  theme(
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    legend.direction = 'horizontal', 
    legend.position = c(0.8, 0.1), 
    legend.text = element_text(family = 'NanumGothic', size = 8, face = 'bold'), 
    legend.title = element_text(family = 'NanumGothic', size = 10, face = 'bold')
  )

gwanakMap <- ggplot(data = new_gwanakDf2,
                    mapping = aes(x = long, 
                                  y = lat,
                                  group = group
                    )) + 
  geom_polygon(fill = 'white',
               color = 'black') +
  ggtitle(label = "관악구 동별 지도") +
  my_theme +
  coord_fixed()

gwanakMap

new_gwanakDf3 <- merge(x = new_gwanakDf2,
                       y = new_c_df2[,c(2,9)],
                       by.x = 'dongNm',
                       by.y = 'dongNm',
                       all.x = T)

# id와 order 기준으로 오름차순 정렬합니다. 이거 정렬 df 새로 만들거나 df를 변경할때마다 꼭 해주어야함
new_gwanakDf3 <- new_gwanakDf3[order(new_gwanakDf3$id, new_gwanakDf3$order), ]


# 구 이름 표기 -> 동별 위도 경도 평균으로 이름

center_long <- new_gwanakDf3 %>% group_by(dongNm) %>% 
  summarise(center_long = mean(long)) %>% 
  as.data.frame()

center_long

center_lat <- new_gwanakDf3 %>% group_by(dongNm) %>% 
  summarise(center_lat = mean(lat)) %>% 
  as.data.frame()

center_lat

dong_center_polygon <- merge(center_long, center_lat,
                             by = "dongNm")

dong_center_polygon

naming_gwanakDf <- merge(new_gwanakDf3, dong_center_polygon,
                         by = "dongNm")

str(naming_gwanakDf)

# new_gwanakDf[is.na(new_gwanakDf$id),]
library(RColorBrewer)

myPal <- brewer.pal(n = 9, name = 'Reds')

ggplot(data = new_gwanakDf3,
       mapping = aes(x = long, 
                     y = lat,
                     group = group
       )) + 
  geom_polygon(mapping = aes(fill = RATIO, group = dongNm),
               color = 'black') +
  ggtitle(label = "관악구 동별 지도") +
  my_theme +
  coord_fixed() +
  scale_fill_gradient(low = "white", high = "red") +
  # 동 이름 
  geom_text(aes(x = naming_gwanakDf$center_long,
                y = naming_gwanakDf$center_lat),
            label = naming_gwanakDf$dongNm,
            size = 3)





