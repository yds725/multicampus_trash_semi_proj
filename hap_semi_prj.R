library(xlsx)
library(ggplot2)
library(dplyr)
library(rlang)
library(reshape2)
library(devtools)

install.packages("rgdal")



p_df = read.xlsx("./input_data/가구원수별_구별.xls", sheetIndex = 1, encoding = "UTF-8", header = T, stringsAsFactors = F)

## 첫번 째 로우에 있는 데이타를 넣음

colnames(p_df) <- p_df[1,]

p_df <- p_df[-1,]

p_df

#is.data.frame(p_df)

target_col = "1인가구"
target_quo = parse_quosure(target_col)

small_df <- p_df %>% select("기간", "구분", "1인가구") 

small_df <- small_df[-c(1),]

small_df <- rename(small_df,  "Year" = "기간" , "District" = "구분",    "Households" = "1인가구")

small_top5_df <- small_df %>% arrange(Households) %>% head(5)

small_top5_df$Households <- as.numeric(small_top5_df$Households)

small_top5_df[1, "Households"]

ggplot(data=small_top5_df, aes(x=District, y=Households)) + geom_col()


#### 1인가구 거처종류별 통계 엑셀
b_df = read.xlsx("./input_data/1인가구 거처종류별 통계.xls", sheetIndex = 1, encoding = "UTF-8", header = T, stringsAsFactors = F)

colnames(b_df) <- b_df[1,]

b_df <- b_df[-1,]

b_df %>% select(기간, 자치구, 합계, 계, 단독주택, 연립주택, 다세대주택) %>% filter(기간 == "2018")


#### 주택종류별 (동별) 엑셀
c_df = read.xlsx("./input_data/주택종류별 주택(동별).xls", sheetIndex = 1, encoding = "UTF-8", stringsAsFactors = F)

colnames(c_df) <- c_df[2,]
c_df <- c_df[c(-1,-2),]

c_df <- rename(c_df,  "주택(계)" = "계")

new_c_df <- c_df %>% filter(자치구 == "관악구") %>% select("자치구", "동", "합계",  "단독주택", "연립주택", "다세대주택", "비거주용건물내주택")

cols.num <- c("합계",  "단독주택", "연립주택", "다세대주택", "비거주용건물내주택")

new_c_df[cols.num] <- sapply(new_c_df[cols.num], as.numeric)

# sapply(new_c_df, class)

new_c_df[c("합계",  "단독주택", "연립주택", "다세대주택", "비거주용건물내주택")] <- as.numeric(new_c_df[c("합계",  "단독주택", "연립주택", "다세대주택", "비거주용건물내주택")])


new_c_df <- new_c_df %>% mutate(SUMS = 단독주택 + 연립주택 + 다세대주택 +비거주용건물내주택) %>% mutate(RATIO = SUMS / 합계)

new_c_df["RATIO"] = lapply(new_c_df["RATIO"], round, 3) 

## 동별 지도를 그려보자?

# 한국행정지도 shape파일ㅇㄹ 
# install_github("cardiomoon/Kormaps")

# library(ggmap)
library(ggplot2)
# library(raster)
# library(rgeos)
# library(maptools)
library(rgdal)

list.dirs(path = "./input_data")

list.files(path = "./input_data/bnd_sido_00_2018_2018")

# 지도 데이터 파일 불러오기?
# sido 파일이 sp객체가 됨
sido <- readOGR(
  dsn = "./input_data/bnd_sido_00_2018_2018",
  layer = "bnd_sido_00_2018_2018_2Q",
  encoding = "UTF-8"
)

class(sido)

# sido객체를 데이트프레임으로 변환
sidoDf <- fortify(model = sido)

str(sidoDf)
# 행정경계 구역 데이터는 UTM-K (GRS80 타원) 좌표 기준  
# 그리고 저 위에 있는 기준으로는 ggplot 경계 지도 그릴 때
# coord_map() 함수 사용 불가
# 구글 지도 api가 제공하는 데이터는 UTM 좌표 기준이다.
head(x = sidoDf, n = 10L)

# sido는 s4 class라서 @ 기호 이용
sido@data

class(sido@data)

str(sido@data)

# sido@data에서 행번호를 id colunㅇ로 만듬
sido@data$id <- rownames(x = sido@data)

# 두 데이터 프레임 병합
new_sidoDf <- merge(x = sidoDf,
                    y = sido@data[, c('id', 'base_year', 'sido_cd', 'sido_nm')],
                    by = 'id',
                    all.x = T)

# id 랑 order 기준으로 정렬 ?
new_sidoDf <- new_sidoDf[order(new_sidoDf$id, new_sidoDf$order), ]

head(x = new_sidoDf, n = 10L)

colnames(new_sidoDf)[8:10] <- c('year', 'sidoCd', 'sidoNm')

#order 4로 나누었을 때 나머지가 1인 행만 남깁니다
new_sidoDf1 <- new_sidoDf[new_sidoDf$order %% 4 == 1, ]

# 새로 데이트 프렘 건수 확인
nrow(new_sidoDf1)

my_theme <- theme(panel.background = element_blank(),
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  plot.title = element_text(hjust = 0.5,
                                            face = 'bold'))

sidoMap <- ggplot(data = new_sidoDf1,
                  mapping = aes(x = long, 
                                y = lat,
                                group = group
                                )) + 
  geom_polygon(fill = 'white',
               color = 'black') +
  my_theme

sidoMap

# 깔끔하게 하기 위해 섬 부속 지역 제외
new_sidoDf2 <- subset(x = new_sidoDf1, subset = new_sidoDf1$piece == 1)

nrow(x = new_sidoDf2)

# WGS84 좌표 -> GRS80 변환?

library(sp)
library(magrittr)

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

install.packages("mapproj")
library(mapproj)

# sidodf 위경도 좌표 반환
changedCoords <- convertCoords(lon = new_sidoDf2$long, lat = new_sidoDf2$lat)

new_sidoDf2 <- cbind(new_sidoDf2, changedCoords)

sidoMap <- ggplot(data = new_sidoDf2,
                  mapping = aes(x = lonWGS84, 
                                y = latWGS84,
                                group = group
                  )) + 
  geom_polygon(fill = 'white',
               color = 'black') +
  my_theme +
  coord_map()

sidoMap


