install.packages("ggplot2")
install.packages("rgdal")
install.packages("xlsx")
install.packages("dplyr")
install.packages("devtools")

# library(ggmap)
library(ggplot2)
# library(raster)
# library(rgeos)
# library(maptools)
library(rgdal)

library(xlsx)
#library(ggplot2)
library(dplyr)
library(rlang)
library(reshape2)
library(devtools)

#### 주택종류별 (동별) 엑셀

c_df = read.xlsx("./input_data/주택종류별 주택(동별).xls", sheetIndex = 1, encoding = "UTF-8", stringsAsFactors = F)

# 헤더 컬럼네임에 올려주기
colnames(c_df) <- c_df[2,]
c_df <- c_df[c(-1,-2),]

c_df <- rename(c_df,  "주택(계)" = "계")

# 
new_c_df <- c_df %>% filter(자치구 == "관악구") %>% select("자치구", "동", "합계",  "단독주택", "연립주택", "다세대주택", "비거주용건물내주택")

# 숫자로 바꾸기
cols.num <- c("합계",  "단독주택", "연립주택", "다세대주택", "비거주용건물내주택")


new_c_df[cols.num] <- sapply(new_c_df[cols.num], as.numeric)

# sapply(new_c_df, class)

#new_c_df[c("합계",  "단독주택", "연립주택", "다세대주택", "비거주용건물내주택")] <- as.numeric(new_c_df[c("합계",  "단독주택", "연립주택", "다세대주택", "비거주용건물내주택")])

# 라티오 단독주택
new_c_df <- new_c_df %>% mutate(SUMS = 단독주택 + 연립주택 + 다세대주택 +비거주용건물내주택) %>% mutate(RATIO = SUMS / 합계)

new_c_df["RATIO"] = lapply(new_c_df["RATIO"], round, 3) 

new_c_df2 <- rename(new_c_df, "dongNm" = "동")

new_c_df2 <- new_c_df2[-1,]


###########################################################
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

# 지도 틀만 그리기
# gwanakMap <- ggplot(data = new_gwanakDf,
#                   mapping = aes(x = long, 
#                                 y = lat,
#                                 group = group
#                   )) + 
#   geom_polygon(fill = 'white',
#                color = 'black') +
#   my_theme
# 
# gwanakMap

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


# 다른 좌표 기준ㅇ로 만든거?
# gwanakMap <- ggplot(data = new_gwanakDf2,
#                   mapping = aes(x = lonWGS84, 
#                                 y = latWGS84,
#                                 group = group
#                   )) + 
#   geom_polygon(fill = 'white',
#                color = 'black') +
#   my_theme +
#   coord_map()
# 
# gwanakMap

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

# gwanakMap <- ggplot(data = new_gwanakDf2,
#                     mapping = aes(x = long, 
#                                   y = lat,
#                                   group = group
#                     )) + 
#   geom_polygon(fill = 'white',
#                color = 'black') +
#   ggtitle(label = "관악구 동별 지도") +
#   my_theme +
#   coord_fixed()

# gwanakMap

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
  ggtitle(label = "관악구 동별 1인가구 거처 비중 분포도") +
  my_theme +
  coord_fixed() +
  scale_fill_gradient(low = "white", high = "red") +
  # 동이름
  geom_text(aes(x = naming_gwanakDf$center_long,
                y = naming_gwanakDf$center_lat),
            label = naming_gwanakDf$dongNm,
            size = 3)


###############################
# 서울특별시_관악구_CCTV .csv  동별로 cctv 설치 개수를 구하자

library(stringr)
# 우편번호 데이터
zipcode_rd = read.table(file = './input_data/서울특별시_zipcode.txt', 
                        sep = "|",
                        stringsAsFactors = F,
                        header = T,
                        colClasses = "character"
                        )

# cctv 관악 데이터
cctv_gwanak = read.csv(file = './input_data/서울특별시_관악구_CCTV_20191031.csv', stringsAsFactors = F)

# 우편번호 동 데이터터
zipcode_dong = read.table(file = './input_data/부가정보_서울특별시.txt',
                          sep = "|",
                          stringsAsFactors = F,
                          fill = T,
                          colClasses = "character")

new_zipcode_rd <- zipcode_rd %>% filter(시군구 == "관악구")

# 아 정규식 표현 짜증나
cctv_gwanak["소재지도로명주소"] = lapply(cctv_gwanak["소재지도로명주소"], gsub, pattern = "서울특별시 관악구 ", replacement = "", fixed = T)

test_cctv_gwanak <- cctv_gwanak

# 도로명문자열 처리를 해주자 
test_cctv_gwanak["소재지도로명주소"] = lapply(test_cctv_gwanak["소재지도로명주소"], gsub, pattern = "(동|길).*", replacement = "\\1")

test_cctv_gwanak["소재지도로명주소"] = lapply(test_cctv_gwanak["소재지도로명주소"], gsub, pattern = "\\s+", replacement = "")

# \b\w*동\b


small_cctv_gwanak <- test_cctv_gwanak %>% select("관리기관명", "소재지도로명주소", "설치목적구분", "카메라대수")

a_new_zipcode_rd <- new_zipcode_rd %>% select("도로명", "우편번호", "행정동명") %>% unique() %>% arrange(desc(행정동명))

# a_new_zipcode_rd <- a_new_zipcode_rd[order(a_new_zipcode_rd[, '도로명'], -a_new_zipcode_rd[,'우편번호']), ]

a_new_zipcode_rd_test <- a_new_zipcode_rd[!duplicated(a_new_zipcode_rd["도로명"]),]


# new_zipcode_rd %>% select("도로명", "우편번호", "행정동명") %>% unique()

merged_df = left_join(small_cctv_gwanak, a_new_zipcode_rd_test, by = c("소재지도로명주소" = "도로명"))

bb_merged_df <- merged_df

# 결측값을 맞는 행정동으로 만들어주자
bb_merged_df[,"행정동명"] <- ifelse(grepl("(동).*", bb_merged_df[,"소재지도로명주소"]), bb_merged_df[,"소재지도로명주소"], bb_merged_df[,"행정동명"] )
# ㅇㅁㄹㅇㅁㄴㅇㄹㄴㅁ

#bb_merged_df["행정동명"] = lapply(bb_merged_df["소재지도로명주소"], gsub, pattern = "\\s+", replacement = "")


#ifelse(grepl("(동).*", bb_merged_df["소재지도로명주소"]))

#b_new_zipcode_rd <- new_zipcode_rd %>% select("도로명", "행정동명")

# 복제 부분 없애기
#b_new_zipcode_rd <- b_new_zipcode_rd[!duplicated(b_new_zipcode_rd["도로명"]),]

#new_zipcode_dong <- zipcode_dong %>% select("V3", "V4") %>% unique()

#new_merged_df <- left_join(merged_df, b_new_zipcode_rd, by = c("소재지도로명주소" = "도로명"))

# merged_df <- merge(cctv_gwanak, a_new_zipcode_rd, by.x = "소재지도로명주소", by.y = "도로명")

#inner_join()

bb_merged_df %>% filter(행정동명 == "")
str(bb_merged_df %>% filter(is.na(행정동명)))

twonine = "동광로49-31"
eightnine = "동마길9"

bb_merged_df[289, ]$소재지도로명주소 <- twonine
bb_merged_df[889,]$소재지도로명주소 <- eightnine

# 문자열 처리를 하고 최대한 행정동을 찾앗지만 아예 안나온느 부분은 수동으로 찾아야함
write.csv(bb_merged_df, file = './output_data/cctv_dong.csv')


### 그렇게 수동으로 완벽히 바꾼걸 다시 읽어주고 동별 카메라 개수파악
cctv_complete = read.xlsx("./input_data/cctv_complete.xlsx", sheetIndex = 1, encoding = "UTF-8", stringsAsFactors = F)

a_cctv_complete = cctv_complete %>% filter(cctv_complete["행정동명"] != "오류")


cctv_numbers <- a_cctv_complete %>% group_by(행정동명) %>% summarise("카메라개수합계" = sum(카메라대수))

cctv_numbers 

new_gwanakDf3 <- merge(x = new_gwanakDf3,
                       y = cctv_numbers[,c(1,2)],
                       by.x = 'dongNm',
                       by.y = '행정동명',
                       all.x = T)

# new_gwanakDf[is.na(new_gwanakDf$id),]
library(RColorBrewer)

myPal <- brewer.pal(n = 9, name = 'Reds')

ggplot(data = new_gwanakDf3,
       mapping = aes(x = long, 
                     y = lat,
                     group = group
       )) + 
  geom_polygon(mapping = aes(fill = 카메라개수합계, group = dongNm),
               color = 'black') +
  ggtitle(label = "관악구 동별 카메라 개수 분포도") +
  my_theme +
  coord_fixed() +
  scale_fill_gradient(low = "white", high = "blue") +
  # 동이름
  geom_text(aes(x = naming_gwanakDf$center_long,
                y = naming_gwanakDf$center_lat),
            label = naming_gwanakDf$dongNm,
            size = 3)


