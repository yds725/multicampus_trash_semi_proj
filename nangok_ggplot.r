cctv_gwanak = read.csv(file = './input_data/서울특별시_관악구_CCTV_20191031.csv', 
                       stringsAsFactors = F)

# 이제 버전을 맞췄으니 필요한 package를
# github에서 다운받아 설치해 보아요!

install.packages("devtools")
install.packages("xlsx")
install.packages("rJava")

Sys.setenv(JAVA_HOME = 'C:\\Program Files\\Java\\jdk1.8.0_231')
library(rJava)

library(devtools)
library(dplyr)
library(xlsx)
library(ggmap)
#require(rJava)

# install.packages()  # CRAN에서 받아서 설치
install_github("dkahle/ggmap")

# 생성한 구글 API Key
googleAPIKey = "AIzaSyA0ORCD9qER6rcKXVIdNLTzN5z5h4hynaQ"

# 구글 API Key를 이용해서 인증을 받아요!
register_google(key=googleAPIKey)

# 37.481462, 126.932986
myLocation <- c(lon=126.93, lat=37.48)

gg_seowon <- get_googlemap(center = myLocation, zoom = 15, maptype = "roadmap")

ggmap(gg_seowon)

cctv_gwanak = read.csv(file = './input_data/서울특별시_관악구_CCTV_20191031.csv', stringsAsFactors = F)

cctv_gwanak

cctv_locations = cctv_gwanak %>% select("위도", "경도")
cctv_locations <- cctv_locations[c("경도", "위도")]

cctv_locations["ID"] <- seq.int(nrow(cctv_locations))

print(cctv_locations)

cctv_gwanak_2 = read.xlsx(file = './input_data/cctv_complete.xlsx', sheetIndex = 1, stringsAsFactors = F, encoding = "UTF-8")

cctv_dongs <- cctv_gwanak_2 %>% select("순번", "행정동명")

joined_df = inner_join(cctv_locations, cctv_dongs, by = c("ID" = "순번"))

new_joined_df <- joined_df %>% filter(행정동명 == "서원동") %>% select("경도", "위도")

gg_seowon <- get_googlemap(center = myLocation, zoom = 15, maptype = "roadmap", markers = new_joined_df )

ggmap(gg_seowon)
#new_joined_df %>% arrange()

arranged_df <- new_joined_df[ order( new_joined_df[,1], new_joined_df[,2] ),]

new_df_1 <- arranged_df[1:12,]
new_df_2 <- arranged_df[13:24,]
new_df_3 <- arranged_df[25:36,]
new_df_4 <- arranged_df[36:47,]
new_df_5 <- arranged_df[48:59,]
#print(arranged_df)

#print(new_df_1)
#rint(new_df_5)
avg_lon <- sapply(new_df_1, FUN = mean)
avg_lon_2 <- sapply(new_df_2, FUN = mean)
avg_lon_3 <- sapply(new_df_3, FUN = mean)
avg_lon_4 <- sapply(new_df_4, FUN = mean)
avg_lon_5 <- sapply(new_df_5, FUN = mean)

#avg_lon_5
#as.vector(unlist(avg_lon))

center_loc <- as.vector(unlist(avg_lon))
center_loc_2 <- as.vector(unlist(avg_lon_2))
center_loc_3 <- as.vector(unlist(avg_lon_3))
center_loc_4 <- as.vector(unlist(avg_lon_4))
center_loc_5 <- as.vector(unlist(avg_lon_5))
#print(avg_lon)

gg_nangok_new <- get_googlemap(center = center_loc, zoom = 17, maptype = "roadmap", markers = arranged_df)

gg_seowon_2 <- get_googlemap(center = center_loc_2, zoom = 17, maptype = "roadmap", markers = arranged_df)

gg_seowon_3 <- get_googlemap(center = center_loc_3, zoom = 17, maptype = "roadmap", markers = arranged_df)

gg_seowon_4 <- get_googlemap(center = center_loc_4, zoom = 17, maptype = "roadmap", markers = arranged_df)

gg_seowon_5 <- get_googlemap(center = center_loc_5, zoom = 17, maptype = "roadmap", markers = arranged_df)


ggmap(gg_nangok_new)
ggmap(gg_seowon_2)
ggmap(gg_seowon_3)
ggmap(gg_seowon_4)
ggmap(gg_seowon_5)



















