#install.packages("xlsx")
library(xlsx)
library(dplyr)
library(reshape2)
#rm(list=ls()) 

d <- read.xlsx("./input_data/가구원수별 가구수(동별).xls",
               sheetIndex = 1,
               encoding = "UTF-8",
               stringsAsFactors = FALSE)

# colnames(d) <- d[1,]

# d <- d[-1,]

d_li <- data.frame(d)
d_li <- filter(d_li,
               자치구 =="관악구")
d_li$일반가구.7 <- ifelse(d_li$일반가구.7 == "X", 0, d_li$일반가구.7) 


for(var1 in 6:11){
  num <- var1 - 4
  for(var2 in 1:22){
    ch = as.integer(d_li[var2,var1])
    d_li[var2,var1] <- as.integer(round(ch / num))
  }
} 

tmp <- mutate(d_li,
              "가구수" = as.integer(일반가구.1) + as.integer(일반가구.2) + as.integer(일반가구.3) + as.integer(일반가구.4) +as.integer(일반가구.5) + as.integer(일반가구.6) + as.integer(일반가구.7))

result <- mutate(tmp,
               "1인가구 비중" = (as.integer(일반가구.1)/가구수) * 100)
result <- result[-1,]
View(result)

#################
## 컴플리트 
cctv_complete = read.xlsx("./input_data/cctv_complete.xlsx", sheetIndex = 1, encoding = "UTF-8", stringsAsFactors = F)

a_cctv_complete = cctv_complete %>% filter(cctv_complete["행정동명"] != "오류")

cctv_numbers <- a_cctv_complete %>% group_by(행정동명) %>% summarise("카메라개수합계" = sum(카메라대수))

h_df = inner_join(cctv_numbers, result, by = c("행정동명" = "동"))

cor(h_df["카메라개수합계"], h_df["1인가구 비중"])

###################
## 1인가구 비중 && 주택 거처별 상관관계계

cor(new_c_df2["RATIO"], result["1인가구 비중"])

#View(new_c_df2)
#View(result)


