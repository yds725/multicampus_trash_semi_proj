install.packages("xlsx")
library(xlsx)
library(dplyr)
library(reshape2)
rm(list=ls()) 

d <- read.xlsx(file.choose(),
               sheetIndex = 1,
               encoding = "UTF-8",
               stringsAsFactors = FALSE)

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
