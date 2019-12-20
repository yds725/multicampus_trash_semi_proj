setwd(dir = "C:/R_EXER/semi_project/1인가구 관악구")

install.packages("xlsx")
library(xlsx)

library(dplyr)

# 구별 전체 1인가구 수 대비 거처종류별 비중.

housing_type_file <- read.xlsx("./input_data/housing_type.xls",
                               sheetIndex = 1,
                               encoding = "UTF-8",
                               header = T, 
                               stringsAsFactors = F)

colnames(housing_type_file) <- housing_type_file[1,]

housing_type_file <- housing_type_file[-1,]

housing_type_test <- housing_type_file %>%
  filter(자치구 != "합계") %>%
  filter(성별 == "계")


housing_type_test$계 <- as.numeric(housing_type_test$계)
housing_type_test$단독주택 <- as.numeric(housing_type_test$단독주택)
housing_type_test$아파트 <- as.numeric(housing_type_test$아파트)
housing_type_test$연립주택 <- as.numeric(housing_type_test$연립주택)
housing_type_test$다세대주택 <- as.numeric(housing_type_test$다세대주택)
housing_type_test$비거주용건물내주택 <- as.numeric(housing_type_test$비거주용건물내주택)

str(housing_type_test)

housing_type_df <- housing_type_test %>%
  mutate(housing_type_rate = ((단독주택 + 다세대주택 + 비거주용건물내주택) / 계) *100) %>%
  arrange(자치구, 기간) %>%
  select(기간, 자치구, housing_type_rate)

# 구별 1인가구 비중.

household_headcount_file <- read.xlsx("./input_data/household_headcount.xls",
                                      sheetIndex = 1,
                                      encoding = "UTF-8",
                                      header = T, 
                                      stringsAsFactors = F)

colnames(household_headcount_file) <- household_headcount_file[1,]

household_headcount_file <- household_headcount_file[-1,]

household_headcount_test <- household_headcount_file %>%
  filter(구분 != "합계")



household_headcount_test$계 <- as.numeric(household_headcount_test$계)

household_headcount_test$`1인가구` <- as.numeric(household_headcount_test$`1인가구`)

household_headcount_df <- household_headcount_test %>%
  mutate(solo_rate = `1인가구` * 100 / 계) %>%
  arrange(구분, 기간) %>%
  select(기간, 구분, solo_rate)
  
# join!

join_df <- inner_join(housing_type_df, household_headcount_df, 
                      by = c("기간","자치구"="구분"))

# correlation efficient

library(Hmisc)

cor.test(join_df$solo_rate, join_df$housing_type_rate)