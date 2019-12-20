install.packages("xlsx")
library(xlsx)

setwd(dir = "C:/R_EXER/semi_project/1인가구 관악구")

# 종량제 봉투 판매량 data load

trashbag_file <- read.xlsx("./input_data/trash_bag.xlsx",
                           sheetIndex = 1,
                           encoding = "UTF-8",
                           header = T, 
                           stringsAsFactors = F)

# colname 바꾸기

names(trashbag_file) <- gsub(x=names(trashbag_file),"\\..*","")

names(trashbag_file) <- paste(names(trashbag_file), trashbag_file[1, ], sep = "_")

# 첫 줄 없애기

trashbag_file <- trashbag_file[-1,]

# 구별 판매량 데이터만 얻어내기

trashbag_data <- trashbag_file %>%
  filter(`시도별_시도별(1)` != "서울특별시") %>%
  filter(`시도별_시도별(2)` == "가정용 봉투")

# 결측치 바꾸기

trashbag_data[trashbag_data=="-"]=0

# 숫자로 바꾸기
# 에러메시지 : Error: (list) object cannot be coerced to type 'double'

trashbag_data[4:22] <- as.numeric(unlist(trashbag_data[4:22]))

# 합계 column 추가

trashbag_data_sum <- trashbag_data %>%
  mutate(`2014` = X2014_2L + X2014_3L + X2014_5L) %>%
  mutate(`2015` = X2015_3L + X2015_5L) %>%
  mutate(`2016` = X2016_2L + X2016_3L + X2016_5L) %>%
  mutate(`2017` = X2017_2L + X2017_3L + X2017_5L) %>%
  select(`시도별_시도별(1)`, `2014`, `2015`, `2016`, `2017`)


# dataframe 구조 바꾸기


raw_trashbag_data_sum <- trashbag_data_sum

raw_trashbag_data_test <- melt(raw_trashbag_data_sum,
                               id.vars = "시도별_시도별(1)",
                               measure.vars = colnames(raw_trashbag_data_sum)[2:5],
                               variable.name = "기간",
                               value.name = "2L_3L_5L_합계")

# 전체 구 1인가구

household_headcount_file <- read.xlsx("./input_data/household_headcount.xls",
                                      sheetIndex = 1,
                                      encoding = "UTF-8",
                                      header = T, 
                                      stringsAsFactors = F)

colnames(household_headcount_file) <- household_headcount_file[1,]

household_headcount_file <- household_headcount_file[-1,]

household_headcount_df <- household_headcount_file %>%
  filter(구분 != "합계") %>%
  select(기간, 구분, `1인가구`)


# 상관관계
# 2015, 2016, 2017 뽑아내기

trashbag_df <- raw_trashbag_data_test %>%
  filter(기간 %in% c("2015", "2016", "2017"))

household_df <- household_headcount_df %>%
  filter(기간 %in% c("2015", "2016", "2017"))

# 1인가구 증가에 따른 2, 3,5L 판매량의 증가

library(Hmisc)

trashbag_df <- trashbag_df[c("기간", "시도별_시도별(1)", "2L_3L_5L_합계")]

library(reshape)

trashbag_df <- rename(trashbag_df,
                      구분 = "시도별_시도별(1)")

# 상관관계 계산

household_df[3] <- as.numeric(unlist(household_df[3]))

cor.test(household_df$`1인가구`, trashbag_df$`2L_3L_5L_합계`)