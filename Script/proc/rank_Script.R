library(xlsx)

dir <- getwd()
dir_db <- "../Desktop/TeamJoel/DB"
# setwd("../")

## Var
enco <- "UTF-8"
######

# 경로 자동설정
if (dir != dir_db) {
  print("경로 재설정")
  setwd(dir_db)
} else {
  print("경로 정상")
}

# Data Input
df <- read.xlsx(file = "외국인관광객.xlsx", sheetIndex = 1, encoding = enco)
head(df, 24)
totalRow <-  floor(nrow(df) / 12)
yearTemp <- 2003

s <- 1
e <- 12

for (rowIdx in 1:totalRow) { # NA행에 년도 다 넣기
  for (idx in seq(s, e, 1)) {
    if (is.na(df$년[idx])) {
      df$년[idx] <- yearTemp
    } else {
      
    }
  }
  
  # temp <- e
  s <- e # index 스위치
  e <- e + 12 # 고정계수 12
  
  yearTemp <- yearTemp + 1 # 년도 더하기기
}

# 확인
colnames(df)
table(is.na(df))
View(df)



# # 데이터 중간 저장
# write.csv(x = val_Option, file = "../DB/rankTotal.csv", quote = F)
# 
# # 중간 데이터 read
# df <- read.csv(file = "rankTotal.csv", header = T, stringsAsFactors = F)
# df

# 