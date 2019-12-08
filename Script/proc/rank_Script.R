library(xlsx)
library(devtools)
library(nnet)
library(base)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

################################################

sample <- read.csv('DB/data_combined.csv', header = F)
# sample <- sample[, 2:21]
head(sample)

# 컬럼
sample$사례수 <- c(3358, 5447, 789, 251, 1286, 588, 415, 160, 1046, 205, 135, 121, 106, 285, 252, 96, 336, 233, 484, 133, 741)

for(idx in 1:ncol(sample[, -21])) {
  sample[, idx] <- round((sample[, idx] / 100) * sample$사례수, 0)
}
sample 

sample$재방문 <- c('No', 'Yes', 'Yes', 'No', 'Yes', 'No', 'No', 'Yes', 'Yes', 'No', 'Yes', 'No', 'No', 'No', 'Yes', 'Yes', 'Yes', 'No', 'Yes', 'No', 'No')

sample$만족도 <- c('low', 'normal', 'low', 'normal', 'low', 'low', 'normal', 'high', 'high', 'high', 'high', 'normal', 'normal', 'high', 'normal', 'high', 'high', 'normal', 'normal', 'normal', 'normal')
# sample$만족도 <- ifelse(sample$만족도 == 'low', 1, ifelse(sample$만족도 == 'normal', 2, 3))

# write.csv(x = sample, file = "proc.csv") # 데이터 파일 출력
unique(sample$만족도)

#softMax
# type.ind <- class.ind(sample$만족도)
# sample <- cbind(sample, type.ind)
# 


# ncol(sample)

df <- read.csv("../DB/proc.csv", header = T)
str(df)

df
lastCol <- ncol(df)
idx <- sample(1:nrow(df), 0.7*nrow(df))

# 데이터 나누기
tr <- df[idx, ]
te <- df[-idx, ]

# 로지스틱 회귀
colnames(df)

fo <- 재방문 ~ .
model <- glm(formula = fo, data = tr, family = "binomial")
# glm 로지스틱 이기 때문에 바이노미얼
summary(model)
str(df)


# 정책 고도화 전략상 NA값의 컬럼은 정책 유도의 필요성이 없음 그러므로 휴향 휴식 부터 NA 컬럼은 제외 시킴
# Coefficients:
#   (Intercept)                       식도락_관광                              쇼핑  
# 7.1097                           -0.3388                           -3.2704  
# 자연경관_감상                          업무수행             고궁_역사_유적지_방문  
# -5.6302                           -1.9245                            1.8475  
# 공연_민속_행사_축제_관람_및_참가                      전통문화체험                 놀이공원_테마파크  
# 17.2758                           -7.1275                           15.5849  
# 유흥_오락                박물관_전시관.방문                    연수_교육_연구  
# 45.6010                           -9.9071                            7.9915  
# 스포츠_활동                              기타                         휴양_휴식  
# 16.3868                          -26.1053                                NA  
# 시티투어버스_이용                          뷰티관광           드라마_영화_촬영지_방문  
# NA                                NA                                NA  
# 의료관광                       레포츠_활동                              시찰  
# NA                                NA                                NA  
# 사례수                         만족도low                      만족도normal  
# NA                                NA                                NA  

# NA컬럼 자름
df_temp <- df[1:13]
df_temp <- cbind(df_temp, df["재방문"])

lastCol <- ncol(df_temp)
idx <- sample(1:nrow(df_temp), 0.7*nrow(df_temp))

# 데이터 나누기
tr <- df[idx, ]
te <- df[-idx, ]

# 로지스틱 회귀
colnames(df)

fo <- 재방문 ~ .
model <- glm(formula = fo, data = tr, family = "binomial")
# glm 로지스틱 이기 때문에 바이노미얼
summary(model)

# 예측 확률값 구하기.
pred <- predict(model, newdata=te, type = "response")

# 정확도 구하기
pr_result <- ifelse(pred>=0.5, 1, 0)
con_ta <- table(pr_result, te$재방문)
dim(con_ta)

# ROC 계수 구하기
# install.packages("ROCR")
library(ROCR)

pr <- prediction(pred, te$재방문)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main = "ROC Curve")

library(ggplot2)

df
ggplot(data = df, mapping = aes(x = 재방문, y = 사례수)) + geom_line()