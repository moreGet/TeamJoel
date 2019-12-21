# install.packages("progress")
library(progress)

# source(file = "Script/proc/rank_Script.R", encoding = "UTF-8")
# 
# # 로지스틱 회귀 : rank_Script.R
# plot(prf, main = "ROC Curve")

# 전반적인 상황 확보
# 기간별 주요 참여 활동(중복응답) 꺾은선 그래프
df <- read.csv(file = "DB/data_combined.csv", header = T, stringsAsFactors = F)

MainAct <- read.csv("DB/고려요인.csv", stringsAsFactors = F, header = T)
colnames(MainAct)
str(MainAct)

# ggplot을 위해 년도만 가져옴
library(dplyr)
end_Idx <- length(MainAct$기간)

# 년도 그룹
MainAct

library(ggplot2)
library(reshape2)
# melt_MainAct <- melt(data = MainAct, id = c("기간"))

# ggplot로 그리기
y <- 2014
yTemp <- 0

# ggplot 4년치
for (idx in y:(y+4)) {
  yTemp <- MainAct %>% filter(substr(기간, 1, 4) == idx)
  yTemp$기간 <- substr(yTemp$기간, 5, 6)  
  melt_MainAct <- melt(data = yTemp, id = c("기간"))
  
  print(
    ggplot(data = melt_MainAct, aes(x = 기간 , y = value, group = variable, color = variable)) + 
      geom_line(size = 1.5) + 
      labs(title = paste(idx, "재방문 시 고려요인(중복)")))
}

############################################################
############################################################
############################################################
############################################################

# 주요 관광수요 산업 컬럼을 구분하기 위해 카이제곱을 실시 해야함
# 이를 위해 범주형 데이터가 필요함. 이는 만족도 범주형을 가지고옴

df_Temp <- read.csv("DB/만족도.csv", header = T, stringsAsFactors = T)
length(df_Temp)

# 범주형 데이터 합치기
df_Temp
str(MainAct)

MuxDf <- cbind(MainAct[-1], df_Temp)
MuxDf

# 전반 적인 만족도와 나머지 데이터간의 관련성 여부
# 귀무 가설(H0) : 재방문시 고려요인 과 전반적인 만족도 걸럼과 관계가 있다.
# 연구 가설(H1) : 재방문시 고려요인 과 전반적인 만족도 걸럼과 관계가 없다.
# install.packages('gmodels')
library(gmodels)

end <- 0
end <- length(MainAct[-1]) # 컬럼 갯수 만큼 카이검정
chiList <- 0

for (idx in 1:end) {
  ta <- CrossTable(unlist(MuxDf[idx]), MuxDf$만족도, chisq = T)
  chiList[idx] <- ta$chisq$p.value
}

df_oneYear <- MuxDf[49:60,1:end]
CrossTable(unlist(df_oneYear$기후_뚜렷한_사계절), MuxDf$만족도[49:60], chisq = T)
# 데이터 정렬 및 검증
length(chiList)
rowName <- colnames(MainAct[-1])
chi_df <- data.frame(rowName=rowName, p_Value=chiList)
chi_df <- chi_df %>% arrange(desc(p_Value))
chi_df

# 카이제곱 검정을 실시 하였고 재방문 고려요인과 만족도 간의 관계가 증명 되었다.
# 민감도 대한민국 타겟팅으로 된 국제사회 이슈 에 대해 가장 영향을 받지 않는 수치는 P-value가 높을수록 좋다.
# rowName   p_Value
# 1          기후_뚜렷한_사계절 0.7294646
# 2        숙박시설_편리한.교통 0.6443329
# 3                    휴양휴식 0.6092449
# 4               이미용_서비스 0.5931206
# 5           경제적인_여행비용 0.5622067
# 6                   유흥_오락 0.5573205
# 7                    뷰티관광 0.5084509
# 8       K.POP_한류스타_팬미팅 0.4700696
# 9                      쇼핑.1 0.4313990
# 10 패션_유행_.등_.세련된_문화 0.4200565
# 11              유흥_놀이시설 0.4049757
# 12               자연풍경감상 0.3984646
# 13              음식_미식탐방 0.3710314
# 14              역사_문화유적 0.3234574
 
# 이후 내림차순으로 P-Value를 정렬하여 이를 통해 잠정적인 상승이 이루어진 요인을 추출함.
# P-Value 값이 가장 큰 요인을 가지고 안정적인 상승세를 이룬 주요 산업을 가지고 정책을 고민 해봄

############################################################
############################################################
############################################################
############################################################

# Val 설명
# MainAct : 고려요인
# MuxDf : 고려요인 x 만족도
reDf <- read.csv("DB/재방문횟수.csv", header = T, stringsAsFactors = F)
reDf

endIdx <- length(reDf)
model <- list()

# 재방문 횟수 별 컬럼 요인
# 포뮬러 미동작
# for (idx in 1:endIdx) {
#   tempDf <- data.frame()
#   tempDf <- cbind(MainAct[-1] ,reDf[idx])
#   name <- colnames(reDf[idx])
#   fo <- name ~ .
#   model_temp <- lm(formula = fo, data = tempDf)
#   model <- c(model, model_temp)
# }

df01 <- cbind(MainAct[-1] ,reDf[1])
df02 <- cbind(MainAct[-1] ,reDf[2])
df03 <- cbind(MainAct[-1] ,reDf[3])
df04 <- cbind(MainAct[-1] ,reDf[4])

modelX1 <- lm(formula = X1회 ~ ., data = df01)
modelX2 <- lm(formula = X2회 ~ ., data = df02)
modelX3 <- lm(formula = X3회 ~ ., data = df03)
modelX4 <- lm(formula = X4회이상 ~ ., data = df04)

modelX1$coefficients
modelX2$coefficients
modelX3$coefficients
modelX4$coefficients

# > modelX1$coefficients
# (Intercept)                   휴양휴식 
# -15.12584903                -0.30769897 
# 유흥_오락                   뷰티관광 
# 0.57903869                 1.72454818 
# 쇼핑.1              음식_미식탐방 
# 0.61130324                -0.30517035 
# 자연풍경감상              역사_문화유적 
# 0.32429982                -0.29787166 
# 패션_유행_.등_.세련된_문화      K.POP_한류스타_팬미팅 
# 0.22488922                -0.09528488 
# 경제적인_여행비용              유흥_놀이시설 
# 0.02799473                 0.27884229 
# 숙박시설_편리한.교통              이미용_서비스 
# -1.92615428                -1.09113882 
# 기후_뚜렷한_사계절 
# -0.21042785 

# > modelX2$coefficients
# (Intercept)                   휴양휴식 
# -38.464478221                0.179250870 
# 유흥_오락                   뷰티관광 
# -0.097617381                0.063342873 
# 쇼핑.1              음식_미식탐방 
# 0.108931551                0.115660654 
# 자연풍경감상              역사_문화유적 
# -0.064091918                0.016847522 
# 패션_유행_.등_.세련된_문화      K.POP_한류스타_팬미팅 
# -0.009930368                0.040898039 
# 경제적인_여행비용              유흥_놀이시설 
# -0.072794050                0.260049767 
# 숙박시설_편리한.교통              이미용_서비스 
# 0.334763370                0.063524408 
# 기후_뚜렷한_사계절 
# 0.036686316 

# > modelX3$coefficients
# (Intercept)                   휴양휴식 
# -14.672365791                0.097016962 
# 유흥_오락                   뷰티관광 
# 0.036435422                0.151887691 
# 쇼핑.1              음식_미식탐방 
# 0.040072251                0.044663141 
# 자연풍경감상              역사_문화유적 
# -0.024301653                0.038909315 
# 패션_유행_.등_.세련된_문화      K.POP_한류스타_팬미팅 
# 0.002208102               -0.048826780 
# 경제적인_여행비용              유흥_놀이시설 
# -0.127192381                0.088460702 
# 숙박시설_편리한.교통              이미용_서비스 
# 0.482939735               -0.075684852 
# 기후_뚜렷한_사계절 
# -0.043230518 

# > modelX4$coefficients
# (Intercept)                   휴양휴식 
# -37.63044373                 0.33519511 
# 유흥_오락                   뷰티관광 
# 0.10940390                 0.67865665 
# 쇼핑.1              음식_미식탐방 
# -0.02611421                 0.19347051 
# 자연풍경감상              역사_문화유적 
# -0.02939098                 0.19699802 
# 패션_유행_.등_.세련된_문화      K.POP_한류스타_팬미팅 
# -0.07031369                 0.04496613 
# 경제적인_여행비용              유흥_놀이시설 
# -0.04198566                -0.12206319 
# 숙박시설_편리한.교통              이미용_서비스 
# 0.85681595                -0.23551648 
# 기후_뚜렷한_사계절 
# 0.16685777 

# 4년간 재방문을 하게 만든 고려요인
# 1회 방문시 고려요인 뷰티 관광
# 2회 방문시 고려요인 숙박시설_편리한.교통
# 3회 방문시 고려요인 숙박시설_편리한.교통
# 4회 방문시 고려요인 숙박시설_편리한.교통

# 방한 방문객은 만족과 상관없이 방문 횟수가 늘면 늘수록 대한민국의 교통및 숙박시설
# 에 강한 만족감을 나타내고 있다.

# 휴양휴식 과 숙박시설 간의 루트를 정부차원에서 지원하여 개발해야한다.

# 정책 개편 방안 주제 Top 5 요인
# 숙박시설 및 편리한 교통
# - 휴양휴식
# - 유흥_오락
# - 뷰티 관광
# - 쇼핑
# - 음식_미식탐방
# 이며 다른 결과와 취합해 상세 정책및 개편안 에 대한 고민을 할 수 있 을 것 같다.

############################################################
############################################################
############################################################
############################################################

# 비율검정
joinAct <- read.csv("DB/주요참여활동.csv", header = T)
MainAct #고려요인
revisit <- read.csv("DB/재방문확률.CSV", header = T)

# 데이터 검정
# 행이 같음.
dim(joinAct) # 60/8 주요참여 활동
dim(MainAct) # 60/15 고려요인
dim(revisit) # 재방문 확률

# 검정 하기에 앞서 결측치, 이상치 제거를 한다.
# All False 인자값 이므로 결측치는 존재하지 않는다.
table(is.na(joinAct))
table(is.na(MainAct))
table(is.na(revisit))

# 검정통계량 으로 빈도 분석.
summary(joinAct)
summary(MainAct)
summary(revisit)

#      집단  검정 대상 관련 함수  동질성 검정  정규 분포 검정 
# 
# 단일 집단  비율      binom.test()        -              - 
#   
# 단일 집단  평균      t.test()            -   shapiro.test() 
# 
# 두 집단    비율      prop.test()         -              - 
#   
# 두 집단    평균      t.test()    var.test()             - 
#   
# 세 집단    비율      prop.test()         -              - 
#   
# 세 집단    평균      aov()       bartlett.test()        - 

# 단일 집단의 방문 의사 검정
# 4년치 데이터의 방문의사 를 베이스로 집단간 차이 검정 실시

# binomTest() 하기
# 날짜 지우기
joinAct_DelCol <- joinAct[-1]
MainAct_DelCol <- MainAct[-1]
revisitAct_DelCol <- revisit[-1]

# 기간         재방문  
# Min.   :201401   낮음:29  
# 1st Qu.:201504   높음:31  
# Median :201607            
# Mean   :201607            
# 3rd Qu.:201709            
# Max.   :201812  

# 연구가설 : 4년간 방한 외국인의 재방문 의사는 높다.
# 귀무가설 : 4년간 방한 외국인의 재방문 의사는 높지 않다.
bio <- binom.test(c(31, 29), p = 0.8)
# Exact binomial test
#
# data:  c(31, 29)
# number of successes = 31, number of trials = 60, p-value = 8.146e-07
# alternative hypothesis: true probability of success is not equal to 0.8
# 95 percent confidence interval:
#   0.3839460 0.6476871
# sample estimates:
#   probability of success 
# 0.5166667 

# FALSE 이므로 귀무가설 기각
bio$p.value > 0.05

# 단일집단 평균 검정
joinAct_DelCol # 60/8 주요참여 활동
MainAct_DelCol # 60/15 고려요인
revisitAct_DelCol <- revisitAct_DelCol %>% 
  mutate(bin = ifelse(재방문=="높음", 1, 0)) %>% 
  mutate(bool = ifelse(재방문=="높음", TRUE, FALSE))
revisitAct_DelCol # 이진값

# write.csv(x = revisitAct_DelCol, file = "DB/재방문확률(2진).csv")
# 단일 집단 평균 검정에 앞서 재방문 확률이 높게된 컬럼에 대해 카이제곱 검정 실시
end <- 0
end <- length(MainAct[-1]) # 컬럼 갯수 만큼 카이검정
chiList_revisit <- 0

# 데이터프레임 만듦
MuxDf_revisit <- cbind(MainAct[-1], revisitAct_DelCol[1])

for (idx in 1:end) {
  ta <- CrossTable(unlist(MuxDf_revisit[idx]), MuxDf_revisit$재방문, chisq = T)
  chiList_revisit[idx] <- ta$chisq$p.value
}

# 카이제곱 검정 실시
chiList_revisit <- as.data.frame(chiList_revisit)
chiList_revisit <- chiList_revisit %>% arrange(desc(chiList_revisit))
row.names(chiList_revisit) <- colnames(MainAct[-1])
chiList_revisit

# 데이터 정렬 및 검증
length(chiList)
chi_df <- data.frame(p_Value=chiList)
chi_df <- chi_df %>% 
  mutate(result = ifelse(p_Value>=0.05, "채택", "기각")) %>% 
  arrange(desc(p_Value))

row.names(chi_df) <- colnames(MainAct[-1])
chi_df

# 단일집단 평균 t.test() 검증 이유 : 4년간 년도별 재방문 의사 긍정적표현의 변화
# install.packages(c("Hmisc", "prettyR"))
library(Hmisc)
library(prettyR)

# 기술 통계량
describe(revisitAct_DelCol$bin)
# Numeric 
# mean median  var  sd valid.n
# x 0.52      1 0.25 0.5      60

# 데이터 프레임 만들기(년도별 평균 검증 을 위해)
revisit_bin_df <- data.frame(x2014 = revisitAct_DelCol$bin[1:12], 
                             x2015 = revisitAct_DelCol$bin[13:24],
                             x2016 = revisitAct_DelCol$bin[25:36],
                             x2017 = revisitAct_DelCol$bin[37:48],
                             x2018 = revisitAct_DelCol$bin[49:60])
revisit_bin_df

# 정규분포
for (idx in 1:length(revisit_bin_df)) {
  vec <- unlist(revisit_bin_df[idx])
  sh <- shapiro.test(x = vec)
  print(sh$p.value > 0.05)
  print("------------------")
  print(describe(vec))
}

# 정규 분포가 아니기 때문에 wilcox 검정 실시.
for (idx in 1:length(revisit_bin_df)) {
  w <- wilcox.test(x = unlist(revisit_bin_df[idx]), mu = 0.52, alternative = "two.sided")
  print(w$p.value > 0.05)
}

# 귀무가설 채택 : 외국인 방한 의사가 매년 바뀌지 않고 52% 이상이 긍정적 이다.

############################################################
############################################################
############################################################
############################################################

# 집단별 비율차이 검증
# 년도별 재방문 의사표현 의 차이 검정

# prop.test 시행
# 연구 : 외국인 방한 의사가 매년 일정하지 않다.
# 귀무 : 외국인 방한 의사가 매년 일정하다.
for (idx in 1:length(revisit_bin_df)) {
  
  ta <- table(unlist(revisit_bin_df[idx]))
  prop_ta <- prop.table(ta)
  
  cat(colnames(revisit_bin_df[idx]))
  print(ta)
  cat("\n")
}

# prop.test()
ptt <- prop.test(x = c(5, 6, 6, 6, 6), n = c(12, 12, 12, 12, 12))
# 5-sample test for equality of proportions without continuity correction
# 
# data:  c(5, 6, 6, 6, 6) out of c(12, 12, 12, 12, 12)
# X-squared = 0.26696, df = 4, p-value = 0.9918
# alternative hypothesis: two.sided
# sample estimates:
#   prop 1    prop 2    prop 3    prop 4    prop 5 
# 0.4166667 0.5000000 0.5000000 0.5000000 0.5000000 
ptt$p.value > 0.05
# [1] TRUE
# 귀무가설 채택

############################################################
############################################################
############################################################
############################################################

# 두 집단 평균 검정
de <- describe(revisit_bin_df) # 기술 통계량
de

for (idx in 1:4) {
  x <- unlist(revisit_bin_df[idx])
  y <- unlist(revisit_bin_df[idx+1])
  v <- var.test(x, y)  
  print(v)
}

# F test to compare two variances
# 
# data:  x and y
# F = 0.97222, num df = 11, denom df = 11, p-value = 0.9636
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.279881 3.377207
# sample estimates:
#   ratio of variances 
# 0.9722222 
# 
# 
# F test to compare two variances
# 
# data:  x and y
# F = 1, num df = 11, denom df = 11, p-value = 1
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.2878776 3.4736991
# sample estimates:
#   ratio of variances 
# 1 
# 
# 
# F test to compare two variances
# 
# data:  x and y
# F = 1, num df = 11, denom df = 11, p-value = 1
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.2878776 3.4736991
# sample estimates:
#   ratio of variances 
# 1 
# 
# 
# F test to compare two variances
# 
# data:  x and y
# F = 1, num df = 11, denom df = 11, p-value = 1
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.2878776 3.4736991
# sample estimates:
#   ratio of variances 
# 1 

# 14 ~ 18년도 까지의 집단의 데이터는 서로 동질 하다.
for (idx in 1:4) {
  x <- unlist(revisit_bin_df[idx])
  y <- unlist(revisit_bin_df[idx+1])
  tValue <- t.test(x, y, alternative = "greater")  
  print(tValue)
}
# 14, 15년도가 14년도 가 15년도보다 방문의사 긍정표현 수치가 조금더 작다.
# 나머지 15년도 ~ 18년도 까지는 재방문 의사 긍정표현 수치가 일정하다.

############################################################
############################################################
############################################################
############################################################

# 상관관계 cor
# install.packages(c("corrgram", "moments"))
# install.packages("corrplot")
library(moments)
library(corrgram)
library(corrplot)

MainAct
joinAct
revisitAct_DelCol

# 방한 고려요인 상관관계도
# 뷰티관광 과 휴양휴식 이 가장 관련이 있음.
corrgram(MainAct[-1], upper.panel = panel.conf)
# 상관관계 그래프
par(mfrow=c(1, 1))
myCorr <- cor(MainAct[-1])
corrplot(myCorr, method = "ellipse", addCoef.col = "yellow", type = "upper")

# 주요 참여활동 상관관계도
# 자연관광 과 역사적 방문 간의 요소가 가장 관련
corrgram(joinAct[-1], upper.panel = panel.conf)
# 상관관계 그래프
par(mfrow=c(1, 1))
myCorr <- cor(joinAct[-1])
corrplot(myCorr, method = "ellipse", addCoef.col = "yellow", type = "upper")

# 방한의사 긍정 표현빈도 상관관계도
corrgram(revisit_bin_df) # 15, 17년도가 가장 비슷한 표현빈도
# 15, 17년도 가 무슨 관계가 있는지?
# 상관관계 그래프
par(mfrow=c(1, 1))
myCorr <- cor(revisit_bin_df)
corrplot(myCorr, method = "ellipse", addCoef.col = "yellow", type = "upper")

# 히스토그램 및 비대칭도 통계량
skwe_kurt_df <- data.frame(labels = c("왜도", "첨도")) # 각 행마다 왜도 첨도를 담고 있는 데이터 프레임
par(mfrow = c(2, 2))

################################# 고려요인 통계량 그래프
for (idx in 2:length(MainAct)) {
  main <- unlist(MainAct[idx])
  title <- colnames(MainAct[idx])
  skwe <- skewness(main) # 왜도
  kurt <- kurtosis(main) # 첨도
  
  skwe_kurt_df <- cbind(skwe_kurt_df, c(skwe, kurt))
  hist(x = main, freq = F, main = title)
  
  # 밀도 분포 곡선
  lines(density(main), col = "blue")
  curve(dnorm(x, mean(main), sd(main)), col="red", add = T)
}

# 고려요인 왜도 첨도
# 왜도 첨도 모음 
colnames(skwe_kurt_df) <- c("labels", colnames(MainAct[-1]))
skwe_kurt_df

################################# 주요 참여활동 통계량 그래프 
par(mfrow = c(2, 2))
skwe_kurt_df_join <- data.frame(labels = c("왜도", "첨도")) # 각 행마다 왜도 첨도를 담고 있는 데이터 프레임

for (idx in 2:length(joinAct)) {
  main <- unlist(joinAct[idx])
  title <- colnames(joinAct[idx])
  skwe <- skewness(main) # 왜도
  kurt <- kurtosis(main) # 첨도
  
  skwe_kurt_df_join <- cbind(skwe_kurt_df_join, c(skwe, kurt))
  hist(x = main, freq = F, main = title)
  
  # 밀도 분포 곡선
  lines(density(main), col = "blue")
  curve(dnorm(x, mean(main), sd(main)), col="red", add = T)
}

# 고려요인 왜도 첨도
# 왜도 첨도 모음 
colnames(skwe_kurt_df_join) <- c("labels", colnames(joinAct[-1]))
skwe_kurt_df_join

############################################################
############################################################
############################################################
############################################################

# 요인분석
par(new=T)

MainAct # 방한 외국인 고려요인
joinAct # 방한 외국인 주요활동 만족요소
revisitAct_DelCol # 방한 외국인 재방문 의사 긍정 표현

##################################### 주성분 상세 정보는 꼭 Ctrl + Enter 하세요.
# 방한 외국인 고려요인 주성분 분석
pcMain <- prcomp(MainAct[-1])
pcMain

df_row <- colnames(MainAct[-1])
pcMain_df <- data.frame(title_df = df_row, Standard_deviations = pcMain$sdev)
# Standard deviations (1, .., p=7):
#   [1] 412.68847  79.92565  48.11241  37.35248  27.94766  22.79224  17.03248
# 방한 외국인 주요활동 만족요소 중 쇼핑이 가장 큰 영향을 줌
ggplot(data = pcMain_df, aes(x = title_df, y = Standard_deviations, col = title_df, fill = title_df)) + geom_bar(stat = "identity") + labs(title = "방한 외국인 고려요인 중 가장 영향력 있는 요인")

# Standard deviations (1, .., p=14):
#   [1] 225.088410 121.262355  74.516223  41.442527  35.727341  32.717229  31.736348  23.294740  21.255884  20.156718  18.096378  11.492287
# [13]   6.946037   5.592728
# 방한 외국인 고려요인 중 휴양휴식이 다른 요소에 가장 많은 영향을 줌

########################################################################

# 방한 외국인 주요활동 만족요소 주성분 분석
pcJoin<- prcomp(joinAct[-1])
pcJoin

df_row <- colnames(joinAct[-1])
pcJoin_df <- data.frame(title_df = df_row, Standard_deviations = pcJoin$sdev)
# Standard deviations (1, .., p=7):
#   [1] 412.68847  79.92565  48.11241  37.35248  27.94766  22.79224  17.03248
# 방한 외국인 주요활동 만족요소 중 쇼핑이 가장 큰 영향을 줌
ggplot(data = pcJoin_df, aes(x = title_df, y = Standard_deviations, col = title_df, fill = title_df)) + geom_bar(stat = "identity") + labs(title = "방한 외국인 주요활동 만족 요소 중 가장 영향력 있는 요인")

############################################################
############################################################
############################################################
############################################################

# install.packages("party")
library(party)

# 의사결정 트리
MainAct # 방한 외국인 고려요인
joinAct # 방한 외국인 주요활동 만족요소
revisitAct_DelCol # 방한 외국인 재방문 의사 긍정 표현

# ctree를 위한 데이터 프레임 통합
tree_Main <- cbind(MainAct[-1], revisitAct_DelCol[2])
tree_Join <- cbind(joinAct[-1], revisitAct_DelCol[2])

# 포뮬러 작성
# 외국인 재방문 긍정의사 를 컬럼 통합
# 의사결정 트리를 각 요인분석 에서 1등한 값을 종속으로 두고 실시함
main_Fo <- 휴양휴식 ~ .
join_Fo <- 쇼핑 ~ .

# ctree Model 생성
# main_Model <- ctree(formula = main_Fo, data = tree_Main)
# join_Model <- ctree(formula = join_Fo, data = tree_Join)
# main_Model <- rpart(formula = main_Fo, data = tree_Main, method = "class")
# join_Model <- rpart(formula = join_Fo, data = tree_Join, method = "class")

# 데이터 분리 패키지 caret
# install.packages(c("rattle", "rpart.plot", "caret"))
library(rattle)
library(rpart.plot)
library(caret)

# 시계열 분석을 위한 패키지 설치
# install.packages(c("tseries", "forecast", "TTR"))
library(tseries)
library(forecast)
library(TTR)

# set.seed(1234)
# idx_Number <- createDataPartition(y = tree_Main$bin, p = 0.7, list = FALSE)
insertZero <- function( val ) {
  
  idx <- length(val)
  
  for (i in 1:idx) {
    val[i] <- 0
  }
  
  # val <- data.frame(val)
  return(val)
}

# main_te <- insertZero(MainAct[-1]) # 시험지 만들기
# join_te <- insertZero(joinAct[-1]) # 시험지 만들기

# # ctree Plot
# main <- ctree_control(maxdepth = 20) # 고려요인 의사결정 트리 깊이 20
# join <- ctree_control(maxdepth = 10) # 주요활동 만족요소 트리 깊이 10
# 
# tree_Main <- ctree(formula = main_Fo, data = MainAct[-1], controls = main)
# plot(tree_Main, main = "고려요인", compress = TRUE)
# 
# tree_Join <- ctree(formula = join_Fo, data = joinAct[-1], controls = join)
# plot(tree_Join, main = "주요활동 만족요소", compress = TRUE)

# 예측값
# install.packages("cvTools")
# library(cvTools)
# library(devtools)
# prd_Main <- predict(tree_Main, main_te)
# prd_Join <- predict(tree_Join, join_te)

# 시계열 분석
par(mfrow = c(1, 2))

fu_Main.ts <- ts(MainAct$휴양휴식, frequency = 12, c(2014, 1)) # ts모델 생성
de <- decompose(fu_Main.ts) # 계절요인, 순환 요인, 추세 요인, 불규칙 요인 으로 나누기
plot(de)

# 이동평균
main_sma12 <- SMA(fu_Main.ts, n = 3)
main_sma24 <- SMA(fu_Main.ts, n = 6)
main_sma36 <- SMA(fu_Main.ts, n = 9)

par(mfrow = c(2,2))

plot.ts(fu_Main.ts)
plot.ts(main_sma12)
plot.ts(main_sma24)
plot.ts(main_sma36)

# 차분을 통해 데이터 정상화
main_diff1 <- diff(fu_Main.ts, differences = 1)
main_diff2 <- diff(fu_Main.ts, differences = 2)
main_diff3 <- diff(fu_Main.ts, differences = 3)

plot.ts(fu_Main.ts)
plot.ts(main_diff1)    # 1차분 정상화
plot.ts(main_diff2)    # 2, 3차분 이 2차분에서 정상화를 보이므로 2차분
plot.ts(main_diff3)
# ------------------------

# 2차분된 데이터의 ARIMA 모형 확인
# 수동 ARIMA 값 ARIMA(3,1,1) 
par(mfrow = c(1, 2))
acf(main_diff2, lag.max = 20) # 절단값(점선밖 세로막대 로부터 최초 점선 안진입 막대) -> MA(0)
pacf(main_diff2, lag.max = 20) # 절단값(점선밖 세로막대 로부터 최초 점선 안진입 막대) -> AR(0)

# 자동 ARIMA 값 ARIMA(0,1,1)
auto.arima(MainAct$휴양휴식)

# ARIMA 모델을 이용한 예측
auto.Main.arima <- arima(fu_Main.ts, order=c(0, 1, 1)) # order 로 보정 자동 값
Main.arima <- arima(fu_Main.ts, order=c(0, 1, 0)) # 수동 값

# 보정 한후 forecast() 사용 h = 예측 할 범위(개월)
auto.main_fcast <- forecast(auto.Main.arima, h = 1)
main_fcast <- forecast(Main.arima, h = 1)

# 차트 출력
par(mfrow = c(1, 2))
plot(auto.main_fcast)
plot(main_fcast)

############################################################
############################################################
############################################################
############################################################

library(cluster)
# 군집분석
# hclust이용을 위해 데이터 프레임 메트릭스 화
mat_Main <- t(tree_Main)
mat_Join <- t(tree_Join)
mat_Join <- mat_Join[-nrow(mat_Join), ] # 바이너리값 삭제

# 계층적 군집 분석을 위해 클러스트링을 수행 해야 한다.
# 그에 사전 작업을 위해 유클리디언 거리 생성 함수를 이용한다.
main_Dist <- dist(mat_Main)
join_Dist <- dist(mat_Join)

# 클러스터링
main_Clust <- hclust(main_Dist, method = "single")
# Call:
#   hclust(d = main_Dist, method = "single")
# 
# Cluster method   : single 
# Distance         : euclidean 
# Number of objects: 15 

join_Clust <- hclust(join_Dist, method = "single")
# Call:
#   hclust(d = join_Dist, method = "single")
# 
# Cluster method   : single 
# Distance         : euclidean 
# Number of objects: 8 

# 물리적으로  비슷한 요인 을 묶어준다.
# 유사도에 근거하여 군집들을 분석 하는데, 유사성은 유클리디언 거리를 사용한다.
par(mfrow = c(1, 1)) # 차트 필드

##############################################
plot(main_Clust, main = "방한 외국인 고려요인")
main_K = 3
rect.hclust(main_Clust, k = main_K, border = rainbow(main_K))

##############################################
plot(join_Clust, main = "방한 외국인 주요활동 만족요소")
join_K = 3
rect.hclust(join_Clust, k = join_K, border = rainbow(join_K))

# 군집 plot으로 가시화
mds <- cmdscale(main_Dist)
plot(mds, type='p', main = "방한 외국인 고려요인 군집", col = "blue")
text(mds, rownames(mat_Main), adj = 1.10)

mds <- cmdscale(join_Dist)
plot(mds, type='p', main = "방한 외국인 주요활동 만족요소 군집", col = "blue")
text(mds, rownames(mat_Join), adj = 1.10)

############################################################
############################################################
############################################################
############################################################

# 연관 규칙을 찾을 수 없어 분석이 불가능.
# install.packages("arulesViz")
# install.packages("arules")
# library(arulesViz)
# library(arules)
# 
# # write.csv(x = MainAct[-1], file = "mainAct_Tran.csv")
# # write.csv(x = joinAct[-1], file = "joinAct_Tran.csv")
# 
# # 연관분석
# tran_Main <- read.transactions(file = "mainAct_Tran.csv", format = "basket")
# tran_Join <- read.transactions(file = "joinAct_Tran.csv", format = "basket")
# 
# rule_Main <- apriori(tran_Main, parameter = list(supp=0.01, conf=0.1))
# rule_Join <- apriori(tran_Join, parameter = list(supp=0.01, conf=0.1))
# 
# plot(rule_Main, method="grouped")

############################################################
############################################################
############################################################
############################################################
# KNN 알고리즘
# KNN(K-Nearest Neighbor) 알고리즘은 범주를 모르는 어떠한 데이터에 대하여 분류 되어 있는 가장 유사한 예제의 범주로  지정해주는 알고리즘이다

# 입력 데이터와 유사한 K개의 데이터를 구하고, 그 K개 데이터의 분류 중 가장 빈도가 높은 클래스를 입력 데이터의 분류로  
# 결정하는 알고리즘이다. 

library(class)

# 정규화 함수
normalize <- function(x) {
  return ( (x - min(x))/(max(x) - min(x)) )
}


# 기간 별 그룹 나누기 위해 전처리
library(dplyr)
mainAct_t <- MainAct
joinAct_t <- joinAct

mainAct_t$기간 <- substr(mainAct_t$기간, 1, 4)
joinAct_t$기간 <- substr(joinAct_t$기간, 1, 4)

ma <- unique(mainAct_t$기간)
jo <- unique(joinAct_t$기간)

mainAct_t$기간 <- factor(mainAct_t$기간, levels = ma, labels = ma)
joinAct_t$기간 <- factor(joinAct_t$기간, levels = jo, labels = jo)

# factor table
table(mainAct_t$기간)
table(joinAct_t$기간)

# 정규화 시키기
main_End <- length(mainAct_t)
knn_MainAct <- as.data.frame(lapply(mainAct_t[2:main_End], normalize))

join_End <- length(joinAct_t)
knn_JoinAct <- as.data.frame(lapply(joinAct_t[2:join_End], normalize))

# 축약
summary(knn_MainAct)
summary(knn_JoinAct)

knn_MainAct <- cbind(knn_MainAct, revisitAct_DelCol[1])
knn_JoinAct <- cbind(knn_JoinAct, revisitAct_DelCol[1])

View(knn_MainAct)
View(knn_JoinAct)
############################################################################### 전처리

knn_Func <- function( val ) {
  # 샘플링
  idx <- sample(x = c("train", "valid", "test"), size = nrow(val), replace = TRUE, prob = c(3, 1, 1))
  
  # idx에 따라 데이터 나누기
  train <- val[idx == "train", ]
  valid <- val[idx == "valid", ]
  test <- val[idx == "test", ]
  
  # 입력x과 출력y 데이터로 분리
  train_x <- train[, -length(train)]
  valid_x <- valid[, -length(train)]
  test_x <- test[, -length(train)]
  train_y <- train[, length(train)]
  valid_y <- valid[, length(train)]
  test_y <- test[, length(train)]
  
  # knn 알고리즘 적용하기(k = 1)
  # k = 1 일 때
  knn_1 <- knn(train = train_x, test = valid_x, cl = train_y, k = 1, use.all = F)
  
  # 분류 정확도 계산하기
  table(knn_1, valid_y)
  
  accuracy_1 <- sum(knn_1 == valid_y) / length(valid_y)
  accuracy_1
  
  knn_21 <- knn(train = train_x, test = valid_x, cl = train_y, k = 21)
  
  # 분류 정확도 계산하기
  table(knn_21, valid_y)
  
  
  accuracy_21 <- sum(knn_21 == valid_y) / length(valid_y)
  accuracy_21
  # 
  # 최적의 k 값 구해보기
  # k가 1부터 train 행 수까지 변화할 때 분류 정확도 구하기
  ## 반복문 for 를 이용하여 k가 1부터 train 행 수까지 변화할 때,
  ## 분류 정확도가 몇 % 되는지 그래프를 그려보고 최적의 k를 확인
  # 분류 정확도 사전 할당
  
  accuracy_k <- NULL
  # kk가 1부터 train 행 수까지 증가할 때 (반복문)
  pb <- progress_bar$new(
    format="[:bar] :current/:total (:percent)", total=250
  )
  
  for(idx in c(1:250)){
    # k가 kk일 때 knn 적용하기
    knn_k <- knn(train = train_x, test = valid_x, cl = train_y, k = idx)
    # 분류 정확도 계산하기
    accuracy_k <- c(accuracy_k, sum(knn_k == valid_y) / length(valid_y))
    pb$tick(0)
    pb$tick(1)
  }
  
  # Error in knn(train = train_x, test = valid_x, cl = train_y, k = idx) :
  #     too many ties in knn
  ## 500 이상의 k 값이 에러가 뜨므로, 250까지로 제한해 본다.
  # k에 따른 분류 정확도 데이터 생성
  valid_k <- data.frame(k = c(1:250), accuracy = accuracy_k)
  colnames(valid_k)
  
  # k에 따른 분류 정확도 그래프 그리기
  par(mfrow = c(1, 1))
  plot(formula = accuracy ~ k, data = valid_k, type = "o", pch = 20, main = "validation - optimal k")
  
  # 분류 정확도가 가장 높으면서 가장 작은 k의 값 구하기
  sort(valid_k$accuracy, decreasing = T)
  maxdata <- max(valid_k$accuracy) # 
  maxdata
  
  min_position <- min(which(valid_k$accuracy == maxdata))
  min_position
  # min_position의 값과 그래프에서 확인해 보면 적정 k값을 확인할 수 있다.
  # 그럼 이제 k가 min_position의 값을 가질 때 모델이 얼마나 분류가 잘 되는지 test 데이터를 이용해서 표현해보자.
  # 최적의 k 값에 test 데이터 적용하기
  
  knn_optimization <- knn(train = train_x, test = test_x, cl = train_y, k = min_position)
  
  # # Confusion Matrix 틀 만들기
  result <- matrix(NA, nrow = 2, ncol = 2)
  rownames(result) <- paste0("real_", c('낮음', '높음'))
  colnames(result) <- paste0("clsf_", c('낮음', '높음'))
  result
  
  # # Confusion Matrix 값 입력하기
  result[1, 1] <- sum(ifelse(test_y == "낮음" & knn_optimization == "낮음", 1, 0))
  result[2, 1] <- sum(ifelse(test_y == "높음" & knn_optimization == "높음", 1, 0))
  result[1, 2] <- sum(ifelse(test_y == "낮음" & knn_optimization == "높음", 1, 0))
  result[2, 2] <- sum(ifelse(test_y == "높음" & knn_optimization == "낮음", 1, 0))
  result
  
  # 테이블
  table(prediction=knn_optimization, answer=test_y)
  
  # 정확도
  accuracy <- sum(knn_optimization == test_y) / sum(result)
  accuracy
}

# KNN 분석
knn_Func(knn_MainAct)
knn_Func(knn_JoinAct)

############################################################
############################################################
############################################################
############################################################

# [32.Naive Bayes] 불가능