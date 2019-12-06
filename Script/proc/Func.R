library(xlsx)
library(devtools)
library(nnet)
library(base)
# source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

dir <- getwd()
dir_db <- "../TeamJoel/DB/"
# setwd("../")

# 경로 자동설정
if (dir != dir_db) {
  # print("경로 재설정")
  setwd(dir_db)
} else {
  # print("경로 정상")
}