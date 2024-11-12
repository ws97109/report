#載入需要套件
install.packages("haven")
library(haven)
library(dplyr)

#匯入資料，輸出成csv檔
net <- read_sav("/Users/weiqihong/Library/Mobile Documents/com~apple~CloudDocs/碩士班/MDA/report/D00224_2 (1)/data.sav")
write.csv(net, file = "/Users/weiqihong/Library/Mobile Documents/com~apple~CloudDocs/碩士班/MDA/report/網路成癮.csv", row.names = FALSE, fileEncoding = "UTF-8")

#資料篩選
new_net <- net %>%
  filter(is.na(q21a_1) & is.na(q21a_2) & is.na(q21a_3) & is.na(q21a_4) & is.na(q21a_5) & q21a_6 == 1)

write.csv(new_net, file = "/Users/weiqihong/Library/Mobile Documents/com~apple~CloudDocs/碩士班/MDA/report/網路成癮(篩選）.csv", row.names = FALSE, fileEncoding = "UTF-8")


