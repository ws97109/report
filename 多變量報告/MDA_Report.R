# 載入需要的套件
install.packages("haven")
install.packages("dplyr")
install.packages("plotly")
install.packages("ggmosaic")
install.packages("ggcorrplot")
install.packages("GGally")
install.packages("factoextra")
install.packages("cluster")
library(haven)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggmosaic)
library(ggcorrplot)
library(GGally)
library(factoextra)
library(cluster)

# 增加堆疊大小限制以避免溢出
options(expressions = 50000)

# 確保已安裝 ggcorrplot
if (!requireNamespace("ggcorrplot", quietly = TRUE)) {
  install.packages("ggcorrplot")
}
library(ggcorrplot)

# 匯入資料，並輸出成 CSV 檔
net <- read.csv("C:/Users/ws971/OneDrive/桌面/多變量報告/網路成癮.csv")

# 篩選資料
new_net2 <- net %>%
  filter(q21a_6 == 1 & q21a_6_text == "已讀" & q37a == 5)
write.csv(new_net2, file = "C:/Users/ws971/OneDrive/桌面/多變量報告/網路成癮_篩選.csv", row.names = FALSE, fileEncoding = "UTF-8")

new_net_selected <- new_net2 %>%
  select(id, q1, q2, q6_h:q11_88_text, q16:q19_02, q21a_6:q23_05_1, q25_01_1:q29_5_text, q37a)
write.csv(new_net_selected, file = "C:/Users/ws971/OneDrive/桌面/多變量報告/網路成癮_篩選2.csv", row.names = FALSE, fileEncoding = "UTF-8")

# 性別編碼
new_net_selected$sex <- ifelse(new_net_selected$q1 == 1, "男", "女")

# 計算每個性別的數量
gender_counts <- table(new_net_selected$sex)

# 創建數據框用於繪圖
gender_df <- data.frame(
  gender = names(gender_counts),
  count = as.numeric(gender_counts)
)

# 計算百分比
gender_df$percentage <- gender_df$count / sum(gender_df$count) * 100

# 創建圓餅圖
ggplot(gender_df, aes(x = "", y = count, fill = gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("男" = "skyblue", "女" = "pink")) +
  labs(title = "受訪者性別分布", 
       fill = "性別") +
  theme_void() +
  theme(legend.position = "bottom")

# 打印性別分布的表格
print(gender_df)

# 創建年齡組別
new_net_selected$age_group <- cut(114 - new_net_selected$q2,
                      breaks = c(0, 18, 30, 40, 50, 60, Inf),
                      labels = c("18歲以下", "19-30歲", "31-40歲", "41-50歲", "51-60歲", "61歲以上"),
                      right = FALSE)

# 計算每個年齡組的人數
age_distribution <- new_net_selected %>%
  group_by(age_group) %>%
  summarise(count = n())

# 繪製條形圖
q1 <- ggplot(age_distribution, aes(x = age_group, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = count), vjust = -0.5) +
  labs(title = "受訪者年齡分佈",
       x = "年齡組別",
       y = "人數") +
  theme_minimal()

ggplotly(q1)

# 優化的計分函數
score_survey_optimized <- function(data) {
  data <- data %>%
    mutate(
      q17_score = ifelse(!is.na(q17_01) & q17_01 == 1 | !is.na(q17_02) & q17_02 == 1, 3, 0),
      q19_score = ifelse(!is.na(q19_01) & q19_01 == 1 | !is.na(q19_02) & q19_02 == 1, 15, 0),
      q22_score = rowSums(select(., starts_with("q22_")), na.rm = TRUE),
      q23_score = rowSums(select(., starts_with("q23_")), na.rm = TRUE) * 1.3,
      q25_score = rowSums(select(., starts_with("q25_")), na.rm = TRUE) * 1.3,
      q26_score = rowSums(select(., starts_with("q26_")), na.rm = TRUE),
      total_score = q17_score + q19_score + q22_score + q23_score + q25_score + q26_score
    )
  return(data)
}

# 對資料套用優化後的計分函數
scored_data <- score_survey_optimized(new_net_selected)

# 查看結果
summary(scored_data$total_score)

# 找出最低分
min_score <- min(scored_data$total_score, na.rm = TRUE)

# 找出擁有最低分的所有行
lowest_scorers <- scored_data[scored_data$total_score == min_score, ]

# 查看特定的列
columns_to_show <- c("id", "q17_score", "q19_score", "q22_score", "q23_score", "q25_score", "q26_score", "total_score")
print(lowest_scorers[, columns_to_show])

# 分類霸凌傾向
scored_data$bullying_tendency <- cut(scored_data$total_score, 
                                     breaks = c(-Inf, 44.7, 69.7, 94.7, Inf),
                                     labels = c("無", "低", "中", "高"),
                                     include.lowest = TRUE)

# 查看分類結果
table(scored_data$bullying_tendency)

# 計算理論中點和實際中位數
theoretical_midpoint <- (18.7 + 109) / 2  # 理論範圍的中點
actual_median <- median(scored_data$total_score, na.rm = TRUE)

# 使用理論中點和實際中位數的平均值作為 "中" 和 "高" 的分界點
mid_high_cutoff <- (theoretical_midpoint + actual_median) / 2

# 使用這個新的切分點來分類
scored_data$bullying_tendency_compromise <- cut(scored_data$total_score, 
                                                breaks = c(-Inf, actual_median, mid_high_cutoff, Inf),
                                                labels = c("低", "中", "高"),
                                                include.lowest = TRUE)

# 查看新的分類結果
table(scored_data$bullying_tendency_compromise)

# 打印具體的分割點
cat("低-中分界點：", actual_median, "\n",
    "中-高分界點：", mid_high_cutoff, "\n")

# 對 q27_1 進行重新分類
scored_data$q27_1_category <- case_when(
  scored_data$q27_1 %in% c(1, 2) ~ "不想",
  scored_data$q27_1 == 3 ~ "普通",
  scored_data$q27_1 %in% c(4, 5) ~ "想",
  TRUE ~ NA_character_
)

# 繪製霸凌傾向與 q27_1 的馬賽克圖
ggplot(data = scored_data) +
  geom_mosaic(aes(x = product(bullying_tendency_compromise), fill = q27_1_category), na.rm = TRUE) +
  labs(title = "霸凌傾向與 q27_1 的馬賽克圖",
       x = "霸凌傾向",
       fill = "q27_1") +
  theme_minimal()

# 繪製箱型圖以比較不同霸凌傾向下的總分
ggplot(scored_data, aes(x = bullying_tendency_compromise, y = total_score, fill = bullying_tendency_compromise)) +
  geom_boxplot() +
  labs(title = "不同霸凌傾向下的總分分佈",
       x = "霸凌傾向",
       y = "總分") +
  theme_minimal()

# 繪製散佈圖以檢查總分與年齡之間的關係
ggplot(scored_data, aes(x = 114 - q2, y = total_score)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "年齡與總分之間的關係",
       x = "年齡",
       y = "總分") +
  theme_minimal()

# 繪製變數之間的對應關係圖（Pair Plot）
set.seed(123)  # 設定隨機種子以確保結果可重現
data_numeric_subset <- scored_data %>% select(total_score, q17_score, q19_score, q22_score, q23_score) %>% na.omit()
ggpairs(data_numeric_subset, title = "部分變數之間的對應關係圖")

# 主成分分析 (PCA)
pca_result <- prcomp(data_numeric_subset, scale. = TRUE)
fviz_pca_biplot(pca_result, repel = TRUE, title = "主成分分析雙向圖 (PCA Biplot)")

# K-means 聚類分析
kmeans_result <- kmeans(data_numeric_subset, centers = 3, nstart = 25)
scored_data$cluster <- as.factor(kmeans_result$cluster)

# 繪製聚類結果的散佈圖
fviz_cluster(kmeans_result, data = data_numeric_subset, geom = "point", stand = FALSE, ellipse.type = "norm", ggtheme = theme_minimal(), main = "K-means 聚類結果")
