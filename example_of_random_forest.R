# 加载库
library(randomForest)
library(ggplot2)
library(MASS)

# 加载内置数据集（波士顿房价数据）
data(Boston)
df <- Boston

# 显示数据基本信息
cat("数据维度:", dim(df), "\n")
cat("\n前5行样本:\n")
print(head(df, 5))

# 划分训练集和测试集
set.seed(42)
train_index <- sample(1:nrow(df), 0.8 * nrow(df))
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# 创建随机森林回归模型
rf_model <- randomForest(
  medv ~ .,          # 使用所有特征预测房价中位数
  data = train_data,
  ntree = 100,       # 树的数量
  mtry = 3,          # 每个节点的随机特征数
  importance = TRUE  # 计算特征重要性
)

# 模型预测
predictions <- predict(rf_model, test_data)

# 模型评估
mse <- mean((test_data$medv - predictions)^2)
r2 <- 1 - sum((test_data$medv - predictions)^2) / sum((test_data$medv - mean(test_data$medv))^2)

cat("\n模型性能:\n")
cat("MSE:", round(mse, 2), "\n")
cat("R²:", round(r2, 2), "\n")

# 特征重要性可视化
importance_df <- data.frame(
  Feature = rownames(importance(rf_model)),
  Importance = importance(rf_model)[, "%IncMSE"]
)

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Feature Importance Ranking",
       x = "Features",
       y = "% Increase in MSE") +
  theme_minimal()

# 实际值与预测值对比图
comparison_df <- data.frame(
  Actual = test_data$medv,
  Predicted = predictions
)

ggplot(comparison_df, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, color = "darkorange") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "steelblue") +
  labs(title = "Actual vs Predicted Values",
       x = "Actual Median Value ($1000s)",
       y = "Predicted Value ($1000s)") +
  theme_minimal() +
  xlim(min(df$medv), max(df$medv)) +
  ylim(min(df$medv), max(df$medv))