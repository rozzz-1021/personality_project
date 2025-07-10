library(readr)
install.packages("here")
library(here)
data <- read_csv(here("data", "personality_dataset.csv"))
library(tidyverse)
glimpse(data)
summary(data)
data_clean <- data %>%
  mutate(across(c(Time_spent_Alone, Social_event_attendance, Going_outside,
                  Friends_circle_size, Post_frequency),
                ~ replace_na(., median(., na.rm = TRUE)))) %>%
  mutate(across(c(Stage_fear, Drained_after_socializing, Personality),
                ~ replace_na(., "Unknown"))) %>%
  mutate(across(c(Stage_fear, Drained_after_socializing, Personality), as.factor))
summary(data_clean)
str(data_clean)
library(ggplot2)
# Personality Distribution
ggplot(data_clean, aes(x = Personality)) +
  geom_bar(fill = "#87CEEB") +
  labs(title = "Distribution of Personality Types", x = "Personality", y = "Count")
# Personality versus Time spent alone
ggplot(data_clean, aes(Personality, Time_spent_Alone)) +
  geom_boxplot() +
  labs(title = "Time Spent Alone by Personality Type", y = "Hours Spent Alone")
# Social Event Attendance vs Feeling Drained
ggplot(data_clean, aes(x = Drained_after_socializing, y = Social_event_attendance, fill = Drained_after_socializing)) +
  geom_boxplot() +
  labs(title = "Social Event Attendance vs Feeling Drained", y = "Event Frequency")
#friend circle and personality
ggplot(data_clean, aes(x = Personality, y = Friends_circle_size)) +
  geom_boxplot() +
  labs(title = "Friend Circle Size by Personality", y = "Size")
# Regression of Friends Number and Personality
model1 <- lm(Friends_circle_size ~ Personality, data = data_clean)
summary(model1)
#Regression of frinend circle size and personality, whether drained after socialising, and time spent alone
data_clean_filtered <- data_clean %>%
  filter(Drained_after_socializing != "Unknown")
model2 <- lm(Friends_circle_size ~ Personality + Time_spent_Alone,
                  data = data_clean_filtered)
summary(model2)
# descriptive statisticsgroup by Personality 
summary_personality <- data_clean %>%
  group_by(Personality) %>%
  summarise(
    Avg_Friends = mean(Friends_circle_size, na.rm = TRUE),
    SD_Friends = sd(Friends_circle_size, na.rm = TRUE),
    Avg_Alone_Time = mean(Time_spent_Alone, na.rm = TRUE),
    SD_Alone_Time = sd(Time_spent_Alone, na.rm = TRUE),
    Avg_Attendance = mean(Social_event_attendance, na.rm = TRUE),
    Count = n()
  )

print(summary_personality)
# descriptive statistic group by Drained_after_socializing 
summary_drained <- data_clean %>%
  group_by(Drained_after_socializing) %>%
  summarise(
    Avg_Friends = mean(Friends_circle_size, na.rm = TRUE),
    SD_Friends = sd(Friends_circle_size, na.rm = TRUE),
    Avg_Alone_Time = mean(Time_spent_Alone, na.rm = TRUE),
    SD_Alone_Time = sd(Time_spent_Alone, na.rm = TRUE),
    Count = n()
  )

print(summary_drained)
model3 <- lm(Friends_circle_size ~ Personality + Drained_after_socializing + Time_spent_Alone, data = data_clean)
summary(model3)
library(broom)
library(dplyr)
glance1 <- glance(model1) %>% mutate(Model = "Model 1: Personality only")
glance2 <- glance(model2) %>% mutate(Model = "Model 2: + Time_spent_Alone")
glance3 <- glance(model3) %>% mutate(Model = "Model 3: + Drained & Alone Time")

model_comparison <- bind_rows(glance1, glance2, glance3) %>%
  select(Model, r.squared, adj.r.squared, AIC, BIC, p.value)

print(model_comparison)
model_interact <- lm(Friends_circle_size ~ Personality * Time_spent_Alone, data = data_clean)
summary(model_interact)
# Backtesting
library(cluster)
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
set.seed(123)
train_index <- sample(seq_len(nrow(data_clean)), size = 0.8 * nrow(data_clean))
train <- data_clean[train_index, ]
test <- data_clean[-train_index, ]
#use model3 build model and predict
model3_bt <- lm(Friends_circle_size ~ Personality + Drained_after_socializing + Time_spent_Alone, data = train)
pred_test <- predict(model3_bt, newdata = test)
#calcualte rmse
rmse <- sqrt(mean((test$Friends_circle_size - pred_test)^2))
print(paste("Test RMSE:", round(rmse, 2)))
#Logistic Regression
data_class <- data_clean %>%
  filter(Personality %in% c("Introvert", "Extrovert"))
data_class <- data_class %>%
  mutate(Personality = factor(Personality))
set.seed(123)
train_idx <- sample(seq_len(nrow(data_class)), size = 0.8 * nrow(data_class))
train_class <- data_class[train_idx, ]
test_class <- data_class[-train_idx, ]
logit_model <- glm(Personality ~ Time_spent_Alone + Friends_circle_size + Social_event_attendance,
                   data = train_class, family = "binomial")
pred_probs <- predict(logit_model, newdata = test_class, type = "response")
pred_class <- ifelse(pred_probs > 0.5, "Introvert", "Extrovert")
accuracy <- mean(pred_class == test_class$Personality)
print(paste("Classification Accuracy:", round(accuracy, 3)))
#k-means clustering
data_cluster <- data_clean %>%
  select(Time_spent_Alone, Friends_circle_size, Social_event_attendance, Post_frequency, Going_outside) %>%
  scale()
set.seed(123)
kmeans_model <- kmeans(data_cluster, centers = 3, nstart = 25)
table(kmeans_model$cluster)
cluster_df <- as.data.frame(data_cluster)
cluster_df$Cluster <- as.factor(kmeans_model$cluster)
pca_res <- prcomp(data_cluster, scale. = TRUE)
pca_df <- as.data.frame(pca_res$x[, 1:2])
pca_df$Cluster <- as.factor(kmeans_model$cluster)
library(ggplot2)
ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "K-means Clustering Visualization (PCA Reduced)", x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()
data_clean$Cluster <- as.factor(kmeans_model$cluster)

group_summary <- data_clean %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Alone = mean(Time_spent_Alone),
    Avg_Friends = mean(Friends_circle_size),
    Avg_Social = mean(Social_event_attendance),
    Avg_Post = mean(Post_frequency),
    Avg_Going = mean(Going_outside),
    Count = n()
  )
print(group_summary)

# Decision Tree
install.packages("rpart")
library(rpart)
tree_model <- rpart(Personality ~ Time_spent_Alone + Friends_circle_size + Social_event_attendance,
                    data = train_class, method = "class")
print(tree_model)
library(rpart.plot)
rpart.plot(tree_model)
