library(tidyverse)
library(patchwork)
library(cluster)

#load data
trends <- read.csv("Mental health Depression disorder Data.csv")
summary(trends)

#clean data
trends <- na.omit(trends)
trends <- trends %>%
  dplyr::select(Entity, Year, Schizophrenia...., Bipolar.disorder...., Eating.disorders...., Anxiety.disorders...., Drug.use.disorders...., Depression...., Alcohol.use.disorders....) %>%
  rename(sch = Schizophrenia....,
         bipol = Bipolar.disorder....,
         eat = Eating.disorders....,
         anx = Anxiety.disorders....,
         drug = Drug.use.disorders....,
         dep = Depression....,
         alc = Alcohol.use.disorders....)
trends$sch <- as.numeric(trends$sch)
trends$bipol <- as.numeric(trends$bipol)
trends$eat <- as.numeric(trends$eat)
summary(trends[, c("sch", "bipol", "eat", "anx", "drug", "dep", "alc")])

# Reshape the data to long format
trends_long <- trends %>% 
  pivot_longer(cols = -c(Entity, Year), names_to = "Condition", values_to = "Percentage")

# Convert Year to numeric
trends_long$Year <- as.numeric(trends_long$Year)

# Calculate the average percentage for each Year and Condition
trends_summary <- trends_long %>%
  group_by(Year, Condition) %>%
  summarize(Percentage = mean(Percentage))

# Create a custom color palette
my_colors <- c("black", "brown", "red", "blue", "green", "yellow", "purple")

# Create the small multiples layout
plots <- list()
for (cond in unique(trends_summary$Condition)) {
  p <- ggplot(trends_summary[trends_summary$Condition == cond, ], aes(x = Year, y = Percentage)) +
    geom_line(color = my_colors[which(unique(trends_summary$Condition) == cond)], size = 1.5) +
    geom_point(size = 3, color = my_colors[which(unique(trends_summary$Condition) == cond)]) +
    labs(title = cond, x = "Year", y = "Percentage") +
    scale_x_continuous(breaks = seq(1990, 2017, by = 3)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      plot.title = element_text(size = 12, face = "bold")
    )
  plots[[cond]] <- p
}

# Combine the small multiples using patchwork
grid_plot <- wrap_plots(plots, ncol = 2) + plot_annotation(title = "Year-over-Year Changes in Mental Health Condition Prevalence")
grid_plot

#calculate average percentage for each country and condition
trends_summary1 <- trends_long %>%
  group_by(Entity, Condition) %>%
  summarize(Percentage = mean(Percentage))
# Calculate the variance for each condition
condition_data_stats <- trends_summary1 %>%
  group_by(Condition) %>%
  summarize(Condition_Variance = var(Percentage))

# Perform weighted k-means clustering
set.seed(123)
condition_data_weighted <- trends_summary1 %>%
  left_join(condition_data_stats, by = "Condition") %>%
  mutate(weight = Condition_Variance / sum(Condition_Variance))

km_model <- kmeans(condition_data_weighted[, c("Percentage", "weight")], centers = 7, nstart = 25)
condition_data_weighted$Cluster <- as.factor(km_model$cluster)

# Calculate the average prevalence for each condition in each cluster
cluster_stats <- condition_data_weighted %>%
  group_by(Cluster, Condition) %>%
  summarize(Avg_Prevalence = mean(Percentage))

# Visualize the results
ggplot(cluster_stats, aes(x = Condition, y = Avg_Prevalence, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Prevalence of Conditions by Cluster",
       x = "Condition", y = "Average Prevalence") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
