# Load necessary libraries
library(tidyverse)
library(caret)
library(cluster)
library(ggplot2)

# Read the data
data <- read.csv("C:\\Users\\yashw\\Desktop\\Data Analytics Final Project\\heart.csv")

# Data Cleaning
# Replace zeros in Cholesterol and RestingBP which are not plausible values
data$Cholesterol[data$Cholesterol == 0] <- median(data$Cholesterol[data$Cholesterol != 0])
data$RestingBP[data$RestingBP == 0] <- median(data$RestingBP[data$RestingBP != 0])

# Convert categorical variables to factor type
data$Sex <- as.factor(data$Sex)
data$ChestPainType <- as.factor(data$ChestPainType)
data$RestingECG <- as.factor(data$RestingECG)
data$ExerciseAngina <- as.factor(data$ExerciseAngina)
data$ST_Slope <- as.factor(data$ST_Slope)
data$FastingBS <- as.factor(data$FastingBS)

# K-means Clustering (Unsupervised Learning)
# Scale the data
scaled_data <- scale(data[, c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")])

# Determine the optimal number of clusters using the elbow method
set.seed(123)
wss <- sapply(1:10, function(k){kmeans(scaled_data, k, nstart = 20)$tot.withinss})
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters", ylab = "Total within-cluster sum of squares")

# Perform K-means clustering with the chosen number of clusters
optimal_clusters <- 3 # assuming 3 is the optimal number from the plot
kmeans_result <- kmeans(scaled_data, optimal_clusters, nstart = 25)

# Visualizing Clusters
# Prepare data for plotting
data_for_plot <- as.data.frame(scaled_data)
data_for_plot$cluster <- as.factor(kmeans_result$cluster)
centers <- as.data.frame(kmeans_result$centers)

# Plot using ggplot
ggplot(data_for_plot, aes(x = Age, y = MaxHR, color = cluster)) +
  geom_point(alpha = 0.5, size = 3) +
  geom_point(data = centers, aes(x = Age, y = MaxHR), color = "black", size = 5, shape = 8) +
  labs(title = "K-means Clustering with Centers",
       x = "Age (scaled)",
       y = "Maximum Heart Rate (scaled)") +
  theme_minimal() +
  scale_color_manual(values = c("red", "green", "blue"))  # Customize colors

# Data Partitioning
set.seed(123) # for reproducibility
index <- createDataPartition(data$HeartDisease, p = 0.70, list = FALSE)
train_data <- data[index, ]
test_data <- data[-index, ]

# Polynomial Regression Analysis
poly_model <- lm(MaxHR ~ poly(Age, 2), data = train_data)

# Summary of the polynomial regression model
summary(poly_model)

# Add fitted values to the train data for plotting
train_data$fitted_values <- predict(poly_model, newdata = train_data)

# Plotting the polynomial regression curve along with the data
ggplot(train_data, aes(x = Age, y = MaxHR)) +
  geom_point(color = 'blue', alpha = 0.5) +
  geom_line(aes(y = fitted_values), color = 'red') +
  labs(title = "Polynomial Regression Model Fit",
       x = "Age",
       y = "Maximum Heart Rate") +
  theme_minimal()

# Monte Carlo Simulation to predict MaxHR based on varying Ages
ages_sim <- seq(from = 20, to = 80, by = 1)
n_sims <- 1000  # Number of simulations
sim_results <- replicate(n_sims, {
  noise <- rnorm(length(ages_sim), mean = 0, sd = summary(poly_model)$sigma)
  predict(poly_model, newdata = data.frame(Age = ages_sim)) + noise
})

# Calculate mean and confidence intervals
sim_means <- apply(sim_results, 1, mean)
sim_upper <- apply(sim_results, 1, quantile, probs = 0.975)
sim_lower <- apply(sim_results, 1, quantile, probs = 0.025)

# Plotting the results of Monte Carlo simulations
plot_df <- data.frame(Age = ages_sim, Mean = sim_means, Upper = sim_upper, Lower = sim_lower)
ggplot(plot_df, aes(x = Age)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.3) +
  geom_line(aes(y = Mean), color = "red") +
  labs(title = "Monte Carlo Simulation for MaxHR Predictions",
       x = "Age", y = "Predicted MaxHR") +
  theme_minimal()

