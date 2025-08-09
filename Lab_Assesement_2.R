library(ggplot2)
library(moments)
library(dplyr)
library(tidyr)
library(GGally)

kidney_data <- read.csv("D:/kidney-stone-dataset.csv", stringsAsFactors = TRUE)
kidney_data_clean <- na.omit(kidney_data)
names(kidney_data_clean)[names(kidney_data_clean) == "target"] <- "StonePresence"
kidney_data_clean$StonePresence <- as.factor(kidney_data_clean$StonePresence)

p_hist_gravity <- ggplot(kidney_data_clean, aes(x = gravity)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.005, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "blue", linewidth = 1) +
  theme_minimal() +
  labs(title = "Histogram and Density Plot of Urine Specific Gravity", x = "Urine Specific Gravity", y = "Density")

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mean_osmo <- mean(kidney_data_clean$osmo, na.rm = TRUE)
median_osmo <- median(kidney_data_clean$osmo, na.rm = TRUE)
mode_osmo <- get_mode(kidney_data_clean$osmo)
skew_osmo <- skewness(kidney_data_clean$osmo, na.rm = TRUE)

p_skew_osmo <- ggplot(kidney_data_clean, aes(x = osmo)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  geom_vline(xintercept = mean_osmo, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = median_osmo, color = "green", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mode_osmo, color = "orange", linetype = "dashed", linewidth = 1) +
  labs(title = paste("Skewness Plot of Urine Osmolality"),
       caption = "Red: Mean, Green: Median, Orange: Mode") +
  theme_minimal()

p_bar_categorical <- ggplot(kidney_data_clean, aes(x = StonePresence, fill = StonePresence)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Bar Graph of Stone Presence", x = "Stone Presence", y = "Count")

numeric_cols_for_boxplot <- c("urea", "calc", "gravity", "osmo", "ph", "cond")
existing_numeric_cols <- intersect(numeric_cols_for_boxplot, names(kidney_data_clean))

if (length(existing_numeric_cols) > 0) {
  numeric_data_long <- kidney_data_clean %>%
    select(all_of(existing_numeric_cols)) %>%
    tidyr::pivot_longer(everything(), names_to = "Variable", values_to = "Value")
  p_boxplot_numeric <- ggplot(numeric_data_long, aes(x = Variable, y = Value)) +
    geom_boxplot(fill = "lightyellow") +
    theme_minimal() +
    labs(title = "Boxplot of Numeric Attributes", x = "Variable", y = "Value") +
    facet_wrap(~Variable, scales = "free_x")
} else {
  message("No specified numeric columns for boxplot found in the dataset.")
  p_boxplot_numeric <- NULL
}

multi_var_numeric_cols <- c("gravity", "osmo", "ph", "cond", "urea", "calc")
existing_multi_var_numeric_cols <- intersect(multi_var_numeric_cols, names(kidney_data_clean))

if (length(existing_multi_var_numeric_cols) > 1) {
  p_scatter_matrix <- ggpairs(kidney_data_clean, columns = existing_multi_var_numeric_cols,
                              aes(color = StonePresence, alpha = 0.6),
                              upper = list(continuous = wrap("points", alpha = 0.3)),
                              lower = list(continuous = wrap("points", alpha = 0.3)),
                              title = "Scatter Matrix of Numeric Attributes by Stone Presence") +
    theme_minimal()
} else {
  message("Not enough numeric columns for a scatter matrix.")
  p_scatter_matrix <- NULL
}

p_scatter_plot_simple <- ggplot(kidney_data_clean, aes(x = gravity, y = calc, color = StonePresence)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Scatter Plot of Urine Specific Gravity vs. Calcium",
       x = "Urine Specific Gravity", y = "Calcium", color = "Stone Presence")

p_violin_urea_stone <- ggplot(kidney_data_clean, aes(x = StonePresence, y = urea, fill = StonePresence)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Violin Plot of Urea by Stone Presence", x = "Stone Presence", y = "Urea", fill = "Stone Presence")


print(p_hist_gravity)
print(p_skew_osmo)
print(p_bar_categorical)
if (!is.null(p_boxplot_numeric)) {
  print(p_boxplot_numeric)
}
if (!is.null(p_scatter_matrix)) {
  print(p_scatter_matrix)
} else {
  print(p_scatter_plot_simple)
}
print(p_violin_urea_stone)

