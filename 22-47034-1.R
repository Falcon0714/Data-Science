kidney <- read.csv("D:/kidney-stone-dataset.csv", stringsAsFactors = TRUE)
kidney_clean <- na.omit(kidney)

names(kidney_clean)[names(kidney_clean) == "target"] <- "StonePresence"
kidney_clean$StonePresence <- as.factor(kidney_clean$StonePresence)


pearson_corr <- cor(kidney_clean$gravity, kidney_clean$osmo, method = "pearson")
paste("Pearson's correlation between gravity and osmo:", round(pearson_corr, 2))


spearman_corr <- cor(kidney_clean$ph, kidney_clean$cond, method = "spearman")
paste("Spearman's correlation between Urine pH and Conductivity:", round(spearman_corr, 2))


kidney_clean$gravity_cat <- cut(kidney_clean$gravity, breaks = 3, labels = c("Low", "Medium", "High"))
chi_sq <- chisq.test(kidney_clean$gravity_cat, kidney_clean$StonePresence)
chi_sq


anova_result <- aov(calc ~ StonePresence, data = kidney_clean)
"ANOVA for Calcium across Stone Presence:"
summary(anova_result)

