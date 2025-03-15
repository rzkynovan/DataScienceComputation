library(dplyr)
library(ggplot2)
library(GGally)
library(car)
library(patchwork)
library(esquisse)


data = read.csv("./week3/ames.csv")
str(data)
summary(data)

#=================================EDA=============================================
# 1. Histogram: Distribution of SalePrice
p1 <- ggplot(data, aes(x = Sale_Price)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of SalePrice", x = "SalePrice", y = "Count")

# 2. Boxplot: Distribution SalePrice
p2 <- ggplot(data, aes(y = Sale_Price)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of SalePrice", y = "SalePrice")

# 3. Scatterplot: SalePrice vs. Gr.Liv.Area (Above Grade Living Area)
p3 <- ggplot(data, aes(x = Gr_Liv_Area, y = Sale_Price)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "SalePrice vs. Gr.Liv.Area", x = "Above Grade Living Area", y = "SalePrice")

# 4. Scatterplot: SalePrice vs. Total.Bsmt.SF (Total Basement SF)
p4 <- ggplot(data, aes(x = Total_Bsmt_SF, y = Sale_Price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "SalePrice vs. Total Basement SF", x = "Total Basement SF", y = "SalePrice")

# 5. Scatterplot: SalePrice vs. Year.Built
p5 <- ggplot(data, aes(x = Year_Built, y = Sale_Price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "SalePrice vs. Year.Built", x = "Year Built", y = "SalePrice")


combined_EDA <- (p1 | p2) / (p3 | p4) / p5

combined_EDA

data$Log_Sale_Price <- log(data$Sale_Price)
data <- data %>%
  select(-Sale_Price)

# 1. Histogram: Distribution of Log SalePrice
p1 <- ggplot(data, aes(x = Log_Sale_Price)) +
  geom_histogram(bins = 30,fill = "skyblue", color = "black") +
  labs(title = "Distribution of SalePrice", x = "SalePrice", y = "Count")

# 2. Boxplot: Distribution Log SalePrice
p2 <- ggplot(data, aes(y = Log_Sale_Price)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of SalePrice", y = "SalePrice")

(p1 | p2)

# Normality Test
ks.test(data$Log_Sale_Price, "pnorm",
        mean = mean(data$Log_Sale_Price),
        sd = sd(data$Log_Sale_Price))
shapiro.test(data$Log_Sale_Price)

ggplot(data, aes(x = Log_Sale_Price)) +
  geom_histogram(aes(y = ..density..), bins = 30,
                 fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram & Density Plot of log(SalePrice)",
       x = "log(SalePrice)",
       y = "Density")


# Create a scatterplot matrix with correlation values in the upper panels
ggpairs(data,
        upper = list(continuous = wrap("cor", size = 3)),
        lower = list(continuous = "smooth"),
        diag  = list(continuous = "densityDiag"),
        title = "Scatterplot Matrix with Correlations")

#================================MODELING AND FEATURE SELECTION=========================
# Fit a linear model using the top predictors (or you could use all numeric predictors)
pseudo_model <- lm(Log_Sale_Price ~ ., data = data)
summary(pseudo_model)

# Calculate VIF values
vif_values <- vif(pseudo_model);sort(vif_values,decreasing =TRUE)

# Calculate the coefficient of variation for each variable, excluding Sale_Price
variable_cv <- sapply(data %>% select(-Log_Sale_Price),
                      function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
variable_cv_sorted <- sort(variable_cv)
print(variable_cv_sorted)

summary(pseudo_model)
