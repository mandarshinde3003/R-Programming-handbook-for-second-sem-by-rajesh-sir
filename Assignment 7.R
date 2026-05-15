# -------------------------------------------------
# Install and Load Required Packages
# -------------------------------------------------
install.packages("readxl")
library(readxl)
# -------------------------------------------------
# Create Dataset
# -------------------------------------------------
set.seed(123)
data <- data.frame(
  ID = 1:100,
  Age = sample(18:60, 100, replace = TRUE),
  Income = sample(20000:100000, 100, replace = TRUE),
  SpendingScore = sample(1:100, 100, replace = TRUE),
  Purchase = sample(c("Yes","No"), 100, replace = TRUE),
  Region = sample(c("North","South","East","West"),
                  100,
                  replace = TRUE)
)
# Display Dataset
head(data)


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Q.1  Retail Income vs Spending Strategy Scenario: A retail company wants to understand whether high-income customers actually spend more.

# 1.
library(readxl)
data <- read_excel("../R Practical - 7 Solution/data/Excel/analysis_data.xlsx")
# Pearson Correlation
correlation <- cor(data$Income,
                   data$SpendingScore,
                   method = "pearson",
                   use = "complete.obs")


# 2. Test significance of the correlation. What conclusion can be drawn?
# Correlation significance test
cor_test <- cor.test(data$Income,
                     data$SpendingScore,
                     method = "pearson")


# 3. Generate a normal distribution using Income and compare with actual data.
# Histogram of actual Income
hist(data$Income,
     probability = TRUE,
     main = "Income Distribution",
     col = "lightblue")
# Generate normal curve
x <- seq(min(data$Income),
         max(data$Income),
         length = 100)
y <- dnorm(x,
           mean = mean(data$Income),
           sd = sd(data$Income))
lines(x, y, col = "red", lwd = 2)


# 4.  Perform a one-sample t-test to check if average income is ₹50,000.
# One-sample t-test
t_test <- t.test(data$Income,
                 mu = 50000)
t_test



# 5. Test whether purchase success rate differs from 50% using binomial test.
# Convert Purchase to success count
success <- sum(data$Purchase == "Yes")
total <- nrow(data)
# Binomial test
binom_test <- binom.test(success,
                         total,
                         p = 0.50)
binom_test



#--------------------------------------------------------------------------------------------------------------------------------------------
# Q.2  Test significance of the correlation. What conclusion can be drawn?
# 1. Compute correlation matrix of Age, Income, SpendingScore.
# Correlation Matrix
cor_matrix <- cor(data[, c("Age",
                           "Income",
                           "SpendingScore")],
                  use = "complete.obs")
cor_matrix


# 2. Identify strongest relationship and justify.
# Display correlation matrix
print(cor_matrix)


# 3. Generate normal distribution for SpendingScore.
hist(data$SpendingScore,
     probability = TRUE,
     col = "lightgreen",
     main = "SpendingScore Distribution")
x <- seq(min(data$SpendingScore),
         max(data$SpendingScore),
         length = 100)
y <- dnorm(x,
           mean = mean(data$SpendingScore),
           sd = sd(data$SpendingScore))
lines(x, y, col = "red", lwd = 2)


# 4. Perform t-test for SpendingScore = 60.
t_test_spending <- t.test(data$SpendingScore,
                          mu = 60)
t_test_spending


# 5. Use Kruskal-Wallis test to compare SpendingScore across regions.
kruskal_result <- kruskal.test(SpendingScore ~ Region,
                               data = data)
kruskal_result



#---------------------------------------------------------------------------------------------------------------------------------------------
# Q.3  Financial Risk Assessment
# 1. Analyze correlation between Income and SpendingScore.
cor(data$Income,
    data$SpendingScore,
    method = "pearson")

# 2. Test statistical significance.
cor.test(data$Income,
         data$SpendingScore,
         method = "pearson")

# 3. Generate Poisson distribution for number of risky customers.
# Simulated risky customer counts
risky_customers <- rpois(100,
                         lambda = 5)
hist(risky_customers,
     main = "Poisson Distribution",
     col = "orange")

# 4. Perform t-test on Income = ₹40,000.
t.test(data$Income,
       mu = 40000)

# 5. Perform binomial test assuming 60% safe customers.
safe <- sum(data$Purchase == "Yes")
total <- nrow(data)
binom.test(safe,
           total,
           p = 0.60)



#------------------------------------------------------------------------------------------------------------------------------------------
# Q.4 Product Demand Forecasting
# 1. Compute Spearman correlation between Age and Income.
spearman_cor <- cor(data$Age,
                    data$Income,
                    method = "spearman")
spearman_cor

# 2. Explain monotonic relationship if any.
# Explanation:
# • Positive correlation → As age increases, income increases.
# • Negative correlation → As age increases, income decreases.
# • Near 0 → Weak monotonic relationship. 

# 3. Generate binomial distribution for purchase events.
# Binomial distribution
purchase_events <- rbinom(100,
                          size = 10,
                          prob = 0.5)
hist(purchase_events,
     col = "skyblue",
     main = "Binomial Distribution")


# 4. Perform t-test on Age = 35.
t.test(data$Age,
       mu = 35)

# 5. Use chi-square goodness-of-fit for Region distribution.
region_table <- table(data$Region)
chisq.test(region_table)


#-----------------------------------------------------------------------------------------------------------------------------------------------
# Q.5 Customer Satisfaction Modeling
# 1. Correlate Income and SpendingScore using Pearson.
cor(data$Income,
    data$SpendingScore,
    method = "pearson")

# 2. Validate using Spearman correlation.
cor(data$Income,
    data$SpendingScore,
    method = "spearman")

# 3. Generate normal distribution for SpendingScore.
hist(data$SpendingScore,
     probability = TRUE,
     col = "pink",
     main = "SpendingScore Distribution")
x <- seq(min(data$SpendingScore),
         max(data$SpendingScore),
         length = 100)
y <- dnorm(x,
           mean = mean(data$SpendingScore),
           sd = sd(data$SpendingScore))
lines(x, y, col = "blue", lwd = 2)

# 4. Perform t-test for mean satisfaction = 55.
t.test(data$SpendingScore,
       mu = 55)

# 5. Apply Wilcoxon test between Age and SpendingScore.
wilcox.test(data$Age,
            data$SpendingScore)


# -------------------------------------------------------------------------------------------------------------------------------------------
# Q.6 Sales Probability Analysis
# 1. Convert Purchase into binary and correlate with Income.
# Convert Purchase to binary
data$PurchaseBinary <- ifelse(data$Purchase == "Yes", 1, 0)
cor(data$PurchaseBinary,
    data$Income)
              
# 2. Generate binomial distribution for purchase trials.
purchase_trials <- rbinom(100,
                          size = 20,
                          prob = 0.6)
hist(purchase_trials,
     col = "lightgreen",
     main = "Purchase Trials Distribution")

# 3. Perform binomial test for observed success rate.
success <- sum(data$PurchaseBinary)
total <- nrow(data)
binom.test(success,
           total,
           p = 0.5)

# 4. Perform t-test on Income = ₹55,000.
t.test(data$Income,
       mu = 55000)

# 5. Apply chi-square test for purchase distribution.
purchase_table <- table(data$Purchase)
chisq.test(purchase_table)



# ---------------------------------------------------------------------------------------------------------------------------------------
# Scenario: Management wants a statistical summary for decision making.
# 1. Compute correlation matrix among all numeric variables.
# 2. Identify strongest predictor of SpendingScore.
# 3. Generate Poisson distribution for simulated demand.
# 4. Perform t-test on Income benchmark ₹60,000.
# 5. Perform Kruskal-Wallis test for Age across regions.

# 1. Compute correlation matrix among all numeric variables.
numeric_data <- data[, sapply(data, is.numeric)]
cor_matrix <- cor(numeric_data,
                  use = "complete.obs")
cor_matrix

# 2. Identify strongest predictor of SpendingScore.
cor_matrix

# 3. Generate Poisson distribution for simulated demand.
demand <- rpois(100,
                lambda = 8)
hist(demand,
     col = "yellow",
     main = "Simulated Demand Distribution")

# 4. Perform t-test on Income benchmark ₹60,000.
t.test(data$Income,
       mu = 60000)

# 5. Perform Kruskal-Wallis test for Age across regions.
kruskal.test(Age ~ Region,
             data = data)