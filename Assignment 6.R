# -------------------------------------------------
# Create Dataset
# -------------------------------------------------
set.seed(123)
data <- data.frame(
  CustomerID = 1:100,
  Gender = sample(
    c("Male", "Female"),
    100,
    replace = TRUE
  ),
  Region = sample(
    c("North", "South", "East", "West"),
    100,
    replace = TRUE
  ),
  Purchase = sample(
    c("Yes", "No"),
    100,
    replace = TRUE
  ),
  Amount = sample(
    1000:10000,
    100,
    replace = TRUE
  )
)
# View Dataset
head(data)


# Q.1  Sales Performance Analysis

# a) Total sales by region
aggregate(Amount ~ Region, data, sum)

# b) Average sales per gender
aggregate(Amount ~ Gender, data, mean)

# c) Frequency distribution of purchase
table(data$Purchase)

# d) Cross-tab: Region vs Purchase
table(data$Region, data$Purchase)

# e) Chi-square test
chisq.test(table(data$Region, data$Purchase))


# ------------------------------------------------------------------------------------------
# Q.2  Customer Segmentation

# a) Count customers by region
table(data$Region)

# b) Average purchase by region using tapply
tapply(data$Amount, data$Region, mean)

# c) Frequency of gender
table(data$Gender)

# d) Cross-tab: Gender vs Region
table(data$Gender, data$Region)

# e) Chi-square test
chisq.test(table(data$Gender, data$Region))


#--------------------------------------------------------------------------------------------
# Q.3 Purchase Pattern Analysis
# a) Total purchase amount
sum(data$Amount)

# b) Row-wise sum using apply
numeric_data <- data.frame(data$Amount)
apply(numeric_data, 1, sum)

# c) Frequency of purchase
table(data$Purchase)

# d) Cross-tab: Purchase vs Region
table(data$Purchase, data$Region)

# e) Chi-square test
chisq.test(table(data$Purchase, data$Region))


# ---------------------------------------------------------------------------------------------------
# Q.4 High-Value Customer Analysis

# a) Filter customers with Amount > 5000
high_value <- data[data$Amount > 5000, ]
high_value

# b) Mean purchase of filtered group
mean(high_value$Amount)

# c) Frequency of region (filtered)
table(high_value$Region)

# d) Cross-tab: Gender vs Purchase (filtered)
table(high_value$Gender, high_value$Purchase)

# e) Chi-square test
chisq.test(table(high_value$Gender, high_value$Purchase))


# ---------------------------------------------------------------------------------------------------------------
# Q.5 Business Decision Analysis

# a) Total revenue
sum(data$Amount)

# b) Average revenue per region
aggregate(Amount ~ Region, data, mean)

# c) Frequency distribution of regions
table(data$Region)

# d) Cross-tab: Region vs Purchase
table(data$Region, data$Purchase)

# e) Statistical inference
chisq.test(table(data$Region, data$Purchase))