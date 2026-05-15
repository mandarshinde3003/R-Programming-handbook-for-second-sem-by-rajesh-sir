# =================================================
# FINAL PROJECT
# COMPLETE DATA ANALYSIS USING R
# =================================================
# PROJECT:
# E-Commerce Sales Data Analysis
# =================================================
# STEP 1 : INSTALL AND LOAD PACKAGES
# =================================================
install.packages("ggplot2")
library(ggplot2)

# =================================================
# STEP 2 : CREATE / IMPORT DATASET
# =================================================
set.seed(123)
sales_data <- data.frame(
  OrderID = 1:200,
  CustomerName = sample(
    c("Rahul","Amit","Priya","Neha",
      "Arjun","Sneha","Rohit","Kiran"),
    200,
    replace = TRUE
  ),
  City = sample(
    c("Pune","Mumbai","Delhi",
      "Bangalore","Hyderabad"),
    200,
    replace = TRUE
  ),
  Product = sample(
    c("Laptop","Mobile","Tablet",
      "Shoes","Watch"),
    200,
    replace = TRUE
  ),
  Category = sample(
    c("Electronics","Fashion"),
    200,
    replace = TRUE
  ),
  Quantity = sample(
    1:10,
    200,
    replace = TRUE
  ),
  Price = sample(
    seq(500,50000,500),
    200,
    replace = TRUE
  ),
  Discount = sample(
    c(0,5,10,15,20),
    200,
    replace = TRUE
  )
)
# Create Revenue Column
sales_data$Revenue <- sales_data$Quantity * sales_data$Price
# Display First Rows
head(sales_data)
# =================================================
# STEP 3 : DATA CLEANING AND PREPROCESSING
# =================================================
# Check Structure
str(sales_data)
# Check Missing Values
sum(is.na(sales_data))
# Remove Duplicate Rows
sales_data <- unique(sales_data)
# Create Discount Amount
sales_data$DiscountAmount <- sales_data$Revenue * sales_data$Discount / 100
# Create Final Amount
sales_data$FinalAmount <- sales_data$Revenue - sales_data$DiscountAmount

# =================================================
# STEP 4 : EXPLORATORY DATA ANALYSIS
# =================================================
# Summary Statistics
summary(sales_data)
# Total Revenue
sum(sales_data$Revenue)
# Average Revenue
mean(sales_data$Revenue)
# Maximum Revenue
max(sales_data$Revenue)
# Minimum Revenue
min(sales_data$Revenue)
# Revenue by City
aggregate(
  Revenue ~ City,
  sales_data,
  sum
)
# Revenue by Product
aggregate(
  Revenue ~ Product,
  sales_data,
  sum
)

# =================================================
# STEP 5 : DATA VISUALIZATION
# =================================================
# Scatter Plot
ggplot(
  sales_data,
  aes(x = Price,
      y = Revenue)
) +
  geom_point()
# Bar Chart
ggplot(
  sales_data,
  aes(x = City)
) +
  geom_bar()
# Histogram
ggplot(
  sales_data,
  aes(x = Revenue)
) +
  geom_histogram(
    bins = 20
  )
# Boxplot
ggplot(
  sales_data,
  aes(x = Category,
      y = Revenue)
) +
  geom_boxplot()
# Line Chart
ggplot(
  sales_data,
  aes(x = OrderID,
      y = Revenue)
) +
  geom_line()
# =================================================
# STEP 6 : CORRELATION ANALYSIS
# =================================================
# Correlation Between Price and Revenue
cor(
  sales_data$Price,
  sales_data$Revenue
)
# Correlation Test
cor.test(
  sales_data$Price,
  sales_data$Revenue
)


# =================================================
# STEP 7 : PROBABILITY DISTRIBUTION
# =================================================
# Generate Normal Distribution
rnorm(
  100,
  mean(sales_data$Revenue),
  sd(sales_data$Revenue)
)
# Generate Binomial Distribution
rbinom(
  50,
  size = 10,
  prob = 0.5
)
# Generate Poisson Distribution
rpois(
  50,
  lambda = 5
)# =================================================
# STEP 8 : HYPOTHESIS TESTING
# =================================================
# One Sample T-Test
t.test(
  sales_data$Revenue,
  mu = 100000
)
# Chi-Square Test
chisq.test(
  table(sales_data$Category)
)
# =================================================
# STEP 9 : INTERPRETATION
# =================================================
cat(
  "\nINTERPRETATION:",
  
  "\n1. Revenue varies across cities and products.",
  
  "\n2. Correlation shows relationship between Price and Revenue.",
  
  "\n3. Graphs help identify trends and patterns.",
  
  "\n4. Statistical tests help validate business assumptions."
)

# =================================================
# STEP 10 : FINAL CONCLUSION
# =================================================
cat(
  "\n\nFINAL CONCLUSION:",
  
  "\nThe project successfully performed complete",
  
  "\ndata analysis using R programming.",
  
  "\nEDA, visualization, correlation analysis,",
  
  "\nand hypothesis testing were performed",
  
  "\nto derive meaningful business insights."
)