# Install and Load Package
install.packages("ggplot2")
library(ggplot2)
# -------------------------------------------------
# Create Dataset
# -------------------------------------------------
set.seed(123)
sales_data <- data.frame(
  OrderID = 1:200,
  CustomerName = sample(
    c("Rahul","Amit","Priya","Neha","Arjun","Sneha","Rohit","Kiran","Pooja","Vikas"),
    200, replace = TRUE
  ),
  
  City =
    sample(c("Pune","Mumbai","Delhi","Bangalore","Hyderabad","Chennai","Kolkata"),
           200, replace = TRUE
    ),
  
  Product = sample(
    c("Laptop","Mobile","Tablet","Shoes",
      "Watch","Headphones","Camera"),
    200,
    replace = TRUE
  ),
  
  Category = sample(
    c("Electronics","Fashion","Accessories"),
    200,
    replace = TRUE
  ),
  
  Quantity = sample(1:10, 200, replace = TRUE),
  Price = sample(
    seq(500, 50000, 500),200,replace = TRUE),
  
  
  Discount = sample(
    c(0,5,10,15,20,25),
    200,
    replace = TRUE
  ),
  PaymentMethod = sample(
    c("Cash","Card","UPI","Net Banking"),
    200,
    replace = TRUE
  )
)
# Create Calculated Columns
sales_data$Revenue <- sales_data$Quantity * sales_data$Price
sales_data$DiscountAmount <- sales_data$Revenue * sales_data$Discount / 100
sales_data$FinalAmount <- sales_data$Revenue - sales_data$DiscountAmount
head(sales_data)



# -------------------------------------------------
# Q1) Basic ggplot Charts
# -------------------------------------------------
# a) Scatter plot of Price vs Revenue
ggplot(sales_data, aes(x = Price, y = Revenue)) +
  geom_point()
# b) Bar chart for City
ggplot(sales_data, aes(x = City)) +
  geom_bar()
# c) Histogram of Price
ggplot(sales_data, aes(x = Price)) +
  geom_histogram(bins = 20)
# d) Boxplot of Revenue by Category
ggplot(sales_data, aes(x = Category, y = Revenue)) +
  geom_boxplot()
# e) Line chart for Revenue by OrderID
ggplot(sales_data, aes(x = OrderID, y = Revenue)) +
  geom_line()

# -------------------------------------------------
# Q2) Customized Visualization
# -------------------------------------------------
# a) Add title to chart
ggplot(sales_data, aes(x = City)) +
  geom_bar() +
  labs(title = "Orders by City")
# b) Change axis labels
ggplot(sales_data, aes(x = Price, y = Revenue)) +
  geom_point() +
  labs(x = "Product Price",
       y = "Total Revenue")
# c) Change color of bars
ggplot(sales_data, aes(x = City)) +
  geom_bar(fill = "blue")
# d) Change theme style
ggplot(sales_data, aes(x = City)) +
  geom_bar() +
  theme_minimal()
# e) Rotate x-axis labels
ggplot(sales_data, aes(x = Product)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45))

# -------------------------------------------------
# Q3) Multi Variable Visualization
# -------------------------------------------------
# a) Price vs Revenue colored by Category
ggplot(
  sales_data,
  aes(x = Price,
      y = Revenue,
      color = Category)
) +
  geom_point()
# b) Price vs Quantity colored by Category
ggplot(
  sales_data,
  aes(x = Price,
      y = Quantity,
      color = Category)
) +
  geom_point()
# c) Revenue vs Discount
ggplot(
  sales_data,
  aes(x = Discount,
      y = Revenue)
) +
  geom_point()
# d) Top cities by revenue
city_rev <- aggregate(
  Revenue ~ City,
  sales_data,
  sum
)
ggplot(
  city_rev,
  aes(x = City,
      y = Revenue)
) +
  geom_bar(stat = "identity")
# e) Top products by sales
product_sales <- aggregate(
  Revenue ~ Product,
  sales_data,
  sum
)
ggplot(
  product_sales,
  aes(x = Product,
      y = Revenue)
) +
  geom_bar(stat = "identity")

# Q4) Bar Chart Scatter Plot Histogram
# -------------------------------------------------
# a) Number of orders per city
ggplot(sales_data, aes(x = City)) +
  geom_bar()
# b) Price vs Revenue
ggplot(
  sales_data,
  aes(x = Price,
      y = Revenue)
) +
  geom_point()
# c) Quantity vs Revenue
ggplot(
  sales_data,
  aes(x = Quantity,
      y = Revenue)
) +
  geom_point()
# d) Distribution of Quantity
ggplot(
  sales_data,
  aes(x = Quantity)
) +
  geom_histogram(bins = 10)
# e) Histogram by Category
ggplot(
  sales_data,
  aes(x = Revenue,
      fill = Category)
) +
  geom_histogram(bins = 20)

# -------------------------------------------------
# Q5) Box Plot Line Chart Faceted Chart
# -------------------------------------------------
# a) Revenue by Category
ggplot(
  sales_data,
  aes(x = Category,
      y = Revenue)
) +
  geom_boxplot()
# b) Price by Product
ggplot(
  sales_data,
  aes(x = Product,
      y = Price)
) +
  geom_boxplot()
# c) Revenue trend by OrderID
ggplot(
  sales_data,
  aes(x = OrderID,
      y = Revenue)
) +
  geom_line()
# d) Discount trend
ggplot(
  sales_data,
  aes(x = OrderID,
      y = Discount)
) +
  geom_line()
# e) City orders by category
ggplot(
  sales_data,
  aes(x = City)
) +
  geom_bar() +
  facet_wrap(~Category)


