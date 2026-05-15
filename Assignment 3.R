set.seed(123)
sales_data <- data.frame(
  OrderID = 1:200,
  CustomerName =
    sample(c("Rahul","Amit","Priya","Neha","Arjun","Sneha","Rohit","Kiran"),200,replace=TRUE),
  City = sample(c("Pune","Mumbai","Delhi","Bangalore","Hyderabad"),200,replace=TRUE),
  Product = sample(c("Laptop","Mobile","Tablet","Shoes","Watch"),200,replace=TRUE),
  Category = sample(c("Electronics","Fashion"),200,replace=TRUE),
  Quantity = sample(1:10,200,replace=TRUE),
  Price = sample(seq(500,50000,500),200,replace=TRUE),
  Discount = sample(c(0,5,10,15,20),200,replace=TRUE),
  PaymentMethod = sample(c("Cash","Card","UPI"),200,replace=TRUE)
)


# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Q.1 Problems on Recoding Variables, Sorting and New Variable Creation 

# a) Create OrderSize (Small, Medium, Large)
sales_data$Revenue <- sales_data$Quantity * sales_data$Price
sales_data$DiscountAmount <- sales_data$Revenue * sales_data$Discount/100
sales_data$FinalAmount <- sales_data$Revenue - sales_data$DiscountAmount 

# b) Sort by Quantity descending
sorted_quantity <- sales_data[order(-sales_data$Quantity), ]
print(sorted_quantity)
  
# c) Sort by City and Revenue
sorted_cit_revenue <- sales_data[order(sales_data$City, sales_data$Revenue), ]
print(sorted_cit_revenue)  
  
# d) Create Tax column
sales_data$Tax <- sales_data$FinalAmount * 0.18
print(sales_data$Tax)

# e) Create TotalBill column 
sales_data$TotalBill <- sales_data$FinalAmount + sales_data$Tax
print(sales_data$TotalBill)

head(sales_data)



# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Q.2 Problems on dplyr select() and filter()
library(dplyr)

# a) Select CustomerName and Product
select(sales_data, CustomerName, Product)

# b) Select numeric columns
select_if(sales_data, is.numeric)

# c) Remove Discount column
select(sales_data, -Discount)

# d) Filter Pune customers
filter(sales_data, City == "Pune")

# e) Filter multiple conditions (Price > 20000 and Quantity > 5)
filter(sales_data, Price > 20000 & Quantity > 5)




# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Q.3 Problems on Using mutate() and arrange() 
install.packages("dplyr")
library(dplyr)

# a) Create Tax variable 
sales_data <- sales_data %>%
  mutate(Tax = FinalAmount * 0.18)
print(sales_data$Tax)

# b) Create Profit variable 
sales_data <- sales_data %>%
  mutate(Profit = FinalAmount * 0.20)
print(sales_data$Profit)

# c) Create OrderCategory
sales_data <- sales_data %>%
  mutate(OrderCategory = ifelse(Revenue > 30000, "High",
                                ifelse(Revenue > 10000, "Medium", "Low")))

# d) Create DiscountCategory
sales_data <- sales_data %>%
  mutate(DiscountCategory = ifelse(Discount > 10, "High Discount",
                                   "Low Discount"))

# e) Create PaymentType variable
sales_data <- sales_data %>%
  mutate(PaymentType = PaymentMethod)

# f) Sort by Revenue descending by using arrange() 
arrange(sales_data, desc(Revenue))



# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Q.4 Problems on Using group_by() and summarise()

# a) Total revenue by city
sales_data %>%
  group_by(City) %>%
  summarise(TotalRevenue = sum(Revenue))

# b) Average price by product
sales_data %>%
  group_by(Product) %>%
  summarise(AveragePrice = mean(Price))

# c) Total quantity sold by product
sales_data %>%
  group_by(Product) %>%
  summarise(TotalQuantity = sum(Quantity))

# d) Maximum revenue by city
sales_data %>%
  group_by(City) %>%
  summarise(MaxRevenue = max(Revenue))

# e) Minimum revenue by product
sales_data %>%
  group_by(Product) %>%
  summarise(MinRevenue = min(Revenue))



# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Q.5 Problems on Customer Analysis

# 1) Count orders per customer
sales_data %>%
  group_by(CustomerName) %>%
  summarise(OrderCount = n())
# 2) Total revenue per customer
sales_data %>%
  group_by(CustomerName) %>%
  summarise(TotalRevenue = sum(Revenue))

# 3) Average order value
mean(sales_data$Revenue)

# 4) Top customer
sales_data %>%
  group_by(CustomerName) %>%
  summarise(TotalRevenue = sum(Revenue)) %>%
  arrange(desc(TotalRevenue))

# 5) Sort customers by revenue
sales_data %>%
  group_by(CustomerName) %>%
  summarise(TotalRevenue = sum(Revenue)) %>%
  arrange(desc(TotalRevenue))




               
               
               