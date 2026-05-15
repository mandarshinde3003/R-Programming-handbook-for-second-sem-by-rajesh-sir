# Create sample sales dataset

sales_data <- data.frame(
  OrderID = 1:10,
  
  CustomerName = c(
    "Rahul","Amit","Priya","Neha","Arjun",
    "Sneha","Rohit","Kiran","Pooja","Vikas"
  ),
  
  City = c(
    "Pune","Mumbai","Delhi","Bangalore","Hyderabad",
    "Pune","Mumbai","Delhi","Bangalore","Hyderabad"
  ),
  
  Product = c(
    "Laptop","Mobile","Tablet","Shoes","Watch",
    "Laptop","Mobile","Tablet","Shoes","Watch"
  ),
  
  Category = c(
    "Electronics","Electronics","Electronics",
    "Fashion","Fashion",
    "Electronics","Electronics","Electronics",
    "Fashion","Fashion"
  ),
  
  Quantity = c(
    2,1,3,5,2,
    1,4,2,3,1
  ),
  
  Price = c(
    50000,25000,15000,3000,5000,
    52000,27000,14000,3500,6000
  )
)

print(sales_data)

write.csv(sales_data, "C:\\Class notes\\Data Science\\Sem 2\\Rajesh sir\\Practical book\\sales_data.csv", row.names = FALSE)

install.packages("writexl")
library(writexl)
write_xlsx(sales_data, "C:\\Class notes\\Data Science\\Sem 2\\Rajesh sir\\Practical book\\sales_data.xlsx")
# --------------------------------------------------------------------------------------------------------------------------------
# Q.1 Importing and Exploring Data from CSV file and Excel File

# a) Import dataset from CSV file and Excel file
library(readxl)
sales_data <- read.csv("C://Class notes//Data Science//Sem 2//Rajesh sir//Practical book//sales_data.csv")
sales_data_Excel <- read_excel("C://Class notes//Data Science//Sem 2//Rajesh sir//Practical book//sales_data.xlsx")

# b) Display first 10 rows
head(sales_data, 10)

# c) Display last 10 rows
tail(sales_data, 10)

# d) Check structure of dataset
str(sales_data)

# e) Display summary statistics
summary(sales_data)


# ---------------------------------------------------------------------------------------------------------------------------------
# Q.2 Exporting Data

# a) Export dataset to CSV file
write.csv(sales_data, "output_sales_data.csv", row.names = FALSE)

# b) Export dataset to Excel file
library(writexl)
write_xlsx(sales_data, "output_sales_data.xlsx")

# c) Export only Electronics category
electronics_data <- sales_data[sales_data$Category == "Electronics", ]
write.csv(electronics_data, "electronics_data.csv", row.names = FALSE)

# d) Export customers from Pune
pune_customers <- sales_data[sales_data$City == "Pune", ]
write.csv(pune_customers, "pune_customers.csv", row.names = FALSE)

# e) Export top 20 rows
top20 <- head(sales_data, 20)
write.csv(top20, "top20.csv", row.names = FALSE)



# ------------------------------------------------------------------------------------------------------------------------------
# Q.3 Modifying Data and Subset Creation

# 1) Create Profit column
sales_data$Profit <- sales_data$Revenue * 0.20
head(sales_data)

# 2) Rename FinalAmount column
names(sales_data)[names(sales_data) == "FinalAmount"] <- "TotalAmount"
head(sales_data)

# 3) Remove DiscountAmount column
sales_data$DiscountAmount <- NULL
head(sales_data)

# 4) Add Tax column
sales_data$Tax <- sales_data$Revenue * 0.18
head(sales_data)

# 5) Select orders with Quantity > 5 using subset
subset(sales_data, Quantity > 5)



# ---------------------------------------------------------------------------------------------------------------------------------------
# Q.4 Column Selection

# 1) Select CustomerName and Product columns
sales_data[, c("CustomerName", "Product")]

# 2) Select first 5 columns
sales_data[, 1:5]

# 3) Select numeric columns
sales_data[sapply(sales_data, is.numeric)]

# 4) Remove Discount column
sales_data$Discount <- NULL
head(sales_data)

# 5) Select specific columns using index
sales_data[, c(2,4,6)]



# -------------------------------------------------------------------------------------------------------------------------------------------------
# Q.5 Sorting Data

# 1) Sort by City alphabetically
sales_data[order(sales_data$City), ]

# 2) Sort by City and Revenue
sales_data[order(sales_data$City, -sales_data$Revenue), ]

# 3) Average price by city
aggregate(Price ~ City, sales_data, mean)

# 4) Maximum order value by city
aggregate(Revenue ~ City, sales_data, max)

# 5) Total quantity sold by city
aggregate(Quantity ~ City, sales_data, sum)



