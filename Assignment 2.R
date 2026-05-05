set.seed(123)
sales_data <- data.frame(
  OrderID = 1:100,
  CustomerName =
    sample(c("Rahul","Amit","Priya","Neha","Arjun","Sneha","Rohit","Kiran"),100,replace=TRUE),
  City = sample(c("Pune","Mumbai","Delhi","Bangalore","Hyderabad"),100,replace=TRUE),
  Product = sample(c("Laptop","Mobile","Tablet","Shoes","Watch"),100,replace=TRUE),
  Category = sample(c("Electronics","Fashion"),100,replace=TRUE),
  Quantity = sample(1:10,100,replace=TRUE),
  Price = sample(seq(500,50000,500),100,replace=TRUE),
  Discount = sample(c(0,5,10,15,20),100,replace=TRUE),
  PaymentMethod = sample(c("Cash","Card","UPI"),100,replace=TRUE)
)
sales_data$Revenue <- sales_data$Quantity * sales_data$Price
sales_data$DiscountAmount <- sales_data$Revenue * sales_data$Discount / 100
sales_data$FinalAmount <- sales_data$Revenue - sales_data$DiscountAmount
head(sales_data)

# Q.1 Filtering Rows
sales_data[sales_data$Price > 20000,]
sales_data[sales_data$City == "Pune",]
sales_data[sales_data$Quantity > 5,]
sales_data[sales_data$Category == "Electronics",]
sales_data[sales_data$Price > 20000 & sales_data$City == "Pune",]
  




# Q.2 Sorting Data
sales_data[order(sales_data$Price),]
sales_data[order(sales_data$Revenue),]
sales_data[order(sales_data$City),]
sales_data[order(-sales_data$Quantity),]
sales_data[order(sales_data$City,-sales_data$Revenue),]





# Q.3 Aggregation 
aggregate(Revenue~City,sales_data,sum)
aggregate(Price~Category,sales_data,mean)
aggregate(Quantity~Product,sales_data,sum)
aggregate(Revenue~City,sales_data,max)
aggregate(Revenue~Product,sales_data,min)




# Q4) Conditional Statements 
sales_data$DiscountFlag <- ifelse(sales_data$Discount > 10, "High Discount", "Normal")
sales_data$OrderCategory <- ifelse(sales_data$Revenue > 30000, "High", ifelse(sales_data$Revenue > 10000, "Medium", "Low"))

for(i in 1:nrow(sales_data)){ print(sales_data$Revenue[i])
} 
i <- 1
total <- 0
while(i<=nrow(sales_data)) {
  total <- total + sales_data$Revenue[i]
  i <- i+1
}
print(total)

sales_data[sales_data$Category != "Fashion",]
sales_data$DiscountFlag <- ifelse(sales_data$Discount > 10, "High Discount", "Normal")
sales_data$OrderCategory <- ifelse(sales_data$Revenue > 30000, "High", ifelse(sales_data$Revenue > 10000, "Medium", "Low"))

for(i in 1:nrow(sales_data)){ print(sales_data$Revenue[i])
  
} 
i <- 1
total <- 0

while(i<=nrow(sales_data)) {
  total <- total + sales_data$Revenue[i]
  i <- i+1
}
print(total)
sales_data[sales_data$Category != "Fashion",]





# Q5. Discount Analysis 
mean(sales_data$Discount)
sales_data[sales_data$Discount >10, ]
sum(sales_data$DiscountAmount)
aggregate(DiscountAmount ~ City, sales_data, sum)
table(sales_data$Discount)





# Q6. 
sales_data[sales_data$Category == "Electronics" & sales_data$Price > 20000,]
sales_data[sales_data$Category == "fashion" & sales_data$Quantity > 5, ]
sales_data[sales_data$City == "Pune" & sales_data$Discount > 10, ]
sales_data[sales_data$Revenue > 50000 & sales_data$PaymentMethod == "Cash",]
sales_data[sales_data$Price > 30000 & sales_data$Quantity >5, ]












