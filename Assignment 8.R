# =================================================
# INSTALL AND LOAD REQUIRED PACKAGE
# =================================================
install.packages("readxl")
library(readxl)

#--------------------------------------------------------------------------------------------------------------------------------------------------------
# Q.1 A Retail Sales Analysis (Dataset: sales.csv)
# Create Sales Dataset
set.seed(123)
sales <- data.frame(
  
  Product = sample(
    c("Laptop","Mobile","Tablet",
      "Shoes","Watch"),
    100,
    replace = TRUE
  ),
  
  Category = sample(
    c("Electronics","Fashion"),
    100,
    replace = TRUE
  ),
  
  Quantity = sample(
    1:100,
    100,
    replace = TRUE
  ),
  
  Price = sample(
    seq(500,50000,500),
    100,
    replace = TRUE
  )
)
# Create Revenue
sales$Revenue <-
  sales$Quantity *
  sales$Price
# a) First 10 Records
head(sales, 10)
# b) Total Revenue for Each Product
aggregate(
  Revenue ~ Product,
  sales,
  sum
)
# c) Top 5 Products with Highest Revenue
top_products <-
  aggregate(
    Revenue ~ Product,
    sales,
    sum
  )
top_products[
  order(-top_products$Revenue),
][1:5, ]
# d) Quantity Sold Greater than 50
subset(
  sales,
  Quantity > 50
)
# e) Average Product Price by Category
aggregate(
  Price ~ Category,
  sales,
  mean
)


# ------------------------------------------------------------------------------------------------------------------------------------
# =================================================
# Q2) E-COMMERCE CUSTOMER ANALYSIS
# =================================================
# Create Customer Dataset
customers <- data.frame(
  
  CustomerID = 1:100,
  
  Gender = sample(
    c("Male","Female"),
    100,
    replace = TRUE
  ),
  
  Age = sample(
    18:70,
    100,
    replace = TRUE
  ),
  
  PurchaseAmount = sample(
    1000:50000,
    100,
    replace = TRUE
  )
)

# a) First 10 Records
head(customers, 10)
# b) Average Purchase Amount
mean(customers$PurchaseAmount)
# c) Customers Above Average Purchase
avg_purchase <-
  mean(customers$PurchaseAmount)
subset(
  customers,
  PurchaseAmount > avg_purchase
)

# d) Count Male and Female Customers
table(customers$Gender)
# e) Create Age Groups
customers$AgeGroup <-
  ifelse(
    customers$Age < 25,
    "Youth",
    
    ifelse(
      customers$Age <= 50,
      "Adult",
      "Senior"
    )
  )
head(customers)
# f) Top 10 Highest Spending Customers
customers[
  order(-customers$PurchaseAmount),
][1:10, ]


#--------------------------------------------------------------------------------------------------------------------------------------
# =================================================
# Q3) BANKING TRANSACTION ANALYSIS
# =================================================
# Create Transactions Dataset
transactions <- data.frame(
  
  AccountID = 1:100,
  
  Type = sample(
    c("Deposit","Withdrawal"),
    100,
    replace = TRUE
  ),
  
  Amount = sample(
    1000:20000,
    100,
    replace = TRUE
  )
)
# a) First 10 Records
head(transactions, 10)
# b) Total Deposits
sum(
  transactions$Amount[
    transactions$Type == "Deposit"
  ]
)
# c) Total Withdrawals
sum(
  transactions$Amount[
    transactions$Type == "Withdrawal"
  ]
)
# d) Transactions Above 10000
subset(
  transactions,
  Amount > 10000
)
# e) Average Transaction Amount
mean(transactions$Amount)
# f) Count Transactions by Type
table(transactions$Type)



#-------------------------------------------------------------------------------------------------------------------------------------
# =================================================
# Q4) HEALTHCARE PATIENT DATASET
# =================================================
# Create Patient Dataset
patients <- data.frame(
  
  PatientID = 1:100,
  
  Age = sample(
    1:90,
    100,
    replace = TRUE
  ),
  
  BloodPressure = sample(
    90:180,
    100,
    replace = TRUE
  ),
  
  Temperature = runif(
    100,
    35,
    40
  )
)
# a) First 10 Records
head(patients, 10)
# b) Patients with Blood Pressure > 140
subset(
  patients,
  BloodPressure > 140
)
# c) Patients with Fever > 37°C
subset(
  patients,
  Temperature > 37
)
# d) Average Patient Age
mean(patients$Age)
# e) Maximum and Minimum Blood Pressure
max(patients$BloodPressure)
min(patients$BloodPressure)
# f) Patients Above 60 Years
sum(patients$Age > 60)




#------------------------------------------------------------------------------------------------------------------------------------------
# =================================================
# Q5) STUDENT PERFORMANCE DATASET
# =================================================
# Create Student Dataset
students <- data.frame(
  
  StudentID = 1:100,
  
  Subject = sample(
    c("Maths","Science","English"),
    100,
    replace = TRUE
  ),
  
  Marks = sample(
    1:100,
    100,
    replace = TRUE
  )
)
# a) First 10 Records
head(students, 10)
# b) Students Scoring Above 80
subset(
  students,
  Marks > 80
)
# c) Average Marks by Subject
aggregate(
  Marks ~ Subject,
  students,
  mean
)
# d) Top Scoring Student
students[
  which.max(students$Marks),
]
# e) Failed Students
subset(
  students,
  Marks < 40
)
# f) Count Students by Subject
table(students$Subject)



# ---------------------------------------------------------------------------------------------------------------------------------------------
# =================================================
# Q6) SOCIAL MEDIA ANALYTICS DATASET
# =================================================
# Create Posts Dataset
posts <- data.frame(
  
  PostID = 1:100,
  
  Likes = sample(
    1:80,
    100,
    replace = TRUE
  ),
  
  Comments = sample(
    1:40,
    100,
    replace = TRUE
  ),
  
  Shares = sample(
    1:20,
    100,
    replace = TRUE
  )
)
# Create Engagement Column
posts$Engagement <-
  posts$Likes +
  posts$Comments +
  posts$Shares
# a) First 10 Records
head(posts, 10)
# b) Total Engagement Per Post
posts[, c("PostID", "Engagement")]
# c) Posts with Engagement > 500
subset(
  posts,
  Engagement > 100
)
# d) Most Liked Post
posts[
  which.max(posts$Likes),
]
# e) Average Engagement
mean(posts$Engagement)
# f) Posts with Low Engagement < 100
subset(
  posts,
  Engagement < 100
)