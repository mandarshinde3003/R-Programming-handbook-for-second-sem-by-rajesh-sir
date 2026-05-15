# -------------------------------------------------
# Install and Load Required Package
# -------------------------------------------------
install.packages("readxl")
library(readxl)
# -------------------------------------------------
# Create Dataset
# -------------------------------------------------
set.seed(123)
data <- data.frame(
  CustomerID = 1:10,
  Name = c(
    "Rahul","Amit","Priya","Neha","Arjun",
    "Sneha","Rohit","Kiran","Pooja","Vikas"
  ),
  Age = sample(20:60, 10),
  Income = sample(
    30000:100000,
    10
  ),
  Membership = sample(
    c("Silver","Gold","Platinum"),
    10,
    replace = TRUE
  ),
  SpendingScore = sample(
    1:100,
    10
  )
)
# Display Dataset
data


# -------------------------------------------------
# Q1) Customer Object Modeling (S3 System)
# -------------------------------------------------
# 1. Create S3 class
customer1 <- list(
  Name = data$Name[1],
  Age = data$Age[1],
  Income = data$Income[1],
  Membership = data$Membership[1]
)
class(customer1) <- "Customer"
# 2. Attributes assigned above
# 3. Create custom print method
print.Customer <- function(x)
{
  cat(
    "Name :", x$Name,
    "\nAge :", x$Age,
    "\nIncome :", x$Income,
    "\nMembership :", x$Membership
  )
}
# 4. Display customer details
print(customer1)
# 5. Modify one attribute
customer1$Income <- 90000
print(customer1)


# -------------------------------------------------
# Q2) Advanced Customer Class (S4 System)
# -------------------------------------------------
# 1. Define S4 class
setClass(
  "Customer",
  slots = list(
    Age = "numeric",
    Income = "numeric",
    Membership = "character"
  )
)
# 2. Create object
cust_obj <- new(
  "Customer",
  Age = 30,
  Income = 75000,
  Membership = "Gold"
)
# 3. Create show method
setMethod(
  "show",
  "Customer",
  function(object)
  {
    cat(
      "Age :", object@Age,
      "\nIncome :", object@Income,
      "\nMembership :", object@Membership
    )
  }
)


cust_obj
# -------------------------------------------------
# Q3) Object Referencing and Copy Behavior
# -------------------------------------------------
# 1. Assign new variable
copy_data <- data
# 2. Modify copied dataset
copy_data$Income[1] <- 99999
# 3. Check original dataset
data$Income[1]
copy_data$Income[1]
# 4. Compare objects
identical(data, copy_data)
# -------------------------------------------------
# Q4) Memory Optimization Study
# -------------------------------------------------
# 1. Create large object
large_object <- rep(data, 1000)
# 2. Check memory usage
object.size(large_object)
# 3. Remove object
rm(large_object)
# 4. Garbage collection
gc()


# -------------------------------------------------
# Q5) List-Based Customer Object System
# -------------------------------------------------
# 1. Convert dataset into list
customer_list <- split(data, data$CustomerID)
# 2. Assign S3 class
class(customer_list[[1]]) <- "Customer"
# 3. Access nested values
customer_list[[1]]$Name
# 4. Modify customer data
customer_list[[1]]$Income <- 85000
# 5. Display structure
str(customer_list)


# -------------------------------------------------
# Q6) Class Inspection and Documentation
# -------------------------------------------------
# 1. Check class
class(data)
# 2. Structure
str(data)
# 3. Attributes
attributes(data)
# 4. Summary
summary(data)
# -------------------------------------------------
# Q7) S4 Slot Manipulation
# -------------------------------------------------
# 1. Create S4 class
setClass(
  "SpendingProfile",
  slots = list(
    SpendingScore = "numeric",
    Income = "numeric"
  )
)

# 2. Create object
profile <- new(
  "SpendingProfile",
  SpendingScore = 80,
  Income = 65000
)
# 3. Display object
profile
# 4. Access slots
profile@Income
profile@SpendingScore
# 5. Modify slots
profile@Income <- 90000
profile
# -------------------------------------------------
# Q8) Advanced Data Structure Integration
# -------------------------------------------------
# 1. Create nested list
nested_obj <- list(
  Dataset = data,
  Summary = summary(data)
)
# 2. Access inner elements
nested_obj$Dataset
nested_obj$Summary
# 3. Modify nested structure
nested_obj$Dataset$Income[1] <- 88888
# 4. Apply class
class(nested_obj) <- "CompanyData"
# 5. Display structure
str(nested_obj)


# -------------------------------------------------
# Q9) Object-Oriented Data Analysis
# -------------------------------------------------
# 1. Convert dataset into S3 object
analysis_obj <- data
class(analysis_obj) <- "CustomerAnalysis"
# 2. Create function
average_income <- function(x)
{
  mean(x$Income)
}
# 3. Apply function
average_income(analysis_obj)
# 4. Add new attribute
attr(
  analysis_obj,
  "Department"
) <- "Sales"
# 5. Validate object
class(analysis_obj)
attributes(analysis_obj)


# -------------------------------------------------
# Q10) Enterprise-Level Object System Design
# -------------------------------------------------
# 1. Create S3 version
s3_customer <- customer1
# Create S4 version
s4_customer <- cust_obj
# 2. Compare systems
class(s3_customer)
class(s4_customer)
# 3. Summary statistics
summary(data)
# 4. Documentation
cat(
  "\nS3 is flexible and informal.",
  "\nS4 is strict and supports validation."
)