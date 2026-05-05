
# 1.  Displays the maximum, minimum, and average value of the results. 

a <- as.numeric(readline("Enter first number: "))
b <- as.numeric(readline("Enter second number: "))

sum_val <- a + b
diff_val <- a - b 
prod_val <- a * b
div_val <- a / b
power_val <- a ^ b
mod_val <- a %% b

result <- c(sum_val,diff_val,prod_val,div_val,power_val,mod_val)
print(result)

cat("Maximum:", max(result),"\n")
cat("Minimum:", min(result),"\n")
cat("Average:", mean(result,"\n"))





# 2. Random Numbers and show in the data frame
n <- as.numeric(readline("Enter how many number: "))

if(is.na(n) || n <= 0){
  stop("Invlaid input. Please enter a valid positive number.")
}else{nums <- sample(1:100,n)}

sqrt_val <- sqrt(nums)
round_val <- round(sqrt_val,2)
ceil_val <- ceiling(sqrt_val)
floor_val <- floor(sqrt_val)

df <- data.frame(
  Numbers = nums,
  SquareRoots = sqrt_val,
  Rounded = round_val,
  Ceiling = ceil_val,
  Floor = floor_val
)
print(df)





# 3. Combines the name and its length into a formatted sentence. 

n <- as.numeric(readline("Enter number of names: "))
names <- character(n)
for (i in 1:n) {
  names[i] <- readline(paste("Enter name", i, ":"))
}
upper_name <- toupper(names)
lengths <- nchar(upper_name)
result <- paste("Name:", upper_name, "- Lenght:",lengths)
print(result)





# 4. create vector and Displays results in a table format. 

nums <- 1:200
filtered <- nums[nums %% 3==0 & nums %% 5==0]
square_root <- sqrt(filtered)
square_val <- filtered ^ 2
result <- data.frame(
  Number = filtered,
  SquareRoot = square_root,
  Square = square_val
)
print(result)





# 5. Write an R program to evaluate a quadratic equation for given a, b, c values and return real or complex ro

a <- as.numeric(readline("Enter a: "))
b <- as.numeric(readline("Enter b: "))
c <- as.numeric(readline("Enter c: "))

D <- b^2 - 4 * a * c

if(D > 0){
  root1 <- (-b + sqrt(D)) / (2*a)
  root2 <- (-b - sqrt(D)) / (2*a)
  print(c(root1,root2))
}else if (D==0){
  root <- -b/(2*a)
  print(root)
}else {
  cat("Complex roots\n")
}













