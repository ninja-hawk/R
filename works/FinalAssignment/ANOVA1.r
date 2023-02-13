# Put DataForTable2.1.xls in the appropriate directory
# Read data
data <- read.csv("FinalAssignment/DataForTable2.1.csv", header = TRUE)

# erase the rows which have
data <- na.omit(data)

quan <- quantile(data$Social.support)

much_low <- subset(data, Social.support < quan[2])
low <- subset(data, Social.support >= quan[2] & Social.support <= quan[3])
high <- subset(data, Social.support >= quan[3] & Social.support <= quan[4])
much_high <- subset(data, Social.support > quan[4])


# define some constant variables
a <- 4
r <- nrow(low)
n <- a * r

# define some degrees of freedom
dft <- n - 1
dfs <- a - 1
dfe <- a * (r - 1)

# compute mean of each and total
mean_much_high <- mean(much_low$Life.Ladder)
mean_high <- mean(high$Life.Ladder)
mean_low <- mean(low$Life.Ladder)
mean_much_low <- mean(much_low$Life.Ladder)
mean_total <- mean(data$Life.Ladder)

# compute the Error Sum of Suquares
ss <- r * (
  (mean_much_high - mean_total) ^ 2 +
  (mean_high - mean_total) ^ 2 +
  (mean_low - mean_total) ^ 2 +
  (mean_much_low - mean_total) ^ 2
)

# compute mean of each
var_much_high <- var(much_high$Life.Ladder)
var_high <- var(high$Life.Ladder)
var_low <- var(low$Life.Ladder)
var_much_low <- var(much_low$Life.Ladder)

# compute the Treatment Sum of Squares from unbiased variance
# NOTICE: the function "var()" return unbiased variance
se <- (r - 1) * (var_much_high + var_high + var_low + var_much_low)

# compute the Total Sum of Squares
st <- ss + se

# compute the Treatment Mean of Suquare
ma <- ss / dfs
me <- se / dfe

# compute the Test Static
f <- ma / me

# compute the F-value
qf <- qf(0.99, dfs, dfe)

print("ss")
print(ss)
print("se")
print(se)
print("st")
print(st)
print("dfs")
print(dfs)
print("dfe")
print(dfe)
print("dft")
print(dft)
print("ma")
print(ma)
print("me")
print(me)
print("F")
print(f)
print("qf")
print(qf)