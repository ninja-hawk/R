# Put ASIA2021.txt in an appropriate directory
# Read data
asia <- read.table("lecture_3/ASIA2021.txt", header = TRUE)

# Question1

# get mean, variance, and length of Positive
pmean <- mean(asia$Positive)
pvar <- var(asia$Positive)
n <- length(asia$Positive)
# get mean and variance of Negative
nmean <- mean(asia$Negative)
nvar <- var(asia$Negative)

# calculate the correlation between Positive and Negative
cor <- cor(asia$Positive, asia$Negative)
t_paired <- (pmean - nmean) /
  sqrt((pvar + nvar -
  2 * cor * sqrt(pvar) * sqrt(nvar)) / n)
print(t_paired)

# one-tailed test
print(qt(0.95, n - 1))

print("The mean of Positive is siginificantly larger than that of Negative")
print("t = 7.819177 > 1.710882(p < 0.05) and ")
print("H0 (pmean = nmean) was rejected")


# Question2
print("------------------------------------------------------")

# gete median of LogGDP
m <- median(asia$LogGDP)

# seperate counties into higher GDP or lower GDP
high <- subset(asia, LogGDP > m)
low <- subset(asia, LogGDP < m)

# get mean, variance, and length of Positive that all higher have
high_mean <- mean(high$Positive)
high_var <- var(high$Positive)
half_n <- length(high$Positive)
print(high_mean)

# get mean, variance, and length of FSP that all LOSERS have
low_mean <- mean(low$Positive)
low_var <- var(low$Positive)
print(low_mean)

#calculate t-value by the equation below
#NOTICE: the number of high = the number of lose = 12
t_independent <- (high_mean - low_mean) / sqrt((high_var + low_var) / half_n)
print(t_independent)

#two-tailed test
print(qt(0.975, 2 * half_n - 2))

print("The mean of Posotove is NOT siginificantly different ")
print("between the higher and the lower")
print("t = 1.497883 < 2.073873 (p < 0.05) and ")
print("H0 (high_mean = low_mean) was NOT rejected")