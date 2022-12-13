# Put ASIA2021.txt in an appropriate directory
# Read data
asia <- read.table("lecture_3/ASIA2021.txt", header = TRUE)

# Question1

# get mean, variance, and length of Support
smean <- mean(asia$Support)
svar <- var(asia$Support)
n <- length(asia$Support)

# print the number of Support (=Choice)
print(n)

# get mean and variance of Choice
cmean <- mean(asia$Choice)
cvar <- var(asia$Choice)

# calculate the correlation between Support and Choice
cor <- cor(asia$Support, asia$Choice)
t_paired <- (cmean - smean) /
  sqrt((svar + cvar -
  2 * cor * sqrt(svar) * sqrt(cvar)) / n)
print(t_paired)

# two-tailed test
print(qt(0.975, n - 1))

# Question2
print("------------------------------------------------------")

sample_size <- 30
# two-tailed test
print(qt(0.975, 2 * sample_size - 2))
print(qt(0.995, 2 * sample_size - 2))