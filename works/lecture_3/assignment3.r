# Put ASIA2021.txt in the same directory
# Read data
asia <- read.table("lecture_3/ASIA2021.txt", header = TRUE)

#Two-tailed t tests
qt <- qt(0.995,n-2)

print(qt)

#Correlation
r <- cor(asia$Ladder, asia$Positive)
#Length of Data
n <- length(asia$Ladder)
#Pearson Correlation
t <- r / (sqrt((1 - r^2) / (n - 2)))

print(t)