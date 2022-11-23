# Put ASIA2021.txt in the same directory
# Read data
asia <- read.table("lecture_3/ASIA2021.txt", header = TRUE)



#Correlation
r <- cor(asia$Ladder, asia$Positive)
#Length of Data
n <- length(asia$Ladder)
#Pearson Correlation
#The parameter to test
t <- r / (sqrt((1 - r^2) / (n - 2)))

print(t)

# Two-tailed t tests
# The example below shows α = 0.01
# in the two-tailed tests so positive tail is 0.995
# The degree of freedom is n- 2 (here is n = 25)
qt <- qt(0.995, n - 2)

print(qt)

print("Beacause T > qt, H0 is rejected")