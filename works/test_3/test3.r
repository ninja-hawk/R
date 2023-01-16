print("Two-way-ANOVA")

# degrees of freedom
dfa <- 2
dfab <- 4
dfe <- 45

# compute the F-value
qf_a_005 <- qf(0.95, dfa, dfe)
qf_a_001 <- qf(0.99, dfa, dfe)
qf_ab_005 <- qf(0.95, dfab, dfe)
qf_ab_001 <- qf(0.99, dfab, dfe)

print("qf_a_005")
print(qf_a_005)
print("qf_a_001")
print(qf_a_001)
print("qf_ab_005")
print(qf_ab_005)
print("qf_ab_001")
print(qf_ab_001)


print("Multiple Comparison")

# the variables needed
r <- 6
ms_e <- 1.14

# means of some cobinations
mean_a1b1 <- 14.57
mean_a1b2 <- 15.87
mean_a2b2 <- 18.03
mean_a3b1 <- 18.50

# compute the Test Static
a1b1_vs_a1b2 <- (mean_a1b2 - mean_a1b1) /
                sqrt(2 * ms_e / r)
a2b2_vs_a3b1 <- (mean_a3b1 - mean_a2b2) /
                sqrt(2 * ms_e / r)

print("a1b1_vs_a1b2")
print(a1b1_vs_a1b2)
print("a2b2_vs_a3b1")
print(a2b2_vs_a3b1)

# compute the t-values
qt005 <- qt(0.975, dfe)
qt001 <- qt(0.995, dfe)

print("qt005")
print(qt005)
print("qt001")
print(qt001)