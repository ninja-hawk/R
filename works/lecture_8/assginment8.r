# Put Asia2021_SR.txt in an appropriate directory
# Read data
asia2021sr <- read.table("lecture_8/Asia2021_SR.txt", header = TRUE)

print("ANOVA")

# Load dplyr
library(dplyr)

# get how many kinds of Rank in the data
rank_levels <- asia2021sr %>%
  group_by(Rank) %>%
  summarize(total_count = n(), .groups = "drop")

print(rank_levels)

# define some constant variables
a <- length(rank_levels$Rank)
r <- rank_levels$total_count[1]
n <- a * r

# define some degrees of freedom
dft <- n - 1
dfs <- a - 1
dfe <- a * (r - 1)

# make some subsets
high <- subset(asia2021sr, Rank == "hi")
middle <- subset(asia2021sr, Rank == "md")
low <- subset(asia2021sr, Rank == "lo")

# compute mean of each and total
mean_high <- mean(high$Ladder)
mean_middle <- mean(middle$Ladder)
mean_low <- mean(low$Ladder)
mean_total <- mean(asia2021sr$Ladder)

# compute the Error Sum of Suquares
ss <- r * (
  (mean_high - mean_total) ^ 2 +
  (mean_middle - mean_total) ^ 2 +
  (mean_low - mean_total) ^ 2
)

# compute mean of each
var_high <- var(high$Ladder)
var_middle <- var(middle$Ladder)
var_low <- var(low$Ladder)

# compute the Treatment Sum of Squares from unbiased variance
# NOTICE: the function "var()" return unbiased variance
se <- (r - 1) * (var_high + var_middle + var_low)

# compute the Total Sum of Squares
st <- ss + se

# compute the Treatment Mean of Suquare
ma <- ss / dfs
me <- se / dfe

# compute the Test Static
f <- ma / me

# compute the F-value
qf <- qf(0.99, dfs, dfe)

print(f)
print(qf)

print("Multiple Comparison")

high_vs_middle <- (mean_high - mean_middle) /
                sqrt(2 * me / r)
high_vs_low <- (mean_high - mean_low) /
                sqrt(2 * me / r)
middle_vs_low <- (mean_middle - mean_low) /
                sqrt(2 * me / r)

print(high_vs_middle)
print(high_vs_low)
print(middle_vs_low)

# compute the t-values
qt005 <- qt(0.975, dfe)
qt001 <- qt(0.995, dfe)

print(qt005)
print(qt001)