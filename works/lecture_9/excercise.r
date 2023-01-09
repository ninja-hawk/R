# Put 3gswnr.txt in an appropriate directory
# Read data
game3 <- read.table("lecture_9/3gswnr.txt", header = TRUE)

print("ANOVA")

# Load dplyr
library(dplyr)

# get how many kinds of level(GS) in the data
gs_levels <- game3 %>%
  group_by(GS) %>%
  summarize(total_count = n(), .groups = "drop")

print(gs_levels)

# define some constant variables
a <- length(gs_levels$GS)
r <- gs_levels$total_count[1]
n <- a * r

# define some degrees of freedom
dft <- n - 1
dfs <- a - 1
dfe <- a * (r - 1)

# make some subsets
grass <- subset(game3, GS == 1)
clay <- subset(game3, GS == 2)
hard <- subset(game3, GS == 3)

# compute mean of each and total
mean_grass <- mean(grass$WNR)
mean_clay <- mean(clay$WNR)
mean_hard <- mean(hard$WNR)
mean_total <- mean(game3$WNR)

# compute the Error Sum of Suquares
ss <- r * (
  (mean_grass - mean_total) ^ 2 +
  (mean_clay - mean_total) ^ 2 +
  (mean_hard - mean_total) ^ 2
)

# compute mean of each
var_grass <- var(grass$WNR)
var_clay <- var(clay$WNR)
var_hard <- var(hard$WNR)

# compute the Treatment Sum of Squares from unbiased variance
# NOTICE: the function "var()" return unbiased variance
se <- (r - 1) * (var_grass + var_clay + var_hard)

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

grass_vs_clay <- (mean_grass - mean_clay) /
                sqrt(2 * me / r)
grass_vs_hard <- (mean_grass - mean_hard) /
                sqrt(2 * me / r)
clay_vs_hard <- (mean_clay - mean_hard) /
                sqrt(2 * me / r)

print(grass_vs_clay)
print(grass_vs_hard)
print(clay_vs_hard)

# compute the t-values
qt005 <- qt(0.975, dfe)
qt001 <- qt(0.995, dfe)

print(qt005)
print(qt001)