# Put Asis2021_SR.txt and Europe2021_SR.txt in an appropriate directory
# Read data
asia2021sr <- read.table("lecture_9/Asia2021_SR.txt", header = TRUE)
europe2021sr <- read.table("lecture_11/Europe2021_SR.txt", header = TRUE)

print("Two-way-ANOVA")

# Load dplyr
library(dplyr)

# get how many kinds of level(GS) in the data
levels <- asia2021sr %>%
  group_by(Rank) %>%
  summarize(total_count = n(), .groups = "drop")

print(levels)

# define some constant variables
a <- length(levels$Rank)
b <- 2 # Asia and Europe
r <- levels$total_count[1]
n <- a * b * r

# print(a)
# print(b)
# print(r)
# print(n)

# define some degrees of freedom
dfr <- a - 1
dfa <- b - 1
dfr_a <- dfr * dfa
dfe <- a * b * (r - 1)
dft <- n - 1

print("dfr")
print(dfr)
print("dfa")
print(dfa)
print("dfr_a")
print(dfr_a)
print("dfe")
print(dfe)
print("dft")
print(dft)

# make some subsets by r
hi_asia <- subset(asia2021sr, Rank == "hi")
md_asia <- subset(asia2021sr, Rank == "md")
lo_asia <- subset(asia2021sr, Rank == "lo")
hi_europe <- subset(europe2021sr, Rank == "hi")
md_europe <- subset(europe2021sr, Rank == "md")
lo_europe <- subset(europe2021sr, Rank == "lo")

# compute means of each subset
mean_hi_asia <- mean(hi_asia$Ladder)
mean_md_asia <- mean(md_asia$Ladder)
mean_lo_asia <- mean(lo_asia$Ladder)
mean_hi_europe <- mean(hi_europe$Ladder)
mean_md_europe <- mean(md_europe$Ladder)
mean_lo_europe <- mean(lo_europe$Ladder)

# print(mean_hi_asia)
# print(mean_hi_europe)
# print(mean_md_asia)
# print(mean_md_europe)
# print(mean_lo_asia)
# print(mean_lo_europe)

# make some subsets of levels
hi <- rbind(hi_asia, hi_europe)
md <- rbind(md_asia, md_europe)
lo <- rbind(lo_asia, lo_europe)

# compute means of levels
mean_hi <- mean(hi$Ladder)
mean_md <- mean(md$Ladder)
mean_lo <- mean(lo$Ladder)
mean_asia <- mean(asia2021sr$Ladder)
mean_europe <- mean(europe2021sr$Ladder)
mean_total <- mean(rbind(asia2021sr, europe2021sr)$Ladder)

# print(mean_hi)
# print(mean_md)
# print(mean_lo)
# print(mean_asia)
# print(mean_europe)
# print(mean_total)

# compute Total Sum of Squares
st <- sum((rbind(asia2021sr, europe2021sr)$Ladder - mean_total)^2)
print("st")
print(st)

mean <- c(
  mean_hi_asia,
  mean_md_asia,
  mean_lo_asia,
  mean_hi_europe,
  mean_md_europe,
  mean_lo_europe
)
sra <- sum((mean - mean_total)^2) * r
print("sra")
print(sra)

# compute Error Sum of Squares
error <- c(
  (hi_asia$Ladder - mean_hi_asia)^2,
  (md_asia$Ladder - mean_md_asia)^2,
  (lo_asia$Ladder - mean_lo_asia)^2,
  (hi_europe$Ladder - mean_hi_europe)^2,
  (md_europe$Ladder - mean_md_europe)^2,
  (lo_europe$Ladder - mean_lo_europe)^2
)
se <- sum(error)
print("se")
print(se)

# compute each Treatment Sum of Squares
rank <- c(mean_hi, mean_md, mean_lo)
sr <- sum((rank - mean_total)^2) * b * r
print("sr")
print(sr)

area <- c(mean_asia, mean_europe)
sa <- sum((area - mean_total)^2) * a * r
print("sa")
print(sa)

sr_a <- sra - (sr + sa)
print("sr_a")
print(sr_a)

# compute Mean Square
ms_r <- sr / dfr
ms_a <- sa / dfa
ms_r_a <- sr_a / dfr_a
ms_e <- se / dfe

print("ms_r")
print(ms_r)
print("ms_a")
print(ms_a)
print("ms_r_a")
print(ms_r_a)
print("ms_e")
print(ms_e)

# compute the Test Static
f_r <- ms_r / ms_e
f_a <- ms_a / ms_e
f_r_a <- ms_r_a / ms_e

print("f_r")
print(f_r)
print("f_a")
print(f_a)
print("f_r_a")
print(f_r_a)

# compute the F-value
qf_r_005 <- qf(0.95, dfr, dfe)
qf_r_001 <- qf(0.99, dfr, dfe)
qf_a_005 <- qf(0.95, dfa, dfe)
qf_a_001 <- qf(0.99, dfa, dfe)

print("qf_r_005")
print(qf_r_005)
print("qf_r_001")
print(qf_r_001)
print("qf_a_005")
print(qf_a_005)
print("qf_a_001")
print(qf_a_001)

print("Multiple Comparison")

# compute the Test Static
hi_vs_md <- (mean_hi - mean_md) /
                sqrt(2 * ms_e / (r * b))
hi_vs_lo <- (mean_hi - mean_lo) /
                sqrt(2 * ms_e / (r * b))
md_vs_lo <- (mean_md - mean_lo) /
                sqrt(2 * ms_e / (r * b))
asia_vs_europe <- (mean_europe - mean_asia) /
                sqrt(2 * ms_e / (r * b))

print("hi_vs_md")
print(hi_vs_md)
print("hi_vs_lo")
print(hi_vs_lo)
print("md_vs_lo")
print(md_vs_lo)
print("asia_vs_europe")
print(asia_vs_europe)

# compute the t-values
qt005 <- qt(0.975, dfe)
qt001 <- qt(0.995, dfe)

print("qt005")
print(qt005)
print("qt001")
print(qt001)