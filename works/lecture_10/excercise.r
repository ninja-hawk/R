# Put 3gswnr.txt in an appropriate directory
# Read data
game3 <- read.table("lecture_9/3gswnr.txt", header = TRUE)

print("Two-way-ANOVA")

# Load dplyr
library(dplyr)

# get how many kinds of level(GS) in the data
gs_levels <- game3 %>%
  group_by(GS) %>%
  summarize(total_count = n(), .groups = "drop")

result_levels <- game3 %>%
  group_by(Result) %>%
  summarize(total_count = n(), .groups = "drop")

print(gs_levels)
print(result_levels)

# define some constant variables
a <- length(gs_levels$GS)
b <- length(result_levels$Result)
r <- gs_levels$total_count[1] / b # the same as result_levels$total_count[1] / a
n <- a * b * r

# print(a)
# print(b)
# print(r)
# print(n)

# define some degrees of freedom
dft <- n - 1
dfs <- a - 1
dfw <- b - 1
dfs_w <- dfs * dfw
dfe <- a * b * (r - 1)

# make some subsets by r
grass_win <- subset(game3, GS == 1 & Result == 1)
clay_win <- subset(game3, GS == 2 & Result == 1)
hard_win <- subset(game3, GS == 3 & Result == 1)
grass_lose <- subset(game3, GS == 1 & Result == 0)
clay_lose <- subset(game3, GS == 2 & Result == 0)
hard_lose <- subset(game3, GS == 3 & Result == 0)

# compute means of each subset
mean_grass_win <- mean(grass_win$WNR)
mean_clay_win <- mean(clay_win$WNR)
mean_hard_win <- mean(hard_win$WNR)
mean_grass_lose <- mean(grass_lose$WNR)
mean_clay_lose <- mean(clay_lose$WNR)
mean_hard_lose <- mean(hard_lose$WNR)

# print(mean_grass_win)
# print(mean_grass_lose)
# print(mean_clay_win)
# print(mean_clay_lose)
# print(mean_hard_win)
# print(mean_hard_lose)

# make some subsets of levels
grass <- subset(game3, GS == 1)
clay <- subset(game3, GS == 2)
hard <- subset(game3, GS == 3)
win <- subset(game3, Result == 1)
lose <- subset(game3, Result == 0)


# compute means of levels
mean_grass <- mean(grass$WNR)
mean_clay <- mean(clay$WNR)
mean_hard <- mean(hard$WNR)
mean_win <- mean(win$WNR)
mean_lose <- mean(lose$WNR)
mean_total <- mean(game3$WNR)

# print(mean_grass)
# print(mean_clay)
# print(mean_hard)
# print(mean_win)
# print(mean_lose)
# print(mean_total)

# compute Total Sum of Squares
st <- sum((game3$WNR - mean_total)^2)
print(st)

mean <- c(
  mean_grass_win,
  mean_clay_win,
  mean_hard_win,
  mean_grass_lose,
  mean_clay_lose,
  mean_hard_lose
)
ssw <- sum((mean - mean_total)^2) * r
print(ssw)

# compute Error Sum of Squares
error <- c(
  (grass_win$WNR - mean_grass_win)^2,
  (clay_win$WNR - mean_clay_win)^2,
  (hard_win$WNR - mean_hard_win)^2,
  (grass_lose$WNR - mean_grass_lose)^2,
  (clay_lose$WNR - mean_clay_lose)^2,
  (hard_lose$WNR - mean_hard_lose)^2
)

se <- sum(error)
print("ss")
print(se)

# compute each Treatment Sum of Squares
surface <- c(mean_grass, mean_clay, mean_hard)
ss <- sum((surface - mean_total)^2) * b * r
print("ss")
print(ss)

winlost <- c(mean_win, mean_lose)
sw <- sum((winlost - mean_total)^2) * a * r
print("sw")
print(sw)

ss_w <- ssw - (ss + sw)
print("ss_w")
print(ss_w)

# compute Mean Square
ms_s <- ss / dfs
ms_w <- sw / dfw
ms_s_w <- ss_w / dfs_w
ms_e <- se / dfe

print("ms_s")
print(ms_s)
print("ms_w")
print(ms_w)
print("ms_s_w")
print(ms_s_w)
print("ms_e")
print(ms_e)

# compute the Test Static
f_a <- ms_s / me
f_b <- ms_w / me
f_s_w <- ms_s_w / ms_e

print("f_a")
print(f_a)
print("f_b")
print(f_b)
print("f_s_w")
print(f_s_w)

# compute the F-value
qf_s_005 <- qf(0.95, dfs, dfe)
qf_s_001 <- qf(0.99, dfs, dfe)
qf_w_005 <- qf(0.95, dfw, dfe)
qf_w_001 <- qf(0.99, dfw, dfe)

print("qf_s_001")
print(qf_s_001)
# print(qf_s_005)
# print(qf_w_001)
print("qf_w_005")
print(qf_w_005)