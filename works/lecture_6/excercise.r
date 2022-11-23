# Put wm2013.txt in the appropriate directory
# Read data
wm <- read.table("lecture_2/wm2013.txt", header = TRUE)

#INDEPENDENT t TESTS
print("------------------------------------------------------")

#get mean, variance, and length of FSP that all WINNERS have
winners <- subset(wm, Result == 1)
win_mean <- mean(winners$FSP)
win_var <- var(winners$FSP)
win_n <- length(winners$FSP)

#get mean, variance, and length of FSP that all LOSERS have
losers <- subset(wm, Result == 0)
lose_mean <- mean(losers$FSP)
lose_var <- var(losers$FSP)
lose_n <- length(losers$FSP)

#calculate t-value by the equation below
#NOTICE: win_n = lose_n
t_independent <- (win_mean - lose_mean) / sqrt((win_var + lose_var) / win_n)
print(t_independent)

#two-tailed test
print(qt(0.995, 2 * win_n - 2))
print(qt(0.975, 2 * win_n - 2))

print("The mean of FSP is siginificantlly different because")
print("t > 1.978497 (p < 0.05) and H0 (win_mean = lose_mean) was rejected")

#PAIRED t TESTS
print("------------------------------------------------------")
#get mean, variance, and length of WNR that all WINNERS have
win_wnr_mean <- mean(winners$WNR)
win_wnr_var <- var(winners$WNR)
#get mean, variance, and length of UFE that all WINNERS have
win_ufe_mean <- mean(winners$UFE)
win_ufe_var <- var(winners$UFE)

#calcualte the correlation between WNR and UFE among all winners
cor <- cor(winners$WNR, winners$UFE)

t_paired <- (win_wnr_mean - win_ufe_mean) /
  sqrt((win_wnr_var + win_ufe_var -
  2 * cor * sqrt(win_wnr_var) * sqrt(win_ufe_var)) / win_n)
print(t_paired)

#one-tailed test
print(qt(0.99, win_n - 1))

print("The mean of WNR is siginificantlly larger than that of UFE")
print("t > 2.387008(p < 0.01) and ")
print("H0 (win_wnr_mean = win_ufe_mean) was rejected")