# Put wm2013.txt in the same directory
# Read data
wm <- read.table("lecture_1/wm2013.txt", header = TRUE)

#Correlation
print(cor(wm$WNR, wm$FNG))

# Regression
reg <- lsfit(wm$WNR, wm$FNG)
ls.print(reg)

# Plot
plot(
  wm$WNR,
  wm$FNG,
  xlim = c(0, 90),
  ylim = c(0, 40),
  xlab = "WNR - Winners earned by player",
  ylab = "FNG - Final Number of Games",
  main = "Relation between WNR & FNG \n 
  in 1st round of 2013 Wimbledon men's singles"
  )
abline(h = 0)
abline(v = 0)
abline(reg, col = "red")

