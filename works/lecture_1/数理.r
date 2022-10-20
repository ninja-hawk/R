# Put wm2013.txt in the same directory
# Read data
wm <- read.csv("lecture_1/ads.csv", header = TRUE)


#Correlation
print(cor(wm$x, wm$y))

# Regression
reg <- lsfit(wm$x, wm$y)
ls.print(reg)

# Plot
plot(
  wm$x,
  wm$y,
  xlab = "X:広告費",
  ylab = "Y:売り上げ",
  main = "線形回帰"
  )
abline(h = 0)
abline(v = 0)
abline(reg, col = "red")
