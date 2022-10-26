# Put ASIA2021.txt in the same directory
# Read data
asia <- read.table("lecture_3/ASIA2021.txt", header = TRUE)

#Correlation
print(cor(asia$Ladder, asia$Positive))

# Regression
reg <- lsfit(asia$Ladder, asia$Positive)
ls.print(reg)

# Plot
plot(
  asia$Ladder,
  asia$Positive,
  xlim = c(0, 10),
  ylim = c(0, 1),
  xlab = "Ladder",
  ylab = "Positive",
  main = "Relation between Ladder & Positive \n 
  in ASIA2021"
  )
abline(h = 0)
abline(v = 0)
abline(reg, col = "red")
