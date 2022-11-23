# Put ASIA2021.txt in an appropriate directory
# Read data
asia <- read.table("lecture_3/ASIA2021.txt", header = TRUE)

# Regression
reg <- lsfit(asia$Positive, asia$Ladder)
ls.print(reg)

# Plot
png("./PositiveVsLadder.png")
plot(
  asia$Positive,
  asia$Ladder,
  xlim = c(0, 1),
  ylim = c(0, 10),
  xlab = "Positive",
  ylab = "Ladder",
  main = "Relation between Positive & Ladder \n 
  in the data of 25 Asian countries in the year of 2021"
  )
abline(h = 0)
abline(v = 0)
abline(reg, col = "red")
dev.off()