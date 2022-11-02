# Put ASIA2021.txt in an appropriate directory
# Read data
asia <- read.table("lecture_3/ASIA2021.txt", header = TRUE)

# Multiple Regression Analysis
# Ladder is a dependent variable
reg <- lsfit(asia[3:9], asia$Ladder)
ls.print(reg)

# Compute Pearson Relation
cor <- cor(asia[2:9], method = "pearson")

# Install Package for Heatmap
install.packages("corrplot")
library(corrplot)

# Draw a Heatmap
png("heatmap.png")
corrplot(cor, method = "number", tl.col = "black")
dev.off()