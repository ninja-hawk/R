# Put DataForTable2.1.xls in the appropriate directory
# Read data
data <- read.csv("FinalAssignment/DataForTable2.1.csv", header = TRUE)
# erase the rows which have
data <- na.omit(data)

# # standardize all variables
# data_sc <- scale(data[3:12])
# data_pc <- prcomp(data_sc)
# print(data_pc)


# Compute Pearson Relation
cor <- cor(data[3:12], method = "pearson")

# Install Package for Heatmap
install.packages("corrplot")
library(corrplot)

# Draw a Heatmap
png("FinalAssignment/heatmap.png")
corrplot(cor, method = "number", tl.col = "black")
dev.off()
