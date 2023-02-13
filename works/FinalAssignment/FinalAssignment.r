# Put DataForTable2.1.xls in the appropriate directory
# Read data
data <- read.csv("FinalAssignment/DataForTable2.1.csv", header = TRUE)

# Load dplyr
library(dplyr)




# erase the rows which have
data <- na.omit(data)

data_mr <- lsfit(data[4:12], data$Life.Ladder)
ls.print(data_mr)

pca_list <- c("Social.support", "Perceptions.of.corruption")
pca_subset <- data[, pca_list]

reg <- lsfit(pca_subset, data$Life.Ladder)
ls.print(reg)


# erase the rows which have
data <- na.omit(data)

# gete median of year
median_year <- median(data$year)


# seperate counties into former years and latter year
former <- subset(data, year < median_year)
latter <- subset(data, year > median_year)


# get mean, variance, and length of Healthy.life.expectancy.at.birth
fmean <- mean(former$Healthy.life.expectancy.at.birth)
fvar <- var(former$Healthy.life.expectancy.at.birth)
fn <- length(former$Healthy.life.expectancy.at.birth)

# get mean and variance of Healthy.life.expectancy.at.birth
lmean <- mean(latter$Healthy.life.expectancy.at.birth)
lvar <- var(latter$Healthy.life.expectancy.at.birth)
ln <- length(latter$Healthy.life.expectancy.at.birth)


t_independent <- (lmean - fmean) /
  sqrt(
    ((ln - 1) * lvar + (fn - 1) * fvar) / (ln + fn - 2) *
    ((1 / ln) + (1 / fn))
  )

print(t_independent)

# one-tailed test
print(qt(0.95, fn + ln - 2))
print(qt(0.99, fn + ln - 2))