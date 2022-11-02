# Put wm2013.txt in the approriate directory
# Read data
wm <- read.table("lecture_2/wm2013.txt", header = TRUE)


# Multiple Regression Analysis
# FNG is a dependent variable
reg <- lsfit(wm[3:8], wm$FNG)
ls.print(reg)

# Standardize all variables
wm_scale <- scale(wm[2:8])
# PCA
wm_pca <- prcomp(wm_scale)

print(wm_pca)