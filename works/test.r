# Place wm2013.txt in the same directory
# read data
wm = read.table("wm2013.txt", header=TRUE)
# print(wm)

# EDA
reg <- lsfit(wm$WNR, wm$FNG)
ls.print(reg)

# PLot
plot(wm$WNR, wm$FNG)
abline(reg, col="red")
