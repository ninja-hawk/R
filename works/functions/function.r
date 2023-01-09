n <- 1
time <- proc.time()

while(n < 10000){
  n <- n + 1
}

time <- proc.time() - time

print(n)
print(time)

game3 <- read.table("lecture_9/3gswnr.txt", header = TRUE)

win <- subset(game3, Result == 1)
lose <- subset(game3, Result == 0)

print(rbind(win, lose))