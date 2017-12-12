


### Lion data
lions <- read.csv("lion_age.csv")
plot(age.years ~ portion.black, data = lions,
     ylab = "Age",
     pch = 18,
     cex =2)
