rm(list = ls())

# Table S1. Milk composition of mammals at mid-lactation
# Table S2. Ecology and life histories of mammals whose milk composition has been described

ls()

list.files()


milk <-read.csv("milk.csv",stringsAsFactors = FALSE, 
                na.strings = c("NA"))

milk <-na.omit(milk)

write.csv(milk, "milk_temp.csv")

milk <- read.csv("milk_temp.csv",
                 stringsAsFactors = FALSE, 
                 na.strings = c("NA","-",""))


milk <- milk[-1,]



library(ggplot2)

milk$fat <- gsub("\\*","",milk$fat)
milk$fat <- as.numeric(milk$fat)




milk$gestation.month <-   gsub("[a-zA-Z ]","",milk$gestation.month)
milk$gestation.month <-   gsub("[ ]","",milk$gestation.month)

milk$gest.month.NUM <- as.numeric(milk$gestation.month)


summary(milk)


par(mfrow = c(1,2))
i.use <- which(milk$fat < 40)
plot(fat ~ protein,
     pch = 18,
     cex =2,
     data = milk[i.use,])

#reverse
plot(protein ~ fat,
     pch = 18,
     cex =2,
     data = milk[i.use,])


plot(protein ~ fat,
     pch = 18,
     cex =2,
     data = milk[,])



par(mfrow = c(1,1))
scatter.smooth(milk$fat,milk$protein,
     pch = 18,
     cex =2,
     lpars = list(lwd = 5,
                  lty = 3,
                  col = 2))



### Subset primates
i.primates <- which(milk$order == "Primates")


#not loged
plot(fat ~ mass.female,
     data = milk[i.primates,],
     ylab = "Fat content of milk",
     pch = 18,
     cex =2)


plot(fat ~ log(mass.female),
     data = milk[i.primates,],
     ylab = "Fat content of milk",
     pch = 18,
     cex =2,
     xlim = c(5,12),
     ylim = c(0,11))

#Add spp names
    with(milk[i.primates,  ],
         text(y = fat,
              x = log(mass.female),
              label = spp))
    

    
scatter.smooth(log(milk[i.primates,
                        "mass.female"]),
               milk[i.primates,"fat"],
               ylab = "Fat content of milk",
               pch = 18,
               xlim = c(5,12),
               ylim = c(0,11),
               cex =2,lpars = list(lwd = 4,col = 2)
               )


#with regression line
plot(fat ~ log(mass.female),
     data = milk[i.primates,],
     ylab = "Fat content of milk",
     pch = 18,
     cex =2,
     xlim = c(5,12),
     ylim = c(0,11))

lm.alt <- lm(fat ~ log(mass.female),
             data = milk[i.primates,])

abline(lm.alt, col =3, lwd  =3)


#regression vs. smoother
scatter.smooth(log(milk[i.primates,
                        "mass.female"]),
               milk[i.primates,"fat"],
               ylab = "Fat content of milk",
               pch = 18,
               xlim = c(5,12),
               ylim = c(0,11),
               cex =2,lpars = list(lwd = 4,col = 2)
)
abline(lm.alt, col =3, lwd  =3)


#Fit 2 models

lm.null <- lm(fat ~ 1,
             data = milk[i.primates,])

lm.alt <- lm(fat ~ log(mass.female),
             data = milk[i.primates,])



#plot both models
plot(fat ~ log(mass.female),
     data = milk[i.primates,],
     ylab = "Fat content of milk",
     pch = 18,
     cex =2,
     xlim = c(5,12),
     ylim = c(0,11))
abline(lm.alt, col =3, lwd  =3)
abline(lm.null, col =4, lwd  =3)

legend("topright",
       col = c(3,4),
       lty = 1,
       lwd = 4,
       legend = c("y ~ 0*log(mass)      + int.null",
                  "y ~ slope*log(mass) + int.alt"))



#plot both models side by side
par(mfrow = c(1,2))
plot(fat ~ log(mass.female),
     data = milk[i.primates,],
     ylab = "Fat content of milk",
     pch = 18,
     cex =2,
     xlim = c(5,12),
     ylim = c(0,11),
     main = "Null model")
abline(lm.null, col =3, lwd  =3)
legend("topright",
       legend = c("y ~ 0*log(mass) + int.null"))


plot(fat ~ log(mass.female),
     data = milk[i.primates,],
     ylab = "Fat content of milk",
     pch = 18,
     cex =2,
     xlim = c(5,12),
     ylim = c(0,11),
     main = "Alt model")
abline(lm.alt, col =4, lwd  =3)
legend("topright",
       legend = c("y ~ slope*log(mass) + int."))


#inference / significance test
anova(lm.null, lm.alt)



summary(lm.alt)


milk2 <- milk[i.primates,]


milk2$mass.log <- log(milk2$mass.female)


par(mfrow = c(1,1))
plot(fat ~ log(mass.female),
     data = milk[i.primates,],
     ylab = "Fat content of milk",
     pch = 18,
     cex =2)
abline(a = 12,b = -0.9, col =2, lwd = 2)
plot.my.residuals(intercept = 12,
                  slope = -0.9,
                  dat = milk2,
                  predictor.x = "mass.log",
                  response.y="fat"
                  )

resid(lm.alt)


my.residuals <- resid(lm.alt)

hist(my.residuals)


#skewed residausl
hist(1-rpois(220,2),5)



# qqplot
fat <- milk[i.primates,"fat"]

log.mass <- log(milk[i.primates,"mass.female"])

par(mfrow = c(1,1))
qqplot(log.mass,my.residuals)


qqplot(log.mass,rstandard(lm.alt))

qqnorm(log.mass)



##



par(mfrow = c(1,2), mar = c(4,4,3,1))
plot(fat ~ log(mass.female),
     data = milk[i.primates,],
     ylab = "Fat content of milk",
     pch = 18,
     cex =2,
     main = "fat ~ log.mass")
plot(my.residuals ~ log.mass,
     main = "residuals ~ log.mass",
     pch = 17,
     cex =2,
     col = 4)
abline(h = 0, col =2,lwd = 2)

par(mfrow = c(1,1), mar = c(4,4,3,1))
plot(my.residuals ~ log.mass,
     main = "residuals ~ log.mass",
     pch = 17,
     cex =2,
     col = 4)
abline(h = 0, col =2,lwd = 2)




par(mfrow = c(2,2), mar = c(4,4,3,1))
plot(lm.alt)











i.primates
summary(lm(fat ~ log(mass.female),data = milk[,]))
summary(lm(fat ~ log(mass.female),data = milk[i.primates,]))

summary(lm(fat ~ gest.month.NUM ,data = milk[,]))

lecatation.months
mass.litter
repro.output     
dev.stage.at.birth
diet
arid
biome