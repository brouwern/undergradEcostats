srrs2 <- read.table ("srrs2.dat", header=T, sep=",")

cty <- read.table ("cty.dat", header=T, sep=",")

names(cty)
cty$Uppm

names(srrs2)
names(cty)

plot(activity, data =  srrs2
     
     
     
     # get county index variable
     
     county.name <- as.vector(srrs2$county)
     uniq <- unique(county.name)
     J <- length(uniq)
     county <- rep (NA, J)
     for (i in 1:J){
       county[county.name==uniq[i]] <- i
     }
     
     # get the county-level predictor
     
     srrs2.fips <- srrs2$stfips*1000 + srrs2$cntyfips
     cty <- read.table ("cty.dat", header=T, sep=",")
     usa.fips <- 1000*cty[,"stfips"] + cty[,"ctfips"]
     usa.rows <- match (unique(srrs2.fips), usa.fips)
     uranium <- cty[usa.rows,"Uppm"]
     u <- log (uranium)
     
     length(u)

     
     
     
names(srrs2)[which(names(srrs2) == "cntyfips" )] <- "ctfips"

x <- merge(srrs2, cty)
dim(x)

dim(srrs2)

MN <- which(x$st == "MN")
PA <- which(x$st == "PA")
MI  <- which(x$st == "MI")
plot(log(activity) ~ Uppm, data = x[MN,])
plot(log(activity) ~ Uppm, data = x[PA,])



par(mfrow = c(1,2))
plot(log(activity) ~ Uppm, data = x[MN,])
plot(log(activity) ~ Uppm, data = x[PA,])



par(mfrow = c(1,2))

plot(log(activity) ~ Uppm, data = na.omit(x[MN,
                                            c("activity","Uppm")]))
abline(lm(log(activity+.001) ~ Uppm, data = na.omit(x[MN,
                                            c("activity","Uppm")])),
       col = 2, lwd = 5)

plot(log(activity) ~ Uppm, data = na.omit(x[PA,
                                                   c("activity","Uppm")]))


plot(log(activity) ~ Uppm, data = na.omit(x[PA,
                                            c("activity","Uppm")]))
abline(lm(log(activity+.001) ~ Uppm, data = na.omit(x[PA,
                                                      c("activity","Uppm")])),
       col = 2, lwd = 5)