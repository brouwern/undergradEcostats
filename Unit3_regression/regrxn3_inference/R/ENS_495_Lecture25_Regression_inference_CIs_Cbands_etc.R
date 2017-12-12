

# data all w/ slope zero but different intercepts
x <- runif(10, 0, 15)

null.with.slope <- rnorm(10,0,1)


plot(null.with.slope ~ x, 
     ylim = c(-5,25),pch = 18,
     cex = 2)

points(rnorm(10,0,1)+10 ~ x, 
     ylim = c(-5,20),pch = 19,
     cex = 2, col = 3)

points(rnorm(10,0,1)+20 ~ x, 
       ylim = c(-5,20),pch = 17,lwd = 2,
       cex = 2, col = 4)



#data w/ intercept of zero but slope not equal to zero
intercept.of.0 <- x+rnorm(10,0,1)

plot(intercept.of.0 ~ x, 
     ylim = c(-1,12),pch = 18,
     cex = 2)


