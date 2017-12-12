

## Create two pigment groups
pigment.mean <- mean(dat$portion.black)

dat$pigment.groups.2 <- ifelse(dat$portion.black <= pigment.mean, "low.pigment",
       "high.pigment")

dat$pigment.groups.2 <- factor(dat$pigment.groups)
summary(dat)
       
table(dat$pigment.groups.2, dat$population)


m.null <- lm(age.years ~ 1, data = dat)
m.alt.1 <- lm(age.years ~ pigment.groups.2, data = dat)
m.alt.2.add <- lm(age.years ~ pigment.groups.2 + population, data = dat)
m.alt.3.interaction <- lm(age.years ~ pigment.groups.2*population, data = dat)

anova(m.null, m.alt.1)
anova(m.alt.1, m.alt.2.add)
anova(m.alt.2.add, m.alt.3.interaction)







## Create 3 pigment groups
my.quartiles <- fivenum(dat$portion.black)

dat$pigment.groups.3 <- ifelse(dat$portion.black <= my.quartiles[2], 
       "low.pigment", 
       "moderate.pigment")
dat$pigment.groups.3 <- ifelse(dat$portion.black >= my.quartiles[4], 
                               "high.pigment",
                               dat$pigment.groups.3)

table(dat$population,dat$pigment.groups.3)

m.null <- lm(age.years ~ 1, data = dat)
m.alt.1 <- lm(age.years ~ pigment.groups.3, data = dat)
m.alt.2.add <- lm(age.years ~ pigment.groups.3 + population, data = dat)
m.alt.3.interaction <- lm(age.years ~ pigment.groups.3*population, data = dat)

anova(m.null, m.alt.1)
anova(m.alt.1, m.alt.2.add)
anova(m.alt.2.add, m.alt.3.interaction)





# Split by age
summary(dat$age.years)

dat$ <- ifelse(dat$age.years > 6, "mature", "immature")
