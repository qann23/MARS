source("mars.R")
source("summary.R")
source("print.R")
source("plot.R")
source("predict.R")
source("anova.R")
library(MASS)
library(ISLR)

# TEST 1: Wage data
data("Wage")
mc <- mars.control(Mmax=10)
mout <- mars(wage ~ age + education, data=Wage, control=mc)
ff <- fitted(mout)
p1 <- predict(mout)
p2 <- predict(mout,newdata=data.frame(age=Wage$age,education=Wage$education))
head(cbind(ff,p1,p2)) # columns should be identical
mout # tests print method
summary.mars(mout) #test summary method
anova.mars(mout) # test anova method
plot.mars(mout) # test plot method

# TEST 2: Auto data
data("Auto")
auto = mars.control(Mmax = 10)
aout = mars(horsepower ~ mpg + cylinders, data = Auto, control = auto)
ff2 = fitted(aout)
predict1 = predict(aout)
predict2 = predict(aout, newdata = data.frame(mpg = Auto$mpg, cylinders = Auto$cylinders))
head(cbind(ff2, predict1, predict2))
aout
summary.mars(aout)
anova.mars(aout)
plot.mars(aout)

# TEST 3: Trees data
data("trees")
credit = mars.control(Mmax = 10)
cout = mars(Girth ~ Height + Volume, data = trees, control = credit)
ff3 = fitted(cout)
predict3 = predict(cout)
predict4 = predict(cout, newdata = data.frame(Height = trees$Height, Volume = trees$Volume))
head(cbind(ff3, predict3, predict4))
cout
summary.mars(cout)
anova.mars(cout)
plot.mars(aout)

