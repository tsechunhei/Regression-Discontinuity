#simulation code
set.seed(1234)
n = 100
x = runif(n)
D = 1*(x>=0.5)

y = pnorm(x,0.5,0.1)+rnorm(n,sd=0.1)+D
cutoff = 0.5
xtilde = x-cutoff
rd = lm(y~D+xtilde)
coef(rd)
summary(rd)
confint(rd, 'D', level=0.95)

plot(x,y)
abline(v=cutoff,lty=2)
x2 = seq(0,cutoff-0.01,0.01)
y2 = predict(rd, newdata = data.frame(xtilde=x2-cutoff,D=1 * (x2>=cutoff)))
points(x2, y2, type='l', col='green',lwd=2)

x3=seq(cutoff,1,0.01)
y3=predict(rd,newdata=data.frame(xtilde=x3-cutoff,D=1*(x3>=cutoff)))
points(x3, y3, type= 'l', col='purple',lwd=2)

#cutoff = 0.5
#E[Y0|X] when D =0, E[Y0|X] when D = 1
#95 percent confidence interval:
#1.345852 1.579982
#true mean is not equal to 0. It is very significant.


#linear RD model to estimate the causal e??ect of legal access to alcohol on death rates:
library(magrittr)
library(dplyr)
mlda = read.csv('mlda.csv')
mlda = mlda %>% mutate(age = agecell - 21, 
                       over21 = 1 * (agecell >= 21))
linear = lm(all ~ over21 + age + age:over21, mlda)
summary(linear)


#quadratic rather than linear specification.
quadratic = lm(all ~ over21 + age + I(age^2) +
                 age:over21 + I(age^2):over21, mlda)
summary(quadratic)

###plot
make_RD_plot = function(reg, dat, inc = 0.01){
  plot(all ~ agecell, dat, xlab = 'Age',
       ylab = "Mortality Rate (per 100,000)",
       pch = 20, col = 'blue')
  abline(v = 21, lty = 2)
  x_min = min(dat$agecell)
  x_max = max(dat$agecell)
  x2 = seq(x_min, 21 - inc, inc)
  x3 = seq(21, x_max, inc)
  y2 = predict(reg, data.frame(age = x2 - 21,
                               over21 = 1 * (x2 >= 21)))
  y3 = predict(reg, data.frame(age = x3 - 21,
                               over21 = 1 * (x3 >= 21)))
  points(x2, y2, lwd = 2)
  points(x3, y3, lwd = 2)
}

make_RD_plot(linear, mlda)
make_RD_plot(quadratic, mlda)


#RD analysis by restricting your sample to ages between 20 and 22:
mlda2 = read.csv('mlda2.csv')
linear2 = lm(all ~ agecell , mlda2)
summary(linear2)
#The age between 20-22 is less significant than only the age over 21. 
#And the R-squared is lower than before.

quadratic2 = lm(all ~ agecell + I(agecell^2), mlda2)
summary(quadratic2)