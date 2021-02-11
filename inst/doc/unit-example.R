## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(covidprobability)

## ---- fig.width=8, fig.height=4-----------------------------------------------

plot(0:21, dlnorm(0:21, 1.63, 0.5), 
     xlab="Days after exposure", 
     ylab="Probability", 
     main="Incubation period",
     type = "b")

## ---- fig.width=8, fig.height=4-----------------------------------------------

plot(0:21, plnorm(0:21, 1.63, 0.5),
     ylab="Proportion", 
     xlab = "Days since exposure",
     main="Infected expected to have developed symptoms",
     type="b")

## ---- fig.width=8, fig.height=4-----------------------------------------------

plot(0:14, sapply(0:14, prop_remaining, asympt = 0.279, mu = 1.63, sigma = 0.5),
     ylab="Proportion remaining", xlab = "Day", ylim = c(0,1), type = "b")


## ---- fig.width=8, fig.height=4-----------------------------------------------

plot(0:14, c(0.13, adjust_pretest(pre0 = 0.13, asymp = 0.279, days = 14)), 
     ylab="pretest probability", xlab="Day", type = "b", ylim = c(0, 0.2))


## ---- fig.width=8, fig.height=4-----------------------------------------------

plot(sens$point, ylim=c(0,1),
     main="PCR Sensitivity with 95% CI",
     xlab="Days since exposure", ylab="Sensitivity",
     type="l"); 
lines(sens$lower, col="grey")
lines(sens$upper, col="grey")


## ---- fig.width=8, fig.height=4-----------------------------------------------

example <- posttest_series(pre0 = 0.13, asympt = 0.279,
                           sens = sens$point, spec = 1)
example_upper <- posttest_series(pre0 = 0.13, asympt = 0.279, sens = sens$upper, 
                                 spec = 1)
example_lower <- posttest_series(pre0 = 0.13, asympt = 0.279, sens = sens$lower,
                                 spec = 1)

plot(example$x, example$y, 
     xlab="Day of testing", 
     ylab="Probability of having COVID-19",
     main="Red = pretest; Blue = posttest",
     ylim=c(0, 0.25), col="blue", type="l")

lines(example$x, example$y, 
      col="blue", type="l")
lines(example_lower$x, example_lower$y, 
      col="lightblue", type="l")
lines(example_upper$x, example_upper$y, 
      col="lightblue", type="l")
lines(1:14, adjust_pretest(0.13, asympt = 0.279), 
      col="red", type="l")

## ---- fig.width=8, fig.height=4-----------------------------------------------

unit_size <- 4

any_example <- probability_any(unit_size, example$y)
any_example_upper <- probability_any(unit_size, example_upper$y)
any_example_lower <- probability_any(unit_size, example_lower$y)

plot(example$x, any_example, 
     xlab="Day of testing", 
     ylab="Probability of any person having COVID-19",
     ylim=c(0, 0.50), col="blue", type="l")
lines(example_lower$x, any_example_lower, 
      col="lightblue", type="l")
lines(example_upper$x, any_example_upper, 
      col="lightblue", type="l")

abline(v=example$x[example[,2]==min(example$y)])


## ---- fig.width=8, fig.height=4-----------------------------------------------

test <- individual_probability(test_day = 9, pre0 = 0.13, sens = sens, spec = 1, 
                               asymp = 0.279, days = 14, mu = 1.63, sigma = 0.5)

plot(1:14, test$point, type="l", ylim=c(0,0.3),
     main = "Individual probabiltiy of undetected COVID-19",
     xlab = "Days since exposure",
     ylab = "Probability")
lines(test$lower, type="l", col="grey")
lines(test$upper, type="l", col="grey")

## ---- fig.width=8, fig.height=4-----------------------------------------------

test_n <- unit_probability(test_day = 9, pre0 =  0.13, sens = sens, spec = 1, 
                           asympt = 0.279, days = 14, mu = 1.63, sigma = 0.5, 
                           n = 10)

plot(1:14, test_n$point, type="b", ylim=c(0,1),
     main = "Unit-wide probabiltiy of undetected COVID-19",
     xlab = "Days since exposures",
     ylab = "Probability")
lines(test_n$lower, type="l", col="grey")
lines(test_n$upper, type="l", col="grey")
abline(v = 9)
text(1:14, (test_n$point + 0.1), round(test_n$point, 2), cex = 1)

## -----------------------------------------------------------------------------
test_n$point[9]
test_n$point[14]

test_n$lower[9]
test_n$lower[14]

