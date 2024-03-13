library (brms)

dat <- data.frame (y = rnorm(100),
                  x = rnorm (100))

brm(y ~ x, data = dat)


library(cmdstanr)

library(cmdstanr)




