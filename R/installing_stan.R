install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

library(cmdstanr)

check_cmdstan_toolchain()

install_cmdstan(cores = 2)

file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
mod <- cmdstan_model(file)
data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))

fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)

install.packages("brms")

library(cmdstanr)
library(brms)
dat <- data.frame(y = rnorm(10), x = rnorm(10))
brm(y ~ x, data = dat)
    
    