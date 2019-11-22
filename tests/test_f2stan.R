devtools::load_all()
library(purrr)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

dat <- Sample_Data_Binomial
dat$x2 <- sample(nrow(Sample_Data_Binomial), replace = TRUE)
dat$c1 <- sample(letters, nrow(Sample_Data_Binomial), replace = TRUE)

f <- list(
  #y|k ~ 0,  # This should fail because the null model is not valid
  # y|k ~ 0 + c1  # This should fail because it contains character column
  # y|k ~ 1 + c1  # This should fail because it contains character column
  # y|k ~ 1
  # y|k ~ 0 + x1
  # y|k ~ 1 + x1
  # y|k ~ 0 + x1 + x2
  # y|k ~ 1 + x1 + x2
  y|k ~ 0 + age  # FIX: Not creating paramters or priors
  # y|k ~ 1 + age
  # y|k ~ 0 + age + gender  # FIX: Not creating paramters or priors
  # y|k ~ 1 + age + gender
  # y|k ~ 0 + x1 + age
  # y|k ~ 1 + x1 + age
  # y|k ~ 0 + x1 + x2 + age
  # y|k ~ 1 + x1 + x2 + age
  # y|k ~ 0 + x1 + age + gender
  # y|k ~ 1 + x1 + age + gender
  # y|k ~ 0 + x1 + x2 + age + gender
  # y|k ~ 1 + x1 + x2 + age + gender
)
f <- unlist(f[[1]])
f

fm1 <- list(
  f2stan(f, data = dat, link = "logit", adaptive_pooling = FALSE),
  f2stan(f, data = dat, link = "logit", adaptive_pooling = TRUE),
  f2stan(f, data = dat, link = "probit", adaptive_pooling = FALSE),
  f2stan(f, data = dat, link = "probit", adaptive_pooling = TRUE)
)

walk(map(fm1, "stan_code"), cat)

# Whenever the model is specified without an intercept but has only factor terms,
# the model needs to throw an error. Alternatively, throw a warning and only complain when
# adaptive pooling is specified as TRUE.
