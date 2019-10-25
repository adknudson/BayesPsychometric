devtools::load_all()
library(tidyverse)

# Synthetic data set
f <- y|k ~ x1 + age + gender

fit <- bayesPF(f, Sample_Data_Binomial, "logit",
               warmup = 2000, iter = 4000, chains = 2, cores = 2)


fsmp <- extractFactorSamples(fit)
