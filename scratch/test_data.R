devtools::load_all()
library(tidyverse)

# Synthetic data set
d <- Sample_Data_Binomial
d$x2 <- runif(nrow(d))

f1 <- y|k ~ x1 + x2 + age + gender
fs1 <- f2stan(f1, d, "logit", TRUE)
cat(fs1$StanCode)

f2 <- y|k ~ x1 + age
fs2 <- f2stan(f2, d, "logit", TRUE)
cat(fs2$StanCode)

f3 <- y|k ~ x1
fs3 <- f2stan(f3, d, "logit", TRUE)
cat(fs3$StanCode)

f4 <- y|k ~ age
fs4 <- f2stan(f4, d, "logit", TRUE)
cat(fs4$StanCode)


f <- y|k ~ x1 + age + gender
fit <- bayesPF(f, Sample_Data_Binomial, "logit", TRUE)
