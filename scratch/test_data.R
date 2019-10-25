devtools::load_all()
library(tidyverse)

# Synthetic data set
f <- y|k ~ x1 + age + gender

fls <- .extractFromFormula(f)
preData <- .preProcessData(fls, Sample_Data_Binomial)

(metadata <- .getDataClasses(fls, preData))

terms <- .getModelTerms(metadata, fls$include_intercept)
postData <- .processData(preData, metadata)

fs <- f2stan(f, Sample_Data_Binomial, "logit")
cat(fs$StanCode)

fit_fs <- bayesPF(f, Sample_Data_Binomial, "logit",
                  warmup = 2000, iter = 5000, chains = 2, cores = 2,
                  return_f2stan = TRUE, return_stan_fit = TRUE)

str(fit_fs)
