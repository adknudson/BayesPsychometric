devtools::load_all()
library(tidyverse)

# Synthetic data set
f <- y|k ~ x1 + age + gender
fls <- .extractFromFormula(f)
preData <- .preProcessData(fls, Sample_Data_Binomial)
data_classes <- .getDataClasses(fls, preData)
terms <- .getModelTerms(fls, data_classes)
postData <- .processData(preData, fls, data_classes)

fs <- f2stan(f, Sample_Data_Binomial, "logit")
cat(fs$StanCode)
fit_fs <- bayesPF(f, Sample_Data_Binomial, "logit",
                  warmup = 2000, iter = 5000, chains = 2, cores = 2)

fsmp <- fit_fs$samples

mean(with(fsmp, a0 + a_age[,1] + a_gender[,2]))
