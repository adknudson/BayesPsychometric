devtools::load_all()
library(tidyverse)

# Synthetic data set
f <- y|k ~ x1 + age + gender
fls <- .extractFromFormula(f)
preData <- .preProcessData(fls, Sample_Data_Binomial)
data_classes <- .getDataClasses(fls, preData)
postData <- .processData(preData, fls, data_classes)

fs <- f2stan(f, Sample_Data_Binomial, "logit")
cat(fs$StanCode)
fit_fs <- bayesPF(f, Sample_Data_Binomial, "logit", chains = 4, cores = 4)
summ_fs <- summary(fit_fs)$summary %>% as_tibble(rownames = "coef")
