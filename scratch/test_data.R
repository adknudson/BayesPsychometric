devtools::load_all()
library(tidyverse)

# Synthetic data set
f <- y|k ~ x1 + age + gender

fs <- f2stan(f, Sample_Data_Binomial, "logit")
.getModelTerms(fs$metadata, TRUE)
.buildPriorFormula(.getModelTerms(fs$metadata, TRUE))
