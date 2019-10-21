devtools::load_all()
library(FangPsychometric)
library(tidyverse)


# Synthetic data set
f <- y|k ~ x1 + age + gender
fs <- f2stan(f, Sample_Data_Binomial, "logit")
cat(fs$StanCode)
fit_fs <- bayesPF(f, Sample_Data_Binomial, "logit", chains = 4, cores = 4)
summ_fs <- summary(fit_fs)$summary %>% as_tibble(rownames = "coef")


# Authentic data set
av2 <- audiovisual %>%
  filter(trial %in% c("baseline", "adapt1")) %>%
  mutate(subject_id = factor(subject_id),
         trial = droplevels(trial))

f <- response|n_trials ~ soa
fav <- f2stan(f, av2, "logit")
cat(fav$StanCode)
fit_fav <- bayesPF(f, av2, "logit", chains = 4, cores = 4)
summary(fit_fav)$summary %>% as_tibble(rownames = "coef")

