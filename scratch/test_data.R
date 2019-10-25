devtools::load_all()
library(tidyverse)

# Synthetic data set
f <- y|k ~ x1 + age + gender

fit <- bayesPF(f, Sample_Data_Binomial, "logit",
               warmup = 2000, iter = 4000, chains = 2, cores = 2)

# The following code combines the factor coefficient estimates to produce combo
# estimates.

tmp <- fit$f2stan$data[paste0("N_", fit$f2stan$metadata$vars$factor)]

levels_int <- lapply(tmp, function(i) {
  1:i
})
names(levels_int) <- fit$f2stan$metadata$vars$factor
levels_fct <- fit$f2stan$data[paste0("levels_", fit$f2stan$metadata$vars$factor)]

int_combo <- do.call(expand_grid, levels_int)
fct_combo <- do.call(expand_grid, levels_fct)

a <- list()
for (i in seq_len(nrow(int_combo))) {
  int_list <- as.list(int_combo[i, ])
  fct_list <- as.list(fct_combo[i, ])

  names(int_list) <- paste0("a_", names(int_list))
  smp <- fit$samples[names(int_list)]
  new_name <- paste0(fct_combo[i,], collapse = " ")
  a[[new_name]] <- Reduce(`+`, map2(smp, int_list, ~ .x[, .y])) + fit$samples$a0
}

bx1 <- list()
for (i in seq_len(nrow(int_combo))) {
  int_list <- as.list(int_combo[i, ])
  fct_list <- as.list(fct_combo[i, ])

  names(int_list) <- paste0("bx1_", names(int_list))
  smp <- fit$samples[names(int_list)]
  new_name <- paste0(fct_combo[i,], collapse = " ")
  bx1[[new_name]] <- Reduce(`+`, map2(smp, int_list, ~ .x[, .y])) + fit$samples$bx1
}

unlist(lapply(a, mean))
unlist(lapply(bx1, mean))

