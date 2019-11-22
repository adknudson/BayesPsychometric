link_functions <- list(
  "logit" = list(link = logit,
                 inv_link = logistic),
  "probit" = list(link = qnorm,
                  inv_link = pnorm)
)

usethis::use_data(link_functions, internal = TRUE, overwrite = TRUE)
