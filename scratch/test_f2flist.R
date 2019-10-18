n <- 300
x1 <- runif(n, -1, 1)
x2 <- runif(n, -1, 1)
a <- 0
b1 <- 10
b2 <- -5
p <- 1 / (1 + exp(-(a + b1*x1 + b2*x2)))
y <- rbinom(n, size = 1, prob = p)
size <- sample(3:5, n, TRUE)
y2 <- rbinom(n, size = size, prob = p)
gender <- factor(sample(c("male", "female"), n, TRUE))
age <- factor(sample(c("<25", "25-50", ">50"), n, TRUE),
              levels = c("<25", "25-50", ">50"), ordered = TRUE)
dat_bern <- data.frame(y = y, x1 = x1, x2 = x2,
                       gender = gender, age = age)
dat_binom <- data.frame(y = y2, k = size, prop = y2 / size, x1 = x1, x2 = x2,
                        gender = gender, age = age)


###################
devtools::load_all()

f <- y|k ~ age
(fls <- .extractFromFormula(f))
vars <- fls[["vars"]]
LHS <- fls[["LHS"]]
if (grepl(pattern = "\\|", LHS)) {
  data_mode <- "binomial"
  LHS <- trimws(strsplit(LHS, split = "\\|")[[1]])
  fls[["LHS"]] <- LHS
} else {
  data_mode <- "bernoulli"
}
.getDataClasses(fls, dat_binom)
.getModelTerms(fls, .getDataClasses(fls, dat_binom))
.buildLinkFormula(fls, .getDataClasses(fls, dat_binom), "logit")
f2flist(y|k ~ age + gender, dat_binom, "logit")
f2flist(y|k ~ x1 + x2, dat_binom, "logit")
f2flist(y|k ~ x1 + x2 + age + gender, dat_binom, "logit")


glm(y ~ 0 + age + gender, dat_bern, family = binomial("logit"))

library(rstan)
library(rethinking)
test_fit <- bayesPF(y|k ~ age, dat_binom, "logit",
                    iter=4500, warmup=2000, chains=4, cores=4)
precis(test_fit, depth = 2)

test_fit2 <- bayesPF(y ~ x1 + x2 + age + gender, dat_bern, "logit",
                     iter=4500, warmup=2000, chains=4, cores=4)
precis(test_fit2, depth = 2)

test_fit3 <- bayesPF(y ~ 0 + x1 + x2, dat_bern, "logit",
                     iter=4500, warmup=2000, chains=4, cores=4)
precis(test_fit3)
