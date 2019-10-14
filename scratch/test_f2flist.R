n <- 50
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
age <- factor(sample(c("<40", ">=40"), 50, TRUE), levels = c("<40", ">=40"))
dat_bern <- data.frame(y = y, x1 = x1, x2 = x2,
                       gender = gender, age = age)
dat_binom <- data.frame(y = y2, k = size, prop = y2 / size, x1 = x1, x2 = x2,
                        gender = gender, age = age)


###################
devtools::load_all()
f <- y|k ~ x1 + x2 + gender
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
f2flist(y|k ~ x1, dat_binom, "logit")
f2flist(y|k ~ x1 + x2 + age, dat_binom, "logit")
