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

link <- "logit"

f <- y ~ x1 + x2 + age
(fls <- .extractFromFormula(f))
if (grepl(pattern = "\\|", fls[["LHS"]])) {
  fls[["data_mode"]] <- "binomial"
  fls[["LHS"]] <- trimws(strsplit(fls[["LHS"]], split = "\\|")[[1]])
} else {
  fls[["data_mode"]] <- "bernoulli"
}
(dc <- .getDataClasses(fls, dat_bern))
.getModelTerms(fls, dc)
.buildLinkFormula(fls, dc, link)
.buildDistributionFormula(fls, link)

f2stan(y|k ~ x1 + age + gender, dat_binom, "logit")
