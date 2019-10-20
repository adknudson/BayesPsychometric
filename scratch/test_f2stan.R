n <- 300
x1 <- runif(n, -1, 1)
x2 <- sample((-n%/%2):(n%/%2), n, TRUE)
a <- 0.6
b1 <- -4.2
b2 <- -0.005
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
f <- f2stan(y|k ~ x1 + gender, dat_binom, "logit")
cat(f$StanCode)

bayesPF(y|k ~ x1 + x2 + gender + age, dat_binom, "logit", chains = 2)



init_list <- list(
  a1 = rep(0, 50),
  a2 = rep(0, 50),
  a3 = rep(0, 50),
  b1 = rep(0, 50),
  b2 = rep(0, 50),
  b3 = rep(0, 50),
  mu_a1 = 0,
  mu_a2 = 0,
  mu_a3 = 0,
  mu_b1 = 0,
  mu_b2 = 0,
  mu_b3 = 0
)

replicate(2, init_list, simplify = FALSE)
