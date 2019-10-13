library(rethinking)

n <- 100
x <- runif(n, -1, 1)
a <- 0
b <- 10
p <- 1 / (1 + exp(-(a + b*x)))
y <- rbinom(n, size = 1, prob = p)
plot(x, y)

dat <- data.frame(x = x, y = y, p = p)

flist <- alist(
  y ~ dbinom(1, theta),
  logit(theta) <- a + b * x,
  a ~ dnorm(0, 32),
  b ~ dnorm(0, 32)
)

simple.fit <- map2stan(flist, dat, debug = TRUE,
                       iter = 4000, warmup = 2000, chains = 6)
fcoef <- lapply(extract.samples(simple.fit), mean)

glm_fit <- glm(y ~ x, dat, family = binomial(link = "logit"))
fcoef2 <- coef(glm_fit)

plot(x, p)
points(x, y, col = "green", pch=4)

curve(1 / (1 + exp(-(a + b*x))),
      from = -1, to = 1, col = "blue", add=TRUE)

curve(1 / (1 + exp(-(fcoef[[1]] + fcoef[[2]]*x))),
      from = -1, to = 1, col = "red", add=TRUE)

curve(1 / (1 + exp(-(fcoef2[1] + fcoef2[2]*x))),
      from = -1, to = 1, col = "red", add=TRUE)

