f2flist <- function(formula, data, link) {

  fterms <- terms(formula)
  fvars  <- attr(fterms, "term.labels")  # names of the variables
  fint   <- attr(fterms, "intercept")    # default intercept term

  # Preliminary checks
  stopifnot(
    all(fvars %in% names(data)) # all terms in data
  )

  fstr <- as.character(formula)
  rel  <- fstr[1]
  LHS  <- fstr[2]

  RHS  <- fstr[3]
  LHS_link <- paste0(link, "(theta)")

  stopi

  response_dist <- "y ~ dbinom(1, theta)"
  linear_model <- paste0("beta_", fvars, " * ", fvars, collapse = " + ")

  # If there is no intercept, do something logical
  if (!fint) {
    NULL
  }
  prior_intercept <- "alpha_0 ~ dnorm(0, 32)"

  linear_model <- paste("alpha_0 +", linear_model)
  linear_model <- paste(LHS_link, "<-", linear_model)

  prior_slope <- paste0(paste0("beta_", fvars, collapse = ", "))
  prior_slope <- paste0("c(", prior_slope, ") ~ dnorm(0, 32)")

  flist <- list(response_dist,
                linear_model,
                prior_intercept,
                prior_slope)
  lapply(flist, function(x) parse(text = x)[[1]])
}


n <- 300
x <- runif(n, -1, 1)
a <- 0
b <- 10
p <- 1 / (1 + exp(-(a + b*x)))
y <- rbinom(n, size = 1, prob = p)
dat <- data.frame(x = x, y = y, p = p)

flist <- f2flist(y ~ 1 + x1 + x2 + x3, dat, "logit")
flist <- f2flist(y ~ x, dat, "logit")

flist

library(rethinking)

flist.fit <- map2stan(flist, dat, debug = TRUE)
summary(flist.fit)
