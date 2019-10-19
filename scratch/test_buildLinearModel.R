devtools::load_all()

# Null
dc <- list(numeric_vars =character(0),  factor_vars =character(0))
.buildLinearModel(dc, 0)
.buildLinearModel(dc, 1)

# 1 Numeric
dc_x <- list(numeric_vars ="x1", factor_vars =character(0))
.buildLinearModel(dc_x, 0)
.buildLinearModel(dc_x, 1)

# 1 Factor
dc_k <- list(numeric_vars =character(0), factor_vars ="k1")
.buildLinearModel(dc_k, 0)
.buildLinearModel(dc_k, 1)

# Multiple numeric
dc_mx <- list(numeric_vars =c("x1", "x2"), factor_vars =character(0))
.buildLinearModel(dc_mx, 0)
.buildLinearModel(dc_mx, 1)

# Multiple factors
dc_mk <- list(numeric_vars =character(0), factor_vars =c("k1", "k2", "k3"))
.buildLinearModel(dc_mk, 0)
.buildLinearModel(dc_mk, 1)

# 1 Numeric, Multiple Factors
dc_xmk <- list(numeric_vars ="x1", factor_vars =c("k1", "k2", "k3"))
.buildLinearModel(dc_xmk, 0)
.buildLinearModel(dc_xmk, 1)

# Multiple Numeric, 1 Factor
dc_mxk <- list(numeric_vars =c("x1", "x2"), factor_vars ="k1")
.buildLinearModel(dc_mxk, 0)
.buildLinearModel(dc_mxk, 1)

# Multiple Numeric, Multiple Factors
dc_mxmk <- list(numeric_vars =c("x1", "x2"), factor_vars =c("k1", "k2", "k3"))
.buildLinearModel(dc_mxmk, 0)
.buildLinearModel(dc_mxmk, 1)

dc_big <- list(numeric_vars =paste0("x", 1:5), factor_vars =paste0("k", 1:10))
.buildLinearModel(dc_big, 0)
.buildLinearModel(dc_big, 1)

#########################

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
dat_binom <- data.frame(y = y2, factor_vars =size, prop = y2 / size, x1 = x1, x2 = x2,
                        gender = gender, age = age)


###################
devtools::load_all()

f <- y|k ~ x1 + x2 + age
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
(dc <- .getDataClasses(fls, dat_binom))

.buildLinearModel(dc, fls[["include_intercept"]])
