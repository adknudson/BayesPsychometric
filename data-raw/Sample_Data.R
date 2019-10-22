## code to prepare `Sample_Data` dataset goes here
set.seed(101)
n <- 300
x1 <- runif(n, -100, 100)
a <- 1.4
b1 <- 0.07

gender <- factor(sample(c("male", "female"), n, TRUE))
a_gender <- list(male = -2, female = 0.7)
b_gender <- list(male = -0.02, female = 0.03)
agender <- unlist(a_gender[gender])
bgender <- unlist(b_gender[gender])

age <- factor(sample(c("<25", "25-50", ">50"), n, TRUE),
              levels = c("<25", "25-50", ">50"), ordered = TRUE)
a_age <- list("<25" = -1, "25-50" = 0, ">50" = 1)
b_age <- list("<25" = -0.012, "25-50" = 0.0056, ">50" = 0.073)
aage <- unlist(a_age[age])
bage <- unlist(b_age[age])

# Alpha
#          Male  Female
# <25      -1.6     1.1
# 25-50    -0.6     2.1
# >50       0.4     3.1
#
# Beta
#          Male    Female
# <25    0.038     0.088
# 25-50  0.0556    0.1056
# >50    0.123     0.173
#

p <- 1 / (1 + exp(-(a + agender + aage + (b1 + bgender + bage)*x1)))
y <- rbinom(n, size = 1, prob = p)
size <- sample(3:5, n, TRUE)
y2 <- rbinom(n, size = size, prob = p)


Sample_Data_Bernoulli <- data.frame(y = y, x1 = x1,
                                    gender = gender, age = age)

Sample_Data_Binomial <- data.frame(y = y2, k = size, x1 = x1,
                                   gender = gender, age = age)


usethis::use_data(Sample_Data_Bernoulli, overwrite = TRUE)
usethis::use_data(Sample_Data_Binomial, overwrite = TRUE)
