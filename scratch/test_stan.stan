data {
    // Number of observations, factor levels, etc.
    int<lower=1> N;
    int<lower=1> N_age;
    int<lower=1> N_gender;

    // Response Data
    int y[N];
    int k[N];

    // Numeric Data
    vector[N] x1;
    vector[N] x2;

    // Factor Data
    int age[N];
    int gender[N];
}
transformed data {
    // Standardized Data
    real mu_x1;
    real sd_x1;
    vector[N] x1_std;

    real mu_x2;
    real sd_x2;
    vector[N] x2_std;

    mu_x1 = mean(x1);
    sd_x1 = sd(x1);
    x1_std = (x1 - mu_x1) / sd_x1;

    mu_x2 = mean(x2);
    sd_x2 = sd(x2);
    x2_std = (x2 - mu_x2) / sd_x2;
}
parameters {
    // Intercept terms
    real a;
    vector[N_age] a_age;
    vector[N_gender] a_gender;

    // Slope terms
    real b_x1;
    vector[N_age] b_x1_age;
    vector[N_gender] b_x1_gender;
    real b_x2;
    vector[N_age] b_x2_age;
    vector[N_gender] b_x2_gender;

    // Adaptive Pooling terms
    real<lower=machine_precision()> sd_a_age;
    real<lower=machine_precision()> sd_a_gender;
    real<lower=machine_precision()> sd_b_x1_age;
    real<lower=machine_precision()> sd_b_x1_gender;
    real<lower=machine_precision()> sd_b_x2_age;
    real<lower=machine_precision()> sd_b_x2_gender;
}
model {
    vector[N] theta;

    // Priors
    a ~ normal(0, 5);
    a_age ~ normal(0, sd_a_age);
    a_gender ~ normal(0, sd_a_gender);
    sd_a_age ~ cauchy(0, 2.5);
    sd_a_gender ~ cauchy(0, 2.5);
    b_x1 ~ normal(0, 5);
    b_x1_age ~ normal(0, sd_b_x1_age);
    b_x1_gender ~ normal(0, sd_b_x1_gender);
    sd_b_x1_age ~ cauchy(0, 2.5);
    sd_b_x1_gender ~ cauchy(0, 2.5);
    b_x2 ~ normal(0, 5);
    b_x2_age ~ normal(0, sd_b_x2_age);
    b_x2_gender ~ normal(0, sd_b_x2_gender);
    sd_b_x2_age ~ cauchy(0, 2.5);
    sd_b_x2_gender ~ cauchy(0, 2.5);

    // General linear model
    for (i in 1:N) {
        theta[i] = a + a_age[age[i]] + a_gender[gender[i]] + (b_x1 + b_x1_age[age[i]] + b_x1_gender[gender[i]]) * x1_std[i] + (b_x2 + b_x2_age[age[i]] + b_x2_gender[gender[i]]) * x2_std[i];
    }
    y ~ binomial_logit(k, theta);
}
generated quantities {
    // Intercept terms
    real alpha;
    vector[N_age] alpha_age;
    vector[N_gender] alpha_gender;

    // Slope terms
    real beta_x1;
    vector[N_age] beta_x1_age;
    vector[N_gender] beta_x1_gender;
    real beta_x2;
    vector[N_age] beta_x2_age;
    vector[N_gender] beta_x2_gender;

    // Calculations
    alpha = a - b_x1 * mu_x1 / sd_x1 - b_x2 * mu_x2 / sd_x2;
    alpha_age = a_age - b_x1_age * mu_x1 / sd_x1 - b_x2_age * mu_x2 / sd_x2;
    alpha_gender = a_gender - b_x1_gender * mu_x1 / sd_x1 - b_x2_gender * mu_x2 / sd_x2;
    beta_x1 = b_x1 / sd_x1;
    beta_x2 = b_x2 / sd_x2;
    beta_x1_age = b_x1_age / sd_x1;
    beta_x1_gender = b_x1_gender / sd_x1;
    beta_x2_age = b_x2_age / sd_x2;
    beta_x2_gender = b_x2_gender / sd_x2;
}
