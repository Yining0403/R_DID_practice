set.seed(123)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)

### Simulate panel: states * years * sex

years   <- 2010:2016
n_per_g <- 300  # per (group x sex) cell (keeps things fast but informative)

df <- expand.grid(
  year  = years,
  group = c("treated","control"),
  sex   = c("female","male"),
  id    = 1:n_per_g
) |>
  mutate(
    id_all = interaction(group, sex, id, drop = TRUE),
    post   = as.integer(year >= 2013),
    treat  = as.integer(group == "treated"),
    fem    = as.integer(sex   == "female")
  )

set.seed(1)
df <- df |>
  group_by(id_all) |>
  mutate(
    age    = round(rnorm(1, 45, 6) + 0.4*(year - min(year)), 0),     # aging over time
    edu    = pmax(8, pmin(20, round(rnorm(1, 14, 2), 0))),          # years of education (mostly time-invariant)
    salary = pmax(15, rnorm(n(), mean = 35 + 0.8*(year - 2010) + 3*(edu-12), sd = 6)) # grows w/ time & edu
  ) |>
  ungroup()

# Baselines and trends and True DDD effect
set.seed(2)
alpha_g <- c(control = 800, treated = 820)             # group FE (levels differ)
lambda_t <- setNames(seq(0, -12, length.out = length(years)), years)  # secular decline
gamma_s <- c(female = 0, male = 0)                     # sex FE (can be 0)
beta_age <- 0.3; beta_edu <- -1.5; beta_salary <- -0.4 # covariate effects on outcome

ddd_true <- -10  # true triple-difference effect (treated*female*post)

df <- df |>
  mutate(
    base = alpha_g[group] + lambda_t[as.character(year)] + gamma_s[sex] +
      beta_age*age + beta_edu*edu + beta_salary*salary,
    mort_rate = base +
      ddd_true * (treat * fem * post) +      # <-- the true DDD effect
      rnorm(n(), 0, 8)  )

### Classic DDD regression with covariates

df <- df |> mutate(gsx = interaction(group, sex, drop = T))

est_ddd <- feols(mort_rate ~ treat*year*fem + age + edu + salary | year + gsx,
                 cluster = ~ gsx,
                 data = df)
summary(est_ddd)

### Visual pre-trend check

pre <- df %>% filter(year < 2013)

est_pre <- feols(
  mort_rate ~ i(year, treat*fem) + age + edu + salary | year + gsx,
  cluster = ~ gsx,
  data = pre
)
summary(est_pre)
wald(est_pre, keep = "year::")

pre_coefs <- broom::tidy(est_pre) |>
  dplyr::filter(grepl("^year::", term))

ggplot(pre_coefs, aes(x = as.numeric(gsub("year::","", term)), y = estimate)) +
  geom_point(size = 1) + geom_errorbar(aes(ymin = estimate - 1.96*std.error,
                                   ymax = estimate + 1.96*std.error), width = 0.15) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Year (pre-period)", y = "Pre-period triple-diff deviation",
       title = "DDD pre-trend check (treated×female vs others)") +
  theme_minimal()


### Event study DDD

iplot(est_pre, ref.line = 0,
      xlab = "Years since policy (relative to -1)",
      ylab = "DDD effect (treated×female vs others)",
      main = "Event-style DDD" )
