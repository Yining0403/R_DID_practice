set.seed(123)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)

### Simulate the panel data

states <- c("AL","AK","AZ","CA","FL","GA","KY","NC")
years <- 2010:2018

### Hypothetical expansion years (NA = never expanded in sample window)

exp_tbl <- tibble::tribble(
  ~state, ~exp_year,
  "AL",   NA,   # never
  "AK",   2015, # late
  "AZ",   2014, # early
  "CA",   2014, # early
  "FL",   NA,   # never
  "GA",   NA,   # never
  "KY",   2014, # early
  "NC",   NA    # never
)

### Base state_specific mortality level + pop sizes
state_fx <- tibble(
  state = states,
  mu = rnorm(length(states), mean = 800, sd = 40)
)

panel <- expand.grid(state = states, year = years) %>%
  as_tibble() %>%
  left_join(state_fx, by = "state") %>%
  left_join(exp_tbl, by = "state") %>%
  mutate(
    pop_mil = runif(n(), 1.5, 10),
    pop = round(pop_mil * 1e6),
    trend = -3 * (year-2010),
    treated = ifelse(!is.na(exp_year) & year >= exp_year, 1, 0),
    te = ifelse(treated == 1, -8, 0),
    mort_rate = mu + trend + te + rnorm(n(), 0, 10)
  )



### TWFE event

est_es <- feols(
  mort_rate ~ sunab(exp_year, year) | state + year,
  cluster = ~ state,
  weights = ~ pop,
  data = panel
)

iplot(est_es, ref.line = 0, main = "Event Study: Medicaid Expansion → Mortality",
      xlab = "Event time (years since expansion)", 
      ylab = "Effect on mortality (per 100k)")


### Quick pre-trend check (graphical)

panel <- panel %>%
  mutate(ever_treated = as.integer(!is.na(exp_year)))

avg_pretrend <- panel %>%
  filter(year < 2014) %>%
  group_by(year, ever_treated) %>%
  summarize(m = weighted.mean(mort_rate, pop), .groups = "drop") %>%
  mutate(group = ifelse(ever_treated == 1, "Ever-treated states", "Never-treated states"))

ggplot(avg_pretrend, aes(year, m, color = group)) + 
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Pre-treatment outcome trends (weighted means)",
       x = "Year", y = "Mortality per 100k", color = NULL) + 
  theme_minimal()

### simple 2*2DID
early_states <- exp_tbl %>% filter(exp_year == 2014) %>% pull(state)

dd_22 <- panel %>%
  filter(year %in% c(2013, 2015),
         state %in% c(early_states, exp_tbl$state[is.na(exp_tbl$exp_year)])) %>%
  mutate(post = as.integer(year == 2015),
         treat = as.integer(state %in% early_states),
         did = post * treat)

est_22 <- feols(mort_rate ~ treat + post + did | 0,
                cluster = ~ state, weights = ~pop, data = dd_22)
summary(est_22)


### Balance snapshot (pre-period covariates/outcomes)
# Here we only have mort_rate—show standardized diff in 2013

bal_2013 <- panel %>%
  filter(year == 2013) %>%
  mutate(treat2014 = as.integer(state %in% early_states))

bal_tbl <- bal_2013 %>%
  group_by(treat2014) %>%
  summarise(m = weighted.mean(mort_rate, pop), .group = "drop")
bal_tbl

smd <- function(x, w, g){
  wm <- function(z, wz) sum(z*wz)/sum(wz)
  wv <- function(z, wz){
    m <- wm(z, wz); sum(wz * (z - m)^2)/sum(wz)
  }
  x1 <- x[g == 1]; w1 <- w[g == 1]
  x0 <- x[g == 0]; w0 <- w[g == 0]
  m1 <- wm(x1, w1); m0 <- wm(x0, w0)
  s1 <- sqrt(wv(x1, w1)); s0 <- sqrt(wv(x0, w0))
  sp <- sqrt(((length(x1) - 1)*s1^2 + (length(x0) - 1) *s0^2)/((length(x1)-1) + (length(x0) -1)))
  (m1 - m0)/sp
}

SMD_2013 <- with(bal_2013, smd(mort_rate, pop, treat2014))
SMD_2013

# or
library(cobalt)
SMD_2013_2 <- bal.tab(treat2014 ~ mort_rate, 
                      data = bal_2013,
                      weights = bal_2013$pop,
                      s.d.denom = "pooled" )
SMD_2013_2
love.plot(SMD_2013_2)
