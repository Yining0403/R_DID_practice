library(bacondecomp)
library(fixest)
library(ggplot2)
library(did)
library(stargazer)
library(dplyr)

# Simulate the data

set.seed(123)

N <- 600
Tt <- 10
id <- 1:N
t <- 1:10

df <- expand.grid(id = id, t = t)

cohort_draw <- runif(N)
g <- ifelse(cohort_draw < .35, 5,
            ifelse(cohort_draw < .70, 7, Inf))

df$g <- g[df$id]
df$D <- as.integer(df$t >= df$g & is.finite(df$g))
df$event <- ifelse(is.finite(df$g), df$t - df$g, NA_integer_)

a_i   <- rnorm(N, 0, 1)        # unit FE
lam_t <- seq(-1, 1, length.out = Tt)  # time FE
x_i   <- rnorm(N)


df$ai   <- a_i[df$id]
df$lt   <- lam_t[df$t]
df$x    <- x_i[df$id]

te_fun <- function(ev) ifelse(is.na(ev) | ev < 0, 0, 0.4 + 0.2*ev)  # dynamic
df$tau  <- te_fun(df$event)

eps <- rnorm(nrow(df), 0, 1)
df$y <- df$ai + df$lt + 0.3*df$x + df$D * df$tau + eps


### TWFE DID
twfe <- feols(y ~ D | id + t, df,
              cluster = ~ id)
summary(twfe)


### Goodman-Bacon decomposition
bd <- bacon(y ~ D, df, 
            id_var= "id", 
            time_var = "t")
head(bd)

sum(bd$estimate * bd$weight)

aggregate(weight ~ type, data = bd, sum)

bd %>%
  group_by(type) %>%
  summarise(wmean = weighted.mean(estimate, weight),
            wsum = sum(weight),
            .groups = "drop")

ggplot(bd, aes(estimate, weight, color = type)) + 
  geom_point(alpha = 0.8, size = 3) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(title = "Bacon decomposition: pieces & weights",
       x = "2x2 DID estimate",
       y = "Weight") +
  theme_minimal()


### Callaway & Sant’Anna (2021): group-time ATTs
df$g_cs <- ifelse(is.finite(df$g), df$g, 0)

cs <- att_gt(yname = "y",
             tname = "t",
             idname = "id",
             gname = "g_cs",
             xformla = ~ x,
             data = df,
             panel = TRUE,
             control_group = "nevertreated") 

# Overall ATT
agg_simple <- aggte(cs, type = "simple")
summary(agg_simple)

# Dynamic effects relative to treatment
agg_dyn <- aggte(cs, type = "dynamic")
summary(agg_dyn)
ggdid(agg_dyn) 

### Sun & Abraham (2020) with fixest::sunab() for event-study
est_sa <- feols(y ~ sunab(g, t) | id + t,
                data = df,
                cluster = "id")
iplot(est_sa)
