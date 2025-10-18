set.seed(123)
library(dplyr)
library(ggplot2)
library(stargazer)

### 2x2 Difference-in-Differences Example
data <- expand.grid(
  group = c("Control", "Treatment"),
  time = c("Before", "After"),
  rep = 1:50
)

mu_control_before <- 20
mu_control_after <- 22
mu_treat_before <- 18
mu_treat_after <- 23

data <- data %>%
  mutate(outcome = case_when(
    group == "Control" & time == "Before" ~ rnorm(n(), mu_control_before, 2),
    group == "Control" & time == "After" ~ rnorm(n(), mu_control_after, 2),
    group == "Treatment" & time == "Before" ~ rnorm(n(), mu_treat_before, 2),
    group == "Treatment" & time == "After" ~ rnorm(n(), mu_treat_after, 2)
  ))

data <- data %>%
  mutate(
    treat = ifelse(group == "Treatment", 1, 0),
    post = ifelse(time == "After", 1, 0),
    did = treat*post
  )

did_model <- lm(outcome ~ treat + post + did, data)
stargazer(did_model, type = "text")

did_effect <- coef(did_model)["did"]
did_effect

plot_data <- data %>%
  group_by(group, time) %>%
  summarize(mean_outcome = mean(outcome), .groups = "drop")

ggplot(plot_data, aes(time, mean_outcome, color = group, group = group)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "grey50") + 
  annotate("text", x = 1.5, y = max(plot_data$mean_outcome) + 1,
           label = "Policy implemented", angle = 90, vjust = -0.5) +
  labs(title = "2x2 Difference-in-Differences Example", 
       y = "Average Outcome", x = "Time") +
  theme_minimal()
  