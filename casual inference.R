set.seed(123)
library(stargazer)
library(cobalt)

data("lalonde")

ps_score <- glm(treat ~ age + educ + race + married + nodegree + re74 + re75, 
                lalonde, 
                family = "binomial")
lalonde$ps <- predict(ps_score, type = "response")
lalonde$w <- ifelse(lalonde$treat == 1, 1/lalonde$ps, 1/(1 - lalonde$ps))

bal_before <- bal.tab(treat ~ age + educ + race + married + nodegree + re74 + re75, 
                      lalonde,
                      method = "weighting")
love.plot(bal_before)
bal_after <- bal.tab(treat ~ age + educ + race + married + nodegree + re74 + re75, 
                     lalonde,
                     weights = lalonde$w,
                     method = "weighting")
love.plot(bal_after)

ATE <- lm(re78 ~ treat, lalonde, weights = w)
summary(ATE)

# nearist-neighbor matching
library(MatchIt)
m_out <- matchit(
  treat ~  age + educ + race + married + nodegree + re74 + re75,
  data = lalonde,
  method = "nearest",
  ditance = "logit",
  ratio = 1,
  replace = F,
  caliper = 0.2
)
summary(m_out)
matched_data <- match.data(m_out)
lm_out <- lm(re78 ~ treat, matched_data, weights = weights)
stargazer(lm_out, type = "text")
