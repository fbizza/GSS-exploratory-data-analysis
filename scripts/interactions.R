load("data/GSS_clean.RData")

GSS_clean$CAPPUN <- factor(GSS_clean$CAPPUN, levels = c(1, 2), labels = c(" Favor", " Oppose"))
GSS_clean$GUNLAW <- factor(GSS_clean$GUNLAW, levels = c(1, 2), labels = c(" Favor", " Oppose"))
GSS_clean$SEX <- factor(GSS_clean$SEX, levels = c(1, 2), labels = c(" Male", " Female"))
GSS_clean$ABRAPE <- factor(GSS_clean$ABRAPE, levels = c(1, 2), labels = c(" Yes", " No"))
#GSS_clean$CONFINAN <- factor(GSS_clean$CONFINAN, levels = c(1, 2, 3), labels = c(" A great deal", " Only some", " Hardly any"))
#GSS_clean$SATJOB <- factor(GSS_clean$SATJOB, levels = c(1, 2, 3), labels = c(" Very satisfied", " Moderately satisfied", " Dissatisfied"))


GSS_clean$CAPPUN <- relevel(GSS_clean$CAPPUN, ref=' Oppose')
GSS_clean$GUNLAW <- relevel(GSS_clean$GUNLAW, ref=' Favor')
GSS_clean$SEX <- relevel(GSS_clean$SEX, ref=' Male')
GSS_clean$ABRAPE <- relevel(GSS_clean$ABRAPE, ref=' Yes')
GSS_clean$CONFINAN <- as.numeric(GSS_clean$CONFINAN)
GSS_clean$SATJOB <- as.numeric(GSS_clean$SATJOB)

# GSS_clean$SATJOB <- factor(GSS_clean$SATJOB, levels = c(1, 2, 3), labels = c(" Very satisfied", " Moderately satisfied", " Dissatisfied"))
# GSS_clean$SATJOB <- relevel(GSS_clean$SATJOB, ref=' Very satisfied')
# logistic_model <- glm(CAPPUN ~ SEX + GUNLAW + ABRAPE + SATJOB + CONFINAN,
#                       data = GSS_clean,
#                       family = binomial)
# 
# summary(logistic_model)
logistic_model <- glm(GUNLAW ~ SEX + CAPPUN + ABRAPE + SATJOB + CONFINAN,
                      data = GSS_clean,
                      family = binomial)

summary(logistic_model)
# GSS_clean$SATJOB <- factor(GSS_clean$SATJOB, levels = c(1, 2, 3), labels = c(" Very satisfied", " Moderately satisfied", " Dissatisfied"))
# GSS_clean$SATJOB <- relevel(GSS_clean$SATJOB, ref=' Very satisfied')
# summary(logistic_model)
# logistic_model <- glm(ABRAPE ~ SEX + CAPPUN + GUNLAW + SATJOB + CONFINAN,
#                       data = GSS_clean,
#                       family = binomial)
# 
# summary(logistic_model)

# linear_model <- lm(SATJOB ~ SEX + CAPPUN + GUNLAW + ABRAPE + CONFINAN, data = GSS_clean)
# 
# summary(linear_model)


# 
# linear_model <- lm(CONFINAN ~ SEX + CAPPUN + GUNLAW + ABRAPE + SATJOB, data = GSS_clean)
# 
# summary(linear_model)

# Backward
fit_all <- glm(GUNLAW ~ SEX + CAPPUN + ABRAPE + CONFINAN*JOBSAT,
#                       data = GSS_clean,
#                       family = binomial)

#AIC
backward_aic <- step(fit_all, direction="backward", k=2)
formula(backward_aic)
summary(backward_aic)

#BIC
backward_bic <- step(fit_all, direction="backward", k=log(nobs(fit_all)))
formula(backward_bic)
summary(backward_bic)

#Forward
# fit_0 <- glm(GUNLAW ~ 1, data = GSS_clean, family="binomial")
#AIC
# forward_AIC <- step(fit_0, scope=formula(fit_all), direction="forward", k=2)
# summary(forward_AIC)

#BIC
# forward_bic <- step(fit_0, scope=formula(fit_all), direction="forward", k=log(nobs(fit_all)))
# summary(forward_bic)

#Both
#AIC
# both_aic <- step(fit_0, scope=formula(fit_all), direction="both", k=2)
# summary(both_aic)

#BIC
# both_bic <- step(fit_0, scope=formula(fit_all), direction="both", k=log(nobs(fit_all)))
# summary(both_bic)


#TEST 
# logistic_model <- glm(GUNLAW ~ SEX + CAPPUN + ABRAPE,
#                       data = GSS_clean,
#                       family = binomial)
# summary(logistic_model)
# 
# 
# 
# 
# coeff <- coef(logistic_model)
# 
# calc_probability <- function(SEX, CAPPUN, ABRAPE, coeff) {
#   intercept <- coeff["(Intercept)"]
#   sex_effect <- ifelse(SEX == " Female", coeff["SEX Female"], 0)
#   cappun_effect <- ifelse(CAPPUN == " Favor", coeff["CAPPUN Favor"], 0)
#   abrape_effect <- ifelse(ABRAPE == " No", coeff["ABRAPE No"], 0)
#   z <- intercept + sex_effect + cappun_effect + abrape_effect
#   p <- 1 / (1 + exp(-z))
#   return(p)
# }
# 
# prob_1 <- calc_probability(SEX = " Female", CAPPUN = " Favor", ABRAPE = " No", coeff)
# prob_2 <- calc_probability(SEX = " Male", CAPPUN = " Oppose", ABRAPE = " Yes", coeff)
# 
# cat("Probabilità Individuo 1:", prob_1, "\n")
# cat("Probabilità Individuo 2:", prob_2, "\n")





GSS_men <- subset(GSS_clean, SEX == " Male")

# Modello logistico solo per gli uomini
logistic_model_men <- glm(GUNLAW ~ CAPPUN + ABRAPE + SATJOB + CONFINAN ,
                          data = GSS_men,
                          family = binomial)

summary(logistic_model_men)