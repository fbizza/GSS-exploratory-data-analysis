# GSS_male <- subset(GSS_clean, SEX == " Male")
# GSS_female <- subset(GSS_clean, SEX == " Female")
# 
# GSS_male <- GSS_male[, !names(GSS_male) %in% "SEX"]
# GSS_female <- GSS_female[, !names(GSS_female) %in% "SEX"]
# 
# m_saturated_male <- dmod(~.^., data=GSS_male)
# plot(m_saturated_male)
# 
# m_saturated_female <- dmod(~.^., data=GSS_female)
# plot(m_saturated_female)
# 
# selected_model_male <- stepwise(m_saturated_male)
# plot(selected_model_male)
# 
# selected_model_female <- stepwise(m_saturated_female)
# plot(selected_model_female)
# 
# formula(selected_model_male)
# formula(selected_model_female)




logistic_model_male <- glm(GUNLAW ~ CAPPUN,
                           data = subset(GSS_clean, SEX == " Male"),
                           family = binomial)

summary(logistic_model_male)

calc_probability_male <- function(CAPPUN, coeff) {
  intercept <- coeff["(Intercept)"]
  
  cappun_effect <- ifelse(CAPPUN == " Favor", coeff["CAPPUN Favor"], 0)
  z <- intercept + cappun_effect
  
  p <- 1 / (1 + exp(-z))
  return(p)
}

coeff_male <- coef(logistic_model_male)

prob_male_progressive <- calc_probability_male(CAPPUN = " Favor", coeff_male)

cat("Probabilità per l'uomo progressista:", prob_male_progressive, "\n")

