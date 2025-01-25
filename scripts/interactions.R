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
# logistic_model <- glm(GUNLAW ~ SEX + CAPPUN + ABRAPE + SATJOB + CONFINAN,
#                       data = GSS_clean,
#                       family = binomial)
# 
# summary(logistic_model)
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



linear_model <- lm(CONFINAN ~ SEX + CAPPUN + GUNLAW + ABRAPE + SATJOB, data = GSS_clean)

summary(linear_model)