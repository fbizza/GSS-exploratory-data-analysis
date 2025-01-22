load("data/GSS_clean.RData")

GSS_clean$CAPPUN <- factor(GSS_clean$CAPPUN, levels = c(1, 2), labels = c(" Favor", " Oppose"))
GSS_clean$GUNLAW <- factor(GSS_clean$GUNLAW, levels = c(1, 2), labels = c(" Favor", " Oppose"))
GSS_clean$SEX <- factor(GSS_clean$SEX, levels = c(1, 2), labels = c(" Male", " Female"))
GSS_clean$ABRAPE <- factor(GSS_clean$ABRAPE, levels = c(1, 2), labels = c(" Yes", " No"))
GSS_clean$CONFINAN <- factor(GSS_clean$CONFINAN, levels = c(1, 2, 3), labels = c(" A great deal", " Only some", " Hardly any"))
GSS_clean$SATJOB <- factor(GSS_clean$SATJOB, levels = c(1, 2, 3), labels = c(" Very satisfied", " Moderately satisfied", " Dissatisfied"))
str(GSS_clean)
logistic_model <- glm(CAPPUN ~ SEX + GUNLAW + ABRAPE + CONFINAN + SATJOB,
                      data = GSS_clean,
                      family = binomial)

summary(logistic_model)

logistic_model <- glm(CAPPUN ~ SEX,
                      data = GSS_clean,
                      family = binomial)

summary(logistic_model)



