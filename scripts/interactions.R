load("data/GSS_clean.RData")

GSS_clean$CAPPUN <- factor(GSS_clean$CAPPUN, levels = c(1, 2), labels = c(" Favor", " Oppose"))
GSS_clean$GUNLAW <- factor(GSS_clean$GUNLAW, levels = c(1, 2), labels = c(" Favor", " Oppose"))
GSS_clean$SEX <- factor(GSS_clean$SEX, levels = c(1, 2), labels = c(" Male", " Female"))
GSS_clean$ABRAPE <- factor(GSS_clean$ABRAPE, levels = c(1, 2), labels = c(" Yes", " No"))
GSS_clean$CONFINAN <- factor(GSS_clean$CONFINAN, levels = c(1, 2, 3), labels = c(" A great deal", " Only some", " Hardly any"))
GSS_clean$SATJOB <- factor(GSS_clean$SATJOB, levels = c(1, 2, 3), labels = c(" Very satisfied", " Moderately satisfied", " Dissatisfied"))
str(GSS_clean)

GSS_clean$CAPPUN <- relevel(GSS_clean$CAPPUN, ref=' Oppose')
GSS_clean$GUNLAW <- relevel(GSS_clean$GUNLAW, ref=' Favor')
GSS_clean$SEX <- relevel(GSS_clean$SEX, ref=' Male')
GSS_clean$ABRAPE <- relevel(GSS_clean$ABRAPE, ref=' Yes')

logistic_model <- glm(CAPPUN ~ SEX + GUNLAW + ABRAPE,
                      data = GSS_clean,
                      family = binomial)

summary(logistic_model)


