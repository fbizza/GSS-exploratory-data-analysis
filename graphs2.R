load("data/GSS_clean.RData")

GSS_clean$CAPPUN <- factor(GSS_clean$CAPPUN, levels = c(1, 2), labels = c(" Favor", " Oppose"))
GSS_clean$GUNLAW <- factor(GSS_clean$GUNLAW, levels = c(1, 2), labels = c(" Favor", " Oppose"))
GSS_clean$SEX <- factor(GSS_clean$SEX, levels = c(1, 2), labels = c(" Male", " Female"))
GSS_clean$ABRAPE <- factor(GSS_clean$ABRAPE, levels = c(1, 2), labels = c(" Yes", " No"))
GSS_clean$CONFINAN <- factor(GSS_clean$CONFINAN, levels = c(1, 2, 3), labels = c(" A great deal", " Only some", " Hardly any"))
GSS_clean$SATJOB <- factor(GSS_clean$SATJOB, levels = c(1, 2, 3), labels = c(" Very satisfied", " Moderately satisfied", " Dissatisfied"))


GSS_clean$CAPPUN <- relevel(GSS_clean$CAPPUN, ref=' Oppose')
GSS_clean$GUNLAW <- relevel(GSS_clean$GUNLAW, ref=' Favor')
GSS_clean$SEX <- relevel(GSS_clean$SEX, ref=' Male')
GSS_clean$ABRAPE <- relevel(GSS_clean$ABRAPE, ref=' Yes')
# GSS_clean$CONFINAN <- as.numeric(GSS_clean$CONFINAN)
# GSS_clean$SATJOB <- as.numeric(GSS_clean$SATJOB)

GSS_table <- xtabs(~ SEX + CAPPUN + GUNLAW + ABRAPE + SATJOB + CONFINAN, data = GSS_clean)
ftable(GSS_table)

library(gRbase)
m.ind <- dmod(~ .^1, data = GSS_table, fit = TRUE)
summary(m.ind)

m.stepwise <- stepwise(m.ind, direction = "forward")
formula(m.stepwise)
summary(m.stepwise)
plot(m.stepwise, main = "Stepwise Model for GSS Data")


