
load("data/GSS_clean.RData")

GSS_clean$CAPPUN <- factor(GSS_clean$CAPPUN, levels = c(1, 2), labels = c(" Favor", " Oppose"))
GSS_clean$GUNLAW <- factor(GSS_clean$GUNLAW, levels = c(1, 2), labels = c(" Favor", " Oppose"))
GSS_clean$SEX <- factor(GSS_clean$SEX, levels = c(1, 2), labels = c(" Male", " Female"))
GSS_clean$ABRAPE <- factor(GSS_clean$ABRAPE, levels = c(1, 2), labels = c(" Yes", " No"))
GSS_clean$CONFINAN <- as.numeric(GSS_clean$CONFINAN)
GSS_clean$SATJOB <- as.numeric(GSS_clean$SATJOB)

m_saturated <- dmod(~.^., data=GSS_clean)
plot(m_saturated)

selected_model <- stepwise(m_saturated)
plot(selected_model)