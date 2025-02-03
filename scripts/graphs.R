library(bnlearn)


load("data/GSS_clean.RData")
GSS_clean$CAPPUN <- factor(GSS_clean$CAPPUN, levels = c(1, 2), labels = c(" Favor", " Oppose"))
GSS_clean$GUNLAW <- factor(GSS_clean$GUNLAW, levels = c(1, 2), labels = c(" Favor", " Oppose"))
GSS_clean$SEX <- factor(GSS_clean$SEX, levels = c(1, 2), labels = c(" Male", " Female"))
GSS_clean$ABRAPE <- factor(GSS_clean$ABRAPE, levels = c(1, 2), labels = c(" Yes", " No"))
GSS_clean$CONFINAN <- factor(GSS_clean$CONFINAN, levels = c(1, 2, 3), labels = c(" A great deal", " Only some", " Hardly any"))
GSS_clean$SATJOB <- factor(GSS_clean$SATJOB, levels = c(1, 2, 3), labels = c(" Very satisfied", " Moderately satisfied", " Dissatisfied"))

data <- GSS_clean
#data <- subset(GSS_clean, SEX == " Male")

data <- lapply(data, as.factor)
data <- as.data.frame(data)


bn_model <- hc(data)  
plot(bn_model, main = "Bayesian Network")


# Blacklist: evita che le opinioni causino il sesso
blacklist <- data.frame(from = c("CAPPUN", "GUNLAW", "ABRAPE", "CONFINAN", "SATJOB"),
                        to = c("SEX", "SEX", "SEX", "SEX", "SEX"))



bn_model <- hc(data, blacklist = blacklist)


plot(bn_model, main = "Bayesian Network with blacklist")

fitted_model <- bn.fit(bn_model, data = GSS_clean)

grain_model <- as.grain(fitted_model)

compiled_model <- compile(grain_model)

# compiled_model <- setEvidence(compiled_model, evidence = list(SEX = " Female"))

conditional_result <- querygrain(compiled_model, nodes = c("GUNLAW", "SEX", "CAPPUN"), type = "conditional")
print(conditional_result)

# compiled_model <- retractEvidence(compiled_model)
# 
# marginal_result <- querygrain(compiled_model, nodes = "GUNLAW", type = "marginal")
# print("Probabilità marginali di GUNLAW:")
# print(marginal_result)
# 
# compiled_model <- retractEvidence(compiled_model)

