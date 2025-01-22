library(bnlearn)


load("data/GSS_clean.RData")
data <- GSS_clean


data <- lapply(data, as.factor)
data <- as.data.frame(data)


bn_model <- hc(data, score = "bic")  
print("Learned Bayesian Network Structure:")
print(bn_model)
plot(bn_model, main = "Bayesian Network")

# Blacklist: evita che le opinioni causino il sesso
blacklist <- data.frame(from = c("CAPPUN", "GUNLAW", "ABRAPE", "CONFINAN", "SATJOB"),
                        to = c("SEX", "SEX", "SEX", "SEX", "SEX"))


# Impara il modello rispettando i vincoli
bn_model <- hc(data, score = "bic", blacklist = blacklist)

# Visualizza il risultato
print("Learned Bayesian Network Structure:")
print(bn_model)
plot(bn_model, main = "Bayesian Network")



