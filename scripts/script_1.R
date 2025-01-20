#DATASET 2 GSS  General Social Survey data in USA
#CAPPUN: Favor or oppose death penalty for murder?  1: favor, 2: oppose
#GUNLAW: would you favor or oppose a law which would require a person to obtain a police
#permit before he or she could buy a gun? 1: favor, 2: oppose
#ABRAPE:Do you think it should be possible for a pregnant woman to obtain legal abortion
#if she became pregnant as a result of rape? 1: yes, 2: no
#SEX: sesso, 2:F, 1: M
#CONFINAN: confidence in banks and financial institutions, 1: a great deal, 2: only some, 3: hardly any
#SATJOB: how satisfied are you with the work you do? 1:very satisfied, 2:moderately satisfied, 3:dissatisfied

original_rows <- nrow(GSS)
cat("\nNumber of rows before cleaning:", original_rows, "\n")

rows_with_na <- sum(apply(GSS, 1, function(row) any(is.na(row))))
cat("Number of rows with at least one NA value:", rows_with_na, "\n")

GSS_clean <- na.omit(GSS)

cleaned_rows <- nrow(GSS_clean)
cat("Number of rows after cleaning:", cleaned_rows, "\n")

legends <- list(
  CAPPUN = c("1" = "Favor", "2" = "Oppose"),
  GUNLAW = c("1" = "Favor", "2" = "Oppose"),
  ABRAPE = c("1" = "Yes", "2" = "No"),
  SEX = c("1" = "Male", "2" = "Female"),
  CONFINAN = c("1" = "A great deal", "2" = "Only some", "3" = "Hardly any"),
  SATJOB = c("1" = "Very satisfied", "2" = "Moderately satisfied", "3" = "Dissatisfied")
)

# Frequency tables with percentages and legends
cat("\nFrequency Tables (Percentages)\n")
for (col_name in colnames(GSS_clean)) {
  cat("\nVariable:", col_name, "\n")
  
  freq_table <- table(GSS_clean[[col_name]], useNA = "ifany")
  percent_table <- round(100 * prop.table(freq_table), 1) 
  
  if (col_name %in% names(legends)) {
    names(freq_table) <- legends[[col_name]][names(freq_table)]
    names(percent_table) <- legends[[col_name]][names(percent_table)]
  }
  
  print(percent_table)
}

cor_matrix <- cor(GSS_clean, method = "pearson")
cat("\nCorrelation Matrix for the Cleaned Dataset:\n")
print(cor_matrix)

library(corrplot)

corrplot(cor_matrix, method = "color", type = "upper", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.cex = 0.8, tl.col = "black", number.cex = 0.7)



