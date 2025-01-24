library(ggplot2)
library(corrplot)
library(reshape2)

question_descriptions <- list(
  CAPPUN = "Favor or oppose death penalty for murder? (1: Favor, 2: Oppose)",
  GUNLAW = "Would you favor or oppose a law requiring a police permit before buying a gun? (1: Favor, 2: Oppose)",
  ABRAPE = "Should a pregnant woman be allowed to obtain legal abortion if pregnant from rape? (1: Yes, 2: No)",
  SEX = "Sex: (1: Male, 2: Female)",
  CONFINAN = "Confidence in banks and financial institutions (1: A great deal, 2: Only some, 3: Hardly any)",
  SATJOB = "How satisfied are you with your job? (1: Very satisfied, 2: Moderately satisfied, 3: Dissatisfied)"
)

load("data/GSS_raw.RData")
rows_NA <- sum(apply(is.na(GSS), 1, any))
cat("Numero di righe con valori NA:", rows_NA, "\n")
GSS_clean <- na.omit(GSS)
cleaned_rows <- nrow(GSS_clean)
cat("Numero di righe rimanenti dopo la rimozione di NA:", cleaned_rows, "\n")


GSS_clean <- na.omit(GSS)


legends <- list(
  CAPPUN = c("1" = "Favor", "2" = "Oppose"),
  GUNLAW = c("1" = "Favor", "2" = "Oppose"),
  ABRAPE = c("1" = "Yes", "2" = "No"),
  SEX = c("1" = "Male", "2" = "Female"),
  CONFINAN = c("1" = "A great deal", "2" = "Only some", "3" = "Hardly any"),
  SATJOB = c("1" = "Very satisfied", "2" = "Moderately satisfied", "3" = "Dissatisfied")
)

consistent_theme <- theme_minimal(base_size = 12) + 
  theme(
    plot.title = element_text(size = 10, hjust = 0.5), 
    panel.grid = element_blank(),  
    panel.background = element_rect(fill = "white"), 
    axis.title = element_text(size = 10)
  )

# Bar plots for each variable
for (col_name in colnames(GSS_clean)) {
  freq_table <- table(GSS_clean[[col_name]], useNA = "ifany")
  percent_table <- round(100 * prop.table(freq_table), 1)
  
  if (col_name %in% names(legends)) {
    names(freq_table) <- legends[[col_name]][names(freq_table)]
    names(percent_table) <- legends[[col_name]][names(percent_table)]
  }
  
  plot_data <- data.frame(
    Category = names(freq_table),
    Frequency = as.numeric(freq_table),
    Percentage = as.numeric(percent_table)
  )
  
  plot <- ggplot(plot_data, aes(x = Category, y = Frequency, fill = Category)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_text(aes(label = paste(Percentage, "%")), vjust = -0.5, size = 4) +
    labs(
      title = question_descriptions[[col_name]], 
      x = "Category", 
      y = "Frequency"
    ) +
    consistent_theme  
  
  ggsave(paste0("results/", col_name, "_plot.png"), plot, width = 8, height = 6, dpi = 300)
  
  print(plot)
}

