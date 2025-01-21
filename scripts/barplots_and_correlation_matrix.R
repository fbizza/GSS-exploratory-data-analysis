library(ggplot2)
library(corrplot)
library(reshape2)  # For reshaping the correlation matrix

question_descriptions <- list(
  CAPPUN = "Favor or oppose death penalty for murder? (1: Favor, 2: Oppose)",
  GUNLAW = "Would you favor or oppose a law requiring a police permit before buying a gun? (1: Favor, 2: Oppose)",
  ABRAPE = "Should a pregnant woman be allowed to obtain legal abortion if pregnant from rape? (1: Yes, 2: No)",
  SEX = "Sex: (1: Male, 2: Female)",
  CONFINAN = "Confidence in banks and financial institutions (1: A great deal, 2: Only some, 3: Hardly any)",
  SATJOB = "How satisfied are you with your job? (1: Very satisfied, 2: Moderately satisfied, 3: Dissatisfied)"
)

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

# Create and display bar plots for each variable with question descriptions
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

# Correlation matrix calculation
cor_matrix <- cor(GSS_clean, method = "pearson")

# Save the correlation matrix plot using corrplot
png("results/correlation_matrix_plot.png", width = 1000, height = 800)
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.cex = 0.8, tl.col = "black", 
         number.cex = 0.7, 
         addCoef.col = "black", cl.cex = 0.8, 
         title = "Correlation Matrix", 
         font.main = 1, font.lab = 1, font.axis = 1, 
         mar = c(0, 0, 3, 0))  
dev.off()

# Create and save a heatmap of the correlation matrix using ggplot2
cor_long <- melt(cor_matrix)
colnames(cor_long) <- c("Variable1", "Variable2", "Correlation")

heatmap <- ggplot(cor_long, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +  # Display correlation values rounded to 2 decimals
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Correlation Matrix Heatmap",
    x = "Variables",
    y = "Variables",
    fill = "Correlation"
  )

ggsave("results/correlation_matrix_heatmap.png", heatmap, width = 8, height = 6, dpi = 300)

print(heatmap)
