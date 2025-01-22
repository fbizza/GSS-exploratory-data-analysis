
library(igraph)
library(bnlearn)
library(ggm)


# Simulate Data (Replace with your actual data loading)
# Example: Replace `data <- read.csv("your_file.csv")` with your dataset
load("data/GSS_clean.RData")
data <- GSS_clean
# Convert variables to factors
data <- lapply(data, as.factor)
data <- as.data.frame(data)

# --------- Bayesian Network Approach ---------
# Estimate the network structure using Hill-Climbing algorithm
bn_model <- hc(data)  # Hill-Climbing structure learning
print("Bayesian Network Structure:")
print(bn_model)

# Visualize the Bayesian Network
plot(bn_model, main = "Bayesian Network")

# --------- Undirected Graph Approach ---------
# Convert factors to numeric for correlation computation
data_numeric <- data.frame(lapply(data, as.numeric))

# Compute correlation matrix
cor_matrix <- cor(data_numeric)

# Thresholding to create adjacency matrix for visualization
threshold <- 0.1
adj_matrix <- (abs(cor_matrix) > threshold) * 1  # Binary adjacency matrix

# Create undirected graph from adjacency matrix
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)

# Visualize the undirected graph
plot(g, 
     vertex.label = colnames(data_numeric), 
     vertex.size = 20, 
     edge.arrow.size = 0.5,
     main = "Undirected Graph")

# Optional: Save the graph plot to a file
# png("undirected_graph.png")
# plot(g, vertex.label = colnames(data_numeric), vertex.size = 20, edge.arrow.size = 0.5)
# dev.off()

# --------- Interpretation ---------
# - Bayesian Network shows directional (causal) dependencies.
# - Undirected Graph highlights associations without causality.
# - Modify the threshold variable to adjust edge density in the undirected graph.
# - Replace simulated data with your actual dataset for real insights.
