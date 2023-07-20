# Load necessary libraries
library(vars)
library(ggplot2)
library(grid)
library(gridExtra)

# Source the custom functions
source("modules/trivar.R")
# Run trivar function
source("modules/make_matrix.R")
# Load the data and prepare the matrix
# Replace "data_matrix" with your data matrix containing V1, V2, and V3
data_matrix <- make_matrix("data/data.txt")
trivar_res <- trivar(data_matrix)  # Please replace your_matrix_here with the actual matrix.

# Compute structural multipliers using irf function from vars package
IRF <- irf(trivar_res$var_model, impulse = colnames(trivar_res$Uhat), response = colnames(trivar_res$Uhat), boot = FALSE, n.ahead = length(trivar_res$time) - trivar_res$p - 1)

# Compute structural shocks Ehat from reduced form shocks Uhat
Ehat <- solve(t(chol(trivar_res$SIGMA))) %*% t(trivar_res$Uhat)


# Cross-multiply the weights for the effect of a given shock on the real oil price (given by the relevant row of IRF) with the structural shock in question
yhat1 <- yhat2 <- yhat3 <- rep(0, length(trivar_res$time) - trivar_res$p)

for(i in 1:(length(trivar_res$time) - trivar_res$p)){
  yhat1[i] <- sum(IRF$irf$V1[1:i,1] * Ehat[1, i:1])  # Replace your_var1_name with the actual variable name
  yhat2[i] <- sum(IRF$irf$V2[1:i,2] * Ehat[2, i:1])  # Replace your_var2_name with the actual variable name
  yhat3[i] <- sum(IRF$irf$V3[1:i,3] * Ehat[3, i:1])  # Replace your_var3_name with the actual variable name
}

# Create dataframe for plotting
plot_data1 <- data.frame(
  time = trivar_res$time[(trivar_res$p + 1):length(trivar_res$time)],
  yhat = yhat1
)
plot_data2 <- data.frame(
  time = trivar_res$time[(trivar_res$p + 1):length(trivar_res$time)],
  yhat = yhat2
)
plot_data3 <- data.frame(
  time = trivar_res$time[(trivar_res$p + 1):length(trivar_res$time)],
  yhat = yhat3
)

# Create plots
figure4_plotter <- function(plot_data, yhat, subtitle) {
  result <- ggplot(plot_data, aes(x = time)) +
    geom_line(aes(y = yhat), color = 'blue') +
    ylim(-100, 100)+
    labs(title = subtitle,
         y = "",                   
         x = "Time") +
    theme_minimal()
  return(result)
}

pl1 <- figure4_plotter(plot_data1, yhat1, "Cumulative Effect of Oil Supply Shock on Real Price of Crude Oil")
pl2 <- figure4_plotter(plot_data2, yhat2, "Cumulative Effect of Aggregate Demand Shock on Real Price of Crude Oil")
pl3 <- figure4_plotter(plot_data3, yhat3, "Cumulative Effect of Oil-Market Specific Demand Shock on Real Price of Crude Oil")

gridExtra::grid.arrange(
  pl1, 
  pl2, 
  pl3, 
  nrow = 3, 
  top=textGrob("Historical Decomposition of Real Price of Oil 1976.1-2007.12")
)