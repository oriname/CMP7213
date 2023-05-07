# Install and Load Libraries
install.packages("doParallel")
install.packages(c("quantmod", "GA", "SpatialEpi", "ggplot2", "tidyverse"))
library(tidyverse)
library(lubridate)
library(GA)
library(quantmod)
library(SpatialEpi)
library(ggplot2)
library(doParallel)

# Define the stock tickers and load the stock data
myStocks <- c("JPM", "PFE", "WMT", "KO", "DIS", "AMZN", "AAPL", "MSFT", "IBM", "GOOG")
getSymbols(myStocks, src="yahoo", from="2017-01-01", to="2022-01-01")

# Calculate the monthly returns and store them in the myRetData variable
myRetData <- do.call(merge, lapply(myStocks, function(x) monthlyReturn(get(x))))
names(myRetData) <- paste(myStocks)
myRetData <- na.omit(myRetData)
dimensions <- as.integer(length(myStocks))

# Convert the wide-format data to long-format data, use the gather() function
myRetData_long <- myRetData %>%
  as_tibble() %>%
  mutate(Time = index(myRetData)) %>%
  gather(Asset, Returns, -Time)

# Calculate the monthly returns for each asset
myRetData_monthly <- myRetData_long %>%
  mutate(Month = floor_date(Time, unit = "month")) %>%
  group_by(Asset, Month) %>%
  summarize(Returns = sum(Returns) * 100, .groups = "drop") # Multiply returns by 100

# Plot monthly returns
p <- ggplot(myRetData_monthly, aes(x = Month, y = Returns, color = Asset)) +
  geom_line(linewidth = 1.5) +
  labs(title = "Portfolio Monthly Returns 2017-2022",
       x = "Time",
       y = "Returns") + # Removed * 100 in the label since returns are already multiplied by 100
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  ylim(-30, 30)
print(p)


# Print the table for the monthly returns of the assets
print(myRetData_monthly)


########################################################



# Calculate the monthly returns for each asset
myRetData_monthly <- myRetData_long %>%
  mutate(Month = floor_date(Time, unit = "month")) %>%
  group_by(Asset, Month) %>%
  summarize(Returns = sum(Returns) * 100, .groups = "drop") # Multiply returns by 100

# Make sure that myRetData_monthly is a data frame or a tibble
myRetData_monthly <- as_tibble(myRetData_monthly)

# Find the top 5 months for each asset
top_months <- myRetData_monthly %>%
  group_by(Asset) %>%
  arrange(desc(Month)) %>%
  slice(1:2)

# Print the top 5 months for each asset
print(top_months)

ggplot(top_months, aes(x = Month, y = Returns, fill = Asset)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Monthly Returns by Asset",
       x = "Month",
       y = "Returns",
       fill = "Asset")

# #############

# Calculate the variance of the monthly returns for each asset
myRetData_variance <- myRetData_monthly %>%
  group_by(Asset) %>%
  summarize(Variance = var(Returns), .groups = "drop")

# Print the variance for each asset
print(myRetData_variance)

ggplot(myRetData_variance, aes(x = Asset, y = Variance, fill = Asset)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Asset variance",
       x = "Asset",
       y = "Variance",
       fill = "Asset")

############################################


# Calculate the variance of the monthly returns for each asset
myRetData_variance <- myRetData_monthly %>%
  group_by(Asset) %>%
  summarize(Variance = var(Returns), .groups = "drop")

# Create a box plot of the variance for each asset
ggplot(myRetData_variance, aes(x = Asset, y = Variance)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Asset variance",
       x = "Asset",
       y = "Variance")

#############################################

# Load the stock data
stockData <- new.env()
getSymbols(myStocks, env = stockData, src="yahoo", auto.assign = TRUE,  from="2017-01-01", to="2022-01-01")

# Extract the closing prices
closingPrices <- lapply(stockData, function(x) Cl(x))

# Merge the closing prices into a single data frame
closingPricesMerged <- do.call(merge, closingPrices)

# Get the 10 most recent dates
recentDates <- tail(index(closingPricesMerged), 10)

# Get the closing prices for the 3 most recent dates
recentClosingPrices <- closingPricesMerged[recentDates,]

# Print the recent closing prices
print(recentClosingPrices)

# Convert the data frame to a long format
closingPricesMelted <- melt(closingPricesMerged)

# Plot the closing prices using ggplot2
ggplot(closingPricesMelted, aes(x = as.Date(variable), y = value, color = variable)) +
  geom_line() +
  labs(title = "Closing Prices for Selected Stocks", x = "Date", y = "Closing Price") +
  scale_color_discrete(name = "Stocks") +
  theme_minimal()



# Filter the closing prices for the 10 most recent dates
recentClosingPrices <- closingPricesMerged[recentDates,]

# Convert the recent closing prices data frame to a long format
recentClosingPricesLong <- recentClosingPrices %>% 
  as.data.frame() %>%
  rownames_to_column(var = "Date") %>%
  pivot_longer(cols = -Date, names_to = "Stock", values_to = "Close")

# Create the line plot using ggplot2
ggplot(recentClosingPricesLong, aes(x = Date, y = Close, color = Stock)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Stock Closing Prices for the 10 Most Recent Dates",
       x = "Date",
       y = "Closing Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Convert the recent closing prices data frame to a long format
recentClosingPricesLong <- recentClosingPrices %>% 
  as.data.frame() %>%
  rownames_to_column(var = "Date") %>%
  pivot_longer(cols = -Date, names_to = "Stock", values_to = "Close")

# Create the line plot with points using ggplot2
ggplot(recentClosingPricesLong, aes(x = Date, y = Close, color = Stock, group = Stock)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Stock Closing Prices for the 10 Most Recent Dates",
       x = "Date",
       y = "Closing Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####################################################


# Define the fitness function with risk constraints, and other helper functions
normalize <- function(x) {
  x / sum(x)
}

getRisk <- function(x, dimensions, baseCovMatrix) {
  if (is.list(x)) {
    x <- unlist(x)
  }
  
  COV_MATRIX <- matrix(data = NA, nrow = dimensions, ncol = dimensions)
  
  for (i in 1:dimensions) {
    for (j in 1:dimensions) {
      COV_MATRIX[j, i] <- baseCovMatrix[j, i] * x[j] * x[i]
    }
  }
  
  final_return <- sum(COV_MATRIX)
  return(final_return)
}

getReturns <- function(x, meanReturns) {
  sum(meanReturns * x)
}

objectiveWeight <- seq(0.1, 0.9, 0.1)

fitness <- function(x, dimensions, baseCovMatrix, meanReturns) {
  weights <- normalize(x)
  risk <- getRisk(weights, dimensions, baseCovMatrix)
  returns <- getReturns(weights, meanReturns)
  
  if (is.na(risk) || sqrt(risk) > 0.12) {
    return(list(fitness = -Inf, best_fitness_values = numeric(100)))
  }
  
  final <- objectiveWeight * returns + (1 - objectiveWeight) * (1 - risk)
  best_fitness_values <- rep(max(final), 100)
  return(list(fitness = final, best_fitness_values = best_fitness_values))
}

monitor <- function(obj) {
  cat("Iteration:", obj@iter, "Best fitness value:", max(obj@fitness), "\n")
}

# Calculate mean returns and base covariance matrix
meanReturns <- colMeans(myRetData)
baseCovMatrix <- cov(myRetData)
dimensions <- ncol(myRetData)

iterations <- 100
pop_sizes <- c(300, 400, 500)
mutation <- 0.1
crossover <- 0.8

GA_list <- list()
best_portfolios <- list()
best_fitness_values <- list()
best_fitness_data <- list()

for (pop_size in pop_sizes) {
  cat("Running GA with population size:", pop_size, "\n")
  
  best_solution <- NULL
  best_fitness <- -Inf
  
  best_fitness_values <- numeric(iterations)
  
  for (i in 1:) {
    GA <- ga(
      type = 'real-valued', 
      fitness = fitness, 
      lower = rep(0, dimensions), 
      upper = rep(1, dimensions), 
      popSize = pop_size, 
      maxiter = iterations, 
      pcrossover = crossover,
      pmutation = mutation,    
      seed = 123,
      keepBest = TRUE,
      monitor = monitor
    )
    
    current_solution <- normalize(GA@solution)
    current_fitness <- fitness(current_solution)
    
    if (current_fitness > best_fitness) {
      best_fitness <- current_fitness
      best_solution <- current_solution
    }
  }
  
  best_fitness_by_iteration[[length(best_fitness_by_iteration) + 1]] <- best_fitness_values
  best_portfolios[[length(best_portfolios) + 1]] <- best_solution
  GA_list[[length(GA_list) + 1]] <- GA
  
  # Save the plot as a PNG file
  png(filename = paste0("pop_size_", pop_size, ".png"))
  
  # Plot the fitness values and generations for this population size
  plot(GA, main = paste("Population size =", pop_size))
  
  # Close the graphics device
  dev.off()
}




# Find the best solution for each population size
for (i in 1:length(pop_sizes)) {
  cat("Best solution for population size", pop_sizes[i], ":\n")
  print(normalize(best_portfolios[[i]]))

}


# Create a data frame with the best fitness values and generations
fitness_data <- data.frame(
  Generation = 1:iterations,
  PopSize_300 = best_fitness_by_iteration[[1]],
  PopSize_400 = best_fitness_by_iteration[[2]],
  PopSize_500 = best_fitness_by_iteration[[3]]
)


# Calculate the standard deviations for each population size

# Calculate the standard deviations for each population size
# Calculate the standard deviations for each population size
# Calculate the standard deviations for each population size
stddev_300 <- sd(sapply(GA_list[[1]]@population, fitness))
stddev_400 <- sd(sapply(GA_list[[2]]@population, fitness))
stddev_500 <- sd(sapply(GA_list[[3]]@population, fitness))




# New code snippet to reshape the data frame:

fitness_data_long <- gather(fitness_data, key = "Population", value = "Best_Fitness", PopSize_300, PopSize_400, PopSize_500)
fitness_data_long$StdDev <- rep(c(stddev_300, stddev_400, stddev_500), each = iterations)
# New code snippet to create the ggplot2 plot:

ggplot(fitness_data_long, aes(x = Generation, y = Best_Fitness, color = Population)) +
  geom_line() +
  geom_point(shape = "|", size = 2, stroke = 1) + 
  geom_errorbar(aes(ymin = Best_Fitness - StdDev, ymax = Best_Fitness + StdDev), width = 0.5) +
  labs(
    title = "Best Fitness with Error Bars by Population Size and Generation",
    x = "Generation",
    y = "Best Fitness"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )



# Find the best solution out of all the populations
best_portfolio <- NULL
best_fitness <- -Inf
best_population <- NULL

for (i in 1:length(best_portfolios)) {
  portfolio <- best_portfolios[[i]]
  portfolio_fitness <- fitness(portfolio)
  
  if (portfolio_fitness > best_fitness) {
    best_fitness <- portfolio_fitness
    best_portfolio <- portfolio
    best_population <- pop_sizes[i] # Store the population size that gave the best solution
  }
}

cat("Best solution out of all the populations (from population size", best_population, "):\n")
cat("Best Fitness out of all the populations (from population size", best_fitness, "):\n")
cat("Best Portfolio out of all the populations (from population size", best_portfolio, "):\n")
print(best_portfolio)

cat("Best fitness for population size 300:", best_fitness_values[[3]], "\n")
cat("Best fitness for population size 400:", best_fitness_values[[1]], "\n")
cat("Best fitness for population size 500:", best_fitness_values[[2]], "\n")

##############################################

# Calculate the risk, return, and Sharpe ratios for each GA run
bestRisks <- sapply(best_solution, getRisk) * 100
bestReturns <- sapply(best_solution, getReturns) * 100

# Assuming a risk-free rate of 12% per year
risk_free_rate <- 0.12
# Compute the Sharpe ratios
sharpe_ratios <- sapply(1:length(best_solution), function(i) {
  portfolio_return <- best_Return[i] / 100
  portfolio_risk <- sqrt(bestRisk[i] / 100)
  sharpe_ratio <- (portfolio_return - risk_free_rate) / portfolio_risk
  return(sharpe_ratio)
})




# Iterate through the best solutions
for (i in 1:length(bestSolution)) {
  portfolio <- bestSolution[[i]]
  portfolio_return <- bestReturns[i]
  portfolio_risk <- bestRisks[i]
  sharpe_ratio <- sharpe_ratios[i]
  
  # Combine the portfolio weights, return, risk, and Sharpe ratio into a single row
  row <- c(portfolio, portfolio_return, portfolio_risk, sharpe_ratio)
  
  # Add the row to the output_table
  output_table <- rbind(output_table, row)
}

# Set column names
colnames(output_table) <- c(paste("Weight_", myStocks, sep = ""), "Return", "Risk", "SharpeRatio")

# Display the output table
print(output_table)




#####################################



# Combine the best fitness values for each population size into a single numeric vector
all_best_fitness_values <- unlist(best_fitness_by_iteration)

# Create a factor vector to indicate the corresponding population size for each fitness value
population_sizes_factor <- rep(pop_sizes, times = sapply(best_fitness_by_iteration, length))

# Perform pairwise t-tests
pairwise_t_test_results <- pairwise.t.test(all_best_fitness_values, g = population_sizes_factor, p.adjust.method = "BH")

# Display the p-values
print(pairwise_t_test_results$p.value)






######################################



# Create a vector of objective weights
objectiveWeights <- seq(0.5)

# Collect the re-normalized best individuals
bestSolutions <- list()

# Run GA for each objective weight
for (i in 1:length(objectiveWeights)) {
  cat("Running GA with Objective Weight Splitting:", objectiveWeights[i], "\n")
  objectiveWeight <- objectiveWeights[i]
  
  # Run GA 30 times for each objective weight
  for (j in 1:30) {
    cat("Running GA iteration:", j, "for objective weight", objectiveWeights[i], "\n")
    
    GA <- ga(type = 'real-valued', 
             fitness = fitness, 
             lower = rep(0, dimensions), 
             upper = rep(1, dimensions), 
             popSize = pop_size, 
             maxiter = iterations,
             pcrossover = crossover,
             pmutation = mutation,    
             seed = 123,
             keepBest = TRUE)
    
    # Store the best solutions in the list, indexed by objective weight and iteration
    bestSolutions[[paste(i, j, sep = "_")]] <- normalize(GA@solution)
    
    # Save the plot as a PNG file
    png(filename = paste0("obj_weight_", objectiveWeights[i], "_iter_", j, ".png"))
    
    # Plot the fitness values and generations for this objective weight and iteration
    plot(GA, main = paste("Objective Weight =", objectiveWeights[i], ", Iteration =", j))
    
    # Close the graphics device
    dev.off()
  }
}



# Calculate the risk, return, and Sharpe ratios for each GA run
bestRisks <- sapply(bestSolutions, getRisk) * 100
bestReturns <- sapply(bestSolutions, getReturns) * 100

# Assuming a risk-free rate of 12% per year
risk_free_rate <- 0.12

# Compute the Sharpe ratios
sharpe_ratios <- sapply(1:length(bestSolutions), function(i) {
  portfolio_return <- bestReturns[i] / 100
  portfolio_risk <- sqrt(bestRisks[i] / 100)
  sharpe_ratio <- (portfolio_return - risk_free_rate) / portfolio_risk
  return(sharpe_ratio)
})

# Find the best portfolio based on the Sharpe ratio
best_portfolio_index <- which.max(sharpe_ratios)
best_portfolio <- bestSolutions[[best_portfolio_index]]
cat("Best Portfolio Weights:", best_portfolio, "\n")
cat("Best Portfolio Return:", bestReturns[best_portfolio_index], "\n")
cat("Best Portfolio Risk:", bestRisks[best_portfolio_index], "\n")
cat("Best Portfolio Sharpe Ratio:", sharpe_ratios[best_portfolio_index], "\n")

# Create an empty data frame
output_table <- data.frame()

#############



#############

# Iterate through the best solutions
for (i in 1:length(bestSolutions)) {
  portfolio <- bestSolutions[[i]]
  portfolio_return <- bestReturns[i]
  portfolio_risk <- bestRisks[i]
  sharpe_ratio <- sharpe_ratios[i]
  
  # Combine the portfolio weights, return, risk, and Sharpe ratio into a single row
  row <- c(portfolio, portfolio_return, portfolio_risk, sharpe_ratio)
  
  # Add the row to the output_table
  output_table <- rbind(output_table, row)
}

# Set column names
colnames(output_table) <- c(paste("Weight_", myStocks, sep = ""), "Return", "Risk", "SharpeRatio")

# Display the output table
print(output_table)

head(output_table)


# Display the best solution
summary(GA)




#plot(GA)



library(ggplot2)

# Create the scatter plot
plot <- ggplot(output_table, aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point(size = 3) +
  # Highlight the best portfolio with a red circle
  geom_point(data = output_table[best_portfolio_index, ], aes(x = Risk, y = Return), color = "red", size = 4, shape = 1) +
  # Set plot labels
  labs(title = "Portfolios with different Sharpe Ratios",
       x = "Risk (%)",
       y = "Return (%)",
       color = "Sharpe Ratio") +
  # Set plot theme
  theme_minimal()

# Display the plot
print(plot)





# Install and load ggplot2 library
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Create a new column 'isBest' in the output_table
output_table$isBest <- FALSE
output_table$isBest[best_portfolio_index] <- TRUE

# Create the scatter plot
ggplot(output_table, aes(x = Risk, y = Return, size = abs(SharpeRatio), color = isBest)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "Portfolios: Risk vs. Return",
       x = "Risk",
       y = "Return",
       size = "Sharpe Ratio",
       color = "Best Portfolio") +
  theme_minimal()





# Load required libraries
library(ggplot2)

# Assuming you have the data frame with portfolio returns, risks, and Sharpe ratios
portfolios <- data.frame(Returns = portfolio_returns,
                         Risks = portfolio_risks,
                         SharpeRatios = sharpe_ratios)

# Find the index of the best portfolio
best_portfolio_index <- which(sharpe_ratios == max(sharpe_ratios))

# Create a new column in the data frame to indicate the best portfolio
portfolios$BestPortfolio <- FALSE
portfolios$BestPortfolio[best_portfolio_index] <- TRUE

# Plot the scatterplot with a different color and shape for the best portfolio
ggplot(portfolios, aes(x = Risks, y = Returns, color = BestPortfolio, shape = BestPortfolio)) +
  geom_point() +
  scale_color_manual(values = c("blue", "red")) +
  scale_shape_manual(values = c(19, 3)) +
  labs(title = "Efficient Frontier",
       x = "Portfolio Risk",
       y = "Portfolio Return",
       color = "Best Portfolio",
       shape = "Best Portfolio") +
  theme_minimal()




# Load required libraries
library(ggplot2)

# Assuming you have the best portfolio index, risks, returns, and Sharpe ratios as given
best_portfolio_index <- 9 # Replace this with your actual best portfolio index
bestReturns <- 2.759482
bestRisks <- 0.3071922
sharpe_ratios <- -1.667214

# Assuming you have a data frame 'portfolios' containing all the portfolios' returns, risks, and Sharpe ratios
# You should replace the dummy data frame below with your actual data frame
portfolios <- data.frame(Returns = runif(10, min = 1, max = 3),
                         Risks = runif(10, min = 0.2, max = 0.5),
                         SharpeRatios = runif(10, min = -2, max = 1))

# Add the best portfolio flag to the data frame
portfolios$BestPortfolio <- FALSE
portfolios$BestPortfolio[best_portfolio_index] <- TRUE

# Plot the scatterplot with a different color and shape for the best portfolio
ggplot(portfolios, aes(x = Risks, y = Returns, color = BestPortfolio, shape = BestPortfolio)) +
  geom_point() +
  scale_color_manual(values = c("blue", "red")) +
  scale_shape_manual(values = c(19, 3)) +
  labs(title = "Efficient Frontier",
       x = "Portfolio Risk",
       y = "Portfolio Return",
       color = "Best Portfolio",
       shape = "Best Portfolio") +
  theme_minimal()








# Load required libraries
library(ggplot2)

# Assuming you have a data frame 'portfolios' containing all the portfolios' returns, risks, and Sharpe ratios
# You should replace the dummy data frame below with your actual data frame
portfolios <- data.frame(Returns = runif(10, min = 1, max = 3),
                         Risks = runif(10, min = 0.2, max = 0.5),
                         SharpeRatios = runif(10, min = -2, max = 1))

# Identify the best portfolio
best_portfolio_index <- which.max(portfolios$SharpeRatios)

# Extract the best portfolio's returns, risks, and Sharpe ratio
bestReturns <- portfolios$Returns[best_portfolio_index]
bestRisks <- portfolios$Risks[best_portfolio_index]
bestSharpeRatio <- portfolios$SharpeRatios[best_portfolio_index]

# Add the best portfolio flag to the data frame
portfolios$BestPortfolio <- FALSE
portfolios$BestPortfolio[best_portfolio_index] <- TRUE

# Plot the scatterplot with a different color and shape for the best portfolio
ggplot(portfolios, aes(x = Risks, y = Returns, color = BestPortfolio, shape = BestPortfolio)) +
  geom_point() +
  scale_color_manual(values = c("blue", "red")) +
  scale_shape_manual(values = c(19, 3)) +
  labs(title = "Efficient Frontier",
       x = "Portfolio Risk",
       y = "Portfolio Return",
       color = "Best Portfolio",
       shape = "Best Portfolio") +
  theme_minimal()





# Load required libraries
library(ggplot2)

# Assuming you have a data frame 'portfolios' containing all the portfolios' returns, risks, and Sharpe ratios
# You should replace the dummy data frame below with your actual data frame
portfolios <- data.frame(Returns = runif(10, min = 1, max = 3),
                         Risks = runif(10, min = 0.2, max = 0.5),
                         SharpeRatios = runif(10, min = -2, max = 1))

# Identify the best portfolio
best_portfolio_index <- which.max(portfolios$SharpeRatios)

# Add the best portfolio flag to the data frame
portfolios$BestPortfolio <- FALSE
portfolios$BestPortfolio[best_portfolio_index] <- TRUE

# Print the table with the best portfolio identified
print(portfolios)

# Plot the scatterplot with a different color and shape for the best portfolio
ggplot(portfolios, aes(x = Risks, y = Returns, color = BestPortfolio, shape = BestPortfolio)) +
  geom_point() +
  scale_color_manual(values = c("blue", "red")) +
  scale_shape_manual(values = c(19, 3)) +
  labs(title = "Efficient Frontier",
       x = "Portfolio Risk",
       y = "Portfolio Return",
       color = "Best Portfolio",
       shape = "Best Portfolio") +
  theme_minimal()
