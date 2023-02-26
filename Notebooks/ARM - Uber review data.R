# Load required libraries
library(arules)
library(arulesViz)

# Load the data
#data <- read.transactions(TransactionTweetsFile,rm.duplicates = FALSE,format = "basket",sep=",")'C:/Users/Sanchu/Downloads/Uber data/data-society-uber-pickups-in-nyc/Uber-Review-Data.csv')

data <- read.csv("C:/Users/Sanchu/Downloads/Uber data/data-society-uber-pickups-in-nyc/Uber-Review-Data.csv", header = TRUE)
data_comment <- data[,-c(1,2)]
data_comment2 <- tokenizers::tokenize_words(
  data_comment,stopwords = stopwords::stopwords("en"), 
  lowercase = T,  strip_punct = T, strip_numeric = T,
  simplify = T)

# Convert the data to transactions
transactions <- as(data_comment2, "transactions")

# Explore the transactions
inspect(transactions)

# Perform association rule mining
rules <- apriori(transactions, parameter = list(support = 0.01, confidence = 0.5, minlen = 2))


# Explore the rules
inspect(rules)

## Plot of which items are most frequent
itemFrequencyPlot(transactions, topN=20, type="absolute")

# Sort the rules by different metrics and extract the top 15
support_rules <- sort(rules, by = "support", decreasing = TRUE)[1:15]
confidence_rules <- sort(rules, by = "confidence", decreasing = TRUE)[1:15]
lift_rules <- sort(rules, by = "lift", decreasing = TRUE)[1:15]

# Print the top rules
inspect(support_rules)
inspect(confidence_rules)
inspect(lift_rules)

# Visualize the rules as a graph
plot(support_rules, method = "graph", engine="htmlwidget")
plot(confidence_rules, method = "graph",engine="htmlwidget")


# Generate a scatterplot of the rules
plot(rules, method = "scatterplot",engine="htmlwidget")

#
plot(rules, method = "two-key plot",engine="htmlwidget")
