setwd("c:/Users/Daniel/Documents/development/papers/GA_work/code")
require(igraph)


#Chop up our data into some useful structures. we basically care about price and bundle vectors.
#They are already grouped by index, so it actually make sense to split them off separately
#We can use expressions like prices[i,] * bundles[i,] < prices[j,] * bundles[i,], and map
#over i and j.

dutchdata <- read.csv("dutch-data.csv")
plabels <- c("price_public","price_f", "price_m")
blabels <- c("public", "female", "male")
prices <- dutchdata[plabels]
bundles <- dutchdata[blabels]
spending <- bundles*prices
incomes <- rowSums(spending)

income_matrix <- matrix(data=incomes,nrow=586,ncol=586)

bundle_price_product <- data.matrix(bundles) %*% t(data.matrix(prices))
#Pretty sure to interpret this correctly, need to take the transpose. Currently read i , j is
#i is revealed to be worse that j
#
comparison_matrix <- income_matrix < bundle_price_product

#this remains the same, regardless of orientation?: (matrix algebra)
path_matrix <- comparison_matrix %*% comparison_matrix

cycle_matrix <- comparison_matrix * t(comparison_matrix)

adjacency_matrix <- cycle_matrix

cycle_matrix <- cycle_matrix * lower.tri(cycle_matrix)

cycles <- which(cycle_matrix == 1, arr.ind = TRUE)

cycledegree <- diag(path_matrix)

#Vertices WITH EDGES
vertices <- c(cycles[,1],cycles[,2])
vertices <- unique(vertices)

#Initialize graph objects
preference_graph<-graph.data.frame(cycles, directed=F)

cyclesat <- function (i){
  cycledegree[i]
}

compare <- function(i, j) 
{
        comparison_matrix[i,j];
}

