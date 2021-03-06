% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/greedy_knapsack.r
\name{greedy_knapsack}
\alias{greedy_knapsack}
\title{Implementation of the greedy_knapsack function.}
\arguments{
\item{x}{A data frame that contains two positive vectors for weights (w) and values (v).}

\item{W}{The maximum knapsack capacity.}
}
\value{
A list containing the values and elements used to fill the knapsack.
}
\description{
The following code implements the knapsack greedy algorithm: it sorts the items in decreasing order of value per unit of weight, it then proceeds to insert them into the sack, starting with as many copies as possible of the first kind of item until there is no longer space in the sack for more. Provided that there is an unlimited supply of each kind of item, if m is the maximum value of items that fit into the sack, then the greedy algorithm is guaranteed to achieve at least a value of m/2.
}
\examples{
set.seed(42)
n <- 2000
w = sample(1:4000, size = n, replace = TRUE)
v = runif(n = n, 0, 10000)
knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE), v = runif(n = n, 0, 10000))
greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
}
\references{
\url{https://en.wikipedia.org/wiki/Knapsack_problem}
}
