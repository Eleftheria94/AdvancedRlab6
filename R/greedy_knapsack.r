#' Implementation of the greedy_knapsack function. 
#' 
#' @name greedy_knapsack
#' @param x A data frame that contains two positive vectors for weights (w) and values (v).
#' @param W The maximum knapsack capacity.
#' @return A list containing the values and elements used to fill the knapsack.
#' @description The following code implements the knapsack greedy algorithm: it sorts the items in decreasing order of value per unit of weight, it then proceeds to insert them into the sack, starting with as many copies as possible of the first kind of item until there is no longer space in the sack for more. Provided that there is an unlimited supply of each kind of item, if m is the maximum value of items that fit into the sack, then the greedy algorithm is guaranteed to achieve at least a value of m/2.
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#' @export greedy_knapsack
#' @examples
#' set.seed(42)
#' n <- 2000
#' w = sample(1:4000, size = n, replace = TRUE)
#' v = runif(n = n, 0, 10000)
#' knapsack_objects <- data.frame(w,v)
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#' greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)


set.seed(42)
n <- 2000
knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE), v = runif(n = n, 0, 10000))

greedy_knapsack <- function(x, W) {
  # Check for inputs
  stopifnot(is.data.frame(x))
  stopifnot(is.integer(x$w) && is.numeric(x$v))  # weights are positive, discrete values
  stopifnot(W > 0 && any(x$w > 0) && any(x$v > 0))
  
  x$fraction <- x$v/x$w
  x <- x[order(x$fraction, decreasing = TRUE),]  # sorting in decreasing order of value per unit of weight
  weight <- 0
  value <- 0
  elements <- c()
  for(i in 1:length(x$fraction)) {
    t_weight <- weight + x$w[i]
    if(t_weight <= W) {
      weight <- t_weight
      value <- value + x$v[i]
      elements <- c(elements, as.numeric(row.names(x[i,])))
    } else {
      break
    }
  }
  return(list(value = round(value), elements = elements))
}
