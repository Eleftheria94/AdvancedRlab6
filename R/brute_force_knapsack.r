#' Implementation of the brute_force_knapsack function. 
#' 
#' @name brute_force_knapsack
#' @param x A data frame that contains two positive vectors for weights (w) and values (v).
#' @param W The maximum knapsack capacity.
#' @return A list containing the value and the elements used to fill the knapsack.
#' @description The following algorithm takes a data frame cx with two variables v and w and returns the maximum knapsack value and which elements (rows in the data frame). The variable W is the knapsack size.
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#' @export brute_force_knapsack
#' @examples
#' set.seed(42)
#' n <- 2000
#' w = sample(1:4000, size = n, replace = TRUE)
#' v = runif(n = n, 0, 10000)
#' knapsack_objects <- data.frame(w,v)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)


set.seed(42)
n <- 2000
knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE), v = runif(n = n, 0, 10000))

brute_force_knapsack <- function(x, W) {
  # Check for inputs
  stopifnot(is.data.frame(x))
  stopifnot(is.integer(x$w) && is.numeric(x$v))  # weights are positive, discrete values
  stopifnot(W > 0 && any(x$w > 0) && any(x$v > 0)) 
  
}
