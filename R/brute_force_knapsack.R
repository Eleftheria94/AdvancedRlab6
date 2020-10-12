#' Implementation of the brute_force_knapsack function. 
#' 
#' @name brute_force_knapsack
#' @param x A data frame that contains two positive vectors for weights (w) and values (v).
#' @param W The maximum knapsack capacity.
#' @param parallel set to FALSE by default.
#' @return A list containing the values and elements used to fill the knapsack.
#' @description The following code implements the knapsack Brute Force Search algorithm.
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#' @export brute_force_knapsack
#' @examples
#' set.seed(42)
#' n <- 2000
#' w = sample(1:4000, size = n, replace = TRUE)
#' v = runif(n = n, 0, 10000)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)



set.seed(42)
n <- 2000
knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE), v = runif(n = n, 0, 10000))


brute_force_knapsack <- function(x, W, parallel = FALSE) {
  
  # Check for inputs
  stopifnot(is.data.frame(x))
  stopifnot(is.integer(x$w) && is.numeric(x$v))  # weights are positive, discrete values
  stopifnot(W > 0 && any(x$w > 0) && any(x$v > 0))
  
  is_valid_solution <- function(solution, bag, W) {
    solutions <- intToBits(solution)[1:dim(x)[1]]
    value <- sum(x$v * as.integer(solutions))
    weight <- sum(x$w * as.integer(solutions))
    if(weight <= W) {
      result <- value
    } else {
      result <- -1
    }
    return(result)
  }
  
  combo_number <- (2^dim(x)[1]) - 1
  
  if(parallel) {
    cores <- parallel::detectCores()
    clusters <- parallel::makeCluster(cores)
    solutions <- parallel::parSapply(cl = clusters,
                                    1:combo_number,
                                    FUN = is_valid_solution,
                                    bag = x,
                                    W = W)
  } else {
    solutions <- sapply(1:combo_number,
                       is_valid_solution,
                       bag = x,
                       W = W)
  }
  
  best_solution <- which.max(solutions)
  value <- solutions[best_solution]
  
  elements <- which(intToBits(best_solution) > 0)
  
  return(list(value = value, elements = elements))
}
