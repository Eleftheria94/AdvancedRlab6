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

  test_func <- function(numb) {
    for (i in numb) {
      index <- c()
      w <- 0
      v <- 0
      n <- nrow(x)
      
      # Enumerating all different combinations using a binary representation of 1 to 2^n and including all elements of that is equal to 1 with intToBits
      
      binary <- intToBits(i) # Convert to raw vectors
      
      for (j in 1:length(binary)) {
        if(bits[j] == 1){
        w <- w + x$w[j]
        v <- v + x$v[j]
        index <- c(index, j)
        }
      }
      
      if (v > value && w <= W) {
        value <- v
        elements <- index
      }
    }
    return(list(value = round(value, 0), elements = elements))
  }
  
  # Parallelization
  
  if (parallel == TRUE) {
    cores <- parallel::detectCores()
    clusters <- parallel::makeCluster(cores) # creates a set of R copies running in parallel
    lst <- parallel::parSapply(clusters, 1:((2^n)-1), test_func)
    parallel::stopCluster(clusters)
    
    max_value <- unlist(sapply(lst, function(numb) numb$value))
    max_value <- which.max(max_value)
    return(lst[[max_value]])
  } else {
    return(test_func(1:((2^n) - 1)))
  }
}
