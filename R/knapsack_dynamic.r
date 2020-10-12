#' Implementation of the "0-1" knapsack_dynamic function. 
#' 
#' @name knapsack_dynamic
#' @param x A data frame that contains two positive vectors for weights (w) and values (v).
#' @param W The maximum knapsack capacity.
#' @return A list containing the values and elements used to fill the knapsack.
#' @description The following algorithm calculates m(n,W). A more detailed explanation of the pseudo-algorithm followed can be found on Wikipedia.
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#' @export knapsack_dynamic
#' @examples
#' set.seed(42)
#' n <- 2000
#' w = sample(1:4000, size = n, replace = TRUE)
#' v = runif(n = n, 0, 10000)
#' knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE), v = runif(n = n, 0, 10000))
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#' knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
#' knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)

set.seed(42)
n <- 2000
knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE), v = runif(n = n, 0, 10000))
W = 2000

knapsack_dynamic <- function(x, W) {
  # Check for inputs
  stopifnot(is.data.frame(x))
  stopifnot(is.integer(x$w) && is.numeric(x$v))  # weights are positive, discrete values
  stopifnot(W > 0 && any(x$w > 0) && any(x$v > 0))
  
  n <- nrow(x)
  w <- x$w
  v <- x$v
  m <- matrix(rep(0, n*W), nrow = n)
  for (i in 2:n) {
    for (j in 1:W) {
      if (w[i] > j) {
        m[i,j] <- m[i-1, j]
      } else {
        m[i,j] <- max(m[i-1, j], m[i-1, j-w[i]] + v[i])
      }
    }
  }
  elements <- vector()
  count_elements <- c(n, W)
  j <- W
  for (i in n:2) {
    if (m[i,j] == m[i-1,j]) {
      count_elements[1] <- count_elements[1] - 1
    } else {
      j <- j - w[i]
      count_elements[1] <- count_elements[1] - 1
      elements <- c(elements, i)
    }
  }
  return(list(value = round(m[n, W]), elements = sort(elements)))
}

# Input:
# Values (stored in array v)
# Weights (stored in array w)
# Number of distinct items (n)
# Knapsack capacity (W)
# NOTE: The array "v" and array "w" are assumed to store all relevant values starting at index 1.
# Pseudo-code used:
#
# if i == 0 or j <= 0 then:
#   value[i, j] = 0
# return
# 
# if (value[i-1,j] == -1) then:     # m[i-1, j] has not been calculated, we have to call function m
#   value[i-1, j] = m(i-1,j)         
# 
# 
# if w[i] > j then:                     # item cannot fit in the bag (THIS WAS MISSING FROM THE PREVIOUS ALGORITHM)
#   value[i, j] = value[i-1, j]
# 
# else: 
#   if (value[i-1, j-w[i]] == -1) then:     #m[i-1,j-w[i]] has not been calculated, we have to call function m
#       value[i-1, j-w[i]] = m(i-1, j-w[i])
# value[i, j] = max(value[i-1,j], value[i-1, j-w[i]] + v[i])
# 


