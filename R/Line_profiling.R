# devtools::install_github("hadley/lineprof")

f <- function(...) {
  lineprof::pause(0.1)
  knapsack_dynamic(knapsack_objects[1:8,],2000)
}

g <- function(...) {
  lineprof::pause(0.1)
  brute_force_knapsack(knapsack_objects[1:8,],2000)
}

h <- function(...) {
  lineprof::pause(0.1)
  greedy_knapsack(knapsack_objects[1:8,],2000)
}


Rprof(tmp <- tempfile(), line.profiling = TRUE)
compareMethods()
Rprof()
summaryRprof()
unlink(tmp)

lineprof(brute_force_knapsack(knapsack_objects[1:12,],2000), interval = 0.01, torture = FALSE)

lineprof(knapsack_dynamic(knapsack_objects[1:20,],3500), interval = 0.01, torture = FALSE)

lineprof(greedy_knapsack(knapsack_objects[1:500,],1000000), interval = 0.01, torture = FALSE)
