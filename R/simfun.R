#' Function to be run repeatedly
#' @return output 
#' 
simfun <- function(reps, seed) {

  # Initialise empty result vector
  runifs <- vector(mode = "list", length = reps)

  for (i in 1:reps) {
    runifs[[i]] <- runif(3)
  }

  return(runifs)
}

