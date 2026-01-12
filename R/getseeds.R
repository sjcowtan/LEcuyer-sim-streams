#' Set seeds for multiple runs
#' 
#' @param N No. runs
#' @param seed Starting seed
#' @return List of N sets of 7 seeds
#' @importFrom parallel nextRNGStream
#' 

getseeds <- function(
  N = 5, 
  seed = NULL
) {

  # Get current RNG and seed for current environment
  oldRNG <- RNGkind()[1L]
  oldseed <- get(x = ".Random.seed", envir = as.environment(-1))

  # On exit: Reset RNG and seed for current environment to previous value
  on.exit(
    set.seed(
      kind = oldRNG, 
      seed = oldseed
    )
  )
  if (is.null(seed)) {
    seed <- oldseed
  } 

  # Make sure the seed is at least plausible
  stopifnot(
    is.numeric(seed), 
    all(is.finite(seed))
  )

  # Set a L'Ecuyer seed
  set.seed(
    kind = "L'Ecuyer-CMRG", 
    seed = seed
  )
  le_seed <- get(".Random.seed", envir = as.environment(-1))

  # Initialise empty seed list, one per simulation
  seeds <- vector(mode = "list", length = N)

  for (i in 1:N) {
    le_seed <- nextRNGStream(le_seed)
    seeds[[i]] <- le_seed
  }

  return(seeds)
}
