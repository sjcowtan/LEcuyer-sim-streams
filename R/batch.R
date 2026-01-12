#' Run a batch 
#' @param N Number of runs
#' @param seed Starting seed
#' @return output
#' @export
#' 
batch <- function(
  N = 10,
  seed = 123456
) {
  # Get current RNG and seed for current environment
  oldRNG <- RNGkind()
  oldseed <- get(x = ".Random.seed", envir = as.environment(-1))

  # On exit: Reset RNG and seed for current environment to previous value
  
  on.exit(
    RNGkind(
      oldRNG[1L], 
      normal.kind = oldRNG[2L], 
      sample.kind = oldRNG[3L]
    )
  )
  on.exit(
    set.seed(
      kind = oldRNG[1L], 
      seed = oldseed
    ),
    add = TRUE
  )

  # Generate a list of seeds, one per simulation
  seeds <- getseeds(N, seed)

  # Set the random number generator to the one compatible
  # with RNGstreams
  RNGkind(
    kind = "L'Ecuyer-CMRG", 
    normal.kind = "Inversion", 
    sample.kind = "Rejection"
  )

  # Loop over simulations, setting the seed each time
  for (i in 1:N) {
    assign(
      x = ".Random.seed", 
      value = seeds[[i]],
      envir = as.environment(-1)
    )
    print(.Random.seed)

    # Run the simulation
    simfun(3)
  }
}