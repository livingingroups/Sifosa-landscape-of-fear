#' Function: f_get_dyadic_distances.R
#'
#' Computes the distance between each pair of individuals over time.
#' 
#' @author Ariana Strandburg-Peshkin (original code from cocomo package)
#' @author Maevatiana N. Ratsimbazafindranahaka (code adaptation)
#' @author Tracy M. Montgomery (code reviewer)
#' 
#' @param xs `N x n_times` matrix giving x coordinates of each individual over time
#' @param ys `N x n_times` matrix giving y coordinates of each individual over time
#' 
#' @returns `N x N x n_times` `dyad_dists` array giving dyadic distance between all pairs of individuals at each time step
#' `dyad_dists[i,j,t]` gives the distance between individuals `i` and `j` at time `t`
#' @export
 
f_get_dyadic_distances <- function(xs, ys){

  # Get the number of time points and individuals
  n_times <- ncol(xs)
  n_inds <- nrow(xs)

  # check for consistency of matrix dimensions
  if(nrow(ys) != n_inds){
    stop('xs and ys must have the same matrix dimensions')
  }
  if(ncol(ys) != n_times){
    stop('xs and ys must have the same matrix dimensions')
  }

  # create a vector to hold dyadic distance over time
  dyad_dists <- array(NA, dim = c(n_inds, n_inds, n_times))

  # Compute dyadic distances for each dyad
  for(i in 1:(n_inds-1)){
    for(j in (i+1):n_inds){

      # Get coordinates for i and j
      xi <- xs[i,]
      yi <- ys[i,]
      xj <- xs[j,]
      yj <- ys[j,]

      # Get distances and store
      dists_ij <- sqrt((xi - xj)^2 + (yi - yj)^2)
      dyad_dists[i,j,] <- dists_ij
      dyad_dists[j,i,] <- dists_ij

    }
  }

  # Output the dyadic distances array
  return(dyad_dists)

}
