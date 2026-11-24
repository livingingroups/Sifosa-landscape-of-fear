#' Function: f_compute_group_ddd.R
#' 
#' Compute dyadic distances within a given group 
#' 
#' @author Maevatiana N. Ratsimbazafindranahaka (primary code author)
#' @author Meredith C. Lutz (code reviewer)

f_compute_group_ddd <- function(gps, curr_group, ddd) {
  # Initialize outputs
  group_ddd <- matrix(NA, nrow = length(gps$timestamps), ncol = nrow(curr_group))
  dyad_list <- rep(NA, nrow(curr_group))
  
  # Loop through each dyad in curr_group
  for (ii in 1:nrow(curr_group)) {
    indA <- gps$ids$idx[gps$ids$id_code == curr_group$indA[ii]]
    indB <- gps$ids$idx[gps$ids$id_code == curr_group$indB[ii]]
    group_ddd[, ii] <- ddd[indA, indB, ]
    dyad_list[ii] <- paste0(curr_group$indA[ii], '-', curr_group$indB[ii])
  }
  
  
  # Define colors for each line
  colors <- rainbow(ncol(group_ddd))
  transparent_colors <- sapply(colors, adjustcolor, alpha.f = 0.5)
  
  # Plot the first line to initialize the plot
  plot(gps$timestamps, group_ddd[, 1], type = "l", col = transparent_colors[1], 
       ylim = range(group_ddd, na.rm = TRUE),  # Ensure all lines fit
       main = paste0("Dyadic distance in group #", unique(curr_group$group_id)), 
       ylab = "Distance", xlab = "Time")
  
  # Add additional lines if there are multiple dyads
  if (ncol(group_ddd) > 1) {
    for (j in 2:ncol(group_ddd)) {
      lines(gps$timestamps, group_ddd[, j], col = transparent_colors[j])
    }
  }
  
  # Add legend
  legend("topright", legend = dyad_list, col = colors, lty = 1, title = "Dyads")
  
  # Return results as a list
  return(list(group_ddd = group_ddd, dyad_list = dyad_list))
}
