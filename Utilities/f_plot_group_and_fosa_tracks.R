#' Function: f_plot_group_and_fosa_tracks.R
#' 
#' Plot Tracks of Animals in a Specified Group and Fosa Tracks (Distinct Orange Colors)
#' 
#' @author Maevatiana N. Ratsimbazafindranahaka (primary code author)
#' @author Tracy M. Montgomery (code reviewer)
#'
#' @param group_to_view Integer representing the ID of the group of sifaka to visualize.
#' @param group_list Data frame at least containing:
#'   - `animal_id`: Unique identifier for each animal.
#'   - `animal_group_id`: Group ID to which each animal belongs.
#' @param fosa_list A character vector of animal IDs corresponding to the Fossas to be highlighted.
#' @param gps gps data in AM format containing:
#'   - `xs`: Matrix of UTM easting coordinates.
#'   - `ys`: Matrix of UTM northing coordinates.
#'   - `ids`: Data frame mapping animal IDs to matrix indices.
#' 
#' @returns A plot showing the movement of the selected group of animals and Fosas.

library(scales)    # For alpha() function (transparency)
library(grDevices) # For colorRampPalette() function

f_plot_group_and_fosa_tracks <- function(group_to_view, group_list, fosa_list, gps) {
  
  # Extract list of animals belonging to the specified group
  animals_to_view <- group_list$animal_id[group_list$animal_group_id == group_to_view]
  
  # Define distinct colors for animals in the group (assuming max 3 tracked individuals)
  group_colors <- c("darkgreen", "darkred", "purple")
  
  # Determine the spatial bounds for the plot
  valid_indices <- gps$ids$idx[gps$ids$id_code %in% animals_to_view]
  min_x <- min(gps$xs[valid_indices,], na.rm = TRUE)
  max_x <- max(gps$xs[valid_indices,], na.rm = TRUE)
  min_y <- min(gps$ys[valid_indices,], na.rm = TRUE)
  max_y <- max(gps$ys[valid_indices,], na.rm = TRUE)
  
  # Initialize the plot using the first sifaka in the group
  first_idx <- gps$ids$idx[gps$ids$id_code == animals_to_view[1]]
  plot(
    gps$xs[first_idx,], gps$ys[first_idx,], 
    main = paste("Plot of Group", group_to_view, "and Fosas"), 
    xlab = "Easting", ylab = "Northing", 
    type = "l", col = alpha(group_colors[1], 0.5),
    xlim = c(min_x, max_x), ylim = c(min_y, max_y)
  )
  
  # Overlay the tracks of the remaining animals in the group
  for (i in seq_along(animals_to_view)[-1]) { 
    curr_idx <- gps$ids$idx[gps$ids$id_code == animals_to_view[i]]
    lines(gps$xs[curr_idx,], gps$ys[curr_idx,], col = alpha(group_colors[i], 0.5), type = "l")
  }
  
  # Generate distinct shades of orange for Fosa tracks
  fosa_colors <- colorRampPalette(c("orange", "darkorange"))(length(fosa_list))
  
  # Overlay the Fosa tracks with their distinct colors
  for (i in seq_along(fosa_list)) {
    fosa_idx <- gps$ids$idx[gps$ids$id_code == fosa_list[i]]
    lines(gps$xs[fosa_idx,], gps$ys[fosa_idx,], col = fosa_colors[i], type = "l")
  }
}
