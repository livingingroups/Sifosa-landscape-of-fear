#' Function: f_get_dyads_per_group.R
#' 
#' Generate dyadic combinations within groups
#'
#' Creates all pairwise combinations of individuals within the same group.
#' 
#' @author Maevatiana N. Ratsimbazafindranahaka (primary code author)
#' @author Meredith C. Lutz (code reviewer)
#' 
#' @param group_data Tibble or data frame of one group containing at least:
#' \itemize{
#'   \item \code{animal_id} Character identifier for each individual.
#'   \item \code{animal_group_id} Numeric or character group identifier.
#'   \item \code{animal_sex} Sex of each individual (not used in computation but listed to define expected format).
#' }
#'
#' @return A tibble with dyads per group including:
#' \itemize{
#'   \item \code{indA} First individual in the dyad.
#'   \item \code{indB} Second individual in the dyad, or \code{NA} if the group has only one individual.
#'   \item \code{group_id} Group identifier associated with the dyad.
#' }
#'
#' @examples
#' # Example usage
#' # f_get_dyads_per_group(group_data)
#'
#' @export

f_get_dyads_per_group <- function(group_data) {
  
  # If less than two individuals in the group, a single entry is returned with
  # the second individual set to NA
  if (nrow(group_data) < 2) {
    return(data.frame(
      indA = group_data$animal_id,
      indB = NA,
      group_id = group_data$animal_group_id
    ))
  }
  
  # Otherwise
  pairs <- as.data.frame(t(combn(group_data$animal_id, 2)))  # get all dyads
  colnames(pairs) <- c("indA", "indB") # Name the columns
  
  pairs$group_id <- unique(group_data$animal_group_id)  # assign group ID
  
  return(pairs)
}
