# Code by Arno Kalkman (arno.kalkman@gmail.com)

#' Group Overlapping Ranges into Episodes
#'
#' This function assigns episode numbers to ranges that overlap in time, grouped by a specified variable.
#' Each unique episode number identifies a set of overlapping ranges within the same group.
#'
#' @param group A vector specifying the grouping variable for each range. Ranges with the same group value
#'              can potentially belong to the same episode, while ranges with different groups cannot overlap
#'              or share an episode. This vector must be of the same length as `range_start` and `range_stop`.
#'              Acceptable types: character, factor, or numeric.
#' @param range_start A vector of start dates or times for each range. This vector must be the same length as `group`
#'                    and `range_stop`, and should be sortable (e.g., Date, POSIXct, numeric).
#' @param range_stop A vector of end dates or times for each range, with each value corresponding to a `range_start`
#'                   value. This vector must be the same length as `group` and `range_start` and should have the same
#'                   data type as `range_start`. Each `range_stop` date should be greater than or equal to its corresponding
#'                   `range_start`.
#'
#' @return A vector of episode numbers, where each unique integer identifies an episode of overlapping ranges
#'         within each group. The length of the returned vector matches the length of the input parameters.
group_overlapping_ranges <- function(group, range_start, range_stop)
{
  # Check that all parameters are vectors
  if (!is.vector(group) || !is.vector(range_start) || !is.vector(range_stop)) {
    #stop("All parameters ('group', 'range_start', 'range_stop') must be vectors.")
  }
  
  # Check that range_start and range_stop are of the same data type
  if (class(range_start) != class(range_stop)) {
    stop("Parameters 'range_start' and 'range_stop' must be of the same data type.")
  }  
  
  # Check that no parameters are missing or contain NA values
  if (is.null(group) || any(is.na(group))) stop("Parameter 'group' is required and must not contain NA values.")
  if (is.null(range_start) || any(is.na(range_start))) stop("Parameter 'range_start' is required and must not contain NA values.")
  if (is.null(range_stop) || any(is.na(range_stop))) stop("Parameter 'range_stop' is required and must not contain NA values.")
  
  # Check that all parameters have at least one element
  if (length(group) == 0) stop("Parameter 'group' must contain at least one element.")
  if (length(range_start) == 0) stop("Parameter 'range_start' must contain at least one element.")
  if (length(range_stop) == 0) stop("Parameter 'range_stop' must contain at least one element.")
  
  # Check that all parameters are of equal length
  if (length(group) != length(range_start) || length(group) != length(range_stop)) {
    stop("All parameters ('group', 'range_start', 'range_stop') must have the same length.")
  }
  
  # Check that each start date is before or equal to its corresponding end date
  if (any(range_start > range_stop)) {
    stop("Each 'range_start' date must be less than or equal to its corresponding 'range_stop' date.")
  }
  
  n <- length(group)

  # Definitions:
  # - A "range" represents an interval with a specific start and end date.
  #   Example: a single prescription period.
  # - An "episode" is a collection of ranges that overlap in time, effectively 
  #   creating a continuous or linked period from multiple overlapping ranges.
  # - The "group" variable is used to categorize ranges that could potentially 
  #   belong to the same episode. Ranges in different groups are considered 
  #   mutually exclusive and cannot be part of the same episode.
  
  
  # Construct a table that tracks the list of mutations over time.
  # Each range is represented twice in this DataFrame:
  # - Once with a "+1" mutation at the start date, indicating the beginning of the range.
  # - Once with a "-1" mutation at the stop date, marking the end of the range.
  mutation_table <- data.frame(
    index = rep(1:n, 2),
    group = rep(group, 2),
    time = c(range_start, range_stop),
    mutation = c(rep(1, n), rep(-1, n))
  )

  # Sort the mutations first by group, then by date, and finally by mutation in descending order.
  # Sorting on mutation in descending order ensures that when calculating the cumulative sum, 
  # increments (+1) are processed before decrements (-1). 
  # This approach guarantees that if a range ends exactly when another begins, they are 
  # treated as overlapping.
  mutation_table <- mutation_table[order(mutation_table$group, mutation_table$time, -mutation_table$mutation), ]
  
  # The cumulative sum of mutations indicates the number of overlapping ranges at any given date.
  # A cumulative sum of 0 signifies that all overlapping ranges have ended at that point, 
  # marking the end of an episode.
  mutation_table$counter <- cumsum(mutation_table$mutation)


  # Each time `new_episode_start` is TRUE, a new episode begins with the following +1 mutation row.
  # Subsequent rows in the same episode have `new_episode_start` as FALSE.
  # The cumulative sum of `new_episode_start` produces a unique episode number for each set of overlapping ranges.
  mutation_table$new_episode_start <- mutation_table$counter == 0
  mutation_table$episode_nr <- cumsum(mutation_table$new_episode_start)

  # Begin constructing the result vector that will hold episode numbers.
  # - First, filter out only the rows with positive mutations, as these indicate the start of ranges 
  #   within an episode.
  # - Then, restore the original order of the data using the index variable to maintain the 
  #   alignment with the original input.
  # - Finally, return the 'episode_nr' vector, which assigns an episode number to each range.
  mutation_table <- mutation_table[mutation_table$mutation >= 1, ]
  mutation_table <- mutation_table[order(mutation_table$index), ]

  return(mutation_table$episode_nr)
}
