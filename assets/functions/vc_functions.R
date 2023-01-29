
# About -------------------------------------------------------------------

# This file contains all custom functions.
# List of functions
# 1. 
# 2. 
# 3. 

# Make pivot_longer -------------------------------------------------------
# reshapes a tibble to long format
to_long_format <- function(df){
  df %>%
    # make long dataframe
    pivot_longer(cols = starts_with("vc_"), 
                 names_to = "comp_no",
                 names_prefix = "vc_",
                 values_to = "place")
}

# Calculate points --------------------------

# This function takes a dataframe with a variable called "place" and calculates points

calculate_points <- function(df, place) {
  tot <- df %>% 
    mutate(points = case_when(
      place == 1 ~ 32,
      place == 2 ~ 26,
      place == 3 ~ 21,
      place == 4 ~ 19,
      between(place, 5, 8) ~ 14,
      between(place, 9, 16) ~  8,
      between(place, 17, 32) ~ 4
    ))
}

# Make pivot_longer --------------------------
# reshapes a tibble to long format
to_long_format <- function(df){
  df %>%
    # make long dataframe
    pivot_longer(cols = starts_with("km_"), 
                 names_to = "comp_no",
                 names_prefix = "km_",
                 values_to = "place")
}
