# About -------------------------------------------------------------------

# This file contains all custom functions.
# List of functions
# 1. to_long format
# 2. calculate_points
# 3. 

# Make pivot_longer -------------------------------------------------------
# reshapes a tibble to long format
to_long_format <- function(df){
  df |> 
    # make long dataframe
    pivot_longer(cols = starts_with("vc_"), 
                 names_to = "comp_no",
                 names_prefix = "vc_",
                 values_to = "place") |> 
    filter(comp_no <= {{ current_comp_no }}) |> 
    mutate(comp_no = as.numeric(comp_no))
}


# Prep data ---------------------------------------------------------------
prep_data <- function(df){
  df |> 
    mutate(place_char = as.character(place))
}

# Calculate points --------------------------

# This function takes a tibble with a variable called "curr_comp" and calculates points
poits_calc <- function(df) {
  tot <- df %>% 
    mutate(points = case_match(place_char,
                               "1" ~ 32,
                               "2" ~ 26,
                               "3" ~ 21,
                               "4" ~ 19,
                               c("5", "6", "7", "8") ~ 14,
                               c("9", "10", "11", "12", "13", "14", "15", "16") ~  8,
                               c("17", "18", "19", "20", "21", "22", "23", "24", 
                                 "25", "26", "27", "28", "29", "30", "31", "32") ~ 4
    ))
}


# Create results table for current competition
curr_comp_table <- function(df){
  df |> 
    arrange(place) |> 
    select(name, place, points) |> 
    rename(Namn = name, 
           Placering = place,
           Poäng = points) |> 
    gt() |> 
    tab_header(
      title = paste0("Resultat från deltävling ", 
                     {{ current_comp_no }}),
      subtitle = "Vårcupen 2023")
}

## Calculate grand total points for all competitions (1-5)
calculate_grand_tot <- function(df){
  df |>  
    group_by(name) |> 
    summarise(sum_points = sum(points, na.rm = TRUE)) |>  
    arrange(desc(sum_points))
}

# Calculate points for the 4 best competitions
calculate_top_4 <- function(df){
  df |> 
  group_by(name) |> 
    slice_max(points, 
              n = 4,
              with_ties = FALSE) |> 
    summarise(sum_top4_points = sum(points, na.rm = TRUE)) |>  
    arrange(desc(sum_top4_points))
}


# Prepare data for standings ----------------------------------------------

# Create results tibble for all competitions
# modify data to produce all necessary variables
# in correct output format
return_res <- function(df){
  # prepare standings table
  df |> 
    select(-place, - place_char) |>  
    pivot_wider(
      names_from = comp_no, 
      values_from = points) |> 
    
    # Create new piv df with totals column
    full_join(grand_tot, by = "name") |>  
    arrange(desc(sum_points)) |> 
    
    # Add ranking
    mutate(rank = min_rank(desc(sum_points))) |> 
    relocate(rank, everything())
}

# Create results tibble for final results, after 5 competitions
final_res <- function(df){
  df |> 
    # Create new column with 4 best results
    full_join(top_4, by = "name") |>  
    arrange(desc(sum_top4_points)) |> 
    select(-rank) |> 
    # Add ranking
    mutate(rank = min_rank(desc(sum_top4_points))) |> 
    relocate(rank, everything())
    
}


# Crete standings table including rank and totals -------------------------

# Totals table
totals_table <- function(df){
  df |> 
    select(-gender) |> 
    rename(`#` = rank,
           Namn = name,
           `Totalt` = sum_points) |> 
    gt() |> 
    tab_spanner(
      label = "Deltävling nr.",
      columns = -c(`#`, Namn, Totalt)
    ) |> 
    tab_header(
      title = paste0("Totalställning efter ", 
                     {{ current_comp_no }}, " deltävlningar")) |> 
    tab_source_note(source_note = "Vårcupen 2023")
}


# Create final results table ----------------------------------------------

filal_table <- function(df){
  df |> 
    select(-gender) |> 
    rename(`#` = rank,
           Namn = name,
           `Totalt` = sum_points, 
           `4 bästa` = sum_top4_points) |>
    gt() |>  
    tab_spanner(
      label = "Deltävling nr.",
      columns = -c(`#`, Namn, Totalt, `4 bästa`)
    ) |> 
    tab_header(
      title = paste0("Slutställning efter ", 
                     {{ current_comp_no }}, " deltävlningar")) |> 
    tab_source_note(source_note = "Vårcupen 2023")
}
