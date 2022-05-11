#input dataframe filtered to indicator, establishment and current funnel, output numerator, denominator and value table
num_den_table <- function(input_df){

  num_den_df <- input_df %>%
    mutate(value = format(round((numerator/denominator)*multiplier, 2),nsmall = 2))
  num_den_df <- arrange(num_den_df, period_end)
  num_den_df$value <- if_else(num_den_df$value == " NaN", "0.00", num_den_df$value)
  num_den_df$value <- if_else(num_den_df$value == "NaN", "Test", num_den_df$value)
  #Rounding up for probabilities for outcomes
  num_den_df$denominator <- round(num_den_df$denominator, 2)
  # Reformat the date column for our preferred style (e.g. Oct 21)
  num_den_df$period_end <- format(num_den_df$period_end, format = "%b %y")
  # select the columns we need and transpose them
  num_den_table <- dplyr::select(num_den_df, "Date" = period_end, "Observed" = numerator,
                                  "Expected" = denominator, "Value" = value) %>%
    #Transpose
    t() %>%
    #turns colnames into variables
    as_tibble(rownames = "x", .name_repair = "minimal")
  # Uses the table we just created, passes it to the create table function
  # and adds a custom theme we created for HQIU (see theme function for settings)
  num_den_table %>%
    create_table() %>%
    table_theme()
}
