#input dataframe filtered to indicator, establishment and current funnel, output numerator, denominator and value table
num_den_table <- function(input_df){

  # This table only shows the last 12 months, where the SPC shows 3 years (typically),
  # so let's take the last 12 values
  num_den_df <- input_df %>%
    mutate(value = round((numerator/denominator)*multiplier, 2))
  num_den_df <- arrange(num_den_df, period_end)
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
    as_tibble(rownames = "x")
  # Uses the table we just created, passes it to the create table function
  # and adds a custom theme we created for HQIU (see theme chunk for settings)
  num_den_table %>%
    create_table() %>%
    table_theme()
}
