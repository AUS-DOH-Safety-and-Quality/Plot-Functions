#input dataframe filtered to indicator, establishment and current funnel, output numerator, denominator and value table
num_den_table <- function(input_df){

  num_den_df <- input_df %>%
    mutate(value = format(round((numerator/denominator)*multiplier, digits = 1),nsmall = 2))
  num_den_df <- arrange(num_den_df, period_end)
  num_den_df$value <- if_else(num_den_df$value == " NaN", "0.00", num_den_df$value)
  #Rounding up for probabilities for outcomes
  num_den_df$denominator <- round(num_den_df$denominator, 1)
  # Reformat the date column for our preferred style (e.g. Oct 21)
  num_den_df$period_end <- format(num_den_df$period_end, format = "%b %y")
  num_den_df$value <- round(as.numeric(num_den_df$value),1)
  # select the columns we need and transpose them
  num_den_table <- dplyr::select(num_den_df, "Date" = period_end, "Numerator" = numerator,
                                  "Denominator" = denominator, "Value" = value) %>%
    #Transpose
    t() %>%
    #turns colnames into variables
    as_tibble(rownames = "x", .name_repair = "minimal")
  num_sum <- sum(as.numeric(num_den_table[2,2:13]))
  den_sum <- sum(as.numeric(num_den_table[3,2:13]))
  num_den_table$Total <- c("Total", num_sum, den_sum, round((num_sum/den_sum)*input_df$multiplier[1], digits = 1))
  # Uses the table we just created, passes it to the create table function
  # and adds a custom theme we created for HQIU (see theme function for settings)
  num_den_table %>%
    create_table() %>%
    table_theme()
}
