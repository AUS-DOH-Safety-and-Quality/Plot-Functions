#Insert dataframe that is filtered for indicator, establishment and current, return spc plot
spc_create <- function(input_df, patterns_df = "No") {

  #spc does not work with 0 or 1 rows
  if (!nrow(input_df) | nrow(input_df) == 1) return(cat("", "Skipped", input_df$establishment[1], input_df$indicator[1], sep = " "))
  #check if unique establishment-indicator set has any values, if not skip this iteration
  if(sum(input_df$numerator) == 0 & sum(input_df$denominator) == 0) return(cat("", "Skipped", input_df$establishment[1], input_df$indicator[1], sep = " "))

  # Set the spc chart headings
  spc_heading <- paste(input_df$descriptionshort[1], input_df$threeletteracronym, sep = " - ")
  spc_period_start <- min(input_df$period_start)
  spc_period_end   <- max(input_df$period_end)
  spc_sub_heading <- paste(format(spc_period_start, format = "%b-%y"), format(spc_period_end, format = "%b-%y"), sep = " to ")

  #for each value in dataframe, check if it is 0, if so exclude row
  input_df$denominator <- if_else(input_df$denominator == 0, NA_real_ , input_df$denominator)

  # Calculate the limits using the qic package
  hqiu_spc <- qic(
    x  = input_df$period_end,
    y  = input_df$numerator,
    n = input_df$denominator,
    data = input_df,
    chart = input_df$spccharttype[1],
    multiply = input_df$multiplier[1],
    y.percent = TRUE)

  # Pull out the data from the ggplot2 object hqiu_spc
  hqiu_spc_df <- hqiu_spc$data
  #Clear NaN and Inf values from cl to determine if spc was successful
  hqiu_spc_df <- mutate(hqiu_spc_df, across(.cols = ucl.95, .fns = clear_na))
  #SPC Plot

  #Checks if confidence limits are below 0, if so set to 0, as negative values are not possible
  if(!is.na(hqiu_spc_df$ucl.95[1])){
    if(hqiu_spc_df$lcl.95[1] < 0) hqiu_spc_df$lcl.95 <- 0
    if(hqiu_spc_df$lcl[1] < 0) hqiu_spc_df$lcl <- 0
  }

  hqiu_spc_plot <- ggplot(hqiu_spc_df, aes(x = x)) +
    # Add the data's line, with points added
    geom_line(aes(y = y), color = brand_colour, size = 0.5) +
    geom_point(aes(y = y),color = brand_colour, size = 4) +
    # Add the centre line
    geom_line(aes (y = cl), color = centre_line_colour) +
    # Add the upper control limit
    geom_line(aes (y = ucl.95), color = brand_colour, linetype = 3, size = 1) +
    # Add the upper warning limit
    geom_line(aes (y = ucl), color = brand_colour, linetype = 5, size = 1) +
    # Add the lower warning limit
    geom_line(aes (y = lcl), color = brand_colour, linetype = 5, size = 1) +
    # Add the lower control limit
    geom_line(aes (y = lcl.95), color = brand_colour, linetype = 3, size = 1) +
    #labels
    labs (x = "", y = "Value",
          title = spc_heading, # See start of chunk for setup
          subtitle = spc_sub_heading,
          caption = "Source: Healthcare Quality Intelligence Unit")+
    hqiu_theme()

  #checks if a patterns dataframe is input
  if(!is.data.frame(patterns_df)) return(hqiu_spc_plot)
  #filters the patterns dataframe to see if the current input df is an spc to be displayed
  filt_pat <- filter(patterns_df, Indicator == input_df$descriptionshort[1], Hospital == input_df$shorthospitalname[1])
  #if not return the normal plot
  if(!nrow(filt_pat)) return(hqiu_spc_plot)

  pats <- c("Astro", "Trend", "Shift", "TwoInThree")
  labels <- c("A", "T", "S", "TT")
  pats_labels <- tibble(pats, labels)

  #if a pattern was detected, circle the point and tag it
  for(i in pats_labels$pats){
    if(!is.na(filt_pat[[i]])) {
      pat_info <- filter(hqiu_spc_df, x == filt_pat[[i]])
      hqiu_spc_plot <- hqiu_spc_plot +
        geom_point(pat_info, mapping = aes(x = x, y = y), colour = "red", size = 8, shape = 1) #+
        #geom_text_repel(pat_info, mapping = aes(x = x, y = y), label = filt_pat, point.size = 4)
  }
}

  hqiu_spc_plot
}
