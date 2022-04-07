print_spc <- function(input_df) {
  
  ind_spc_df <- input_df
  #check if unique establishment-indicator set has any values, if not skip this iteration
  if(sum(ind_spc_df$numerator) == 0 & sum(ind_spc_df$denominator) == 0) return(cat("", "Skipped", input_df$establishment[1], input_df$indicator[1], sep = " "))
  #spc does not work with 0 or 1 rows
  if (!nrow(ind_spc_df) | nrow(ind_spc_df) == 1) return(cat("", "Skipped", input_df$establishment[1], input_df$indicator[1], sep = " "))
  
  # Set the spc chart headings
  spc_heading <- paste(ind_spc_df$descriptionshort[1], ind_spc_df$threeletteracronym, sep = " - ")
  spc_period_start <- min(ind_spc_df$period_start)
  spc_period_end   <- max(ind_spc_df$period_end)
  spc_sub_heading <- paste(format(spc_period_start, format = "%b-%y"), format(spc_period_end, format = "%b-%y"), sep = " to ")
  
  #for each value in dataframe, check if it is 0, if so exclude row
  for (i in 1:length(ind_spc_df$denominator)){
    if(ind_spc_df$denominator[i] == 0) ind_spc_df$denominator[i] <- NA
  }
  
  # Calculate the limits using the qic package
  hqiu_spc <- qic( 
    x  = ind_spc_df$period_end, 
    y  = ind_spc_df$numerator,
    n = ind_spc_df$denominator,
    data = ind_spc_df,
    chart = ind_spc_df$spccharttype[1],
    multiply = ind_spc_df$multiplier[1],
    y.percent = TRUE)
  
  # Pull out the data from the ggplot2 object hqiu_spc
  hqiu_spc_df <- hqiu_spc$data
  # #Clear NaN and Inf values from cl to determine if spc was successful
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
    geom_point(aes(y = y),color = brand_colour, size = 2) + 
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
          subtitle = spc_sub_heading, # needs to be Apr-13 to Mar-14 format in next update
          caption = "Source: Healthcare Quality Intelligence Unit")+
    hqiu_theme()
  #creates a unique name
  unique_name <- paste("SPC_", input_df$descriptionshort[1], "_", input_df$threeletteracronym[1], ".png", sep = "")
  #saves the plot into the specified path
  #DIRECT THIS WHERE YOU WANT TO SAVE
  ggsave(filename = unique_name, plot = hqiu_spc_plot, device = "png", path = "C:/Users/he198926/Desktop/SPC_test", width = 10, height = 8)
}