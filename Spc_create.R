spc_create <- function(input_df, patterns_df = "No", brand_colour = "#00667B") {
  squis_spc <- input_df
  #spc does not work with 0 or 1 rows
  if (!nrow(squis_spc) | nrow(squis_spc) == 1) return(cat("", "Skipped", squis_spc$establishment[1], squis_spc$indicator[1], sep = " "))
  #check if unique establishment-indicator set has any values, if not skip this iteration
  if(sum(squis_spc$numerator) == 0 & sum(squis_spc$denominator) == 0) return(cat("", "Skipped", squis_spc$establishment[1], squis_spc$indicator[1], sep = " "))

  squis_spc$denominator<- if_else(squis_spc$denominator < 0.1, 0.1, squis_spc$denominator)

  # Set the spc chart headings
  spc_heading <- paste(squis_spc$descriptionshort[1], squis_spc$shorthospitalname, sep = " - ")
  spc_period_start <- min(squis_spc$period_start)
  spc_period_end   <- max(squis_spc$period_end)
  spc_sub_heading <- paste(format(spc_period_start, format = "%b-%y"), format(spc_period_end, format = "%b-%y"), sep = " to ")

  # Calculate the limits using the qic package
  hqiu_spc <- qicharts2::qic(
    x  = squis_spc$period_end,
    y  = squis_spc$numerator,
    n = squis_spc$denominator,
    data = squis_spc,
    chart = squis_spc$spccharttype[1],
    multiply = squis_spc$multiplier[1],
    y.percent = TRUE)

  # Pull out the data from the ggplot2 object hqiu_spc
  hqiu_spc_df <- hqiu_spc$data
  #Clear NaN and Inf values from cl to determine if spc was successful
  hqiu_spc_df <- mutate(hqiu_spc_df, across(.cols = ucl.95, .fns = clear_na)) %>%
    drop_na(y)
  #SPC Plot

  #Checks if confidence limits are below 0, if so set to 0, as negative values are not possible
  if(!is.na(hqiu_spc_df$ucl.95[1])){
    if(hqiu_spc_df$lcl.95[1] < 0) hqiu_spc_df$lcl.95 <- 0
    if(hqiu_spc_df$lcl[1] < 0) hqiu_spc_df$lcl <- 0
  }
  if(squis_spc$y_axis_label[1] == "Percentage"){
    propToPercent <- c("y", "cl", "ucl.95", "ucl", "lcl.95", "lcl")
    for(i in propToPercent){
      hqiu_spc_df[i] <-  hqiu_spc_df[i]*100 %>%
        round()
    }
  }
  if(squis_spc$y_axis_label[1] == "Rate" || squis_spc$descriptionshort[1] == "Vaginal birth (2) after C section"){
    squis_spc$y_axis_label[1] <- paste("Rate per", squis_spc$multiplier[1], sep = " ")
  }
  hqiu_spc_plot <- ggplot(hqiu_spc_df, aes(x = x)) +
    # Add the data's line, with points added
    geom_line(aes(y = y), color = brand_colour, size = 0.5) +
    geom_point(aes(y = y),color = brand_colour, size = 4) +
    # Add the centre line
    geom_line(aes (y = cl), color = "black") +
    # Add the upper control limit
    geom_line(aes (y = ucl.95), color = brand_colour, linetype = 3, size = 1) +
    # Add the upper warning limit
    geom_line(aes (y = ucl), color = brand_colour, linetype = 5, size = 1) +
    # Add the lower warning limit
    geom_line(aes (y = lcl), color = brand_colour, linetype = 5, size = 1) +
    # Add the lower control limit
    geom_line(aes (y = lcl.95), color = brand_colour, linetype = 3, size = 1) +
    #labels, see start of chunk for setup
    labs (x = "", y = squis_spc$y_axis_label[1],
          title = spc_heading,
          subtitle = spc_sub_heading,
          caption = "Source: Healthcare Quality Intelligence Unit")+
    hqiu_theme()

  #checks if a patterns dataframe is input and if any patterns were detected
  if(is.data.frame(patterns_df) && !is_empty(squis_patterns[[1]])){
    #filters the patterns dataframe to see if the current input df is an spc to be displayed
    filt_pat <- filter(patterns_df, Indicator == squis_spc$descriptionshort[1], Hospital == squis_spc$shorthospitalname[1])
    if(nrow(filt_pat)){
      #filter columns to only those that are related to patterns, x and y co-ordinates
      pattern_info <- filter(hqiu_spc_df, hqiu_spc_df$x %in% filt_pat[4:7]) %>%
        subset(select = c(x, y))
      #creates a dataframe that has the dates of patterns as well as the pattern name for the current spc
      pat_info <- tibble(value = c(filt_pat$Astro, filt_pat$Trend, filt_pat$TwoInThree, filt_pat$Shift),
                         Pattern = c("\u2252", "\u24e3", "\u2154" , "\u24e2"))%>%
        drop_na()
      #joins the two dataframes to now hold the x, y and pattern identifier
      pat_agg <- left_join(pat_info, pattern_info, by = c("value" = "x"))

      if(squis_spc$betteris[1] == "Lower") nudge_y <- -max(hqiu_spc_df$y)/10 else nudge_y <- max(hqiu_spc_df$y)/10
      #enables rendering of Unicode symbols
      hqiu_spc_plot <- hqiu_spc_plot +
        #Adds the circle and tag around points
        geom_point(data = pat_agg, mapping = aes(x = value, y = y), colour = "pink", size = 8, shape = 21, stroke = 2) +
        ggrepel::geom_text_repel(data = pat_agg, mapping = aes(x = value, y = y), label = pat_agg$Pattern, size = 8, nudge_y = nudge_y)
    }
  }
  suppressWarnings(print(hqiu_spc_plot))
}
