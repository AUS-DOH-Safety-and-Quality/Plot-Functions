fpl_create <- function(input_df, highlight_hosp = "No", highlight_outlier = TRUE, brand_colour = "#00667B"){
  #Input_df = data frame filtered to indicator and current_funnel
  #highlight_hosp = shorthospitalname of site to individually highlight
  #highlight_outlier = if false, do not highlight outliers with their threeletteracronym
  #brand_colour = colour of control limits

  #return early if 0 or 1 rows, cannot funnel with these
  if (!nrow(input_df) | nrow(input_df) == 1) return()
  funnel <- FunnelPlotR::funnel_plot(denominator=input_df$denominator, numerator=input_df$numerator,
                             group = input_df$establishment, limit=99,
                             data_type = input_df$funnelcharttype[1], sr_method = "CQC", multiplier = input_df$multiplier[1],
                             draw_unadjusted = TRUE,
                             draw_adjusted = TRUE,
                             label = NA,
                             highlight  = NA)
  #pulls out the data for the limits of the plot
  lim_data <- funnel$limits_lookup
  # Create the labels for our funnel plot
  funnel_period_start <- min(input_df$period_start)
  funnel_period_end   <- max(input_df$period_end)
  date_range <- paste(format(funnel_period_start, format = "%b-%y"),
                      format(funnel_period_end, format = "%b-%y"),
                      sep = " to ")

  #checks chart type and sets values for centre line per chart
  if (input_df$funnelcharttype[1] == "PR"){
    centre_line <- (sum(input_df$numerator)/ sum(input_df$denominator))
  }else if (input_df$funnelcharttype[1] == "SR"){
    centre_line <- 1
  }
  centre_line <- centre_line * input_df$multiplier[1]

  {

  cut_off_max <- case_when(
    input_df$indicator[1] == "Q0009" ~ 50, #AMI
    input_df$indicator[1] == "Q0010" ~ 50, #Stroke
    input_df$indicator[1] == "Q0012" ~ 30, #Pneumonia
    input_df$indicator[1] == "Q0014" ~ 550, #HSMR
    input_df$indicator[1] == "Q0040" ~ 40, #FNOF
    input_df$indicator[1] == "Q0006" ~ 5, #Apgar
    input_df$indicator[1] == "Q0016" ~ 60, #C Section
    #input_df$indicator[1] == "Q0004" ~ X, #Complaints Res 30 day - Higher is better
    #input_df$indicator[1] == "Q0057" ~ X, #Hand Hygiene - Higher is better
    #input_df$indicator[1] == "Q0101" ~ X, #Incident Patient Outcome Harm - Proportion
    input_df$indicator[1] == "Q0078" ~ 100, #Incident Reporting Rate - Unsure
    #input_df$indicator[1] == "Q0003" ~ X, #Incidents Open Disclosure - Percentage
    # input_df$indicator[1] == "Q0022" ~ X, #Incidents SAC1 Eval 6m - Percentage
    # input_df$indicator[1] == "Q0120" ~ X, #Incidents SAC128 - Percentage
    # input_df$indicator[1] == "Q0103" ~ X, #Induction Rate - Proportion
    # input_df$indicator[1] == "Q0144" ~ X, #MH Consumer Experience - Higher is better
    # input_df$indicator[1] == "Q0149" ~ X, #MH Outcomes Clinician rated IP - Change in value
    # input_df$indicator[1] == "Q0148" ~ X, #MH Outcomes Consumer rated IP - Change in value
    # input_df$indicator[1] == "Q0130" ~ X, #MH % ED Attendances Admitted <4hrs - Proportion
    input_df$indicator[1] == "Q0020" ~ 50, #Perinatal Mortality
    input_df$indicator[1] == "Q0084" ~ 0.15, #Post-partum Haemorrhage - Proportion
    input_df$indicator[1] == "Q0139" ~ 30, #Restraint Rates
    input_df$indicator[1] == "Q0140" ~ 30, #Seclusion Rates
    # input_df$indicator[1] == "Q0127" ~ X, #Staff Committed To Safety - Proportion, Higher is better
    # input_df$indicator[1] == "Q0128" ~ X, #Staff Feel Empowered - Proportion, Higher is better
    # input_df$indicator[1] == "Q0129" ~ X, #Staff Friends And Family - Proportion, Higher is better
    # input_df$indicator[1] == "Q0032" ~ X, #Staff Safe To Speak Up - Proportion, Higher is better
    # input_df$indicator[1] == "Q0126" ~ X, #Staff Treated Fairly - Proportion, Higher is better
    input_df$indicator[1] == "Q0019" ~ 10, #Stillbirth
    input_df$indicator[1] == "Q0121" ~ 30, #VBAC
    TRUE ~ 0)

  cut_off_min <- case_when(
    input_df$indicator[1] == "Q0127" ~ 0.5, #Staff Committed To Safety - Proportion, Higher is better
    input_df$indicator[1] == "Q0128" ~ 0.5, #Staff Feel Empowered - Proportion, Higher is better
    input_df$indicator[1] == "Q0129" ~ 0.5, #Staff Friends And Family - Proportion, Higher is better
    input_df$indicator[1] == "Q0057" ~ 0.7, #Hand Hygiene - Higher is better
    TRUE ~ 0)
  }

  if(input_df$y_axis_label[1] == "Rate" | input_df$descriptionshort[1] == "Vaginal birth (2) after C section"){
    input_df$y_axis_label[1] <- paste("Rate per", input_df$multiplier[1], sep = " ")
  }

  fpl_plot <- ggplot(funnel$plot$data, aes(x=denominator, y = funnel$plot$data$rr*input_df$multiplier[1]))+
    hqiu_funnel_theme()+
    geom_point(colour = brand_colour) +
    geom_line(data = lim_data, aes(x=number.seq, y=ll95, linetype = "95%"), size = 1, colour = brand_colour)+
    geom_line(data = lim_data, aes(x=number.seq, y=ul95, linetype = "95%"), size = 1, colour = brand_colour)+
    geom_line(data = lim_data, aes(x=number.seq, y=ll998,linetype = "99%"), size = 1, colour = brand_colour)+
    geom_line(data = lim_data, aes(x=number.seq, y=ul998,linetype = "99%"), size = 1, colour = brand_colour)+
    geom_hline(data = lim_data, aes(yintercept = centre_line))+
    scale_colour_manual(values = c(brand_colour,brand_colour))+
    scale_linetype_manual(values = c(3,5))+
    labs(linetype = "Control Limits",
         title=input_df$descriptionshort[1],
         subtitle = date_range,
         caption = "Source: Healthcare Quality Intelligence Unit",
         x = "",
         y = input_df$y_axis_label[1])+
    #enables comma notation of x axis
    scale_x_continuous(labels = scales::comma)
  #input_df$y_axis_label[1]
  #check betteris, if higher, set highlight points to be points that are below the lower 99 limit, else,
  #if Lower, set it to points above the upper 99 limit
  if(input_df$betteris[1] == "Higher"){
    #if point value is less than the lower control limit flag the point
    highlight_points <- ifelse(funnel$plot$data$rr*input_df$multiplier[1] < fpl_plot$data$LCL99,
                               funnel$plot$data$rr*input_df$multiplier[1], NA)
  }else highlight_points <- ifelse(funnel$plot$data$rr*input_df$multiplier[1] > fpl_plot$data$UCL99,
                                   funnel$plot$data$rr*input_df$multiplier[1], NA)

  #create a dataframe that holds the establishment and hospital names to serve as a lookup table
  #We only have the establishment code in the plot output so we need this to find the hospital name
  outlier_label <- data.frame(establishment = unique(input_df$establishment),
                              threeletteracronym = unique(input_df$threeletteracronym),
                              shorthospitalname = unique(input_df$shorthospitalname))
  #create a dataframe that holds the points on the graph, so that we can overlay them with a circle and hospital name
  outlier_points <- data.frame(x = funnel$plot$data$denominator, y = highlight_points, establishment = funnel$plot$data$group) %>%
    #drop values that aren't outliers
    drop_na(y)
  #join the two tables
  outlier_lookup <- left_join(outlier_points, outlier_label, by ="establishment")
  if (highlight_outlier == TRUE){
  #Add a point geom, over the top of outliers, that is a red hollow circle
  fpl_plot <- fpl_plot + geom_point(data = outlier_lookup, aes(x = x, y = y), colour = "blue", size = 5, shape = 1)+
    #add a text geom, over the top of outliers, that is text relaying the name of the hospital for that outlier
    ggrepel::geom_text_repel(data = outlier_lookup, aes(x=x, y=y), label = outlier_lookup$threeletteracronym)
  }
  #check if the hospital input is in the data provided
  if(highlight_hosp %in% unique(input_df$shorthospitalname)){
    #filter the outlier label to only the input hospital
    highlight <- filter(outlier_label, shorthospitalname == highlight_hosp)
    #join by establishment to find highlighted point's x and y values
    highlight_point <- left_join(highlight, fpl_plot$data, by = c("establishment" = "group"))
      fpl_plot <- fpl_plot +
        #add circle around point
      geom_point(highlight_point, mapping = aes(x=denominator, y = rr*input_df$multiplier[1]), colour = "black", size = 5, shape = 1) +
      #add text of hospitalshortname
      ggrepel::geom_text_repel(highlight_point, mapping = aes(x=denominator, y = rr*input_df$multiplier[1]), label = highlight_point$shorthospitalname, point.size = 7, size = 5)
  }

  #cut off limits
  if (cut_off_min != 0){
    return(suppressWarnings(print(fpl_plot + coord_cartesian(ylim = c(cut_off_min, 1)))))
  }

  if (cut_off_max != 0){
    return(suppressWarnings(print(fpl_plot + coord_cartesian(ylim = c(0, cut_off_max)))))
  }
  suppressMessages(suppressWarnings(print(fpl_plot)))
}
