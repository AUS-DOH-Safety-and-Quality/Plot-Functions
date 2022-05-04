#Input data frame filtered to indicator and current funnel, cut off value for the height of the y value added manually, to be added as a column in data
fpl_create <- function(input_df, highlight_hosp = "No"){
  funnel_test <- funnel_plot(denominator=input_df$denominator, numerator=input_df$numerator,
                             group = input_df$establishment, limit=99,
                             data_type = input_df$funnelcharttype[1], sr_method = "CQC", multiplier = input_df$multiplier[1],
                             draw_unadjusted = TRUE,
                             draw_adjusted = TRUE,
                             label = NA,
                             highlight  = NA)
  #pulls out the data for the limits of the plot
  lim_data <- funnel_test$limits_lookup
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

  X <- 0
  {
  cut_off <- case_when(
    input_df$indicator[1] == "Q0009" ~ 25, #AMI
    input_df$indicator[1] == "Q0010" ~ 25, #Stroke
    input_df$indicator[1] == "Q0012" ~ 15, #Pneumonia
    input_df$indicator[1] == "Q0014" ~ 450, #HSMR
    input_df$indicator[1] == "Q0040" ~ 20, #FNOF
    input_df$indicator[1] == "Q0006" ~ 5, #Apgar
    input_df$indicator[1] == "Q0016" ~ 60, #C Section
    input_df$indicator[1] == "Q0004" ~ X, #Complaints Res 30 day - Higher is better
    input_df$indicator[1] == "Q0057" ~ X, #Hand Hygiene - Higher is better
    input_df$indicator[1] == "Q0101" ~ X, #Incident Patient Outcome Harm - Proportion
    input_df$indicator[1] == "Q0078" ~ 100, #Incident Reporting Rate - Unsure
    input_df$indicator[1] == "Q0003" ~ X, #Incidents Open Disclosure - Percentage
    input_df$indicator[1] == "Q0022" ~ X, #Incidents SAC1 Eval 6m - Percentage
    input_df$indicator[1] == "Q0120" ~ X, #Incidents SAC128 - Percentage
    input_df$indicator[1] == "Q0103" ~ X, #Induction Rate - Proportion
    input_df$indicator[1] == "Q0144" ~ X, #MH Consumer Experience - Higher is better
    input_df$indicator[1] == "Q0149" ~ X, #MH Outcomes Clinician rated IP - Change in value
    input_df$indicator[1] == "Q0148" ~ X, #MH Outcomes Consumer rated IP - Change in value
    input_df$indicator[1] == "Q0130" ~ X, #MH % ED Attendances Admitted <4hrs - Proportion
    input_df$indicator[1] == "Q0020" ~ 50, #Perinatal Mortality
    input_df$indicator[1] == "Q0084" ~ X, #Post-partum Haemorrhage - Proportion
    input_df$indicator[1] == "Q0139" ~ 30, #Restraint Rates
    input_df$indicator[1] == "Q0140" ~ 30, #Seclusion Rates
    input_df$indicator[1] == "Q0127" ~ X, #Staff Committed To Safety - Proportion, Higher is better
    input_df$indicator[1] == "Q0128" ~ X, #Staff Feel Empowered - Proportion, Higher is better
    input_df$indicator[1] == "Q0129" ~ X, #Staff Friends And Family - Proportion, Higher is better
    input_df$indicator[1] == "Q0032" ~ X, #Staff Safe To Speak Up - Proportion, Higher is better
    input_df$indicator[1] == "Q0126" ~ X, #Staff Treated Fairly - Proportion, Higher is better
    input_df$indicator[1] == "Q0019" ~ 30, #Stillbirth
    input_df$indicator[1] == "Q0121" ~ 30) #VBAC
  }

  fpl_plot <- ggplot(funnel_test$plot$data, aes(x=denominator, y = funnel_test$plot$data$rr*input_df$multiplier[1]))+
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
         y = input_df$y_axis_label[1])

  #check betteris, if higher, set highlight points to be points that are below the lower 99 limit, else,
  #if Lower, set it to points above the upper 99 limit
  if (input_df$betteris[1] == "Higher"){
    #if point value is less than the lower control limit flag the point
    highlight_points <- ifelse(funnel_test$plot$data$rr*input_df$multiplier[1] < fpl_plot$data$LCL99,
                               funnel_test$plot$data$rr*input_df$multiplier[1], NA)
  }else highlight_points <- ifelse(funnel_test$plot$data$rr*input_df$multiplier[1] > fpl_plot$data$UCL99,
                                   funnel_test$plot$data$rr*input_df$multiplier[1], NA)

  #create a dataframe that holds the establishment and hospital names to serve as a lookup table
  #We only have the establishment code in the plot output so we need this to find the hospital name
  outlier_label <- data.frame(establishment = unique(input_df$establishment),
                              threeletteracronym = unique(input_df$threeletteracronym),
                              shorthospitalname = unique(input_df$shorthospitalname))
  #create a dataframe that holds the points on the graph, so that we can overlay them with a circle and hospital name
  outlier_points <- data.frame(x = funnel_test$plot$data$denominator, y = highlight_points, establishment = funnel_test$plot$data$group) %>%
    #drop values that aren't outliers
    drop_na(y)
  #join the two tables
  outlier_lookup <- left_join(outlier_points, outlier_label, by ="establishment")
  #Add a point geom, over the top of outliers, that is a red hollow circle
  fpl_plot <- fpl_plot + geom_point(data = outlier_lookup, aes(x = x, y = y), colour = "blue", size = 5, shape = 1)+
    #add a text geom, over the top of outliers, that is text relaying the name of the hospital for that outlier
    geom_text_repel(data = outlier_lookup, aes(x=x, y=y), label = outlier_lookup$threeletteracronym)

  if(highlight_hosp %in% unique(input_df$shorthospitalname)){
    highlight <- filter(outlier_label, shorthospitalname == highlight_hosp)
    highlight_point <- left_join(highlight, fpl_plot$data, by = c("establishment" = "group"))
      fpl_plot <- fpl_plot +
      geom_point(highlight_point, mapping = aes(x=denominator, y = rr*input_df$multiplier[1]), colour = "black", size = 5, shape = 1) +
      geom_text_repel(highlight_point, mapping = aes(x=denominator, y = rr*input_df$multiplier[1]), label = highlight_point$shorthospitalname, nudge_y = (centre_line/2))
  }

  #temporary manual cut of limits until new row finalised
  if(cut_off != 0){
    fpl_plot + coord_cartesian(ylim = c(0,cut_off))
  }else fpl_plot
  #R by default returns final line ran
}
