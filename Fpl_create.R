#Input data frame filtered to indicator and current funnel, cut off value for the height of the y value added manually, to be added as a column in data
fpl_create <- function(input_df){
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
    squis_test$indicator[1] == "Q0009" ~ 25, #AMI
    squis_test$indicator[1] == "Q0010" ~ 25, #Stroke
    squis_test$indicator[1] == "Q0012" ~ 15, #Pneumonia
    squis_test$indicator[1] == "Q0014" ~ 450, #HSMR
    squis_test$indicator[1] == "Q0040" ~ 20, #FNOF
    squis_test$indicator[1] == "Q0006" ~ 5, #Apgar
    squis_test$indicator[1] == "Q0016" ~ 60, #C Section
    squis_test$indicator[1] == "Q0004" ~ X, #Complaints Res 30 day - Higher is better
    squis_test$indicator[1] == "Q0057" ~ X, #Hand Hygiene - Higher is better
    squis_test$indicator[1] == "Q0101" ~ X, #Incident Patient Outcome Harm - Proportion
    squis_test$indicator[1] == "Q0078" ~ 100, #Incident Reporting Rate - Unsure
    squis_test$indicator[1] == "Q0003" ~ X, #Incidents Open Disclosure - Percentage
    squis_test$indicator[1] == "Q0022" ~ X, #Incidents SAC1 Eval 6m - Percentage
    squis_test$indicator[1] == "Q0120" ~ X, #Incidents SAC128 - Percentage
    squis_test$indicator[1] == "Q0103" ~ X, #Induction Rate - Proportion
    squis_test$indicator[1] == "Q0144" ~ X, #MH Consumer Experience - Higher is better
    squis_test$indicator[1] == "Q0149" ~ X, #MH Outcomes Clinician rated IP - Change in value
    squis_test$indicator[1] == "Q0148" ~ X, #MH Outcomes Consumer rated IP - Change in value
    squis_test$indicator[1] == "Q0130" ~ X, #MH % ED Attendances Admitted <4hrs - Proportion
    squis_test$indicator[1] == "Q0020" ~ 50, #Perinatal Mortality 
    squis_test$indicator[1] == "Q0084" ~ X, #Post-partum Haemorrhage - Proportion
    squis_test$indicator[1] == "Q0139" ~ 30, #Restraint Rates
    squis_test$indicator[1] == "Q0140" ~ 30, #Seclusion Rates
    squis_test$indicator[1] == "Q0127" ~ X, #Staff Committed To Safety - Proportion, Higher is better
    squis_test$indicator[1] == "Q0128" ~ X, #Staff Feel Empowered - Proportion, Higher is better
    squis_test$indicator[1] == "Q0129" ~ X, #Staff Friends And Family - Proportion, Higher is better
    squis_test$indicator[1] == "Q0032" ~ X, #Staff Safe To Speak Up - Proportion, Higher is better
    squis_test$indicator[1] == "Q0126" ~ X, #Staff Treated Fairly - Proportion, Higher is better
    squis_test$indicator[1] == "Q0019" ~ 30, #Stillbirth 
    squis_test$indicator[1] == "Q0121" ~ 30) #VBAC
  }
  
  plot <- ggplot(funnel_test$plot$data, aes(x=denominator, y = funnel_test$plot$data$rr*input_df$multiplier[1]))+
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
  #temporary manual cut of limits until new row finalised
  if(cut_off != 0){
    plot + coord_cartesian(ylim = c(0,cut_off))
  }else plot
  #R by default returns final line ran
}
