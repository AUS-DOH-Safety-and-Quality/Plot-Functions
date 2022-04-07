print_fpl <- function(input_df){
  squis_test <- input_df
  
  #creates a unique name
  unique_name <- paste("Funnel_", squis_test$descriptionshort[1], squis_test$indicator[1], ".png", sep = "")
  #Certain characters not allowed in file names
  unique_name <- gsub("<", "less than " , unique_name)
  unique_name <- gsub("%", "Percentage" , unique_name)
  print(unique_name)
  
  #Create Funnel
  ind_funnel <- funnel_plot(denominator=squis_test$denominator, numerator=squis_test$numerator,
                            group = squis_test$establishment, limit=99,
                            data_type = squis_test$funnelcharttype[1], sr_method = "CQC", multiplier = squis_test$multiplier[1],
                            draw_unadjusted = TRUE,
                            draw_adjusted = FALSE,
                            label = "outlier")
  #extract limits
  lim_data <- ind_funnel$limits_lookup
  
  funnel_period_start <- min(squis_test$period_start)
  funnel_period_end   <- max(squis_test$period_end)
  date_range <- paste(format(funnel_period_start, format = "%b-%y"),
                      format(funnel_period_end, format = "%b-%y"),
                      sep = " to ")
  #checks chart type and sets values for centre line per chart
  if (squis_test$funnelcharttype[1] == "PR"){
    centre_line <- (sum(squis_test$numerator)/ sum(squis_test$denominator))
  }else if (squis_test$funnelcharttype[1] == "SR"){
    centre_line <- 1
  }
  centre_line <- centre_line * squis_test$multiplier[1]
  X <- 0
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
  
  #Funnel Plot
  fpl_plot <- ggplot(ind_funnel$plot$data, 
                     aes(x=denominator, y = ind_funnel$plot$data$rr*squis_test$multiplier[1]))+
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
         title=squis_test$descriptionshort[1],
         subtitle = date_range,
         caption = "Source: Healthcare Quality Intelligence Unit",
         x = "",
         y = squis_test$y_axis_label[1])
  #checks cut off values to see where to cut plot
  if (cut_off != 0){
    hqiu_funnel_finished <- fpl_plot + coord_cartesian(ylim = c(0, cut_off))
  }else hqiu_funnel_finished <- fpl_plot
  
  #saves the plot into the specified path
  #DIRECT THIS WHERE YOU WANT TO SAVE
  ggsave(filename = unique_name, plot = hqiu_funnel_finished, device = "png", path = "C:/Users/he198926/Desktop/SPC_test", width = 10, height = 8)
  return(cat("Completed", squis_test$descriptionshort[1], " "))
}