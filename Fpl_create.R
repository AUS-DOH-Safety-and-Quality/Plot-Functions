#Input data frame filtered to indicator and current funnel, cut off value for the height of the y value added manually, to be added as a column in data
fpl_create <- function(input_df, cut_off = 0){
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