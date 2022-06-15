#apply to funnel to use theme , p + hqiu_funnel_theme()
hqiu_funnel_theme <- function(){
  theme(
    legend.position = c(.5, 0.99), legend.background = element_blank(), legend.key = element_blank(), legend.direction = "horizontal",
    panel.background = element_blank(), plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
    axis.ticks = element_blank(), plot.caption = element_text(face = "italic"), legend.title = element_text(face = "bold")
  )}

#apply to plot to use theme, p + hqiu_theme()
hqiu_theme <- function(){
  theme(
    panel.background = element_blank(), plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
    axis.ticks = element_blank(), plot.caption = element_text(face = "italic")
  )}

#Takes input vectors and converts to dataframe with first entry as column name
create_table <- function(...){
  #binds the vectors into dataframe
  output <- data.frame(...)
  #takes first item in vector and makes it the column name
  colnames(output) <- output[1,]
  #removes repeated row
  output <- output[-1,]
  return(output)
}
#Take input data frame from create_table function and adhere to DOH style guide - green header white text
table_theme <- function(input_df){
  flextable::flextable(input_df) %>%
    #fit size to whole screen
    flextable::autofit() %>%
    #background colour
    flextable::bg(part="header", bg = "#00667B") %>%
    #text colour
    flextable::color(part = "header", color = "white") %>%
    flextable::bold(part = "header")
}

#Reduces NaN and Inf values to NA
clear_na <- function(input_df){
  input_df[is.nan(input_df)] <- NA
  input_df[is.infinite(input_df)] <- NA
  return(input_df)
}

#Changes NA values to zero.
clear_zero <- function(input_df){
  input_df[is.na(input_df)] <- 0
  return(input_df)
}