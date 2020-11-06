 # title: Player of Interest App
# author: "De Pinto, M. & González Demori, A."
# date: "5/17/2020"
# objective: Create Function to Search using a Player as reference, Player of Interest

#' @title poi_app
#' @description Function to read data to be used in the Infographics
#' @param poi Player of Interest Unique ID
#' @param distance Selected Distance. Only "minkowski" is available.
#' @param input_directory Data Directory
#' @param output_directory Output Directory
#' @import purrr
#' @return Player's of Interest Infographics
#' @author Andrés E. González Demori

poi_app <- function(poi, distance, input_directory, output_directory) {
  
  print("Generating Plots for Player of Interest")
  
  # font_import() # Import all fonts, needed before ggplot
  
  # GLOBAL OPTIONS
  options(digits = 6, scipen = 999)
  
  #-----Get Data Process-----
  # Validate if poi exists
  # Message length warning of PoI Available Data
  print("Reading Data")
  input       <- read_data(input_directory, poi, distance)
  idb         <- pluck(input, "idb")
  diss_matrix <- pluck(input, "diss_matrix")
  rm(input)
  
  #-----The Theme-----
  theme_list <- the_theme()
  
  #-----Transformation Process-----
  print("Prepating Data")
  input       <- prepare_data(idb, diss_matrix, poi, theme_list)
  idb         <- pluck(input, "idb")
  chart1_data <- pluck(input, "chart1_data")
  chart2_data <- pluck(input, "chart2_data")
  chart3_data <- pluck(input, "chart3_data")
  rm(input)

  #-----Plot Infographics Process-----
  # Use tryCatch
  infographic1(chart1_data, poi, theme_list, output_directory)
  infographic2(chart2_data, poi, theme_list, output_directory)
  infographic3(chart3_data, poi, theme_list, output_directory)
  
  print("Done. Please check Infographics.")
}