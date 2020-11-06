#' @title the_theme
#' @description Function where Theme attributes are defined
#' @import tibble
#' @return The Theme Colouts & Font
#' @author Andrés E. González Demori

the_theme <- function() {
  
  colours <- c(
    "Blue",  
    "King Blue",
    "Green",
    "White",
    "Gray",
    "Light Gray",
    "Light Gray 1",
    "Light Gray 2",
    "Black",
    "Red",
    "Light Blue"
    )
  codes <- c(
    "#0073e5",
    "#233a4a",
    "#7ddc1f",
    "#f5f5f5",
    "#444444",
    "#CCCCCC",
    "#f6f6f6",
    "#eaeaea",
    "#000000",
    "#ff3232",
    "#97a1bd")
  
  # Colours
  theme_colours <- tibble(colour = colours, code   = codes)
  
  # Fonts
  theme_font <- "Impact"
  
  # Return
  theme_list <- list(theme_colours = theme_colours, theme_font = theme_font)
  return(theme_list)
}


#' @title search_poi
#' @description Get PoI Unique Id
#' @param input_directory IDB full directory
#' @param output_directory Filtered DataBase by Pattern in .CSV
#' @param pattern Pattern to be use to Look Up Player in DataBase. E.g. Player Last Name, with no special characters.
#' @import dplyr
#' @import stringr
#' @import readr
#' @return The Theme Colouts & Font
#' @author Andrés E. González Demori

search_poi <- function(input_directory, output_directory, pattern) {

  #-----Validations-----
  if (str_length(pattern) == 0) {
    
    stop("Please provide a valid Player Unique Id.")
  }
  
  #-----Read Process-----
  # Read IDB
  file_name <- "^MOD_IDB.RDS$"
  file_to_read <- list.files(input_directory, file_name, full.names = TRUE)
  
  idb <- tryCatch({
    
    readRDS(file_to_read)
    
  }, error = function(error) {
    
    print('Input data "MOD_IDB.RDS" is not in specified directory.')
  })
  
  #-----Search Process-----
  # Strings to Lower
  pattern <- str_to_lower(pattern)
  
  idb <- idb %>% 
    mutate(player_name_fm = str_to_lower(player_name_fm),
           player_name_fs = str_to_lower(player_name_fs))
  
  # Search
  df <- filter(idb, str_detect(player_name_fm, pattern))
  
  if (nrow(df) == 0) df <- filter(idb, str_detect(player_name_fs, pattern))
  
  if (nrow(df) == 0) stop("Pattern not found in DataBase.\n
                          Please select another.")
  
  #Select Variables
  df <- df %>% 
    select(season, 
           player_name_fm, player_name_fs, fm_player_age, player_nationality_1, 
           club, division, unique_id)
  
  #-----Output-----
  file_name <- str_c("search_results_", pattern, ".CSV")
  write_delim(df, file.path(output_directory, file_name), delim = ";")
  print("Please check output.")
}