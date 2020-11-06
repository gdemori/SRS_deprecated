# title: Read Data Function
# author: "De Pinto, M. & González Demori, A."
# date: "5/17/2020"
# objective: Function to read data to be used in the Infographics

#' @title read_data
#' @description Function to read data to be used in the Infographics
#' @param input_directory Data Directory
#' @param poi Player of Interest Unique Id
#' @param distance Selected Distance
#' @import stringr
#' @import dplyr
#' @return list containing idb & diss_matrix dataframes
#' @author Andrés E. González Demori
#' 
read_data <- function(input_directory, poi, distance = "minkowski") {
  
  #-----Read & Validation Process-----
  # Validate Distance
  distances <- c("minkowski")
  if (!distance %in% distances) {
    
    stop(str_c("Selected distance must be one the following: ", distances))
  }
  
  # Read IDB
  file_name <- "^MOD_IDB.RDS$"
  file_to_read <- list.files(input_directory, file_name, full.names = TRUE)
  
  idb <- tryCatch({
    
    readRDS(file_to_read)
  
    }, error = function(error) {
    
      print('Input data "MOD_IDB.RDS" is not in specified directory.')
    })
  
  # Validate PoI
  if (!poi %in%  unique(pull(idb, unique_id))) {
    
    stop("Player of Interest Unique Id not found in Database.\n
         Please check value.")
  }
  
  #  Read Dissimilarity Matrix
  file_name <- str_c("diss_matrix_", str_to_lower(distance), ".RDS")
  file_to_read <- list.files(input_directory, file_name, full.names = TRUE)
  
  # Read, if exists
  if (file.exists(file_to_read)) {
    
    diss_matrix <- tryCatch({
      
      readRDS(file_to_read)
      
    }, error = function(error) {
      
      print(str_c("Input data ", str_c("diss_matrix_", distance, ".RDS"),
                  " is not in specified directory."))
    })
    
    # If does not exist, Calculate
  } else if (!file.exists(file_to_read)) {
    
    # Calculate
    diss_matrix <- idb  %>%
      select(starts_with("PC")) %>%
      dist(method = distance) %>%
      as_tibble()
    
    # Save for future Explorations
    print(str_c("Saving Dissimilarity Matrix ", distance,
                " for future explorations."))
    file_name <- str_c("diss_matrix_", distance, ".RDS")
    saveRDS(file.path(input_directory, file_name))
  }

  # Function Return Object
  return(list(
    idb         = idb,
    diss_matrix = diss_matrix))
}


#' @title get_diss_matrix
#' @description Function calculate Dissimilarity Matrix by Selected Distance
#' @param idb The Integrated Data Base (IDB)
#' @param distance Selected Distance (euclidean, maximum, minkowskim, manhattan)
#' @param output_dir Save Output Directory
#' @import dplyr
#' @import stringr
#' @return list containing data needed to plot
#' @author Andrés E. González Demori
#' 
get_diss_matrix <- function(idb, distance, output_dir) {
  
  # GET DISSIMILARITY MATRIX
  diss_matrix <- idb %>% 
    select(starts_with("PC")) %>% 
    dist(method = distance)
  
  diss_matrix$unique_id <- idb$unique_id
  names(diss_matrix) <- c("unique_id", idb$unique_id)
  
  # Function Return Object
  file_name <- str_c("diss_matrix_", distance, ".RDS")
  saveRDS(diss_matrix, file.path(diss_matrix_dir, file_name))
}

# # Output Folder by POI
# output_directory <- file.path(output_directory, unique_id)
# 
# if(!dir.exists(output_directory)) {
#   dir.create(output_directory)
#   
# } else {
#   
#   stop(str_c("Output Directory of ", unique_id, " already exists. Please verify."))
# }