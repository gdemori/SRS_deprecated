# title: Infographic 1
# author: "De Pinto, M. & González Demori, A."
# date: "5/17/2020"
# objective: Function to Create Infographic 1

#' @title infographic1
#' @description Function to create Infographic1
#' @param chart1_data The Prepared Data
#' @param poi Player of Interest
#' @param theme_list Theme Data
#' @param output_directory Output Directory
#' @import purrr
#' @import dplyr
#' @import stringr
#' @import grid
#' @import gridExtra
#' @import gtable
#' @return Player of Interest Infographic1
#' @author Andrés E. González Demori
 
infographic1 <- function(chart1_data, poi, theme_list, output_directory) {
  
  print("Generating Infographic 1")
  
  # The Data
  personal_info  <- pluck(chart1_data, "personal_info")
  contract_info  <- pluck(chart1_data, "contract_info")
  att_table      <- pluck(chart1_data, "att_table")
  player_stats   <- pluck(chart1_data, "player_stats")
  club_stats     <- pluck(chart1_data, "club_stats")
  att_plot       <- pluck(chart1_data, "att_plot")
  att_list       <- pluck(chart1_data, "att_list")
  # pos_plot       <- pluck(chart1_data, "pos_plot")
  
  # Theme Data
  theme_colours <-  pluck(theme_list, "theme_colours")
  theme_font    <-  pluck(theme_list, "theme_font")
  
  #-----Create Chart 1-----
  
  # TO DO LIST
  # Create Player's Postion Plot
  # Divide into 2 viewports
  # create best role
  # create best position
  # create small pitch with pos above 15 (with colours)
  
  # Chart Dimensions
  width <- 40
  height <- 35
  selected_unit <- "cm"
  
  # Main Layout Dimensions
  layout_nrow <- 5
  layout_ncol <- 8
  
  # File
  file_name <- str_c("Infographic1_", poi, ".png")
  png(file.path(output_directory, file_name),
      width = width, height = height, units = selected_unit, res = 1200)
  
  #-----Layout & Viewports-----
  # Main Layout
  grid.newpage()
  
  vp_main_layout <- grid.layout(
    nrow = layout_nrow,
    ncol = layout_ncol,
    widths = unit(rep(width / layout_ncol, layout_ncol),
                  rep(selected_unit, layout_ncol)),
    heights = unit(
      c(height / layout_nrow,
        height / layout_nrow,
        2 * height / layout_nrow,
        0.5 * height / layout_nrow,
        0.5 * height / layout_nrow),
      rep(selected_unit, layout_nrow)))
  
  # grid.show.layout(vp_main_layout)
  
  # Viewports
  # Main Viewport
  vp_main <- viewport(name = "vp_main", layout = vp_main_layout)
  
  # Player Name Viewport
  vp_player_name <- viewport(layout.pos.row = 1, 
                             layout.pos.col = 1:8, 
                             name = "vp_player_name")
  
  # Personal Info Viewport
  vp_personal_info <- viewport(layout.pos.row = 2,
                               layout.pos.col = 1:2, 
                               name = "vp_personal_info")
  
  # Contract Info Viewport
  vp_contract_info <- viewport(layout.pos.row = 2,
                               layout.pos.col = 3:4, 
                               name = "vp_contract_info")
  
  # PLayer Position Viewport
  vp_player_position <- viewport(layout.pos.row = 2,
                                 layout.pos.col = 5:8, 
                                 name = "vp_player_position")
  
  # Player Attributes Viewport
  vp_player_att <- viewport(layout.pos.row = 3,
                            layout.pos.col = 1:5, 
                            name = "vp_player_att")
  
  # Player Attributes Plot Viewport
  vp_player_att_plot <- viewport(layout.pos.row = 3, 
                                 layout.pos.col = 6:8, 
                                 name = "vp_player_att_plot")
  
  # Player Stats Viewport
  vp_player_stats <- viewport(layout.pos.row = 4,
                              layout.pos.col = 1:6,
                              name = "vp_player_stats")
  
  # Club Stats Viewport
  vp_club_stats <- viewport(layout.pos.row = 5,
                            layout.pos.col = 1:6,
                            name = "vp_club_stats")
  
  # Footy Data Tales Viewport
  vp_author <- viewport(layout.pos.row = 5, 
                        layout.pos.col = 7:8, 
                        name = "vp_author")
  
  # Viewport Tree
  vp_chart1 <- vpTree(vp_main, vpList(vp_player_name, 
                                      vp_personal_info,
                                      vp_contract_info,
                                      vp_player_position,
                                      vp_player_att,
                                      vp_player_att_plot,
                                      vp_player_stats,
                                      vp_club_stats,
                                      vp_author))
  
  # Push Chart 1 Viewport
  pushViewport(vp_chart1)
  
  #-----Main Viewport-----
  seekViewport("vp_main")
  grid.rect(gp = gpar(fill = "#f5f5f5", col = "#f5f5f5"))
  
  #-----Player Name Viewport-----
  seekViewport("vp_player_name")
  
  grid.text(pull(filter(personal_info, personal_info == "NAME")),
            x = unit(0.5, "npc"), 
            y = unit(0.5, "npc"),
            just = "centre",
            gp = gpar(fontfamily = theme_font, 
                      col = "#000000",
                      cex = 6, alpha = 0.8))
  
  # Table 1 Personal Information
  seekViewport("vp_personal_info")
  
  #  Background
  grid.rect(gp = gpar(fill = "#CCCCCC", col = "#000000"))
  
  # Text References 
  x_reference <- 0.5
  x_increase <- 4.5
  y_reference <- (height / layout_nrow) - 0.5
  y_increase <- 1.75
  
  # Text Format
  cex_title <- 2
  cex <- 1.2
  
  # Title
  grid.text("PERSONAL INFO",
            x = unit(x_reference, selected_unit),
            y = unit(y_reference, selected_unit),
            just = c("left", "top"),
            gp = gpar(fontfamily = theme_font,
                      col = "#000000",
                      cex = cex_title))
  
  # Left Side
  grid.text(
    str_c(
      pull(filter(personal_info, personal_info ==            "SEASON"), personal_info),
      pull(filter(personal_info, personal_info ==              "NAME"), personal_info),
      pull(filter(personal_info, personal_info ==       "NATIONALITY"), personal_info),
      pull(filter(personal_info, personal_info == "OTHER NATIONALITY"), personal_info),
      pull(filter(personal_info, personal_info == "AGE / CURRENT AGE"), personal_info),
      pull(filter(personal_info, personal_info ==     "DATE OF BIRTH"), personal_info),
      sep = "\n"),
    just = c("left", "top"),
    x = unit(x_reference, selected_unit),
    y = unit(y_reference - y_increase, selected_unit),
    gp = gpar(fontfamily = theme_font,
              col = "#000000",
              cex = cex))
  
  # Right Side
  grid.text(
    str_c(
      pull(filter(personal_info, personal_info ==            "SEASON"), value),
      pull(filter(personal_info, personal_info ==              "NAME"), value),
      pull(filter(personal_info, personal_info ==       "NATIONALITY"), value),
      pull(filter(personal_info, personal_info == "OTHER NATIONALITY"), value),
      pull(filter(personal_info, personal_info == "AGE / CURRENT AGE"), value),
      pull(filter(personal_info, personal_info ==     "DATE OF BIRTH"), value),
      sep = "\n"),
    just = c("left", "top"),
    x = unit(x_reference + x_increase, selected_unit),
    y = unit(y_reference - y_increase, selected_unit),
    gp = gpar(fontfamily = theme_font,
              col = "#000000",
              cex = cex))
  
  #-----Contract Information Viewport-----
  seekViewport("vp_contract_info")
  
  #  Background
  grid.rect(gp = gpar(fill = "#CCCCCC", col = "#000000"))
  
  # Title
  grid.text("CONTRACT INFO",
            x = unit(x_reference, selected_unit), 
            y = unit(y_reference, selected_unit),
            just = c("left", "top"),
            gp = gpar(fontfamily = theme_font,
                      col = "#000000",
                      cex = cex_title))
  
  # Left Side
  grid.text(
    str_c(
      pull(filter(contract_info, contract_info ==         "CLUB"), contract_info),
      pull(filter(contract_info, contract_info ==        "SQUAD"), contract_info),
      pull(filter(contract_info, contract_info == "CONTRACT END"), contract_info),
      pull(filter(contract_info, contract_info ==         "WAGE"), contract_info),
      pull(filter(contract_info, contract_info ==        "VALUE"), contract_info),
      pull(filter(contract_info, contract_info == "SQUAD STATUS"), contract_info),
      sep = "\n"),
    just = c("left", "top"),
    x = unit(x_reference, selected_unit),
    y = unit(y_reference - y_increase, selected_unit),
    gp = gpar(fontfamily = theme_font,
              col = "#000000",
              cex = cex))
  
  # Right Side
  grid.text(
    str_c(
      pull(filter(contract_info, contract_info ==          "CLUB"), value),
      pull(filter(contract_info, contract_info ==         "SQUAD"), value),
      pull(filter(contract_info, contract_info ==  "CONTRACT END"), value),
      pull(filter(contract_info, contract_info ==          "WAGE"), value),
      pull(filter(contract_info, contract_info ==         "VALUE"), value),
      pull(filter(contract_info, contract_info ==  "SQUAD STATUS"), value),
      sep = "\n"),
    just = c("left", "top"),
    x = unit(x_reference + x_increase, selected_unit),
    y = unit(y_reference - y_increase, selected_unit),
    gp = gpar(fontfamily = theme_font, col = "#000000", cex = cex))
  
  #-----Player Position Viewport-----
  seekViewport("vp_player_position")
  
  #  Background
  grid.rect(gp = gpar(fill = "#CCCCCC", col = "#000000"))
  
  # Text References 
  x_reference <- 0.5
  x_increase <- 4.5
  y_reference <- (height / layout_nrow) - 0.5
  y_increase <- 1.75
  
  # Text Format
  cex_title <- 2
  cex <- 1.2
  
  # Title
  grid.text("PLAYER POSITION(S)",
            x = unit(x_reference, selected_unit),
            y = unit(y_reference, selected_unit),
            just = c("left", "top"),
            gp = gpar(fontfamily = theme_font,
                      col = "#000000",
                      cex = cex_title))
  
  #-----Player Attributes Viewport-----
  seekViewport("vp_player_att")
  
  # Attributes Table
  att_table <- list(att_list$technical_att_table, att_list$mental_att_table,
                    att_list$physical_att_table, att_list$hidden_att_table) %>% 
    modify(~ mutate(., id = 1:nrow(.))) %>% 
    reduce(left_join, by = "id") %>% 
    mutate_all(~ if_else(is.na(.), "", as.character(.))) %>% 
    select(-id)
  
  # Text References
  x_reference <- 0.5
  x_increase <- 4.5
  x_padding <- 1.5
  y_reference <- 2 * (height / layout_nrow) - 0.5
  y_increase <- 1.75
  
  # Text Format
  cex_title <- 2
  
  # Title
  grid.text("PLAYER ATTRIBUTES",
            x = unit(x_reference, selected_unit),
            y = unit(y_reference, selected_unit),
            just = c("left", "top"),
            gp = gpar(fontfamily = theme_font,
                      col = "#000000",
                      cex = cex_title))
  
  # Text Format
  cex_subtitle <- 1.5
  cex <- 1.2
  
  # Titles
  # Technical
  x_reference_technical <- x_reference
  
  grid.text("Technical",
            just = c("left", "top"), 
            x = unit(x_reference_technical, selected_unit),
            y = unit(y_reference - y_increase, selected_unit),
            gp = gpar(fontfamily = theme_font,
                      col = "#000000",
                      cex = cex_subtitle))
  
  # Tactical
  x_reference_tactical <- x_reference + (x_increase + x_padding)
  
  grid.text("Tactical",
            just = c("left", "top"),
            x = unit(x_reference_tactical, selected_unit),
            y = unit(y_reference - y_increase, selected_unit),
            gp = gpar(fontfamily = theme_font,
                      col = "#000000",
                      cex = cex_subtitle))
  
  # Physical
  x_reference_physical <- x_reference + 2 * (x_increase + x_padding)
  
  grid.text("Physical",
            just = c("left", "top"),
            x = unit(x_reference_physical, selected_unit),
            y = unit(y_reference - y_increase, selected_unit),
            gp = gpar(fontfamily = theme_font, col = "#000000", cex = cex_subtitle))
  
  # Personal
  x_reference_personal <- x_reference + 3 * (x_increase + x_padding)
  
  grid.text("Personal",
            just = c("left", "top"),
            x = unit(x_reference_personal, selected_unit),
            y = unit(y_reference - y_increase, selected_unit),
            gp = gpar(fontfamily = theme_font,
                      col = "#000000",
                      cex = cex_subtitle))
  
  # Technical Left Side
  grid.text(
    str_c(
      pull(filter(att_table, technical ==     "Corners"), technical),
      pull(filter(att_table, technical ==    "Crossing"), technical),
      pull(filter(att_table, technical ==   "Dribbling"), technical),
      pull(filter(att_table, technical ==   "Finishing"), technical),
      pull(filter(att_table, technical == "First Touch"), technical),
      pull(filter(att_table, technical ==     "Heading"), technical),
      pull(filter(att_table, technical ==  "Long Shots"), technical),
      pull(filter(att_table, technical == "Long Throws"), technical),
      pull(filter(att_table, technical ==     "Marking"), technical),
      pull(filter(att_table, technical ==     "Passing"), technical),
      pull(filter(att_table, technical ==   "Penalties"), technical),
      pull(filter(att_table, technical ==  "Free Kicks"), technical),
      pull(filter(att_table, technical ==    "Tackling"), technical),
      pull(filter(att_table, technical ==   "Technique"), technical),
      sep = "\n"),
    just = c("left", "top"), 
    x = unit(x_reference_technical, selected_unit),
    y = unit(y_reference - 1.5 * y_increase, selected_unit),
    gp = gpar(fontfamily = theme_font, col = "#000000", cex = cex))
  
  # Technical Right Side
  grid.text(
    str_c(
      pull(filter(att_table, technical ==     "Corners"), t_att),
      pull(filter(att_table, technical ==    "Crossing"), t_att),
      pull(filter(att_table, technical ==   "Dribbling"), t_att),
      pull(filter(att_table, technical ==   "Finishing"), t_att),
      pull(filter(att_table, technical == "First Touch"), t_att),
      pull(filter(att_table, technical ==     "Heading"), t_att),
      pull(filter(att_table, technical ==  "Long Shots"), t_att),
      pull(filter(att_table, technical == "Long Throws"), t_att),
      pull(filter(att_table, technical ==     "Marking"), t_att),
      pull(filter(att_table, technical ==     "Passing"), t_att),
      pull(filter(att_table, technical ==   "Penalties"), t_att),
      pull(filter(att_table, technical ==  "Free Kicks"), t_att),
      pull(filter(att_table, technical ==    "Tackling"), t_att),
      pull(filter(att_table, technical ==   "Technique"), t_att),
      sep = "\n"),
    just = c("left", "top"),
    x = unit(x_reference_technical + x_increase, selected_unit),
    y = unit(y_reference - 1.5 * y_increase, selected_unit),
    gp = gpar(fontfamily = theme_font, col = "#000000", cex = cex))
  
  # Tactical Left Side
  grid.text(
    str_c(
      pull(filter(att_table, mental ==     "Aggresion"), mental),
      pull(filter(att_table, mental ==  "Anticipation"), mental),
      pull(filter(att_table, mental ==       "Bravery"), mental),
      pull(filter(att_table, mental ==     "Composure"), mental),
      pull(filter(att_table, mental == "Concentration"), mental),
      pull(filter(att_table, mental ==    "Creativity"), mental),
      pull(filter(att_table, mental ==     "Decisions"), mental),
      pull(filter(att_table, mental == "Determination"), mental),
      pull(filter(att_table, mental ==         "Flair"), mental),
      pull(filter(att_table, mental ==    "Leadership"), mental),
      pull(filter(att_table, mental ==  "Off The Ball"), mental),
      pull(filter(att_table, mental ==   "Positioning"), mental),
      pull(filter(att_table, mental ==      "Teamwork"), mental),
      pull(filter(att_table, mental ==     "Work Rate"), mental),
      sep = "\n"),
    just = c("left", "top"),
    x = unit(x_reference_tactical, selected_unit),
    y = unit(y_reference - 1.5 * y_increase, selected_unit),
    gp = gpar(fontfamily = theme_font, col = "#000000", cex = cex))
  
  # Tactical Right Side
  grid.text(
    str_c(
      pull(filter(att_table, mental ==     "Aggresion"), m_att),
      pull(filter(att_table, mental ==  "Anticipation"), m_att),
      pull(filter(att_table, mental ==       "Bravery"), m_att),
      pull(filter(att_table, mental ==     "Composure"), m_att),
      pull(filter(att_table, mental == "Concentration"), m_att),
      pull(filter(att_table, mental ==    "Creativity"), m_att),
      pull(filter(att_table, mental ==     "Decisions"), m_att),
      pull(filter(att_table, mental == "Determination"), m_att),
      pull(filter(att_table, mental ==         "Flair"), m_att),
      pull(filter(att_table, mental ==    "Leadership"), m_att),
      pull(filter(att_table, mental ==  "Off The Ball"), m_att),
      pull(filter(att_table, mental ==   "Positioning"), m_att),
      pull(filter(att_table, mental ==      "Teamwork"), m_att),
      pull(filter(att_table, mental ==     "Work Rate"), m_att),
      sep = "\n"),
    just = c("left", "top"),
    x = unit(x_reference_tactical + x_increase, selected_unit),
    y = unit(y_reference - 1.5 * y_increase, selected_unit),
    gp = gpar(fontfamily = theme_font, col = "#000000", cex = cex))
  
  # Physical Left Side
  grid.text(
    str_c(
      pull(filter(att_table, physical ==    "Acceleration"), physical),
      pull(filter(att_table, physical ==         "Agility"), physical),
      pull(filter(att_table, physical ==         "Balance"), physical),
      pull(filter(att_table, physical ==         "Jumping"), physical),
      pull(filter(att_table, physical == "Natural Fitness"), physical),
      pull(filter(att_table, physical ==            "Pace"), physical),
      pull(filter(att_table, physical ==         "Stamina"), physical),
      pull(filter(att_table, physical ==        "Strength"), physical),
      pull(filter(att_table, physical ==       "Left Foot"), physical),
      pull(filter(att_table, physical ==      "Right Foot"), physical),
      sep = "\n"),
    just = c("left", "top"),
    x = unit(x_reference_physical, selected_unit),
    y = unit(y_reference - 1.5 * y_increase, selected_unit),
    gp = gpar(fontfamily = theme_font, col = "#000000", cex = cex))
  
  # Right Side
  grid.text(
    str_c(
      pull(filter(att_table, physical ==    "Acceleration"), p_att),
      pull(filter(att_table, physical ==         "Agility"), p_att),
      pull(filter(att_table, physical ==         "Balance"), p_att),
      pull(filter(att_table, physical ==         "Jumping"), p_att),
      pull(filter(att_table, physical == "Natural Fitness"), p_att),
      pull(filter(att_table, physical ==            "Pace"), p_att),
      pull(filter(att_table, physical ==         "Stamina"), p_att),
      pull(filter(att_table, physical ==        "Strength"), p_att),
      pull(filter(att_table, physical ==       "Left Foot"), p_att),
      pull(filter(att_table, physical ==      "Right Foot"), p_att),
      sep = "\n"),
    just = c("left", "top"),
    x = unit(x_reference_physical + x_increase, selected_unit),
    y = unit(y_reference - 1.5 * y_increase, selected_unit),
    gp = gpar(fontfamily = theme_font, col = "#000000", cex = cex))
  
  # Personal Left Side
  grid.text(
    str_c(
      pull(filter(att_table, hidden ==      "Adaptability"), hidden),
      pull(filter(att_table, hidden ==          "Ambition"), hidden),
      pull(filter(att_table, hidden ==       "Consistency"), hidden),
      pull(filter(att_table, hidden ==       "Controversy"), hidden),
      pull(filter(att_table, hidden ==         "Dirtiness"), hidden),
      pull(filter(att_table, hidden == "Important Matches"), hidden),
      pull(filter(att_table, hidden ==  "Injury Proneness"), hidden),
      pull(filter(att_table, hidden ==           "Loyalty"), hidden),
      pull(filter(att_table, hidden ==          "Pressure"), hidden),
      pull(filter(att_table, hidden ==   "Professionalism"), hidden),
      pull(filter(att_table, hidden ==     "Sportsmanship"), hidden),
      pull(filter(att_table, hidden ==       "Temperament"), hidden),
      pull(filter(att_table, hidden ==       "Versatility"), hidden),
      sep = "\n"),
    just = c("left", "top"),
    x = unit(x_reference_personal, selected_unit),
    y = unit(y_reference - 1.5 * y_increase, selected_unit),
    gp = gpar(fontfamily = theme_font, col = "#000000", cex = cex))
  
  # Personal Right Side
  grid.text(
    str_c(
      pull(filter(att_table, hidden ==      "Adaptability"), h_att),
      pull(filter(att_table, hidden ==          "Ambition"), h_att),
      pull(filter(att_table, hidden ==       "Consistency"), h_att),
      pull(filter(att_table, hidden ==       "Controversy"), h_att),
      pull(filter(att_table, hidden ==         "Dirtiness"), h_att),
      pull(filter(att_table, hidden == "Important Matches"), h_att),
      pull(filter(att_table, hidden ==  "Injury Proneness"), h_att),
      pull(filter(att_table, hidden ==           "Loyalty"), h_att),
      pull(filter(att_table, hidden ==          "Pressure"), h_att),
      pull(filter(att_table, hidden ==   "Professionalism"), h_att),
      pull(filter(att_table, hidden ==     "Sportsmanship"), h_att),
      pull(filter(att_table, hidden ==       "Temperament"), h_att),
      pull(filter(att_table, hidden ==       "Versatility"), h_att),
      sep = "\n"),
    just = c("left", "top"),
    x = unit(x_reference_personal + x_increase, selected_unit),
    y = unit(y_reference - 1.5 * y_increase, selected_unit),
    gp = gpar(fontfamily = theme_font, col = "#000000", cex = cex))
  
  #-----Player Attributes Plot Viewport-----
  seekViewport("vp_player_att_plot")
  print(att_plot, vp = vp_player_att_plot)
  
  #-----Player Stats Viewport-----
  seekViewport("vp_player_stats")
  
  # Text References
  x_reference <- 0.5
  y_reference <- 0.5 * (height / layout_nrow) + 0.25
  
  # Text Format
  cex_title <- 2
  
  # Title
  grid.text("PLAYER STATS",
            x = unit(x_reference, selected_unit),
            y = unit(y_reference, selected_unit),
            just = c("left", "top"),
            gp = gpar(fontfamily = theme_font,
                      col = "#000000",
                      cex = cex_title))
  
  # The Table
  player_stats_grob <- tableGrob(
    player_stats,
    rows = NULL,
    theme = ttheme_default(
      base_size = 14,
      core = list(fg_params = list(hjust = 1, x = 0.9)),
      base_family = theme_font))
  
  #-----Club Stats Viewport-----
  seekViewport("vp_club_stats")
  
  # Text References
  x_reference <- 0.5
  y_reference <- 0.5 * (height / layout_nrow) + 0.25
  
  # Title
  grid.text("CLUB STATS",
            x = unit(x_reference, selected_unit),
            y = unit(y_reference, selected_unit),
            just = c("left", "top"),
            gp = gpar(fontfamily = theme_font,
                      col = "#000000",
                      cex = cex_title))
  
  # The Table
  club_stats_grob <- tableGrob(
    club_stats,
    rows = NULL,
    theme = ttheme_default(
      base_size = 14,
      core = list(fg_params = list(hjust = 1, x = 0.9)),
      base_family = theme_font))
  
  # Tables Cols Width
  max_width <- unit.pmax(player_stats_grob$widths,
                         club_stats_grob$widths)
  
  player_stats_grob$widths <- max_width
  club_stats_grob$widths <- max_width
  
  seekViewport("vp_player_stats")
  grid.draw(player_stats_grob)
  
  seekViewport("vp_club_stats")
  grid.draw(club_stats_grob)
  
  dev.off()
}
