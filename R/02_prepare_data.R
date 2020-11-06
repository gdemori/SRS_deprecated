# title: Prepare Data Function
# author: "De Pinto, M. & González Demori, A."
# date: "5/17/2020"
# objective: Function to prepare data to be used in the Infographics

#' @title prepare_data
#' @description Function to prepare data to be used in the Infographics
#' @param idb The Integrated Data Base (IDB)
#' @param diss_matrix Players' Dissimilatiry Matrix
#' @param poi Player of Interest Unique ID
#' @param theme_list Theme Data
#' @import dplyr
#' @import purrr
#' @import lubridate
#' @return list containing data needed to plot
#' @author Andrés E. González Demori

prepare_data <- function(idb, diss_matrix, poi, theme_list) {
  
  # Theme Data
  theme_colours <-  pluck(theme_list, "theme_colours")
  theme_font    <-  pluck(theme_list, "theme_font")
  
  #-----Add New Variables-----
  # Current Age by Sys.Date
  idb <- idb %>%
    mutate(fm_player_current_age = floor(
      time_length(Sys.Date() - fm_player_date_of_birth, "years"))) %>% 
    relocate(fm_player_current_age, .after = fm_player_age)
  
  #-----Get Dissimilarity Matrix-----
  # SELECTED PLAYER
  df <- diss_matrix %>% 
    select(unique_id, !!poi) %>% 
    rename(distance = 2)
  
  # Add PoI Main Data
  df <- df %>% 
    mutate(
      poi_season = pull(filter(idb, unique_id == poi), season),
      poi_name = pull(filter(idb, unique_id == poi), player_name_fm),
      poi_age = pull(filter(idb, unique_id == poi), fm_player_age),
      poi_current_age = pull(filter(idb, unique_id == poi), fm_player_current_age),
      poi_club = pull(filter(idb, unique_id == poi), club))
  
  # Join IDB with PoI Data
  idb <- inner_join(idb, df, by = "unique_id")
  
  #-----Charts-----
  chart1_data <- get_chart1_data(idb, theme_list)
  chart2_data <- get_chart2_data(idb, poi, theme_list)
  chart3_data <- get_chart3_data(idb, poi, theme_list)
  
  return(list(
    idb = idb,
    chart1_data = chart1_data,
    chart2_data = chart2_data,
    chart3_data = chart3_data))
}


#' @title get_chart1_data
#' @description Function to Get Chart 1 Data
#' @param idb The Integrated Data Base
#' @param theme_list Theme Data
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import purrr
#' @import lubridate
#' @import ggplot2
#' @return list containing data to be plotted
#' @author Andrés E. González Demori

get_chart1_data <- function(idb, theme_list) {
  
  # Theme Data
  theme_colours <-  pluck(theme_list, "theme_colours")
  theme_font    <-  pluck(theme_list, "theme_font")
  
  #-----Player's Personal Info-----
  personal_info <- get_personal_info(idb, poi)
  
  #-----Contract Information-----
  # Select Table Data
  contract_info <- idb %>% 
    select(
      unique_id,
      club_id, club, fm_player_squad_club, 
      fm_player_contract_end, fm_player_wage, fm_player_value, fm_player_squad_status) %>% 
    filter(unique_id == poi) %>% 
    select(-unique_id)
  
  # Transform & Rename Table Data
  contract_info <- contract_info %>%
    unite("club", c("club_id", "club"), sep = " ") %>% 
    mutate(
      fm_player_wage = 
        str_c(format(52 * fm_player_wage, big.mark = ",")," eur/year"),
      fm_player_value = 
        str_c("eur ", format(fm_player_value, big.mark = ","))
    ) %>% 
    mutate_all(as.character) %>%
    rename(
      Club = club,
      Squad = fm_player_squad_club,
      `Contract End` = fm_player_contract_end,
      Wage = fm_player_wage,
      Value = fm_player_value,
      `Squad Status` = fm_player_squad_status
    ) %>%
    rename_all(str_to_upper)
  
  # Create Table
  contract_info <- contract_info %>% 
    gather(contract_info, value)
  
  #-----PLayer Statistics-----
  # Select Table Data
  player_stats <- idb %>%
    select(
      unique_id,
      fs_player_minutes_played_overall,
      fs_player_appearances_overall,
      fs_player_min_per_match,
      fs_player_goals_overall,
      fs_player_min_per_goal_overall,
      fs_player_goals_involved_per_90_overall,
      fs_player_goals_per_90_overall,
      fs_player_assists_overall,
      fs_player_assists_per_90_overall,
      fs_player_min_per_assist_overall,
      fs_player_clean_sheets_overall,
      fs_player_conceded_overall,
      fs_player_min_per_conceded_overall,
      fs_player_yellow_cards_overall,
      fs_player_red_cards_overall,
      fs_player_min_per_card_overall
    ) %>%
    filter(unique_id == poi) %>% 
    select(-unique_id)
  
  # Rename Table Data & Create Table
  player_stats <- player_stats %>% 
    rename(
      Min       = fs_player_minutes_played_overall,
      App       = fs_player_appearances_overall,
      MinxMatch = fs_player_min_per_match,
      G         = fs_player_goals_overall,
      GxMin     = fs_player_min_per_goal_overall,
      GIx90     = fs_player_goals_involved_per_90_overall,
      Gx90      = fs_player_goals_per_90_overall,
      A         = fs_player_assists_overall,
      Ax90      = fs_player_assists_per_90_overall,
      AxMatch   = fs_player_min_per_assist_overall,
      CS        = fs_player_clean_sheets_overall,
      GC        = fs_player_conceded_overall,
      GCxMatch  = fs_player_min_per_conceded_overall,
      Y         = fs_player_yellow_cards_overall,
      R         = fs_player_red_cards_overall,
      MinxCard  = fs_player_min_per_card_overall
    )
  
  #-----Club Statistics-----
  # Select Table Data
  club_stats <- idb %>%
    select(
      unique_id,
      fs_club_league_position,
      fs_club_points_per_game,
      fs_club_wins,
      fs_club_draws,
      fs_club_losses,
      fs_club_goals_scored,
      fs_club_goals_conceded,
      fs_club_goal_difference,
      fs_club_minutes_per_goal_scored,
      fs_club_minutes_per_goal_conceded,
      fs_club_clean_sheets,
      fs_club_average_possession,
      fs_club_shots_on_target,
      fs_club_shots_off_target,
      fs_club_btts_count,
      fs_club_fts_count
    ) %>% 
    filter(unique_id == poi) %>% 
    select(-unique_id)
  
  # Rename Club Stats
  club_stats <- club_stats %>% 
    rename(
      Pos     = fs_club_league_position,
      PtsxG   = fs_club_points_per_game,
      W       = fs_club_wins,
      D       = fs_club_draws,
      L       = fs_club_losses,
      G       = fs_club_goals_scored,
      GC      = fs_club_goals_conceded,
      Diff    = fs_club_goal_difference,
      GxMin   = fs_club_minutes_per_goal_scored,
      GCxMin  = fs_club_minutes_per_goal_conceded,
      CS      = fs_club_clean_sheets,
      AvgPos  = fs_club_average_possession,
      SOnT    = fs_club_shots_on_target,
      SOffT   = fs_club_shots_off_target,
      BTTS    = fs_club_btts_count,
      FTS     = fs_club_fts_count
    )
  
  #-----Attributes Plot-----
  # Player's Attributes
  poi_att_list <- get_player_att(idb, poi)
  player_att           <- pluck(poi_att_list, "player_att")
  att_list             <- pluck(poi_att_list, "att_list")
  technical_att_table  <- pluck(att_list, "technical_att_table")
  mental_att_table     <- pluck(att_list, "mental_att_table")
  physical_att_table   <- pluck(att_list, "physical_att_table")
  hidden_att_table     <- pluck(att_list, "hidden_att_table")
  defensive_att_table  <- pluck(att_list, "defensive_att_table")
  attacking_att_table  <- pluck(att_list, "attacking_att_table")
  att_table            <- pluck(poi_att_list, "att_table")
  
  # Plot Data
  defending <- defensive_att_table %>% 
    summarize(avg = mean(as.numeric(d_att))) %>% 
    mutate(field = "Defending")
  
  physical <- physical_att_table %>% 
    summarize(avg = mean(as.numeric(p_att))) %>% 
    mutate(field = "Physical")
  
  speed <- physical_att_table %>% 
    filter(physical %in% c("Acceleration", "Agility", "Pace")) %>% 
    summarize(avg = mean(as.numeric(p_att))) %>% 
    mutate(field = "Speed")
  
  vision <- filter(mental_att_table, mental == "Creativity") %>% 
    select(m_att) %>% 
    rename(avg = m_att) %>% 
    mutate(field = "Vision", avg = as.numeric(avg))
  
  attacking <- attacking_att_table %>% 
    summarize(avg = mean(as.numeric(a_att))) %>% 
    mutate(field = "Attacking")
  
  technical <- technical_att_table %>% 
    filter(!technical %in% c("Long Throws", "Marking", "Tackling")) %>% 
    summarize(avg = mean(as.numeric(t_att))) %>% 
    mutate(field = "Technical")
  
  aerial <- filter(physical_att_table, physical == "Jumping") %>% 
    select(p_att) %>% 
    rename(avg = p_att) %>% 
    mutate(field = "Aerial", avg = as.numeric(avg))
  
  mental <- mental_att_table %>% 
    summarise(avg = mean(as.numeric(m_att))) %>% 
    mutate(field = "Mental")
  
  plot_data <- list(defending, physical, speed, vision, 
                    attacking,  technical, aerial, mental) %>%
    reduce(bind_rows) %>% 
    mutate(field = factor(field, c("Defending", "Physical", "Speed",
                                   "Vision", "Attacking", "Technical",
                                   "Aerial", "Mental")))
  
  # The Plot
  att_plot <- plot_data %>%
    ggplot(aes(field, avg, fill = avg)) + 
    geom_col() +
    geom_hline(yintercept = 20, linetype = "solid", alpha = 0.3) +
    geom_hline(yintercept = 10, linetype = "dotted", alpha = 0.7) +
    scale_fill_gradient(
      low = pull(filter(theme_colours, colour == "Light Gray"), code),
      high = pull(filter(theme_colours, colour == "King Blue"), code),
      aesthetics = "fill") +
    theme_minimal() +
    theme(
      text = element_text(family = theme_font),
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(
        family = theme_font, size = 16,
        color = pull(filter(theme_colours, colour == "Gray"), code),
        angle = seq(-20, -340, length.out = 8)),
      panel.grid.major.x = element_blank()
    ) +
    coord_polar()
  
  #-----Player Position Plot----
  
  # Add plot
  
  #----Output----
  output <- list(
    personal_info = personal_info, 
    contract_info = contract_info,
    att_table     = att_table,
    player_stats  = player_stats,
    club_stats    = club_stats,
    att_plot      = att_plot,
    att_list      = att_list
    # pos_plot      = pos_plot
  )
  
  return(output)
}


#' @title get_chart2_data
#' @description Function to Get Chart 2 Data
#' @param idb The Integrated Data Base
#' @param theme_list Theme Data
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import purrr
#' @import lubridate
#' @import ggplot2
#' @return list containing data to be plotted
#' @author Andrés E. González Demori

get_chart2_data <- function(idb, poi, theme_list) {
  
  # Theme Data
  theme_colours <-  pluck(theme_list, "theme_colours")
  theme_font    <-  pluck(theme_list, "theme_font")
  
  # Auxiliary Objects
  players_attributes <- get_players_attributes()
  gk_att        <- pluck(players_attributes, "gk_att")
  technical_att <- pluck(players_attributes, "technical_att")
  mental_att    <- pluck(players_attributes, "mental_att")
  physical_att  <- pluck(players_attributes, "physical_att")
  hidden_att    <- pluck(players_attributes, "hidden_att")
  defensive_att <- pluck(players_attributes, "defensive_att")
  attacking_att <- pluck(players_attributes, "attacking_att")
  other_att     <- pluck(players_attributes, "other_att")
  
  #-----Player's Personal Info-----
  personal_info <- get_personal_info(idb, poi)
  
  # PoI Dataframe
  poi_idb <- filter(idb, 
                    player_id == str_extract(poi, "(?<=^\\d{4}_\\d{4}_)\\d+$"))

  #-----Player Value Per Season-----
  # Data
  poi_idb <- poi_idb %>% 
    mutate(
      season = str_replace(season, "_", "-"),
      fm_player_value = round(fm_player_value / 1000000, 2),
      fm_player_best_rating = if_else(fm_player_best_rating > 100,
                                      100,
                                      fm_player_best_rating),
      fm_player_best_pot_rating = if_else(fm_player_best_rating > 100,
                                          100,
                                          fm_player_best_rating),
      poi_season = str_replace(poi_season, "_", "-"))
    
  # The Plot
  value_plot <- poi_idb %>% 
    ggplot(aes(season, fm_player_value, group = 1)) +
    geom_line() +
    geom_point() +
    geom_label(aes(label = str_c(fm_player_value, " M")),
              vjust = -1, 
              size = 3.25,
              family = theme_font,
              fill = "#f6f6f6") +
    geom_hline(yintercept = 0) +
    labs(title = "", subtitle = "") +
    annotate(geom = "text",
             x = seq_len(nrow(poi_idb)),
             y = - 0.02 * (1.05 * max(poi_idb$fm_player_value)), 
             label = str_c(poi_idb$fm_player_age, " yo"),
             size = 3.25,
             family = theme_font) +
    scale_y_continuous(name = "Euro (M)",
                       limits = c(- 0.02* (1.05 * max(poi_idb$fm_player_value)),
                                  (1.05 * max(poi_idb$fm_player_value)))) +
    theme_minimal() +
    theme(
      text = element_text(family = theme_font),
      legend.position = "bottom",
      axis.title.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(fill = NA),
    )
  
  #-----Player Best Rating & Position per Season Plot-----
  # Data
  rating_plot <- poi_idb %>%
    gather("fm_player_rating",
           "fm_player_rating_value",
           fm_player_best_rating:fm_player_best_pot_rating) %>% 
    mutate(fm_player_rating = if_else(
      fm_player_rating == "fm_player_best_rating",
      "Current",
      "Potential"))
  
  # The Plot
  # Best Role Legend Missing
  rating_plot <- rating_plot %>% 
    ggplot(aes(season, fm_player_rating_value, 
               fill = fm_player_best_position)) +
    geom_col(width = 0.5, alpha = 0.8) +
    scale_fill_brewer(direction = -1) +
    facet_grid(fm_player_rating ~ .) +
    geom_text(aes(label = str_c(fm_player_rating_value, "%\n (",
                                fm_player_best_role, ") "),
                  family = theme_font),
              vjust = -0.5,
              size = 3.25) +
    labs(title = "",
         subtitle = "",
         fill = "Best Position") +
    scale_y_continuous(name = "Rating (%)",
                       breaks = seq(0, 100, by = 10)) +
    coord_cartesian(ylim  = c(min(rating_plot$fm_player_rating_value) - 10,
                              max(rating_plot$fm_player_rating_value) + 7.5)) +
    theme_minimal() +
    theme(
      text = element_text(family = theme_font),
      legend.position = "bottom",
      axis.title.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(fill = NA),
      panel.background = element_rect(fill = "white"),
      strip.background = element_rect(fill = "white")
    )
  
  #-----Player Attributes Over Seasons-----
  # Change Variable to Select by Player's Position
  # Search which Attributes are more important for each position
  # Attributes by Season (Raw, with OCR Errors)
  att_by_season <- poi_idb %>% 
    select(season, !!attacking_att) %>% 
    gather("attribute", "value", -season) %>%
    spread(season, value) %>% 
    mutate(
      attribute = str_extract(attribute, "(?<=^fm_player_)\\w+"),
      attribute = str_to_title(str_replace_all(attribute, "_", " "))) %>% 
    rename(Attribute = attribute)
  
  #-----Player Spider Plots by Season-----
  # Spider Plots of Same Player (All Data Available)
  att_list             <- get_player_att_all_seasons(idb, poi)
  technical_att_table  <- pluck(att_list, "technical_att_table")
  mental_att_table     <- pluck(att_list, "mental_att_table")
  physical_att_table   <- pluck(att_list, "physical_att_table")
  hidden_att_table     <- pluck(att_list, "hidden_att_table")
  defensive_att_table  <- pluck(att_list, "defensive_att_table")
  attacking_att_table  <- pluck(att_list, "attacking_att_table")

  # Plot Data
  defending <- defensive_att_table %>%
    group_by(season) %>% 
    summarize(avg = mean(as.numeric(value))) %>% 
    mutate(field = "Defending")
  
  physical <- physical_att_table %>%
    group_by(season) %>%
    summarize(avg = mean(as.numeric(value))) %>% 
    mutate(field = "Physical")
  
  speed <- physical_att_table %>%
    filter(attribute %in% c("Acceleration", "Agility", "Pace")) %>%
    group_by(season) %>%
    summarize(avg = mean(as.numeric(value))) %>% 
    mutate(field = "Speed")
  
  vision <- mental_att_table %>% 
    filter(attribute == "Creativity") %>% 
    group_by(season) %>%
    select(season, value) %>% 
    rename(avg = value) %>% 
    mutate(field = "Vision", avg = as.numeric(avg))
  
  attacking <- attacking_att_table %>%
    group_by(season) %>% 
    summarize(avg = mean(as.numeric(value))) %>% 
    mutate(field = "Attacking")
  
  technical <- technical_att_table %>% 
    filter(!attribute %in% c("Long Throws", "Marking", "Tackling")) %>%
    group_by(season) %>% 
    summarize(avg = mean(as.numeric(value))) %>% 
    mutate(field = "Technical")
  
  aerial <- physical_att_table %>% 
    filter(attribute == "Jumping") %>% 
    group_by(season) %>% 
    select(season, value) %>% 
    rename(avg = value) %>% 
    mutate(field = "Aerial", avg = as.numeric(avg))
  
  mental <- mental_att_table %>%
    group_by(season) %>% 
    summarise(avg = mean(as.numeric(value))) %>% 
    mutate(field = "Mental")
  
  plot_data <- list(defending, physical, speed, vision, 
                    attacking,  technical, aerial, mental) %>%
    reduce(bind_rows) %>% 
    mutate(
      season = as.factor(str_replace(season, "_", "-")),
      field = factor(field, c("Defending", "Physical", "Speed",
                                   "Vision", "Attacking", "Technical",
                                   "Aerial", "Mental")))
  
  # The Plot
  att_plot <- plot_data %>%
    ggplot(aes(field, avg, fill = avg)) + 
    geom_col() +
    geom_hline(yintercept = 20, linetype = "solid", alpha = 0.3) +
    geom_hline(yintercept = 10, linetype = "dotted", alpha = 0.7) +
    facet_wrap(~ season, nrow = 2, ncol = 3) +
    scale_fill_gradient(
      low = pull(filter(theme_colours, colour == "Light Gray"), code),
      high = pull(filter(theme_colours, colour == "King Blue"), code),
      aesthetics = "fill") +
    theme_minimal() +
    theme(
      text = element_text(family = theme_font),
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "none",
      strip.text = element_text(size = 20),
      strip.background = element_rect(
        fill = pull(filter(theme_colours, colour == "Light Blue"), code)),
      panel.border = element_rect(fill = NA),
      axis.text.x = element_text(
        family = theme_font, size = 16,
        color = pull(filter(theme_colours, colour == "Gray"), code),
        angle = seq(-20, -340, length.out = 8)),
      panel.grid.major.x = element_blank(),
    ) +
    coord_polar()
  
  #-----Output-----
  return(list(
    personal_info   = personal_info,
    value_plot      = value_plot,
    rating_plot     = rating_plot,
    att_by_season   = att_by_season,
    att_plot        = att_plot))
}


#' @title get_chart3_data
#' @description Function to Get Chart 2 Data
#' @param idb The Integrated Data Base
#' @param poi Player of Interest Unique ID
#' @param theme_list Theme Data
#' @import dplyr
#' @import stringr
#' @import purrr
#' @import ggplot2
#' @return list containing data to be plotted
#' @author Andrés E. González Demori

get_chart3_data <- function(idb, poi, theme_list) {
  
  # Theme Data
  theme_colours <-  pluck(theme_list, "theme_colours")
  theme_font    <-  pluck(theme_list, "theme_font")
  
  #-----Player's Personal Info-----
  personal_info <- get_personal_info(idb, poi)
  
  #-----Top 10 Closests Players-----
  poi_id <- str_extract(poi, "(?<=_)\\d+$")
  is_under_21 <- if_else(unique(pull(idb, poi_age)) < 21, "Under 21", "Above 21")
  
  # PoI Data
  df_poi <- filter(idb, unique_id == !!poi)
  
  # Closests Players Data
  df <- idb %>% 
    arrange(distance) %>% 
    filter(player_id != !!poi_id)
  
  # If PoI Age Under 21
  if (is_under_21 == "Under 21") {
    
    under_21 <- df %>% 
      filter(fm_player_age > 21) %>% 
      slice(1:7)
    
    # Get Top 10
    df <- df %>% 
      filter(!unique_id %in% under_21$unique_id) %>% 
      slice(1:3)
    
    # Bind Data
    df_stats <- bind_rows(df_poi, under_21, df)
    df <- bind_rows(df_poi, under_21)
    
    # If PoI Age Above 21
  } else {
    
    above_21 <- df %>% 
      filter(fm_player_age <= 21) %>% 
      slice(1:7)
    
    # Get Top 10
    df <- df %>% 
      filter(!unique_id %in% above_21$unique_id) %>% 
      slice(1:3)
    
    # Bind Data
    df_stats <- bind_rows(df_poi, above_21, df)
    df <- bind_rows(df_poi, above_21)
  }
  
  #-----Main Player's Stats by Season-----
  stats_vars <- c(
    "fs_player_minutes_played_overall",
    "fs_player_goals_overall",
    "fs_player_goals_per_90_overall",
    "fs_player_assists_overall",
    "fs_player_assists_per_90_overall",
    "fs_player_clean_sheets_overall") %>% 
    tibble(stat = .) %>% 
    mutate(rank = 1:n())
  
  stats_by_season <- df_stats %>%
    mutate(
      season = str_replace(season, "_", "-"),
      club = str_to_title(str_replace_all(club, "_", " "))) %>% 
    select(season, player_name_fm, club, fm_player_age,
           fm_player_cur_a, fm_player_pot_a,
           fm_player_best_rating, fm_player_best_pot_rating,
           fm_player_best_role, fm_player_best_pot_role,
           pull(stats_vars, stat), distance) %>%
    arrange(distance) %>% 
    select(-distance) %>% 
    rename(
      Season       = season,
      Name         = player_name_fm,
      Club         = club,
      Age          = fm_player_age,
      `Cur A`      = fm_player_cur_a, 
      `Pot A`      = fm_player_pot_a,
      Rating       = fm_player_best_rating,
      `Pot Rating` = fm_player_best_pot_rating,
      Role         = fm_player_best_role,
      `Pot Role`   = fm_player_best_pot_role,
      Min          = fs_player_minutes_played_overall,
      G            = fs_player_goals_overall,
      Gx90         = fs_player_goals_per_90_overall,
      A            = fs_player_assists_overall,
      Ax90         = fs_player_assists_per_90_overall,
      CS           = fs_player_clean_sheets_overall
    )

  #-----Top 3 Bar Plot-----
  # Get Players Unique IDs & Attributes 
  my_players <- pull(df, unique_id)
  my_players_att_list <- pmap(list(idb = list(idb), poi = my_players),
                               get_player_att)
  
  my_players_att_list <- set_names(my_players_att_list, my_players)

  # Plot Data by Player
  my_players_att_plot_data <- map(my_players_att_list, att_plot_data)
  
  # Add Player Label
  my_players_att_plot_data <- modify2(
    my_players_att_plot_data, names(my_players_att_plot_data),
    ~ mutate(.x, unique_id = .y))
  
  # Bind Data
  plot_data <- my_players_att_plot_data %>% 
    bind_rows() %>% 
    left_join(select(df, season, player_name_fm, fm_player_age, unique_id),
              by = "unique_id")
  
  # The Plot
  att_plot <- plot_data %>%
    mutate(season = str_replace(season, "_", "-"),
           name = str_c(player_name_fm, "\n", season)) %>% 
    ggplot(aes(name, avg, fill = name)) + 
    geom_col(position = "dodge") +
    facet_wrap(~ as.factor(field), nrow = 2, ncol = 4, scales = "fixed") +
    geom_hline(yintercept = 20, linetype = "solid", alpha = 0.3) +
    geom_hline(yintercept = 10, linetype = "dotted", alpha = 0.7) +
    scale_fill_manual(values = c("#011f4b", "#03396c", "#005b96", "#6497b1")) +
    scale_y_continuous(name = NULL, 
                       breaks = c(1, 5, 10, 15, 20),
                       limits = c(0, 20)) +
    labs(title = "", subtitle = "", fill = NULL) + 
    theme_minimal() +
    theme(
      text = element_text(family = theme_font),
      axis.title = element_blank(),
      legend.position = "none",
      strip.background = element_rect(
        fill = pull(filter(theme_colours, colour == "Light Blue"), code)),
      strip.text = element_text(size = 20),
      panel.border = element_rect(fill = NA),
      axis.text.x = element_text(
        family = theme_font, size = 16,
        color = pull(filter(theme_colours, colour == "Gray"), code),
        angle = 90, vjust = 0.5, hjust = 1),
      axis.text.y = element_text(size = 16),
      panel.grid.major.x = element_blank()
    )
  
  #-----Output-----
  return(list(
    personal_info   = personal_info,
    stats_by_season = stats_by_season,
    att_plot        = att_plot))
}


#' @title get_players_attributes
#' @description Function to Get Players' Attributes Names
#' @return list containing data needed to plot
#' @author Andrés E. González Demori

get_players_attributes <- function() {
  
  gk_att <- c(
    "fm_player_gk_rating"       , "fm_player_gk_pot_rating"  ,
    "fm_player_aerial_ability"  , "fm_player_command_of_area", 
    "fm_player_communication"   , "fm_player_eccentricity"   ,
    "fm_player_handling"        , "fm_player_kicking"        , 
    "fm_player_one_on_ones"     , "fm_player_reflexes"       ,
    "fm_player_rushing_out"     , "fm_player_throwing"       , 
    "fm_player_tendency_to_punch")
  
  technical_att <- c(
    "fm_player_corners"  , "fm_player_crossing"   , "fm_player_dribbling"  ,
    "fm_player_finishing", "fm_player_first_touch", "fm_player_free_kicks" ,
    "fm_player_heading"  , "fm_player_long_shots" , "fm_player_long_throws",
    "fm_player_marking"  , "fm_player_passing"    , "fm_player_penalties"  ,
    "fm_player_tackling" , "fm_player_technique")
  
  mental_att <- c(
    "fm_player_aggresion"    , "fm_player_anticipation" , "fm_player_bravery"   ,
    "fm_player_composure"    , "fm_player_concentration", "fm_player_decisions" ,
    "fm_player_determination", "fm_player_flair"        , "fm_player_leadership", 
    "fm_player_off_the_ball" , "fm_player_positioning"  , "fm_player_teamwork"  ,
    "fm_player_creativity"   , "fm_player_work_rate")
  
  physical_att <- c(
    "fm_player_acceleration", "fm_player_agility"        , "fm_player_balance",
    "fm_player_jumping"     , "fm_player_natural_fitness", "fm_player_pace"   ,
    "fm_player_stamina"     , "fm_player_strength")
  
  hidden_att <- c(
    "fm_player_adaptability"    , "fm_player_consistency"      ,
    "fm_player_dirtiness"       , "fm_player_important_matches",
    "fm_player_injury_proneness", "fm_player_versatility"      , 
    "fm_player_temperament"     , "fm_player_sportsmanship"    ,
    "fm_player_professionalism" , "fm_player_pressure"         ,
    "fm_player_ambition"        , "fm_player_loyalty"          ,
    "fm_player_controversy")
  
  defensive_att <- c(
    "fm_player_acceleration", "fm_player_anticipation", "fm_player_heading",
    "fm_player_jumping"     , "fm_player_stamina"     , "fm_player_marking",
    "fm_player_pace"        , "fm_player_positioning" , "fm_player_strength",
    "fm_player_tackling")
  
  attacking_att <- c(
    "fm_player_crossing"    , "fm_player_dribbling" , "fm_player_free_kicks" ,
    "fm_player_finishing"   , "fm_player_creativity", "fm_player_first_touch",
    "fm_player_flair"       , "fm_player_long_shots", "fm_player_off_the_ball")
  
  other_att <- c("fm_player_right_foot", "fm_player_left_foot")
  
  # Output
  return(list(
    gk_att = gk_att, technical_att = technical_att, mental_att = mental_att,
    physical_att = physical_att, hidden_att = hidden_att,
    defensive_att = defensive_att, attacking_att = attacking_att,
    other_att = other_att))
}


#' @title get_personal_info
#' @description Function to Get Players' Basic Personal Info
#' @param idb Integrated DataBase
#' @param poi Player of Interest Unique Id
#' @import dplyr
#' @import tidyr
#' @return data.frame containing players' personal info
#' @author Andrés E. González Demori

get_personal_info <- function(idb, poi) {
  
  # Personal Info
  personal_info <- idb %>%
    select(
      unique_id, season,
      player_name_fm, player_nationality_1, player_nationality_2,
      fm_player_age, fm_player_current_age, fm_player_date_of_birth) %>%
    filter(unique_id == poi) %>%
    select(-unique_id)
  
  # Modify Season & Rename Table Data
  personal_info <- personal_info %>%
    unite(fm_player_age, c("fm_player_age", "fm_player_current_age"), sep = " / ") %>% 
    mutate(season = str_replace(season, "_", "-")) %>% 
    mutate_all(as.character) %>% 
    rename(
      Season = season,
      Name = player_name_fm,
      Nationality = player_nationality_1,
      `Other Nationality` = player_nationality_2,
      `Age / Current Age` = fm_player_age,
      `Date of Birth` = fm_player_date_of_birth
    ) %>%
    rename_all(str_to_upper)
  
  # Create Table
  personal_info <- personal_info %>% 
    gather(personal_info, value) %>% 
    filter(personal_info != "None")
  
  # Output
  return(personal_info)
}


#' @title get_players_attributes
#' @description Function to Get Players' Attributes Names
#' @param idb Integrated DataBase
#' @param poi Player of Interest
#' @import purrr
#' @import dplyr
#' @import tidyr
#' @return list containing data needed to plot
#' @author Andrés E. González Demori

get_player_att <- function(idb, poi) {
  
  # Auxiliary Objects
  players_attributes <- get_players_attributes()
  gk_att        <- pluck(players_attributes, "gk_att")
  technical_att <- pluck(players_attributes, "technical_att")
  mental_att    <- pluck(players_attributes, "mental_att")
  physical_att  <- pluck(players_attributes, "physical_att")
  hidden_att    <- pluck(players_attributes, "hidden_att")
  defensive_att <- pluck(players_attributes, "defensive_att")
  attacking_att <- pluck(players_attributes, "attacking_att")
  other_att     <- pluck(players_attributes, "other_att")
  
  # Select Table Data
  player_att <- idb %>% 
    select(
      unique_id,
      fm_player_acceleration:fm_player_tendency_to_punch,
      -gk_att) %>% 
    filter(unique_id == poi) %>% 
    select(-unique_id)
  
  # Create Labels for Tables and Spider Plot
  player_att <- player_att %>% 
    gather(attribute, value) %>% 
    mutate(
      category = case_when(
        attribute %in% technical_att ~ "Technical",
        attribute %in% mental_att ~ "Mental", 
        attribute %in% physical_att ~ "Physical",
        attribute %in% hidden_att ~ "Personality",
        attribute %in% other_att ~ "Preferred Foot"),
      type = case_when(
        attribute %in% technical_att ~ "Technical",
        attribute %in% mental_att ~ "Mental",
        attribute %in% physical_att ~ "Physical",
        attribute %in% hidden_att ~ "Personality",
        attribute %in% other_att ~ "Preferred Foot"),
      type = case_when(
        attribute %in% defensive_att ~ "Defensive",
        attribute %in% attacking_att ~ "Attacking",
        TRUE ~ type))
  
  # Rename Attributes
  player_att <- player_att %>% 
    mutate(attribute = case_when(
      attribute == "fm_player_acceleration"      ~ "Acceleration"     ,
      attribute == "fm_player_adaptability"      ~ "Adaptability"     ,
      attribute == "fm_player_aggresion"         ~ "Aggresion"        ,
      attribute == "fm_player_agility"           ~ "Agility"          ,
      attribute == "fm_player_ambition"          ~ "Ambition"         ,
      attribute == "fm_player_anticipation"      ~ "Anticipation"     ,
      attribute == "fm_player_balance"           ~ "Balance"          ,
      attribute == "fm_player_bravery"           ~ "Bravery"          ,
      attribute == "fm_player_composure"         ~ "Composure"        ,
      attribute == "fm_player_consistency"       ~ "Consistency"      ,
      attribute == "fm_player_concentration"     ~ "Concentration"    ,
      attribute == "fm_player_controversy"       ~ "Controversy"      ,
      attribute == "fm_player_corners"           ~ "Corners"          ,
      attribute == "fm_player_creativity"        ~ "Creativity"       ,
      attribute == "fm_player_crossing"          ~ "Crossing"         ,
      attribute == "fm_player_decisions"         ~ "Decisions"        ,
      attribute == "fm_player_determination"     ~ "Determination"    ,
      attribute == "fm_player_dirtiness"         ~ "Dirtiness"        ,
      attribute == "fm_player_dribbling"         ~ "Dribbling"        ,
      attribute == "fm_player_finishing"         ~ "Finishing"        ,
      attribute == "fm_player_first_touch"       ~ "First Touch"      ,
      attribute == "fm_player_flair"             ~ "Flair"            ,
      attribute == "fm_player_heading"           ~ "Heading"          ,
      attribute == "fm_player_important_matches" ~ "Important Matches",
      attribute == "fm_player_leadership"        ~ "Leadership"       ,
      attribute == "fm_player_injury_proneness"  ~ "Injury Proneness" ,
      attribute == "fm_player_jumping"           ~ "Jumping"          ,
      attribute == "fm_player_long_shots"        ~ "Long Shots"       ,
      attribute == "fm_player_long_throws"       ~ "Long Throws"      ,
      attribute == "fm_player_loyalty"           ~ "Loyalty"          ,
      attribute == "fm_player_marking"           ~ "Marking"          ,
      attribute == "fm_player_natural_fitness"   ~ "Natural Fitness"  ,
      attribute == "fm_player_off_the_ball"      ~ "Off The Ball"     ,
      attribute == "fm_player_pace"              ~ "Pace"             ,
      attribute == "fm_player_passing"           ~ "Passing"          ,
      attribute == "fm_player_penalties"         ~ "Penalties"        ,
      attribute == "fm_player_positioning"       ~ "Positioning"      ,
      attribute == "fm_player_pressure"          ~ "Pressure"         ,
      attribute == "fm_player_professionalism"   ~ "Professionalism"  ,
      attribute == "fm_player_free_kicks"        ~ "Free Kicks"       ,
      attribute == "fm_player_sportsmanship"     ~ "Sportsmanship"    ,
      attribute == "fm_player_stamina"           ~ "Stamina"          ,
      attribute == "fm_player_strength"          ~ "Strength"         ,
      attribute == "fm_player_tackling"          ~ "Tackling"         ,
      attribute == "fm_player_teamwork"          ~ "Teamwork"         ,
      attribute == "fm_player_technique"         ~ "Technique"        ,
      attribute == "fm_player_temperament"       ~ "Temperament"      ,
      attribute == "fm_player_versatility"       ~ "Versatility"      ,
      attribute == "fm_player_work_rate"         ~ "Work Rate"        ,
      attribute == "fm_player_left_foot"         ~ "Left Foot"        ,
      attribute == "fm_player_right_foot"        ~ "Right Foot"       ))
  
  # Tables Technical, Mental, Physical, Hidden, Defensive & Attacking
  technical_att_table <- player_att %>% 
    filter(category == "Technical") %>% 
    select(attribute, value) %>% 
    mutate(value = as.character(value)) %>% 
    rename(technical = attribute, t_att = value)
  
  mental_att_table <- player_att %>% 
    filter(category == "Mental") %>% 
    select(attribute, value) %>% 
    mutate(value = as.character(value)) %>% 
    rename(mental = attribute, m_att = value)
  
  physical_att_table <- player_att %>% 
    filter(category %in% c("Physical", "Preferred Foot")) %>% 
    select(attribute, value) %>% 
    mutate(value = as.character(value)) %>% 
    rename(physical = attribute, p_att = value)
  
  hidden_att_table <- player_att %>% 
    filter(category == "Personality") %>% 
    select(attribute, value) %>% 
    mutate(value = as.character(value)) %>% 
    rename(hidden = attribute, h_att = value)
  
  defensive_att_table <- player_att %>% 
    filter(type == "Defensive") %>% 
    select(attribute, value) %>% 
    mutate(value = as.character(value)) %>% 
    rename(defensive = attribute, d_att = value)
  
  attacking_att_table <- player_att %>% 
    filter(type == "Attacking") %>% 
    select(attribute, value) %>% 
    mutate(value = as.character(value)) %>% 
    rename(attacking = attribute, a_att = value)
  
  att_list <- list(technical_att_table = technical_att_table,
                   mental_att_table    = mental_att_table,
                   physical_att_table  = physical_att_table,
                   hidden_att_table    = hidden_att_table,
                   defensive_att_table = defensive_att_table,
                   attacking_att_table = attacking_att_table)
  
  att_table <- list(technical_att_table, mental_att_table, 
                    physical_att_table, hidden_att_table) %>% 
    modify(~ mutate(., id = 1:nrow(.))) %>% 
    reduce(left_join, by = "id") %>% 
    mutate_all(~ if_else(is.na(.), "", as.character(.))) %>% 
    select(-id)
  
  # Output
  return(list(
    player_att = player_att,
    att_list   = att_list,
    att_table  = att_table))
}


#' @title get_player_att_all_seasons
#' @description Function to Get Players' Attributes Names of All Seasons
#' @param idb Integrated DataBase
#' @param poi Player of Interest Unique Id
#' @import purrr
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @return tibble containing data needed to plot attributes by season
#' @author Andrés E. González Demori

get_player_att_all_seasons <- function(idb, poi) {
  
  # Auxiliary Objects
  players_attributes <- get_players_attributes()
  gk_att        <- pluck(players_attributes, "gk_att")
  technical_att <- pluck(players_attributes, "technical_att")
  mental_att    <- pluck(players_attributes, "mental_att")
  physical_att  <- pluck(players_attributes, "physical_att")
  hidden_att    <- pluck(players_attributes, "hidden_att")
  defensive_att <- pluck(players_attributes, "defensive_att")
  attacking_att <- pluck(players_attributes, "attacking_att")
  other_att     <- pluck(players_attributes, "other_att")
  
  # Select Table Data for All Seasons
  poi <- str_extract(poi, "(?<=^\\d{4}_\\d{4}_)\\d+")
  
  player_att <- idb %>% 
    select(
      unique_id, season,
      fm_player_acceleration:fm_player_tendency_to_punch,
      -gk_att) %>% 
    filter(str_detect(unique_id, str_c(poi, "$"))) %>% 
    select(-unique_id)
  
  # Create Labels for Tables and Spider Plot
  player_att <- player_att %>% 
    gather(attribute, value, -season) %>% 
    mutate(
      category = case_when(
        attribute %in% technical_att ~ "Technical",
        attribute %in% mental_att ~ "Mental", 
        attribute %in% physical_att ~ "Physical",
        attribute %in% hidden_att ~ "Personality",
        attribute %in% other_att ~ "Preferred Foot"),
      type = case_when(
        attribute %in% technical_att ~ "Technical",
        attribute %in% mental_att ~ "Mental",
        attribute %in% physical_att ~ "Physical",
        attribute %in% hidden_att ~ "Personality",
        attribute %in% other_att ~ "Preferred Foot"),
      type = case_when(
        attribute %in% defensive_att ~ "Defensive",
        attribute %in% attacking_att ~ "Attacking",
        TRUE ~ type))
  
  # Rename Attributes
  player_att <- player_att %>% 
    mutate(attribute = case_when(
      attribute == "fm_player_acceleration"      ~ "Acceleration"     ,
      attribute == "fm_player_adaptability"      ~ "Adaptability"     ,
      attribute == "fm_player_aggresion"         ~ "Aggresion"        ,
      attribute == "fm_player_agility"           ~ "Agility"          ,
      attribute == "fm_player_ambition"          ~ "Ambition"         ,
      attribute == "fm_player_anticipation"      ~ "Anticipation"     ,
      attribute == "fm_player_balance"           ~ "Balance"          ,
      attribute == "fm_player_bravery"           ~ "Bravery"          ,
      attribute == "fm_player_composure"         ~ "Composure"        ,
      attribute == "fm_player_consistency"       ~ "Consistency"      ,
      attribute == "fm_player_concentration"     ~ "Concentration"    ,
      attribute == "fm_player_controversy"       ~ "Controversy"      ,
      attribute == "fm_player_corners"           ~ "Corners"          ,
      attribute == "fm_player_creativity"        ~ "Creativity"       ,
      attribute == "fm_player_crossing"          ~ "Crossing"         ,
      attribute == "fm_player_decisions"         ~ "Decisions"        ,
      attribute == "fm_player_determination"     ~ "Determination"    ,
      attribute == "fm_player_dirtiness"         ~ "Dirtiness"        ,
      attribute == "fm_player_dribbling"         ~ "Dribbling"        ,
      attribute == "fm_player_finishing"         ~ "Finishing"        ,
      attribute == "fm_player_first_touch"       ~ "First Touch"      ,
      attribute == "fm_player_flair"             ~ "Flair"            ,
      attribute == "fm_player_heading"           ~ "Heading"          ,
      attribute == "fm_player_important_matches" ~ "Important Matches",
      attribute == "fm_player_leadership"        ~ "Leadership"       ,
      attribute == "fm_player_injury_proneness"  ~ "Injury Proneness" ,
      attribute == "fm_player_jumping"           ~ "Jumping"          ,
      attribute == "fm_player_long_shots"        ~ "Long Shots"       ,
      attribute == "fm_player_long_throws"       ~ "Long Throws"      ,
      attribute == "fm_player_loyalty"           ~ "Loyalty"          ,
      attribute == "fm_player_marking"           ~ "Marking"          ,
      attribute == "fm_player_natural_fitness"   ~ "Natural Fitness"  ,
      attribute == "fm_player_off_the_ball"      ~ "Off The Ball"     ,
      attribute == "fm_player_pace"              ~ "Pace"             ,
      attribute == "fm_player_passing"           ~ "Passing"          ,
      attribute == "fm_player_penalties"         ~ "Penalties"        ,
      attribute == "fm_player_positioning"       ~ "Positioning"      ,
      attribute == "fm_player_pressure"          ~ "Pressure"         ,
      attribute == "fm_player_professionalism"   ~ "Professionalism"  ,
      attribute == "fm_player_free_kicks"        ~ "Free Kicks"       ,
      attribute == "fm_player_sportsmanship"     ~ "Sportsmanship"    ,
      attribute == "fm_player_stamina"           ~ "Stamina"          ,
      attribute == "fm_player_strength"          ~ "Strength"         ,
      attribute == "fm_player_tackling"          ~ "Tackling"         ,
      attribute == "fm_player_teamwork"          ~ "Teamwork"         ,
      attribute == "fm_player_technique"         ~ "Technique"        ,
      attribute == "fm_player_temperament"       ~ "Temperament"      ,
      attribute == "fm_player_versatility"       ~ "Versatility"      ,
      attribute == "fm_player_work_rate"         ~ "Work Rate"        ,
      attribute == "fm_player_left_foot"         ~ "Left Foot"        ,
      attribute == "fm_player_right_foot"        ~ "Right Foot"       ))
  
  # Tables Technical, Mental, Physical, Hidden, Defensive & Attacking
  technical_att_table <- player_att %>% 
    filter(category == "Technical") %>% 
    select(season, attribute, value)
  
  mental_att_table <- player_att %>% 
    filter(category == "Mental") %>% 
    select(season, attribute, value)
  
  physical_att_table <- player_att %>% 
    filter(category %in% c("Physical", "Preferred Foot")) %>% 
    select(season, attribute, value)
  
  hidden_att_table <- player_att %>% 
    filter(category == "Personality") %>% 
    select(season, attribute, value)
  
  defensive_att_table <- player_att %>% 
    filter(type == "Defensive") %>% 
    select(season, attribute, value)
  
  attacking_att_table <- player_att %>% 
    filter(type == "Attacking") %>% 
    select(season, attribute, value)
  
  att_list <- list(technical_att_table = technical_att_table,
                    mental_att_table    = mental_att_table,
                    physical_att_table  = physical_att_table,
                    hidden_att_table    = hidden_att_table,
                    defensive_att_table = defensive_att_table,
                    attacking_att_table = attacking_att_table)
  
  # Output
  return(att_list)
}


#' @title att_plot_data
#' @description Function to Get Players' Attributes Data to be Plotted
#' @param player_att_list Player's Attribute List
#' @import purrr
#' @import dplyr
#' @return a list
#' @author Andrés E. González Demori

att_plot_data <- function(player_att_list) {

  # The Data
  player_att           <- pluck(player_att_list, "player_att")
  att_list             <- pluck(player_att_list, "att_list")
  technical_att_table  <- pluck(att_list, "technical_att_table")
  mental_att_table     <- pluck(att_list, "mental_att_table")
  physical_att_table   <- pluck(att_list, "physical_att_table")
  hidden_att_table     <- pluck(att_list, "hidden_att_table")
  defensive_att_table  <- pluck(att_list, "defensive_att_table")
  attacking_att_table  <- pluck(att_list, "attacking_att_table")
  att_table            <- pluck(player_att_list, "att_table")
  
  # Get each Field
  defending <- defensive_att_table %>% 
    summarize(avg = mean(as.numeric(d_att))) %>% 
    mutate(field = "Defending")
  
  physical <- physical_att_table %>% 
    summarize(avg = mean(as.numeric(p_att))) %>% 
    mutate(field = "Physical")
  
  speed <- physical_att_table %>% 
    filter(physical %in% c("Acceleration", "Agility", "Pace")) %>% 
    summarize(avg = mean(as.numeric(p_att))) %>% 
    mutate(field = "Speed")
  
  vision <- filter(mental_att_table, mental == "Creativity") %>% 
    select(m_att) %>% 
    rename(avg = m_att) %>% 
    mutate(field = "Vision", avg = as.numeric(avg))
  
  attacking <- attacking_att_table %>% 
    summarize(avg = mean(as.numeric(a_att))) %>% 
    mutate(field = "Attacking")
  
  technical <- technical_att_table %>% 
    filter(!technical %in% c("Long Throws", "Marking", "Tackling")) %>% 
    summarize(avg = mean(as.numeric(t_att))) %>% 
    mutate(field = "Technical")
  
  aerial <- filter(physical_att_table, physical == "Jumping") %>% 
    select(p_att) %>% 
    rename(avg = p_att) %>% 
    mutate(field = "Aerial", avg = as.numeric(avg))
  
  mental <- mental_att_table %>% 
    summarise(avg = mean(as.numeric(m_att))) %>% 
    mutate(field = "Mental")
  
  # Bind to Single Dataframe
  plot_data <- list(defending, physical, speed, vision, 
                    attacking,  technical, aerial, mental) %>%
    reduce(bind_rows) %>% 
    mutate(field = factor(field, c("Defending", "Physical", "Speed",
                                   "Vision", "Attacking", "Technical",
                                   "Aerial", "Mental")))
  
  # Output
  return(plot_data)
}
