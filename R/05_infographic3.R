# title: Infographic 3
# author: "De Pinto, M. & González Demori, A."
# date: "5/17/2020"
# objective: Function to Create Infographic 2

#' @title infographic3
#' @description Function to create Infographic3
#' @param chart3_data The Prepared Data
#' @param poi Player of Interest
#' @param theme_list Theme Data
#' @param output_directory Output Directory
#' @import purrr
#' @import dplyr
#' @import stringr
#' @import grid
#' @import gridExtra
#' @import gtable
#' @return Player of Interest Infographic3
#' @author Andrés E. González Demori

infographic3 <- function(chart3_data, poi, theme_list, output_directory) {
  
  print("Generating Infographic 3")
  
  # The Data
  personal_info   <- pluck(chart3_data, "personal_info")
  stats_by_season <- pluck(chart3_data, "stats_by_season")
  att_plot        <- pluck(chart3_data, "att_plot")
  
  # Theme Data
  theme_colours <-  pluck(theme_list, "theme_colours")
  theme_font    <-  pluck(theme_list, "theme_font")

  # Chart Dimensions
  width <- 40
  height <- 35
  selected_unit <- "cm"
  
  # Main Layout Dimensions
  layout_nrow <- 6
  layout_ncol <- 1
  
  # File
  file_name <- str_c("Infographic3_", poi, ".png")
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
      rep(height / layout_nrow, layout_nrow),
      rep(selected_unit, layout_nrow)))
  
  # grid.show.layout(vp_main_layout)
  
  # Viewports
  # Main Viewport
  vp_main <- viewport(name = "vp_main", layout = vp_main_layout)
  
  # Player Name Viewport
  vp_player_name <- viewport(layout.pos.row = 1, 
                             layout.pos.col = 1, 
                             name = "vp_player_name")
  
  # Player Rating Plot
  vp_stats_by_season <- viewport(layout.pos.row = 2:3,
                                 layout.pos.col = 1, 
                                 name = "vp_stats_by_season")
  
  # Attributes by Season
  vp_att_plot <- viewport(layout.pos.row = 4:6,
                          layout.pos.col = 1, 
                          name = "vp_att_plot")
  
  # Viewport Tree
  vp_chart3 <- vpTree(vp_main, vpList(vp_player_name, 
                                      vp_stats_by_season,
                                      vp_att_plot))
  
  # Push Chart 1 Viewport
  pushViewport(vp_chart3)
  
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
  
  #-----Players Stats by Season-----
  seekViewport("vp_stats_by_season")
  grid.rect(gp = gpar(fill = "#CCCCCC", col = "#000000"))
  
  # Text References
  x_reference <- 0.5
  y_reference <- 2 * (height / layout_nrow) - 0.25
  
  # Text Format
  cex_title <- 2
  
  # Title
  grid.text("PLAYERS STATS BY SEASON",
            x = unit(x_reference, selected_unit),
            y = unit(y_reference, selected_unit),
            just = c("left", "top"),
            gp = gpar(fontfamily = theme_font,
                      col = "#000000",
                      cex = cex_title))
  
  # The Table
  fill_col <- rep_len(
    c(pull(filter(theme_colours, colour == "Light Gray 1"), code),
      pull(filter(theme_colours, colour == "Light Gray 2"), code)),
    nrow(stats_by_season))
  
  stats_by_season_grob <- tableGrob(
    stats_by_season,
    rows = NULL,
    theme = ttheme_default(
      base_size = 14,
      core = list(
        bg_params = list(fill = fill_col),
        fg_params = list(hjust = 1, x = 0.9)),
      colhead = list(
        bg_params = list(
          fill = pull(filter(theme_colours, colour == "Light Blue"), code))),
      base_family = theme_font))
  
  # Add Border
  stats_by_season_grob <- gtable_add_grob(
    stats_by_season_grob,
    grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
    t = 2, b = nrow(stats_by_season_grob),
    l = 1, r = ncol(stats_by_season_grob))
  
  # Draw
  grid.draw(stats_by_season_grob)
  
  #-----Player Attributes Comparison-----
  seekViewport("vp_att_plot")
  
  # Text References
  x_reference <- 0.5
  y_reference <- 3 * (height / layout_nrow) - 0.25
  
  # Text Format
  cex_title <- 2
  
  # Title
  grid.text("ATTRIBUTES COMPARISON",
            x = unit(x_reference, selected_unit),
            y = unit(y_reference, selected_unit),
            just = c("left", "top"),
            gp = gpar(fontfamily = theme_font,
                      col = "#000000",
                      cex = cex_title))
  
  
  print(att_plot, vp = vp_att_plot)
  
  dev.off()
}

