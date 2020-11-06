# title: Infographic 2
# author: "De Pinto, M. & González Demori, A."
# date: "5/17/2020"
# objective: Function to Create Infographic 2

#' @title infographic2
#' @description Function to create Infographic2
#' @param chart2_data The Prepared Data
#' @param poi Player of Interest
#' @param theme_list Theme Data
#' @param output_directory Output Directory
#' @import purrr
#' @import dplyr
#' @import stringr
#' @import grid
#' @import gridExtra
#' @import gtable
#' @return Player of Interest Infographic2
#' @author Andrés E. González Demori

infographic2 <- function(chart2_data, poi, theme_list, output_directory) {
  
  print("Generating Infographic 2")
  
  # The Data
  personal_info   <- pluck(chart2_data, "personal_info")
  rating_plot     <- pluck(chart2_data, "rating_plot")
  value_plot      <- pluck(chart2_data, "value_plot")
  att_by_season   <- pluck(chart2_data, "att_by_season")
  att_plot        <- pluck(chart2_data, "att_plot")
  
  # Theme Data
  theme_colours <-  pluck(theme_list, "theme_colours")
  theme_font    <-  pluck(theme_list, "theme_font")
  
  # Chart Dimensions
  width <- 40
  height <- 35
  selected_unit <- "cm"
  
  # Main Layout Dimensions
  layout_nrow <- 10
  layout_ncol <- 6
  
  # File
  file_name <- str_c("Infographic2_", poi, ".png")
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
                             layout.pos.col = 1:6, 
                             name = "vp_player_name")
  
  # Player Rating Plot
  vp_rating_plot <- viewport(layout.pos.row = 2:5,
                             layout.pos.col = 1:3, 
                             name = "vp_rating_plot")
  
  # Attributes by Season
  vp_att_by_season <- viewport(layout.pos.row = 2:5,
                               layout.pos.col = 4:6, 
                               name = "vp_att_by_season",
                               just = "top")

  # Attributes Plots
  vp_att_plots <- viewport(layout.pos.row = 6:10,
                           layout.pos.col = 3:6, 
                           name = "vp_att_plots")
  
  # Value Plot
  vp_value_plot <- viewport(layout.pos.row = 6:10,
                            layout.pos.col = 1:2, 
                            name = "vp_value_plot")
  
  # Viewport Tree
  vp_chart2 <- vpTree(vp_main, vpList(vp_player_name, 
                                      vp_rating_plot,
                                      vp_att_by_season,
                                      vp_att_plots,
                                      vp_value_plot))
  
  # Push Chart 1 Viewport
  pushViewport(vp_chart2)
  
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
  
  #-----Player Rating Chart-----
  seekViewport("vp_rating_plot")
  grid.rect(gp = gpar(fill = "#CCCCCC", col = "#000000"))
  
  # Text References
  x_reference <- 0.5
  y_reference <- 4 * (height / layout_nrow) - 0.25
  
  # Text Format
  cex_title <- 2
  
  # Title
  grid.text("RATING BY SEASON",
            x = unit(x_reference, selected_unit),
            y = unit(y_reference, selected_unit),
            just = c("left", "top"),
            gp = gpar(fontfamily = theme_font,
                      col = "#000000",
                      cex = cex_title))
  
  # Plot
  print(rating_plot, vp = vp_rating_plot)
  
  #-----Player Attributes by Season-----
  seekViewport("vp_att_by_season")
  
  #  Background
  grid.rect(gp = gpar(fill = "#CCCCCC", col = "#000000"))
  
  # Text References
  x_reference <- 0.5
  y_reference <- 4 * (height / layout_nrow) - 0.25
  
  # Text Format
  cex_title <- 2
  
  # Title
  grid.text("ATTRIBUTES BY SEASON",
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
    nrow(att_by_season))
  
  att_by_season_grob <- tableGrob(
    att_by_season,
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
  att_by_season_grob <- gtable_add_grob(
    att_by_season_grob,
    grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
    t = 2, b = nrow(att_by_season_grob), l = 1, r = ncol(att_by_season_grob))
  
  # Draw
  grid.draw(att_by_season_grob)
  
  #-----Player Attributes Charts-----
  seekViewport("vp_att_plots")
  print(att_plot, vp = vp_att_plots)
  
  #-----Player Attributes Charts-----
  seekViewport("vp_value_plot")
  
  # Text References
  x_reference <- 0.5
  y_reference <- 5 * (height / layout_nrow) - 0.25
  
  # Text Format
  cex_title <- 2
  
  # Title
  grid.text("VALUE BY SEASON",
            x = unit(x_reference, selected_unit),
            y = unit(y_reference, selected_unit),
            just = c("left", "top"),
            gp = gpar(fontfamily = theme_font,
                      col = "#000000",
                      cex = cex_title))
  
  # Plot
  print(value_plot, vp = vp_value_plot)
  
  dev.off()
}
