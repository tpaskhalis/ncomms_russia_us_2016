#!/usr/bin/env Rscript
# coding: utf-8

#===============================================================================
# 
#  Gregory Eady, 2022-09-164
#
#  ggplot2 theme to simplify graphing
#
#===============================================================================

library("ggplot2")
library("grid")
library("gridExtra")

my.theme <- function(base_size = 10, base_family = "",
                     remove.ticks.x = FALSE,
                     remove.ticks.y = FALSE,
                     grid.x_colour = NA, grid.y_colour = NA,
                     grid.x_linetype = NA, grid.y_linetype = NA,
                     grid.x_size = NA, grid.y_size = NA,
                     strip_colour = NA, strip_text = "black",
                     background_colour = NA,
                     plot_background_colour = NA,
                     text_colour = NA,
                     tick_colour = "black", tick_length = 0.2,
                     borderless = 0, bordersize = 0.5){ 
    if((!is.na(grid.x_linetype) | !is.na(grid.x_size)) & is.na(grid.x_colour)) {
        grid.x_colour <- "black"
    }
    if((!is.na(grid.y_linetype) | !is.na(grid.y_size)) & is.na(grid.y_colour)) {
        grid.y_colour <- "black"
    }

  if(is.na(grid.x_size)) grid.x_size <- 0.25
  if(is.na(grid.x_linetype)) grid.x_linetype <- 1
  if(is.na(grid.y_size)) grid.y_size <- 0.25
  if(is.na(grid.y_linetype)) grid.y_linetype <- 1

  if(borderless == 2) {
    border <- theme(panel.border = element_blank(),
                    strip.background = element_blank())
  }
  
  else if(borderless == 1) {
    border <- theme(axis.line = element_line(colour = "black", size = 0.25),
                    panel.border = element_blank(),
                    # panel.background = element_blank(),
                    strip.background = element_blank())
  }
  else if(borderless == 0) border <- theme()
  if(remove.ticks.x == TRUE) border <- border + theme(axis.ticks.x = element_blank())
  if(remove.ticks.y == TRUE) border <- border + theme(axis.ticks.y = element_blank())
  theme(axis.text.x       = element_text(family = base_family, colour = "black", size = base_size, vjust = 1, lineheight = 0.9),
        axis.text.y       = element_text(family = base_family, colour = "black", size = base_size, hjust = 1, lineheight = 0.9),
        axis.ticks        = element_line(colour = tick_colour, size = 0.2),
        axis.ticks.length = unit(tick_length, "lines"),
        axis.title.x      = element_text(family = base_family, face = "bold", size = base_size*0.9, colour = "black", vjust = 0),
        axis.title.y      = element_text(family = base_family, face = "bold", size = base_size*0.9, angle = 90, colour = "black", vjust = 1),
        legend.background = element_blank(),#element_rect(fill = "grey95"),
        legend.key = element_blank(),
        legend.key.size = unit(0.6, "lines"),
        legend.text = element_text(family = base_family, size = base_size, color = "black", face = "plain", lineheight = 1),
        legend.text.align = -1, 
        legend.title = element_blank(),
        legend.title.align = 1,
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = c(0, 1),
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(fill = "transparent", colour = "black", size = bordersize),
        panel.grid.major.x = element_line(colour = grid.x_colour, size = grid.x_size, linetype = grid.x_linetype),
        panel.grid.major.y = element_line(colour = grid.y_colour, size = grid.y_size, linetype = grid.y_linetype),
        panel.grid.minor = element_blank(),
        panel.margin = unit(0.75, "lines"),
        strip.background = element_blank(),
        strip.text.x = element_text(family = base_family, size = base_size, face = "bold", colour = strip_text),
        strip.text.y = element_text(family = base_family, size = base_size, face = "plain", angle = -90, colour = strip_text),
        plot.background = element_rect(fill = NA, colour = NA),
        plot.title = element_text(family = base_family, size = base_size * 1.1, vjust = 0.5, hjust = 0, face = "bold")) +
    border
}

