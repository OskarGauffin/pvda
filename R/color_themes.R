#' Get custom colours
#'
#' @description So far, this function provides hex codes for blue and green in
#' "Uppsala Monitoring Centre - style"
#'
#' @param colour A character string, must be either "blue" or "green".
#' @return A hex code as a string.
#'
#' @examples
#' par(lwd = 5)
#' hist(runif(30),
#'   col = pvutils:::custom_colours("blue"),
#'   border = pvutils:::custom_colours("green")
#' )
#'
custom_colours <- function(colour = c("blue", "green")) {
  checkmate::assert_string(colour)
  checkmate::assert_subset(colour, c("blue", "green"))

  umc_blue <- "#253962"
  umc_green <- "#69B4A7"

  ifelse(colour == "blue", umc_blue, umc_green)
}

#' Add custom colour theme
#'
#' @description This function provides a custom colour ggplot2-theme for blue
#' and green in "Uppsala Monitoring Centre - style"
#'
#' @return None
#'
#' @examples
#' library("ggplot2")
#' ggplot(mtcars, aes(mpg, disp)) +
#'   geom_point() +
#'   pvutils:::custom_ggtheme()
#'
custom_ggtheme <- function() {
  ggplot2::theme(
    panel.background = ggplot2::element_rect(
      fill = pvutils:::custom_colours("blue"),
      color = pvutils:::custom_colours("green")
    ),
    panel.grid.major = ggplot2::element_line(
      color = pvutils:::custom_colours("green"),
      size = 0.1,
      linetype = "dotted"
    ),
    panel.grid.minor = ggplot2::element_line(
      color = pvutils:::custom_colours("green"),
      size = 0.1
    )
  )
}

# Should perhaps use something like the below instead:
# theme_gray
# function (base_size = 11, base_family = "")
# {
#   half_line <- base_size/2
#   theme(
#     line = element_line(colour = "black", size = 0.5,
#                         linetype = 1, lineend = "butt"),
#     rect = element_rect(fill = "white", colour = "black",
#                         size = 0.5, linetype = 1),
#     text = element_text(family = base_family, face = "plain",
#                         colour = "black", size = base_size,
#                         lineheight = 0.9,  hjust = 0.5,
#                         vjust = 0.5, angle = 0,
#                         margin = margin(), debug = FALSE),
#
#     axis.line = element_blank(),
#     axis.text = element_text(size = rel(0.8), colour = "grey30"),
#     axis.text.x = element_text(margin = margin(t = 0.8*half_line/2),
#                                vjust = 1),
#     axis.text.y = element_text(margin = margin(r = 0.8*half_line/2),
#                                hjust = 1),
#     axis.ticks = element_line(colour = "grey20"),
#     axis.ticks.length = unit(half_line/2, "pt"),
#     axis.title.x = element_text(margin = margin(t = 0.8 * half_line,
#                                                 b = 0.8 * half_line/2)),
#     axis.title.y = element_text(angle = 90,
#                                 margin = margin(r = 0.8 * half_line,
#                                                 l = 0.8 * half_line/2)),
#
#     legend.background = element_rect(colour = NA),
#     legend.margin = unit(0.2, "cm"),
#     legend.key = element_rect(fill = "grey95", colour = "white"),
#     legend.key.size = unit(1.2, "lines"),
#     legend.key.height = NULL,
#     legend.key.width = NULL,
#     legend.text = element_text(size = rel(0.8)),
#     legend.text.align = NULL,
#     legend.title = element_text(hjust = 0),
#     legend.title.align = NULL,
#     legend.position = "right",
#     legend.direction = NULL,
#     legend.justification = "center",
#     legend.box = NULL,
#
#     panel.background = element_rect(fill = "grey92", colour = NA),
#     panel.border = element_blank(),
#     panel.grid.major = element_line(colour = "white"),
#     panel.grid.minor = element_line(colour = "white", size = 0.25),
#     panel.margin = unit(half_line, "pt"), panel.margin.x = NULL,
#     panel.margin.y = NULL, panel.ontop = FALSE,
#
#     strip.background = element_rect(fill = "grey85", colour = NA),
#     strip.text = element_text(colour = "grey10", size = rel(0.8)),
#     strip.text.x = element_text(margin = margin(t = half_line,
#                                                 b = half_line)),
#     strip.text.y = element_text(angle = -90,
#                                 margin = margin(l = half_line,
#                                                 r = half_line)),
#     strip.switch.pad.grid = unit(0.1, "cm"),
#     strip.switch.pad.wrap = unit(0.1, "cm"),
#
#     plot.background = element_rect(colour = "white"),
#     plot.title = element_text(size = rel(1.2),
#                               margin = margin(b = half_line * 1.2)),
#     plot.margin = margin(half_line, half_line, half_line, half_line),
#     complete = TRUE)
# }
