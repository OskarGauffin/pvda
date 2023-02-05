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
#'   col = pvutils::custom_colours("blue"),
#'   border = pvutils::custom_colours("green")
#' )
#'
#' @export
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
#'   pvutils::custom_ggtheme()
#' @export
custom_ggtheme <- function() {
  ggplot2::theme(
    panel.background = ggplot2::element_rect(
      fill = custom_colours("blue"),
      color = custom_colours("green")
    ),
    panel.grid.major = ggplot2::element_line(
      color = custom_colours("green"),
      size = 0.1,
      linetype = "dotted"
    ),
    panel.grid.minor = ggplot2::element_line(
      color = custom_colours("green"),
      size = 0.1
    )
  )
}
