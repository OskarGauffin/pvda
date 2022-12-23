#' Get custom colours
#'
#' So far, this function provides hex codes for blue and green in "Uppsala Monitoring Centre - style"
#'
#' @param colour A character string, must be either "blue" or "green".
#' @return A hex code as a string.
#'
#' @examples
#' par(lwd=5)
#' hist(runif(30), col=custom_colours("blue"), border=custom_colours("green"))

custom_colours <- function(colour = c("blue", "green")){

  checkmate::assert_string(colour)
  checkmate::assert_subset(colour, c("blue", "green"))

  umc_blue = "#253962"
  umc_green = "#69B4A7"

  ifelse(colour == "blue", umc_blue, umc_green)
}

custom_ggtheme <- function(){
theme(panel.background = element_rect(fill = custom_colours("blue"), color = custom_colours("green")),
      panel.grid.major = element_line(color = custom_colours("green"), size=0.1, linetype = 'dotted'),
      panel.grid.minor = element_line(color = custom_colours("green"), size = 0.1))
}

library("ggplot2")
ggplot2::ggplot(mtcars, aes(mpg, disp)) + geom_point() + custom_ggtheme()
