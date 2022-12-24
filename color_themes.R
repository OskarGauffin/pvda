'# A function to return one of two custom colours, for use in plotting.
'# @params colour Must be either blue or green.


custom_colours <- function(colour = c("blue", "green")){

  colour ="green"
  checkmate::check_string(colour)
  checkmate::checkSubset(colour, c("green","blue"))

  umc_blue =
  umc_green =

  return_colour <- ifelse(colour == "blue", umc_blue, umc_green)

  return(return_colour)
}

gitcreds::gitcreds_set()
