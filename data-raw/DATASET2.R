# Create a dataset with Drug A, Event 1: obs = 4, exp_rrr = 1.1
tiny_dataset <- data.frame(report_id = 1:110,
           drug = paste0("Drug_", c(rep("A", 10), sample(toupper(letters), 100, replace=TRUE))),
           event = paste0("Event_", as.character(c(rep(1, 4), 2:8, rep(1, 3), 2:97))),
           group = sample(c("Male", "Female"), 110, replace = TRUE))
#  usethis::use_data(tiny_dataset, overwrite = TRUE)
