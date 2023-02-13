#' @title Write to excel
#' @description Writes output from a disproportionality analysis to an excel file
#' @param df The data frame to export. See `?da` for details.
#' @param write_path A string giving the file path
#' @return Nothing.
#' @export

write_to_excel <- function(df, write_path = NULL) {

  if(!is.null(write_path)){

    write_path_w_file <- paste0(write_path, "/da.xlsx")

    writexl::write_xlsx(df,
                        path = write_path_w_file,
                        col_names = TRUE,
                        format_headers = TRUE)

    cat(paste0("Excel file was written to '", write_path_w_file, "'"))
  }
}
