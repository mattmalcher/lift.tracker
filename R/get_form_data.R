#' Get Form Data
#'
#' @param form_url - the share link for the sheet which the google form drops
#'   data into
#'
#' @return a dataframe of lift breakage data
#' @export
#'
#' @examples
get_form_data <- function(form_url) {
  sheet_data <- googlesheets4::range_read(ss = form_url)

  names(sheet_data) <- c("form_time", "category", "when", "what", "how", "who")

  sheet_data$when <- as.Date(sheet_data$when)

  sheet_data
}
