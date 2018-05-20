#' Get variable names from the NCES's Common Core of Data for year 2009 to 2016
#'
#'
#' @param endyear A numeric year indicated the end year of a school year (e.g., 2016 to get 2015-2016 data).
#' @param View Do you want to View the data frame in the R data viewer
#' @return A tibble with variable names, descriptions, and miscellaneous information
#'
#' @import dplyr stringr purrr
#' @importFrom utils download.file
#' @importFrom janitor clean_names
#' @importFrom readxl read_excel
#' @importFrom rvest html_nodes html_attr
#' @importFrom xml2 read_html
#' @importFrom utils read.csv
#' @export
#'
#' @examples
#' # Get variables from all schools in New York state for the 2014-2015 school year.
#' library(ccdr)
#' ny_vars <- get_ccd_vars(endyear = 2015, View = F)

get_ccd_vars <- function(endyear = 2016, View = F) {

  if (endyear < 2008 | endyear > 2016) {
    stop("WARNING: `endyear` must be between 2009 and 2016")
  }

  message(paste0("getting layout for school year: ", endyear-1, "-", endyear))
  if (endyear <= 2014) {
    if (between(endyear, 2012, 2014)) {
      y <- paste0("sc", str_sub(endyear-1, -2))

      ccd_urls <-
        xml2::read_html("https://nces.ed.gov/ccd/pubschuniv.asp") %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href") %>%
        str_subset(paste0("(?=.*", y, ")(.*lay)")) %>%
        paste0("https://nces.ed.gov/ccd/", .)
    } else {
      y <- paste0("psu", str_sub(endyear-1, -2))

      ccd_urls <-
        xml2::read_html("https://nces.ed.gov/ccd/pubschuniv.asp") %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href") %>%
        str_subset(paste0("(?=.*", y, ")(.*lay)")) %>%
        paste0("https://nces.ed.gov/ccd/", .)
    }
    con <- url(ccd_urls)
    vars <- readLines(con)
    # filtering out the front matter
    vars <- vars[-c(1:(which(str_detect(vars, "^NCESSCH"))-1))] %>%
      subset(str_detect(., "\\S")) %>%
      subset(!str_detect(., "^(\\s+)")) %>%
      map(~ scan(text =.,
                 what = "character",
                 quote = "")) %>%
      subset(!map_lgl(., ~ str_detect(.[1], "^[0-9]"))) %>%
      subset(!map_lgl(., ~ str_detect(.[1], "NOTE"))) %>%
      map(~ str_replace(., ",", "")) %>%
      subset(map_lgl(.,
                     ~ str_detect(.[1], "^[\\S]{1}[A-Z?0-9][^:]")
      )) %>%
      map(~ paste(.[1],
                  paste(.[4:length(.)],
                        collapse = " "),
                  sep = ",")) %>%
      paste(collapse = "\n") %>%
      read.csv(text = ., header = F,
               col.names = c("variable_name", "description"),
               stringsAsFactors = F) %>%
      mutate(variable_name = str_remove(variable_name, "\\+")) %>%
      mutate(variable_name = str_remove(variable_name, "[\\d]{2}$"))

    close(con)

    if (View == T) {
      View(vars)
    }

    vars

  } else {
    y <- paste(str_sub(endyear-1, -2), str_sub(endyear, -2), sep = "-")

    ccd_urls <-
      xml2::read_html("https://nces.ed.gov/ccd/pubschuniv.asp") %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href") %>%
      str_subset(paste0("(.*", y, ")(.*lay|.*Lay)")) %>%
      paste0("https://nces.ed.gov/ccd/", .)

    vars <- map(ccd_urls, ~ {
      tmp_dir <- tempdir()
      tf <- tempfile(tmpdir = tmp_dir, fileext = ".xlsx")
      download.file(URLencode(.), tf, mode="wb")

      vars <- readxl::read_excel(tf) %>%
        janitor::clean_names() %>%
        mutate(variable_name = str_remove(variable_name, "\\+"))
    })

    unlink(tmp_dir)

    vars <- bind_rows(vars)

    vars <- vars %>%
      select(variable_name, prior_year_name,
             description, variable_type,
             change_to_categorical_values_flag:variables_used_to_derive_field)

    if (View == T) {
      View(vars)
    }
    vars
  }
}
