# get vars from CCD
get_ccd_vars <- function(endyear = 2016, View = F) {
  
  if (endyear < 2008 | endyear > 2016) {
    stop("WARNING: `endyear` must be between 2009 and 2016")
  }
  
  message(paste0("getting layout for school year: ", endyear-1, "-", endyear))
  if (endyear <= 2013) {
    if (between(endyear, 2012, 2014)) {
      y <- paste0("sc", str_sub(endyear-1, -2))
      
      ccd_urls <- 
        read_html("https://nces.ed.gov/ccd/pubschuniv.asp") %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        str_subset(paste0("(?=.*", y, ")(.*lay)")) %>% 
        paste0("https://nces.ed.gov/ccd/", .)
      
      tmp_dir <- tempdir()
      tf <- tempfile(tmpdir = tmp_dir, fileext = ".txt")
      download.file(ccd_urls, tf) 
    } else {
      y <- paste0("psu", str_sub(endyear-1, -2))
      
      ccd_urls <- 
        read_html("https://nces.ed.gov/ccd/pubschuniv.asp") %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        str_subset(paste0("(?=.*", y, ")(.*lay)")) %>% 
        paste0("https://nces.ed.gov/ccd/", .)
      
      tmp_dir <- tempdir()
      tf <- tempfile(tmpdir = tmp_dir, fileext = ".txt")
      download.file(ccd_urls, tf)
    }
    vars <- read_lines(tf) 
    # filtering out the front matter
    vars <- vars[-c(1:which(str_detect(vars, "^NCESSCH"))-1)] %>% 
      subset(str_detect(., "\\S")) %>% 
      map(~ scan(text =., what = "character", encoding = "latin1")) %>% 
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
      read_csv(col_names = c("variable", "description"))
    
    if (View == T) {
      View(vars)
    }
    
    vars
    
  } else {
    y <- paste(str_sub(endyear-1, -2), str_sub(endyear, -2), sep = "-") 
    
    ccd_urls <- 
      read_html("https://nces.ed.gov/ccd/pubschuniv.asp") %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      str_subset(paste0("(.*", y, ")(\\S+)(.*lay|.*Lay)")) %>% 
      paste0("https://nces.ed.gov/ccd/", .)
    
    vars <- map(ccd_urls, ~ {
      tmp_dir <- tempdir()
      tf <- tempfile(tmpdir = tmp_dir, fileext = ".xlsx")
      download.file(., tf)
      
      vars <- readxl::read_excel(tf) %>% 
        janitor::clean_names()
    })
    
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