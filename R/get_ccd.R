get_ccd <- function(states = "all", endyear = 2016,
                    variables = "all") {
  
  if (endyear < 2008 | endyear > 2016) {
    stop("WARNING: `endyear` must be between 2009 and 2016")
  }
  
  message(paste0("getting school year: ", endyear, "-", endyear+1))
  message(paste0("getting variables: ", variables))
  
  if (endyear <= 2013) {
    y <- paste0("sc", str_sub(endyear, -2))
  } else {
    y <- paste0(str_sub(endyear-1, -2), str_sub(endyear, -2)) 
  }
  
  ccd_urls <- read_html("https://nces.ed.gov/ccd/pubschuniv.asp") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_subset(paste0("(?=.*", y, ")(.*txt|.*csv)(.zip)")) %>%
    .[!str_detect(., "GEOID")] %>% 
    paste0("https://nces.ed.gov/ccd/", .)
  
  ccd_dat <- map(ccd_urls, ~ {
    # download zip file for CCD csv
    tmp_dir <- tempdir()
    tf <- tempfile(tmpdir = tmp_dir, fileext = ".zip")
    download.file(., tf)
    # unzip into temporary directory
    fname <- unzip(tf, list = TRUE)$Name
    unzip(tf, files = fname, exdir = tmp_dir, overwrite = TRUE)
    fpath <- file.path(tmp_dir, fname)
    
    if (str_detect(fname, ".txt")) {
      message("tab-delimited file; using read.delim")
      datf <- read.delim(fpath, stringsAsFactors = F) %>%
        as_tibble() %>% 
        janitor::clean_names()
      
    } else {
      message("comma-delimited file; using read_csv")
      datf <- read.csv(fpath, stringsAsFactors = F) %>%
        as_tibble() %>% 
        janitor::clean_names()
    }
    
    if (states != "all") {
      if ("statename" %in% names(datf)) {
        datf <- datf %>%
          filter(statename %in% toupper(states))
      } else {
        states <- state.abb[state.name %in% states]
        datf <- datf %>%
          filter(mstate %in% states)
      }
    } 
  })
  
  unlink(fpath)
  unlink(tf)
  
  if (length(ccd_dat) > 1){
    ccd_dat <- reduce(
      ccd_dat, full_join,
      by = c(reduce(map(ccd_dat, names), intersect))
    )
  } else {
    ccd_dat <- ccd_dat[[1]]
  }
  
  if (variables != "all") {
    ccd_dat <- 
      ccd_dat %>% 
      select(endyear,
             contains("schid"),
             one_of(variables))
  }
  
  ccd_dat
  }