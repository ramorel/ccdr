# ccdr - A tiny package for accessing the Common Core of Data

The [Common Core of Data](https://nces.ed.gov/ccd/) is a comprehensive database containing administrative and demographic data on all public K-12 schools in the United States.  
(Currently) consists of two functions: `get_ccd` to get school-level data and `get_ccd_vars` to get the names of the variables in the datasets and their descriptions. Maybe I will expand this to include options for accessing district-level and state-level files. For now, I need school-level data, so that's what I built.

There is no API for the CCD right now (maybe some day?). The `get_ccd` function downloads the relevant flat files from the [CCD's website](https://nces.ed.gov/ccd/pubschuniv.asp). This is less than ideal, but alas. The files are big and they take a while to download. There's no way aroud that at this point. 

![](https://github.com/ramorel/ccdr/blob/master/img/452601.jpgs)
