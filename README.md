<img src="https://github.com/user-attachments/assets/9cd05b11-239a-4ced-9edc-6a5f303b3b16" alt="detective mayfly" width="1024" height="1024"/>

# ORDEQBioassessment

This package facilitates routine metric data processing for Oregon DEQ's bioassessment program. This package contains all the necessary data and functions to pull data from AWQMS; calculate O:E, MMI, and BCG scores; as well as calculate metrics.

# Installation

To install, use the following code:

```         
library(devtools)
devtools::install_github("TravisPritchardODEQ/ORDEQBioassessment", 
                         host = "https://api.github.com", 
                         dependencies = TRUE, force = TRUE, upgrade = "never")
```

# Use

Download the [frontend file](https://github.com/TravisPritchardODEQ/ORDEQBioassessment/raw/refs/heads/main/frontend.R "Download the frontend file from github"), and work your way through it. Add data filtering code as needed.

# To Do

-   Add option to limit running the code to only sites that do not yet have calculated metrics

-   ~~Create AWQMS import files~~

-   ~~Get final list of metrics to upload to AWQMS~~
