library(tidyverse)
library(curl)
library(pushoverr)

user <- curl::curl_escape(Sys.getenv("QC_USER"))
pass <- curl::curl_escape(Sys.getenv("QC_PASS"))
server <- Sys.getenv("QC_SERVER")

url <- paste0("imaps://", user, ":", pass, "@", server)

qc_handle <- curl::new_handle(customrequest = 'GETQUOTA ""')

qc_response <- curl::curl_fetch_memory(url, qc_handle)

quotas <- qc_response$header %>%
    rawToChar() %>%
    str_extract('(?<=QUOTA \\"\\" \\()(\\w+ \\d+ \\d+ *)+')

qc_names <- quotas %>%
    str_extract_all("[A-Z]+") %>%
    unlist()

qc_values <- quotas %>%
    str_extract_all("\\d+ \\d+", simplify = T)  %>%
    map(str_split, pattern = " ", simplify = T) %>%
    map(as.numeric)

names(qc_values) <- qc_names

qc_ratio <- qc_values %>%
    map(~.x[1]/.x[2])

timestamp()
print(qc_ratio)

if(qc_ratio$STORAGE >= 0.995) {
    po_title <- glue::glue("Email {scales::percent(qc_ratio$STORAGE, accuracy = .01)} Full!")
    po_message <- glue::glue("Email is {scales::percent(qc_ratio$STORAGE, accuracy = .01)} full at {round(qc_values$STORAGE[1]/1024/1024, 3)} Gb")
    
    pushover_emergency(title = po_title,
                       message = po_message)
}


