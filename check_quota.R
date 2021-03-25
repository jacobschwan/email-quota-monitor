library(magrittr)

send_healthcheck <- function(id, fail = F) {
    if(fail) {
        url <- paste0("https://hc-ping.com/",id,"/fail")
    } else {
        url <- paste0("https://hc-ping.com/",id)
    }
    
    httr::GET(url = url)
}


user <- curl::curl_escape(Sys.getenv("QC_USER"))
pass <- curl::curl_escape(Sys.getenv("QC_PASS"))
server <- Sys.getenv("QC_SERVER")
recheck_freq <- as.numeric(Sys.getenv("QC_FREQ")) * 60
threshold <- as.numeric(Sys.getenv("QC_THRESHOLD"))

hc_id <- Sys.getenv("HEALTHCHECK_ID")

url <- paste0("imaps://", user, ":", pass, "@", server)

qc_handle <- curl::new_handle(customrequest = 'GETQUOTA ""')

previous_ratio <- 0

repeat {
    timestamp()
    qc_response <- curl::curl_fetch_memory(url, qc_handle)
    
    quotas <- qc_response$header %>%
        rawToChar() %>%
        stringr::str_extract('(?<=QUOTA \\"\\" \\()(\\w+ \\d+ \\d+ *)+')
    
    qc_names <- quotas %>%
        stringr::str_extract_all("[A-Z]+") %>%
        unlist()
    
    qc_values <- quotas %>%
        stringr::str_extract_all("\\d+ \\d+", simplify = T)  %>%
        purrr::map(stringr::str_split, pattern = " ", simplify = T) %>%
        purrr::map(as.numeric)
    
    names(qc_values) <- qc_names
    
    qc_ratio <- qc_values %>%
        purrr::map(~.x[1]/.x[2])
    
    print(qc_ratio)
    print(purrr::map(qc_values, ~.x/1024/1024))
    
    if(qc_ratio$STORAGE >= threshold & qc_ratio$STORAGE != previous_ratio) {
        po_title <- glue::glue("Email {scales::percent(qc_ratio$STORAGE, accuracy = .01)} Full!")
        po_message <- glue::glue("Email is {scales::percent(qc_ratio$STORAGE, accuracy = .01)} full at {round(qc_values$STORAGE[1]/1024/1024, 3)} GB")
        
        pushoverr::pushover_emergency(title = po_title,
                           message = po_message)
    }
    
    previous_ratio <- qc_ratio$STORAGE
    
    if(hc_id != "") {
        send_healthcheck(hc_id)
    }

    Sys.sleep(recheck_freq)
}




