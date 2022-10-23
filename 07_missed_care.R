# Missed care 

mc <- read.xlsx("data-raw/missed_care.xlsx", sheet = "MissedCare", detectDates = TRUE) %>% 
                tibble()

colnames(mc) <- c("funder", "client", "reason", "date.time", "brn", "status", 
                  "notes", "date", "time", "length", "theme", "psw.id", "interval", 
                  "related.mc")

mc$date <- date(mc$date.time)


mc.count <- mc %>%
                mutate(date = floor_date(date, "week")) %>% 
                group_by(date, funder) %>% 
                summarise(mc.count = n()) %>% 
                createBarPlot("date", "mc.count", TRUE, "funder", "Date", 
                              "Number of Missed Care Visits")



mc.themes <- mc %>%
                mutate(date = floor_date(date, "week"), 
                       theme = replace(theme, is.na(theme), "Missing Value")) %>%
                group_by(date, theme) %>% 
                summarise(theme.count = n()) %>% 
                createBarPlot("date", "theme.count", TRUE, "theme", "Date", 
                              "Missed Care Theme Count")


                
