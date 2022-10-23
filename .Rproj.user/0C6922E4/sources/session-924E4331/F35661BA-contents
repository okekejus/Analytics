library("plotly")
library("tidyverse")
library("lubridate")
library("openxlsx")
library("ggthemes")
library("ggplot2")
library("scales")


# Data --------------------------------------------------------------------

weekly.hmap.raw <- read.xlsx("data-raw/Heat Map.xlsx", 
                             detectDates = TRUE, sheet = "Heat_Map") %>% 
                tibble() %>% 
                mutate(acceptance.rate = round((accepted/(accepted + refused))*100, 2), 
                       time.spec = round((time.gender/refused) * 100, 2), 
                       no.staff = round((no.staff/refused) * 100, 2), 
                       counter.rate = round((counters.prov/refused) * 100, 2))

weekly.apr.raw <- read.xlsx("data-raw/APRs.xlsx", 
                            detectDates = TRUE) %>% 
                tibble() %>% 
                mutate(overall = overall * 100, 
                       assess = assess * 100)

monthly.hmap <- read.xlsx("data-raw/Monthly_Heat_Map.xlsx", detectDates = TRUE, sheet = "Sheet1") %>% 
                tibble() %>% 
                mutate(counter.offer.rate = round(counter.offer.rate * 100, 2), 
                       hours.countered = round(hours.countered * 100, 2), 
                       assess.to.date = round(assess.to.date * 100, 2), 
                       acceptance.rate = round((accepted/total)*100, 2), 
                       falls.per.1000 = round((client.falls/monthly.clients)*1000, 2), 
                       missed.care.rate = round((missed/mc.denom)*100, 2), 
                       staff.per.1000 = round((staff.rs/workers.in.area)*1000, 2), 
                       other.per.1000 = round((rs.events/monthly.clients)*1000, 2))




colnames(weekly.apr.raw) <- c("entry.date", "assess", "initial.past.due", "apr.due", "hccss", "overall")




# Setting ranges 
acceptance.groups <- c(0, 70, 90, 94, 100)
incomplete.groups <- c(0, 1, Inf)
counter.groups <- c(0, 90, 99, 100)
hours.countered.groups <- c(0, 25, 50, 100)
mc.groups <- c(0, 0.04, 1)



acceptance.scale <- c("red", "yellow", "green") # higher numbers are desirable
risk.scale <- c("green", "yellow", "red") # lower numbers are desirable 


## Quality Check -----------------------------------------------------------

# will prompt user to complete data before uploading
# if (count(weekly.hmap.raw) != count(weekly.apr.raw)) {
#                 
#                 stop("There is a mismatch in the length of APR and Heat Map files.")
#                 
# }


# Line Plots ----------------------------------------------------------------------------------
weekly.accepted <- weekly.hmap.raw %>% 
                createLinePlot("entry.date", "accepted", TRUE,
                               "hccss", "Date", "Accepted") 
weekly.refused <- weekly.hmap.raw %>% 
                createLinePlot("entry.date", "refused", TRUE,
                               "hccss", "Date", "Refused") 


monthly.refusals <- monthly.hmap %>%
                createLinePlot("month", "refused", TRUE, "hccss", "Month", "Refused")


monthly.accepted <- monthly.hmap %>% 
                createLinePlot("month", "accepted", TRUE, "hccss", "Month", "Accepted")

monthly.apr.due <- monthly.hmap %>% 
                filter(hccss != "Overall" | hccss!= "CC") %>% 
                createLinePlot("month", "apr.due", TRUE, "hccss", "Month", 
                               "Total APRs Due")


monthly.initial <- monthly.hmap %>% 
                filter(hccss == "CENT") %>% 
                createLinePlot("month", "initial", FALSE, NULL, "Month", "Initial Assessments Past Due")



# Bar Plots ---------------------------------------------------------------
weekly.initial.due <- weekly.apr.raw %>% 
                filter(hccss == "CENT") %>%
                createBarPlot("entry.date", "initial.past.due", FALSE, 
                              "hccss", "Date", "Assessmemts Past Due")


weekly.completed.assess <- weekly.apr.raw %>% 
                filter(hccss == "CENT") %>%
                createBarPlot("entry.date", "overall", FALSE, 
                              "hccss", "Date", "Assessments Past Due")

weekly.apr.due <- weekly.apr.raw %>% 
                select(hccss, entry.date, assess, initial.past.due, apr.due) %>% 
                filter(!is.na(assess))%>%
                filter(hccss == "CENT" | hccss == "CE" | hccss == "CW") %>%
                createBarPlot("entry.date", "apr.due", group = TRUE, groupVar = "hccss", 
                              "Date", "APRs Due")



# Heat Maps -----------------------------------------------------------------------------------

# acceptance.rate <- weekly.hmap.raw %>%
#                 mutate(acceptance.rate = round((accepted/(refused + accepted)) * 100, 2)) %>%
#                 createHeatMap("entry.date", "hccss", "acceptance.rate", acceptance.scale, 
#                               "Week", "Acceptance Rate (%)")
# 



acceptance.rate <- weekly.hmap.raw %>% 
                mutate(cat = cut(acceptance.rate, breaks = acceptance.groups)) %>% 
                filter(week(entry.date) >= week(Sys.Date()) - 8) %>% 
                ggplot(aes(entry.date, hccss, fill = cat)) + 
                geom_tile(color = "white", lwd = 0.5) + 
                geom_text(aes(label = paste0(acceptance.rate, "%")),  color = "white") +
                scale_fill_manual(breaks = levels(cat), 
                                  values = c("red", "orange", "yellow", "green")) + 
                scale_x_date(breaks = "7 days") + 
                labs(x = "Date", 
                     y = "HCCSS")



weekly.time.spec <- weekly.hmap.raw %>%
                createHeatMap("entry.date", "hccss", "no.staff", risk.scale, 
                              "Week", "Refusals: Time Specific (%)")


weekly.no.staff <- weekly.hmap.raw %>%
                createHeatMap("entry.date", "hccss", "no.staff", risk.scale, 
                              "Week", "Refusals: No Staff (%)")


incomplete.entries <- weekly.hmap.raw %>% 
                createHeatMap("entry.date", "hccss", "incomplete", risk.scale, "Week",
                              "Incomplete Entries")

counter.offer <- weekly.hmap.raw %>%
                createHeatMap("entry.date", "hccss", "counter.rate", acceptance.scale, 
                              "Date", "Counter Offer Rate (%)") 


monthly.acceptance.rate <- monthly.hmap %>% 
                filter(hccss == "CE" | hccss == "CW" | hccss == "CENT" | hccss == "TC") %>% 
                mutate(cat = cut(acceptance.rate, breaks = acceptance.groups)) %>% 
                ggplot(aes(month, hccss, fill = cat)) + 
                geom_tile(color = "white", lwd = 0.5) + 
                geom_text(aes(label = paste0(acceptance.rate, "%")),  color = "white", size = 3) +
                scale_fill_manual(breaks = levels(cat), 
                                  values = c("red", "orange", "yellow", "green")) +
                labs(x = "Date", 
                     y = "HCCSS")



monthly.mc.rate <- monthly.hmap %>% 
                filter(hccss == "CE" | hccss == "CW" | hccss == "CENT" | hccss == "TC") %>% 
                mutate(cat = cut(missed.care.rate, mc.groups)) %>% 
                ggplot(aes(month, hccss, fill = cat)) + 
                geom_tile(color = "white", lwd = 0.5) + 
                geom_text(aes(label = paste0(missed.care.rate, "%")),  color = "white", size = 3) +
                scale_fill_manual(breaks = levels(cat), 
                                  values = c("green", "red")) +
                labs(x = "Date", 
                     y = "HCCSS")
                
                
# monthly.mc.rate <- monthly.hmap %>% 
#                 filter(hccss == "CE" | hccss == "CW" | hccss == "CENT" | hccss == "TC") %>%
#                 createHeatMap("month", "hccss", "missed.care.rate", risk.scale, "Date", "MC Rate per HCCSS (%)")

monthly.falls <- monthly.hmap %>% 
                filter(hccss == "CE" | hccss == "CW" | hccss == "CENT" | hccss == "TC"| 
                                       hccss == "CC") %>%  
                createHeatMap("month", "hccss", "falls.per.1000", risk.scale, "Date", "Falls per 1000 Clients (%)")

monthly.rs <- monthly.hmap %>% 
                filter(hccss == "CE" | hccss == "CW" | hccss == "CENT" | hccss == "TC"| 
                                       hccss == "CC")  %>% 
                createHeatMap("month", "hccss", "other.per.1000", risk.scale, "Date", "Risk Events per 1000 Clients (%)")

monthly.staff.rs <- monthly.hmap %>% 
                filter(hccss == "CE" | hccss == "CW" | hccss == "CENT" | hccss == "TC"| 
                                       hccss == "CC") %>%
                createHeatMap("month", "hccss", "staff.per.1000", risk.scale, "Month", "Staff Risk Events")


monthly.counter.rate <- monthly.hmap %>% 
                filter(hccss == "CE" | hccss == "CW" | hccss == "CENT" | hccss == "TC") %>%
                select(month, hccss, hours.countered) %>% 
                createHeatMap("month", "hccss", "hours.countered", acceptance.scale, "Month", 
                              "Counter Offer Rate (%)")


monthly.hours.countered <- monthly.hmap %>% 
                filter(hccss == "CE" | hccss == "CW" | hccss == "CENT" | hccss == "TC") %>%
                select(month, hccss, hours.countered) %>% 
                createHeatMap("month", "hccss", "hours.countered", acceptance.scale, "Month", 
                              "Hours Countered (%)")


monthly.atd <- monthly.hmap %>% 
                filter(hccss != "CC" | hccss != "Overall") %>% 
                createHeatMap("month", "hccss", "assess.to.date", acceptance.scale, 
                              "Month", "Assessments Up to Date (%)")


# Hours -------------------------------------------------------------------

hours.countered <- weekly.hmap.raw %>% 
                mutate(hour.counter = round((hours.offered/hours.rej)*100, 2)) %>% 
                select(entry.date, hccss, hour.counter) %>% 
                createLinePlot("entry.date", "hour.counter", TRUE, "hccss", "Date", 
                               "Hours Countered (%)")


shrink <- weekly.hmap.raw %>% 
                mutate(acceptance.rate = round((accepted/(accepted+refused))*100, 2)) %>% 
                select(entry.date, hccss, acceptance.rate) %>% 
                data.matrix()
data <- shrink



# Missed Care -------------------------------------------------------------



missed.care <- read.xlsx("data-raw/missed_care.xlsx",
                         detectDates = TRUE)[-c(1:4), -c(18, 5)] %>%
                mutate(week = cut.Date(Date.of.MC, breaks = "1 week", labels = FALSE)) %>%
                tibble()


colnames(missed.care) <- c("hccss", "client.id", "reason",
                           "sc", "brn",
                           "status", "notes", "mc.date",
                           "visit.time", "visit.length", "rescheduled",
                           "quarter", "theme", "psw.id",
                           "interval.care", "action.items",
                           "additional.notes", "related.mc", "week")
# Missed Care Stats  --------------------------------------------------------------------------------------------------------
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








