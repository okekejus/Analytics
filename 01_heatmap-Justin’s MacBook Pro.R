library("plotly")
library("tidyverse")
library("lubridate")
library("RODBC")
library("openxlsx")
library("ggthemes")
library("ggplot2")
library("scales")


# Data ----------------------------------------------------------------------------------------
myconnection <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                  DBQ=data-raw/Quality Database.accdb")

weekly.hmap.raw <- sqlFetch(myconnection, "WeeklyHeatMap") %>% 
                tibble() %>% 
                select(-c(ID))

weekly.apr.raw <- sqlFetch(myconnection, "APRs") %>% 
                tibble() %>% 
                select(-c(ID)) %>% 
                mutate(overall = overall * 100, 
                       assess = assess * 100)



monthly.hmap <- sqlFetch(myconnection, "MonthlyHeatMap") %>% 
                tibble() %>% 
                select(-c(ID)) %>% 
                mutate(counter_offer_rate = round(counter_offer_rate * 100, 2), 
                       hours_countered = round(hours_countered * 100, 2), 
                       assess_to_date = round(assess_to_date * 100, 2))


close(myconnection)

colnames(weekly.hmap.raw) <- c("hccss", "accepted", "refused", "time.gender", "no.staff", "incomplete", 
                               "counters.prov", "hours.rej", "hours.offered", "entry.date")

colnames(weekly.apr.raw) <- c("entry.date", "assess", "initial.past.due", "apr.due", "hccss", "overall")

# weekly.hmap <- left_join(weekly.hmap.raw, weekly.apr.raw)

# colnames(weekly.hmap) <- c("hccss", "accepted", "refused", "time.gender", "no.staff", "incomplete", 
#                            "counters.prov", "hours.rej", "hours.offered", "entry.date", "assess", "initial.past.due", 
#                            "apr.due", "overall")
colnames(monthly.hmap) <- c("month", "hccss", "accepted", "refused", "total", "mc.denom", "missed", "serviced", 
                            "canceled.nsnf", "client.falls", "rs.events", "monthly.clients", "staff.rs", "workers.in.area", 
                            "counter.offer.rate", "hours.countered", "assess.to.date", "initial", "apr.due", "nsnf.total")



# remove(weekly.hmap.raw, weekly.apr.raw)


acceptance.scale <- c("red", "yellow", "green") # higher numbers are desirable
risk.scale <- c("green", "yellow", "red") # lower numbers are desirable 
# common theme is that red is bad 
## Quality Check -----------------------------------------------------------

# 
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

acceptance.rate <- weekly.hmap.raw %>%
                mutate(acceptance.rate = round((accepted/(refused + accepted)) * 100, 2)) %>%
                createHeatMap("entry.date", "hccss", "acceptance.rate", acceptance.scale, 
                              "Week", "Acceptance Rate (%)")


weekly.time.spec <- weekly.hmap.raw %>%
                mutate(time.spec = round((time.gender/refused) * 100, 2)) %>%
                createHeatMap("entry.date", "hccss", "no.staff", risk.scale, 
                              "Week", "Refusals: Time Specific (%)")

weekly.no.staff <- weekly.hmap.raw %>%
                mutate(no.staff = round((no.staff/refused) * 100, 2)) %>%
                createHeatMap("entry.date", "hccss", "no.staff", risk.scale, 
                              "Week", "Refusals: No Staff (%)")


incomplete.entries <- weekly.hmap.raw %>% 
                createHeatMap("entry.date", "hccss", "incomplete", risk.scale, "Week",
                              "Incomplete Entries")

counter.offer <- weekly.hmap.raw %>%
                mutate(counter.rate = round((counters.prov/refused) * 100, 2)) %>%
                createHeatMap("entry.date", "hccss", "counter.rate", acceptance.scale, 
                              "Date", "Counter Offer Rate (%)") 


monthly.acceptance.rate <- monthly.hmap %>% 
                filter(hccss == "CE" | hccss == "CW" | hccss == "CENT" | hccss == "TC") %>% 
                mutate(acceptance.rate = round((accepted/total)*100, 2)) %>% 
                createHeatMap("month", "hccss", "acceptance.rate", acceptance.scale, "Date", "Acceptance Rate per HCCSS (%)")


monthly.mc.rate <- monthly.hmap %>% 
                filter(hccss == "CE" | hccss == "CW" | hccss == "CENT" | hccss == "TC") %>% 
                mutate(missed.care.rate = round((missed/mc.denom)*100, 2)) %>% 
                createHeatMap("month", "hccss", "missed.care.rate", risk.scale, "Date", "MC Rate per HCCSS (%)")

monthly.falls <- monthly.hmap %>% 
                filter(hccss == "CE" | hccss == "CW" | hccss == "CENT" | hccss == "TC"| 
                                       hccss == "CC") %>% 
                mutate(falls.per.1000 = round((client.falls/monthly.clients)*1000, 2)) %>% 
                createHeatMap("month", "hccss", "falls.per.1000", risk.scale, "Date", "Falls per 1000 Clients (%)")

monthly.rs <- monthly.hmap %>% 
                filter(hccss == "CE" | hccss == "CW" | hccss == "CENT" | hccss == "TC"| 
                                       hccss == "CC") %>% 
                mutate(other.per.1000 = round((rs.events/monthly.clients)*1000, 2)) %>% 
                createHeatMap("month", "hccss", "other.per.1000", risk.scale, "Date", "Risk Events per 1000 Clients (%)")

monthly.staff.rs <- monthly.hmap %>% 
                filter(hccss == "CE" | hccss == "CW" | hccss == "CENT" | hccss == "TC"| 
                                       hccss == "CC") %>% 
                mutate(staff.per.1000 = round((staff.rs/workers.in.area)*1000, 2)) %>% 
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





#                 
# weekly.hmap.raw %>% 
#                 mutate(acceptance.rate = round((accepted/(accepted+refused))*100, 2)) %>% 
#                 select(entry.date, hccss, acceptance.rate) %>% 
#                 ggplot(aes(x = entry.date, y = hccss, fill = acceptance.rate)) + 
#                 geom_tile(color = "white")+
#                 scale_fill_gradientn(colours = c("red", "orange", "yellow", "green"), 
#                                      values = rescale(c(49, 59, 89, 90)), 
#                                      guide = "colorbar") 
#                 


                