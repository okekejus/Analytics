library("openxlsx")
library("ggthemes")

# need to change week numbers to actual dates 

# Data --------------------------------------------------------------------

rejection.rate <- read_csv("data-raw/rejection_rate.csv")

offers <- read.xlsx("data-raw/Offer_Tracking.xlsx",
                    sheet = "Offers",
                    detectDates = TRUE) %>%
                tibble() %>%
                mutate(acceptance_rate = round(acceptance_rate * 100, 2),
                       week = cut.Date(date, breaks = "1 week", labels = FALSE))


offers.reshaped <- offers %>% 
                select(date, HCCSS, acceptance_rate) %>% 
                pivot_wider(names_from = HCCSS, values_from = acceptance_rate )

weekly.acceptance.rate <- offers %>%
                group_by(week, HCCSS) %>%
                summarise(total_accepted = sum(accepted, na.rm = TRUE),
                          total_refused = sum(refused, na.rm = TRUE),
                          rate = total_accepted/(total_refused + total_accepted) * 100) %>% 
                select(week, HCCSS, rate) %>% 
                pivot_wider(names_from = HCCSS, values_from = rate)

# Adding newest week to old dataset 
# addon <- tibble(HCCSS = NA,
#                 above_five = NA,
#                 five_or_less = NA,
#                 total = NA,
#                 rate = NA,
#                 week = as.integer(readline(prompt = "Tracking week: ")))
# 
# 
# 
# hccss <- c("Central", "Central East", "Central West", "Toronto Central")
# 
# for (i in hccss) {
#                 new_dat <- rejectionPercentage(i, as.integer(readline(prompt = "Tracking week for loop: ")))
#                 
#                 addon <- rbind(addon, new_dat)
# }
# 
# 
# addon <- addon[-1, ]
# 
# rejection.rate <- rbind(rejection.rate, addon)
# 
# write_csv(rejection.rate, "data-raw/rejection_rate.csv")

# Rejection Rate ----------------------------------------------------------
rejection.rate.plot <- rejection.rate %>% 
                group_by(HCCSS) %>% 
                plot_ly(x = ~week, y = ~rate, type = "scatter", mode = "lines+markers", 
                        color = ~HCCSS)

# Acceptance (Weekly/Daily) ---------------------------------------------------------------------------
# Daily acceptance
daily.acceptance <- plot_ly(offers.reshaped, x = ~date, y = ~CENT, type = "bar", name = "Central") %>% 
                add_trace(y  = ~CE, name = "Central East") %>% 
                add_trace(y  = ~CW, name = "Central West") %>% 
                add_trace(y = ~TC, name = "Toronto Central") %>%
                layout(yaxis = list(title = ''), barmode = 'group')
# Weekly acceptance
weekly.acceptance.plot <- weekly.acceptance.rate %>% 
                plot_ly(x = ~week, y = ~CENT, type = "bar", name = "Central") %>% 
                add_trace(y  = ~CE, name = "Central East") %>% 
                add_trace(y = ~CW, name = "Central West") %>% 
                add_trace(y = ~TC, name = "Toronto Central") %>% 
                layout(xaxis = list(title = ""), yaxis = list(title = ""))




# Rejection Demographics  ---------------------------------------------------------------------

# Line level referrals (who are the new clients)
# Percentage of rejected referrals due to time specific requests
# Percentage of rejected referrals at 5 hours per week or less
# percentage of rejected referrals at 5 hours per week or less (for each week since implementation)

rejection.tracker <- read.xlsx("data-raw/Offer_Tracking.xlsx",
                               sheet = "Rejection Tracker",
                               detectDates = TRUE)[-74, ] %>%
                tibble() %>%
                mutate(week = cut.Date(Date, breaks = "1 week", labels = FALSE), 
                       BRN = as.integer(BRN), 
                       HoursOffered = as.integer(HoursOffered), 
                       CounterOffer = as.integer(CounterOffer))


# reasons for rejection (Overall)
all.rejection.reasons <- rejection.tracker %>%
                group_by(MainReason) %>%
                summarise(reason = n()) %>%
                mutate(percent = round((reason/sum(reason)) * 100, 2)) %>%
                select(MainReason, percent)

all.rejection.reasons

all.rej.reason.pie <- plot_ly(all.rejection.reasons, labels = ~MainReason, values = ~percent, type = 'pie')%>% 
                layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# less than five hours (should've been accepted)
under.five <- rejection.tracker %>%
                group_by(HCCSS) %>% 
                filter(HoursOffered <= 5) %>% 
                summarise(count = n())


over.five <- rejection.tracker %>% 
                group_by(HCCSS) %>% 
                filter(HoursOffered > 5) %>% 
                summarise(count2 = n())

hour.count <- left_join(under.five, over.five) %>% 
                rename(under5 = count, 
                       over5 = count2) %>% 
                mutate(total = under5 + over5, 
                       percent.under.5 = round((under5/total) * 100, 2))





# less_than_five_bp <- less_than_five %>%
#                 group_by(week, HCCSS) %>%
#                 summarise(count = n()) %>%
#                 ggplot() +
#                 geom_bar(aes(x = week, y = count, fill = HCCSS), stat = "identity", position = "dodge") +
#                 theme_hc() +
#                 labs(y = "Rejected referrals <= 5 hours",
#                      x = "Week",
#                      title = "Rejected referrals <=5 hours per HCCSS")



# sc <- read_csv('data-raw/coordinator_list.csv') 


rejection.by.sc <- rejection.tracker %>% 
                filter(HoursOffered <= 5) %>% 
                group_by(SC) %>% 
                summarise(count = n()) %>% 
                plot_ly(x = ~SC, y = ~count, type = "bar") %>%
                layout(xaxis = list(title = ""), 
                       yaxis = list(title = ""))



# less_than_five_sc <- less_than_five %>%
#                 group_by(SC) %>%
#                 summarise(total = n()) %>%
#                 ggplot() +
#                 geom_bar(aes(x = SC, y = total), stat = "identity") +
#                 theme_hc() +
#                 labs(y = "Rejected offers <= 5 hours",
#                      x = "Service Coordinator",
#                      title = "Rejected offers <= 5 hours by SC")
# 
# 
# 
# less_than_five_reason <- less_than_five %>%
#                 group_by(MainReason) %>%
#                 summarise(total = n()) %>%
#                 mutate(percent = round((total/sum(total)) * 100, 2)) %>%
#                 select(MainReason, percent)
# 
# less_than_five_location <-
#                 less_than_five %>%
#                 group_by(HCCSS) %>%
#                 summarise(total = n()) %>%
#                 mutate(percent = round(total/sum(total) * 100, 2)) %>%
#                 select(HCCSS, percent) %>%
#                 ggplot() +
#                 geom_bar(aes(x = HCCSS, y = percent), stat = "identity") +
#                 theme_hc() +
#                 labs(y = "Percentage of rejections",
#                      x = "HCCSS",
#                      title = "Percentage of rejections <= 5 hours per HCCSS")
# 
# less_than_five_location






# Missed Care ---------------------------------------------------------------------------------
# Missed Care (June 13th - June 28th)
# - Toronto Central
# - Central
# - Overall (TC / Central) compares to (CE / CW)
# 
# mc <- read.xlsx("data-raw/Offer_Tracking.xlsx",
#                 sheet = "Missed Care",
#                 detectDates = TRUE) %>%
#                 tibble() %>%
#                 select(MCDate, everything()) %>%
#                 filter(Status != "Duplicate") %>%
#                 mutate(week = cut.Date(MCDate, breaks = "1 week", labels = FALSE))
# 
# more <- str_detect(mc$Reason, "Not missed care") %>%
#                 which()
# 
# mc <- mc[-c(more), ] # removed not missed care and duplicates
# 
# # overall reasons for missed care
# missed_care_reason <- mc %>%
#                 group_by(Reason) %>%
#                 summarise(reason_count = n()) %>%
#                 mutate(percent = round((reason_count/sum(reason_count)) * 100, 2)) %>%
#                 arrange(desc(reason_count))  %>%
#                 select(Reason, percent)
# 
# weekly_mc <- mc %>%
#                 group_by(week, HCCSS) %>%
#                 summarise(count = n()) %>%
#                 ggplot() +
#                 geom_bar(aes(x = week, y = count, fill = HCCSS), stat = "identity", position = "dodge") +
#                 theme_hc() +
#                 labs(x = "Week",
#                      y = "Missed Cares")
# 
# weekly_mc
# 
# # missed care percentage by location
# mc_locations <- mc %>%
#                 group_by(HCCSS) %>%
#                 summarise(HCCSS_count = n()) %>%
#                 mutate(percent = round((HCCSS_count/sum(HCCSS_count)) * 100, 2))
# 
# 
# mc_new_clients <- tibble(clientID = c(70583),
#                          activationDate = c("2022-06-15"))
# 
# mc_new_clients <- mc %>%
#                 subset(ClientID %in% mc_new_clients$clientID)
