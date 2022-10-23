

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
# remove duplicates (done)
# count missed cares by HCCSS, by week (needs work)
# highlight the most common reason for a missed care visit for each week (highighted as themes)
# Also highlight the most common time for missed care visits
# also highlight the PSW with the most missed cares

missed.care <- missed.care %>%
                filter(status != "Duplicate")

mc.count <- missed.care %>%
                group_by(mc.date, hccss) %>%
                summarise(count = n()) %>%
                ggplot() +
                geom_bar(aes(x = mc.date, y = count, fill = hccss), stat = "identity") +
                labs(x = "", y = "") +
                theme_hc() # needs a little work, kind of hard to read at the moment.

mc.themes <- missed.care %>%
                filter(mc.date >= filter.date) %>%
                group_by(theme) %>%
                summarise(count = n()) %>%
                arrange(desc(count))


mc.rank <- missed.care %>%
                filter(mc.date >= filter.date & !is.na(psw.id)) %>%
                group_by(psw.id) %>%
                summarise(count = n())


mc.missing <- missed.care %>%
                filter(is.na(psw.id))
