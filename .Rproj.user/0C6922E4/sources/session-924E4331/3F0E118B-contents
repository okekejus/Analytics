# Data --------------------------------------------------------------------
safa.scorecard <- read.xlsx("data-raw/SAFA Scorecard.xlsx",
                            detectDates = TRUE, 
                            sheet = "Target & Availability") %>% 
                tibble() %>% 
                mutate(Availability = Availability * 100, 
                       Target = Target * 100) %>% 
                rename(availability = Availability, 
                       targetpercent = Target)


psw.utilization <- read.xlsx("data-raw/SAFA Scorecard.xlsx",
                             detectDates = TRUE, 
                             sheet = "Utilization") %>% 
                tibble() %>% 
                mutate(percent = percent * 100)


twelve.weeks <- read.xlsx("data-raw/12 Weeks.xlsx", 
                          detectDates = TRUE, 
                          sheet = "12 Weeks") %>% 
                tibble()


colnames(twelve.weeks) <- c("psw", "psw.id", "coordinator", "coordinator.id", "gender", "worker.type", 
                            "group", "resigned", "one", "two", "three", "four", "five", "six", "seven", 
                            "eight", "nine", "ten", "eleven", "twelve", "complete")

twelve.weeks[9:20] <- apply(twelve.weeks[9:20], 2, toPercent)


average.twelve <- colMeans(twelve.weeks[9:20], na.rm = TRUE) 

names(average.twelve) <- NULL

twelve.weeks.split <- split(twelve.weeks, twelve.weeks$group)

grouped.means <- lapply(twelve.weeks.split, function(x){
                colMeans(x[, c("one", "two", "three", 
                               "four", "five", "six", 
                               "seven", "eight", "nine", 
                               "ten", "eleven", "twelve")], 
                         na.rm = TRUE)
})



new.names <- format(as.Date(names(grouped.means)), "%m-%Y")


names(grouped.means) <- new.names


grouped.means <- lapply(grouped.means, namesToNull)


output <- tibble(group = "Average", 
                 weeks = c(1:12), 
                 value = average.twelve)

remainder <- length(grouped.means)

complete <- 1


while (complete < length(grouped.means) + 1) {
                
                new.dat <- reshapeTwelve(grouped.means[complete])
                
                output <- rbind(output, new.dat)
                
                complete <- complete + 1
                
                remainder <- remainder - 1
                
                print(remainder)
}


# Under-utilized  ------------------------------------------------------------
psw.utilization$week.num <- week(psw.utilization$week)
comp.week <- week(Sys.Date()) - 4
under.utilized <- psw.utilization %>% 
                mutate(percent = round(percent, 2)) %>% 
                rename(ID = psw.id, 
                       `Target (%)` = percent,
                       Week = week) %>% 
                select(-week.num)



comp.week

utilized <- psw.utilization %>% 
                filter(week.num >= comp.week & percent >= 50) %>% 
                arrange(desc(week)) %>% 
                mutate(percent = round(percent, 2)) %>% 
                rename(ID = psw.id, 
                       `Target (%)` = percent,
                       Week = week) %>% 
               
                select(-week.num)



# Plots -------------------------------------------------------------------
target <- safa.scorecard %>% 
                select(week, availability, targetpercent) %>%
                rename(Availability = availability, 
                       Target = targetpercent) %>%
                pivot_longer(!week, names_to = "metric") %>% 
                createLinePlot("week", "value", TRUE, "metric", "Date", "Percentage")


# Cohort Comparison  ------------------------------------------------------
cohort.comparison <- output %>% 
                createLinePlot("weeks", "value", TRUE, "group", "Date", "Target (%)")

# need to gather and list the stats for each one for now. Going to put a message 
# in the box saying it is coming soon

# Resignations ------------------------------------------------------------
resign <- twelve.weeks %>% 
                group_by(group) %>% 
                summarise(total = n(), 
                          resigned = sum(resigned == 1)) %>% 
                mutate(percentage = round((resigned/total)*100, 2))



monthly.resignations <- resign %>% 
                createLinePlot("group", "percentage", FALSE, NULL, 
                               "Month Hired", "Resignations (%)")