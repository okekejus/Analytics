# Risk and Safety Database (Falls Data)


risk.db <- read.xlsx("data-raw/risk_and_safety.xlsx", detectDates = TRUE)  %>% 
                tibble() %>% 
                mutate(creation.date = parse_date_time(creation.date, "%m/%d/%y %H:%M:%S")) %>% 
                filter(event.type != "Not a Risk Event")


# rank client risk types 
# separate falls from the rest of the data 
# Total falls
# Client with the most falls (group by client ID) 
# Number of falls by funder 
# Entries without Case Notes (because they didn’t use the correct form) 
# How many unwitnessed falls? 
# Falls by Coordinator (eventually)
# 911 call rate for falls, hospitalization call rate for falls (eventually)


# Cleanup  -------------------------------------------------------

# Fix department listings
for.correction <- str_detect(risk.db$department, "Home Services") %>% 
                which()

risk.db$department[c(for.correction)] <- "Home Services"

for.correction <- str_detect(risk.db$department, "Holocaust") %>% 
                which()

risk.db$department[c(for.correction)] <- "Holocaust"

for.correction <- str_detect(risk.db$department, "Social Work") %>% 
                which()

risk.db$department[c(for.correction)] <- "Social Work"

for.correction <- str_detect(risk.db$department, "Adult Day Program") %>% 
                which()

risk.db$department[c(for.correction)] <- "Adult Day Program"

for.correction <- str_detect(risk.db$department, "Intake") %>% 
                which()

risk.db$department[c(for.correction)] <- "Intake"

# Fix Funder listings 
risk.db$primary.funder <- str_to_title(risk.db$primary.funder)

# count risk events by date - split them based on categories 

risk.db$creation.date <- as.Date(risk.db$creation.date) %>% 
                floor_date("month")



# Count of Events  --------------------------------------------------------
# Overall
risk.count <- risk.db %>% 
                group_by(creation.date, event.type) %>% 
                summarise(count = n()) 


risk.count.wider <- risk.count %>% 
                pivot_wider(names_from = event.type, values_from = count)

colnames(risk.count.wider) <- c("creation.date", "complaint", "prev.risk", "risk.event",
                                "risk.identified", "staff.risk", "client.hospital", "adv.event")

risk.event.count <- risk.db %>% 
                group_by(event.type, creation.date) %>% 
                summarise(count = n()) %>% 
                createBarPlot("creation.date", "count", TRUE, "event.type", "Date", 
                              "Event Type")




# Ranking -----------------------------------------------------------------

# Risk events by department (and date)
risk.by.dep <- risk.db %>% 
                group_by(department, creation.date) %>% 
                summarise(count = n()) %>% 
                createBarPlot("creation.date", "count", TRUE, "department", "Date", 
                              "Number of Events")
 

risk.by.funder <- risk.db %>% 
                mutate(primary.funder = str_to_title(primary.funder)) %>% 
                group_by(primary.funder, creation.date) %>% 
                summarise(count = n()) %>% 
                createBarPlot("creation.date", "count", TRUE, "primary.funder", "Date", 
                              "Number of Events")


# need to talk to Emma about standardizing entries so categories are neater 

event.type.plot <- risk.db %>% 
                group_by(event.type) %>% 
                summarise(count = n()) %>% 
                plot_ly(x = ~event.type, y = ~count, type = "bar") 




client.events.plot <- risk.db %>% 
                group_by(creation.date, risk.type.client) %>% 
                summarise(count = n()) %>% 
                createBarPlot("creation.date", "count", TRUE, "risk.type.client", 
                               "Date", "Client Risk Events")


staff.events.plot <- risk.db %>% 
                group_by(risk.type.staff, creation.date) %>% 
                summarise(count = n()) %>% 
                createBarPlot("creation.date", "count", TRUE, "risk.type.staff", 
                              "Date", "Staff Risk Events")



# Falls -------------------------------------------------------------------

falls.by.funder <- risk.db %>% 
                filter(risk.type.client == "Client Fall") %>%
                group_by(creation.date, primary.funder) %>% 
                summarise(count = n()) %>% 
                createLinePlot("creation.date", "count", TRUE, "primary.funder", 
                               "Date", "Total Number of Falls")


unwitnessed.falls <- risk.db %>% 
                filter(fall.witness == "Unwitnessed") %>% 
                group_by(creation.date, primary.funder) %>% 
                summarise(count = n()) %>% 
                createBarPlot("creation.date", "count", TRUE, "primary.funder", 
                               "Date", "Number of Unwitnessed Falls")




