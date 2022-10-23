# Communications dashboard 


# Data --------------------------------------------------------------------


social.media <- read_csv("data-raw/all_media_data.csv")



# Social Media ------------------------------------------------------------


sm.likes <- social.media %>% 
                createLinePlot("Date", "likes", TRUE, "media", "Date", "Total Likes")

sm.posts <- social.media %>% 
                createLinePlot("Date", "posts", TRUE, "media", "Date", "Posts Per Month")

sm.imp <- social.media %>% 
                createLinePlot("Date", "impressions", TRUE, "media", "Date", "Total Impressions")

sm.comments <- social.media %>% 
                createLinePlot("Date", "comments", TRUE, "media", "Date", "Total Comments")

sm.shares <- social.media %>% 
                createLinePlot("Date", "shares", TRUE, "media", "Date", "Total Shares")

sm.views <- social.media %>% 
                createLinePlot("Date", "views", TRUE, "media", "Date", "Total Views")

sm.clicks <- social.media %>% 
                createLinePlot("Date", "clicks", TRUE, "media", "Date", "Total Clicks")



