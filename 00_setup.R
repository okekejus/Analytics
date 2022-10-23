# Packages ----------------------------------------------------------------

list.of.packages <- c("plotly", "tidyverse", "lubridate", "openxlsx", 
                      "shiny", "shinydashboard", "DT", "fresh")



# Functions ---------------------------------------------------------------

checkPackages <- function() {
                
                new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
               
                if(length(new.packages)) {install.packages(new.packages)}
                
                
                
              
                
} # double checking to make sure the packages are included. 


## Finance Analysis --------------------------
namesToNull <- function(x){
                names(x) <- NULL
                
                return(x)
} # strips the names from a named vector, allows for reshaping. 

## Rejection Tracker -------------------------
rejectionPercentage <- function(x, y){
                
                total_rejections <- rejection_tracker %>%
                                filter(HCCSS == x & week == y) %>%
                                count() %>%
                                as.integer()
                
                more_than_five <- rejection_tracker %>%
                                filter(HCCSS == x & HoursOffered > 5 & week == y) %>%
                                count() %>%
                                as.integer()
                
                less_than_five <- rejection_tracker %>%
                                filter(HCCSS == x & HoursOffered <= 5 & week == y) %>%
                                count() %>%
                                as.integer()
                
                new_dat <- tibble(HCCSS = x,
                                  week = y,
                                  above_five = more_than_five,
                                  five_or_less = less_than_five,
                                  total = more_than_five + less_than_five,
                                  rate = round((less_than_five/total) * 100, 2))
                
                returnValue(new_dat)
                
} # determine the percentage of visits < or > 5 hours that have been declined


## PSW Utilization --------
toPercent <- function(.data) {
                .data <- round(.data * 100, 2)
                return(.data)
}

reshapeTwelve <- function(x){
                
                # takes list item as input 
                weeks <- c(1:12)
                group <- names(x)
                
                
                
                avgs <- unlist(x, use.names = FALSE)
                
                new.dat <- tibble(group = group, 
                                  weeks = weeks, 
                                  value = avgs)
                
                
                returnValue(new.dat)
                
                
} # this is for cohort comparison 

detectQuarter <- function(x) {
                
                entry.month <- lubridate::month(x)
                
                q1 <- "03-01"
                q2 <- "07-01"
                q3 <- "10-01"
                q4 <- "01-01"
                
                if (between(entry.month, 3, 6)){
                                
                                q1 <- paste0(year(x), "-", q1) %>% 
                                                date()
                                
                                return(q1)
                                
                } else if (between(entry.month, 7, 9)){
                                
                                q2 <- paste0(year(x), "-", q2) %>% 
                                                date()
                                
                                return(q2)
                } else if (between(entry.month, 10, 12)){
                                
                                q3 <- paste0(year(x), "-", q3) %>% 
                                                date()
                                
                                return(q3)
                } else if (between(entry.month, 1, 3)){
                                
                                q4 <- paste0(year(x), "-", q4) %>% 
                                                date()
                                
                                return(q4)
                }
                

}



# Plotly Helper Functions ---------------------------------------------------------------------------------------------------
createLinePlot <- function(.data, my_x, my_y, group = FALSE, 
                           groupVar = NULL, x_name,y_name) {
                
                
                if(group == TRUE){
                                
                                .data <- .data %>% 
                                                group_by(across({{ groupVar }})) %>%
                                                plot_ly(x=as.formula(paste0('~', my_x)), 
                                                        y=as.formula(paste0('~', my_y)), 
                                                        color=as.formula(paste0('~', groupVar)),
                                                        type = "scatter",
                                                        mode = "lines+markers") %>% 
                                                layout(xaxis = list(title = x_name), yaxis = list(title = y_name))
                                
                                return(.data)
                                
                } else {
                                .data <- .data %>% 
                                                plot_ly(x=as.formula(paste0('~', my_x)),
                                                        y=as.formula(paste0('~', my_y)),
                                                        type = "scatter",
                                                        mode = "lines+markers")  %>% 
                                                layout(xaxis = list(title = x_name), yaxis = list(title = y_name))
                                
                                return(.data)
                }
                
}



createBarPlot <- function(.data, my_x, my_y, group = FALSE, 
                          groupVar = NULL, x_name, y_name) {
                
                if(group) {
                                .data <- .data %>% 
                                                group_by(across({{ groupVar }})) %>%
                                                plot_ly(x = as.formula(paste0('~', my_x)), 
                                                        y = as.formula(paste0('~', my_y)),
                                                        type = "bar", 
                                                        color = as.formula(paste0('~', groupVar))) %>%
                                                layout(xaxis = list(title = x_name),
                                                       yaxis = list(title = y_name))
                                
                                return(.data)
                                
                } else {
                                .data <- .data %>% 
                                                group_by(across({{ groupVar }})) %>%
                                                plot_ly(x = as.formula(paste0('~', my_x)), 
                                                        y = as.formula(paste0('~', my_y)),
                                                        type = "bar") %>%
                                                layout(xaxis = list(title = x_name),
                                                       yaxis = list(title = y_name))   
                                return(.data)
                }
}




createHeatMap <- function(.data, my_x, my_y, my_z, colors, x_name, y_name){
                
                .data <- .data %>% 
                                plot_ly(x = as.formula(paste0('~', my_x)), 
                                        y = as.formula(paste0('~', my_y)), 
                                        z = as.formula(paste0('~', my_z)), 
                                        type = "heatmap", 
                                        colors = colorRamp(colors)) %>% 
                                layout(xaxis = list(title = x_name), yaxis = list(title = y_name))
                
                return(.data)
                
}




# Execute -------------------------------------------------------------------------------------------------------------------

checkPackages()
remove(list.of.packages)
