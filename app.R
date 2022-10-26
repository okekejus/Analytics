library("shiny")
library("DT")
library("fresh")
library("shinydashboard")
library("plotly")
library("googlesheets4")


mytheme <- create_theme(adminlte_color(light_blue = "#00AEEF"), 
                        adminlte_sidebar(width = "160px",
                                         dark_bg = "#dedfe0",
                                         dark_hover_bg = "#fcc75b",
                                         dark_color = "#2E3440"), 
                        adminlte_global(content_bg = "#ffffff"))


header <- dashboardHeader(title = "Analytics")


sidebar <- dashboardSidebar( 
                sidebarMenu(id = "sidebarmenu",
                            menuItem("Heat Map", tabName = "hmap"),
                            menuItem("PSW Utilization", tabName = "util") ,
                            menuItem("Risk Events", tabName = "risk"), 
                            menuItem("Rejection Tracker", tabName = "reject"),
                            menuItem("Communications", tabName = "communications")
                )
)


body <- dashboardBody(
                use_theme(mytheme),
                tabItems(tabItem(tabName = "hmap",
                                 fluidPage(tabsetPanel(
                                                 tabPanel("Weekly", 
                                                          h2("Weekly Heat Map"),
                                                          fluidRow(tabBox(title = "Offers", 
                                                                          id = "offer.tabset",
                                                                          height = "450px", 
                                                                          tabPanel("Accepted", plotlyOutput(outputId = "accepted.plot")),
                                                                          tabPanel("Rejected", plotlyOutput(outputId = "rejected.plot"))), 
                                                                   tabBox(title = "Assessments Compliance",
                                                                          id = "apr.tabset", 
                                                                          width = 6,
                                                                          height = "450px",
                                                                          tabPanel("Completed", plotlyOutput(outputId = "completed.assess")),
                                                                          tabPanel("Initial Assess. Past Due", plotlyOutput(outputId = "due.assess")), 
                                                                          tabPanel("APRs Past Due", plotlyOutput(outputId = "due.apr")))
                                                          ),
                                                          fluidRow(box(title = "Hours Countered", 
                                                                       width = 12, 
                                                                       plotlyOutput("hours.countered.plot"),
                                                                       footer = "(Hours Offered/Hours Rejected) * 100")
                                                                   ),
                                                          fluidRow(tabBox(title = "Heat Maps",
                                                                          id = "hmap.tabset", 
                                                                          width = 12, 
                                                                          height = "550px", 
                                                                          tabPanel("Acceptance Rate", plotlyOutput(outputId = "accepted.hmap")),
                                                                          tabPanel("Time/Gender Specific", plotlyOutput(outputId = "time.hmap")),
                                                                          tabPanel("No Staff", plotlyOutput(outputId = "staff.hmap")),
                                                                          tabPanel("Counter Offers", plotlyOutput(outputId = "counter.hmap")), 
                                                                          tabPanel("Incomplete Entries", plotlyOutput(outputId = "incomp.hmap")))
                                                          ), 
                                                          fluidRow(tabBox(title = "Missed Care", id = "mc.tabset", width = 12, height = "550px", 
                                                                          tabPanel("Count", plotlyOutput("mc.count")), 
                                                                          tabPanel("Themes", plotlyOutput("mc.themes"))), 
                                                                   box(title = "test",
                                                                       width = 12,
                                                                       tableOutput("test")))
                                                 ),
                                                 tabPanel("Monthly", 
                                                          h2("Monthly Heat Map"), 
                                                          fluidRow(tabBox(title = "Offers", 
                                                                          id = "monthly.offer.tabset",
                                                                          width = 12, 
                                                                          height = "550px", 
                                                                          tabPanel("Accepted", plotlyOutput("monthly.accepted")), 
                                                                          tabPanel("Refused", plotlyOutput("monthly.refused")), 
                                                                          tabPanel("APRs Due", plotlyOutput("monthly.apr.due")), 
                                                                          tabPanel("Initial Assessments", plotlyOutput("monthly.initial")))), 
                                                          fluidRow(tabBox(title = "Heat Maps", 
                                                                          id = "monthly.hmap.tabset", 
                                                                          width = 12, 
                                                                          height = "500px", 
                                                                          tabPanel("Acceptance Rate", plotlyOutput("monthly.acceptance.rate")), 
                                                                          tabPanel("Missed Care Rate", plotlyOutput("monthly.mc.rate")), 
                                                                          tabPanel("Falls per 1000 Clients", plotlyOutput("monthly.falls")), 
                                                                          tabPanel("Other Risk Events", plotlyOutput("monthly.rs")), 
                                                                          tabPanel("Staff Risk Events", plotlyOutput("monthly.staff.rs")), 
                                                                          tabPanel("Counter Offer Rate", plotlyOutput("monthly.counter.rate")), 
                                                                          tabPanel("Hours Countered", plotlyOutput("monthly.hours.countered")), 
                                                                          tabPanel("Assessments up to Date", plotlyOutput("monthly.atd")))))
                                 )
                                 )
                ), 
                
                tabItem(tabName = "risk",
                        fluidPage(fluidRow(h2("Risk and Safety Events")),
                                  fluidRow(box(title = "Risk Event Count", width = 12, plotlyOutput("risk.event.count"))),
                                  fluidRow(box(title = "Risk Events by Department", plotlyOutput(outputId = "risk.by.dep"), width = 6), 
                                           box(title = "Risk Events by Funder", plotlyOutput(outputId = "risk.by.funder"), width = 6)), 
                                  fluidRow(tabBox(title = "Event Details", id = "risk.tabset", width = 12,
                                                  tabPanel("Client Events", plotlyOutput(outputId = "client.events.plot")),
                                                  tabPanel("Staff Events", plotlyOutput(outputId = "staff.events.plot")))),
                                  fluidRow(box("Falls by Funder", plotlyOutput("falls.by.funder")))
                        )),
                
                tabItem(tabName = "reject",
                        fluidPage(fluidRow(h2("Rejection Tracker")),
                                  fluidRow(box(title = "Rejections <= 5 hours", width = 12, plotlyOutput(outputId = "rejection.rate.plot"))),
                                  fluidRow(tabBox(title = "Referral Acceptance",id = "acceptance.tabset",width = 12, 
                                                  height = "550px",
                                                  tabPanel("Daily Acceptance",plotlyOutput(outputId = 'daily.acceptance')),
                                                  tabPanel("Weekly Comparison",plotlyOutput(outputId = "weekly.acceptance.plot")))),
                                  fluidRow(tabBox(title = "Rejection Reasons", id = "reasons.tabset", width = 6, height = "550px", 
                                                  tabPanel("Overall", plotlyOutput(outputId = "rejection.overrall")),
                                                  tabPanel("<= 5")
                                  ),
                                  tabBox(title = "Rejections per HCCSS",id = "rejections.per.tabset",width = 6, height = "550px", 
                                         tabPanel("Overall"),
                                         tabPanel("<= 5"))
                                  ),
                                  fluidRow(box(title = "Rejections per SC" , width = 12, plotlyOutput(outputId = "rejection.by.sc"))
                                  )
                        )
                ),
                
                tabItem(tabName = "util",
                        fluidPage(fluidRow(h2("PSW Utilization")),
                                  fluidRow(box(title = "Target Hours", width = 12, plotlyOutput(outputId = "target.plot"),
                                               footer = "Target = (Total Scheduled PSW Hours/Total Target Hours) * 100")),
                                  fluidRow(box(title = "Resignations", width = 6, height = "500px", plotlyOutput(outputId = "monthly.resignations")),
                                           box(title = "Cohort Comparison", width = 6, plotlyOutput(outputId = "cohort.comparison"))),
                                  fluidRow(box(title = "Utilization", width = 6, height = "500px", dataTableOutput(outputId = "utilized")),
                                           box(title = "Under-utilized", width = 6,dataTableOutput(outputId = "under.utilized"), height = "500px"))
                        )
                ),
                tabItem(tabName = "communications",
                        fluidPage(tabsetPanel(tabPanel("Social Media",
                                                       fluidRow(h2("Social Media Dashboard")),
                                                       fluidRow(tabBox(title = "Interactions", id = "sm.tabset", width = 12,
                                                                       tabPanel("Likes", plotlyOutput("sm.likes")), 
                                                                       tabPanel("Posts", plotlyOutput("sm.posts")), 
                                                                       tabPanel("Impressions", plotlyOutput("sm.imp")), 
                                                                       tabPanel("Comments", plotlyOutput("sm.comments")),
                                                                       tabPanel("Shares", plotlyOutput("sm.shares")), 
                                                                       tabPanel("Views", plotlyOutput("sm.views")), 
                                                                       tabPanel("Clicks", plotlyOutput("sm.clicks"))))
                        ), 
                        tabPanel("Website"))
                        ) 
                )
                )
)


ui <- dashboardPage(header, sidebar, body)






server <- function(input, output, session){
                
                source('00_setup.R') 
                
                
                
                weekly.hmap <- reactivePoll(2000, session,
                                         checkFunc = function(){
                                                         checkLength(sheet_id, "WeeklyHeatMap")
                                                         }, 
                                         valueFunc = function(){
                                                         googlesheets4::read_sheet(sheet_id, "WeeklyHeatMap")
                                                         }
                                         )
                
                
                
                observeEvent(input$sidebarmenu, {
                                
                                
                                if(input$sidebarmenu == "hmap"){
                                                source("01_heatmap.R")
                                           
                                                
                                                output$accepted.plot <- renderPlotly({weekly.accepted})
                                                output$rejected.plot <- renderPlotly({weekly.refused})
                                                output$hours.countered.plot <- renderPlotly(hours.countered)
                                                
                                                output$accepted.hmap <- renderPlotly({acceptance.rate})
                                                output$time.hmap <- renderPlotly({weekly.time.spec})
                                                
                                                output$staff.hmap <- renderPlotly({weekly.no.staff})
                                                output$counter.hmap <- renderPlotly({counter.offer})
                                                output$incomp.hmap <- renderPlotly({incomplete.entries})
                                                
                                                output$due.assess <- renderPlotly({weekly.initial.due})
                                                output$completed.assess <- renderPlotly({weekly.completed.assess})
                                                output$due.apr <- renderPlotly({weekly.apr.due})
                                                output$mc.count <- renderPlotly(mc.count)
                                                output$mc.themes <- renderPlotly(mc.themes)
                                                
                                                
                                                output$monthly.accepted <- renderPlotly(monthly.accepted)
                                                output$monthly.refused <- renderPlotly(monthly.refusals)
                                                output$monthly.acceptance.rate <- renderPlotly(monthly.acceptance.rate)
                                                output$monthly.mc.rate <- renderPlotly(monthly.mc.rate)
                                                output$monthly.falls <- renderPlotly(monthly.falls)
                                                output$monthly.rs <- renderPlotly(monthly.rs)
                                                output$monthly.staff.rs <- renderPlotly(monthly.staff.rs)
                                                output$monthly.counter.rate <- renderPlotly(monthly.counter.rate)
                                                output$monthly.hours.countered <- renderPlotly(monthly.hours.countered)
                                                output$monthly.atd <- renderPlotly(monthly.atd)
                                                output$monthly.apr.due <- renderPlotly(monthly.apr.due)
                                                output$monthly.initial <- renderPlotly(monthly.initial)
                                                
                                } else if (input$sidebarmenu == "risk"){
                                                source("03_risk_and_safety.R")
                                                output$risk.by.dep <- renderPlotly(risk.by.dep)          
                                                output$risk.by.funder <- renderPlotly(risk.by.funder)
                                                output$client.events.plot <- renderPlotly(client.events.plot)
                                                output$staff.events.plot <- renderPlotly(staff.events.plot)
                                                output$falls.by.funder <- renderPlotly(falls.by.funder)
                                                output$risk.event.count <- renderPlotly(risk.event.count)
                                                
                                } else if (input$sidebarmenu == "reject"){
                                                source("04_rejection_tracker.R")
                                                
                                                output$rejection.rate.plot <- renderPlotly(rejection.rate.plot)
                                                output$rejection.overrall <- renderPlotly(all.rej.reason.pie)
                                                output$daily.acceptance <- renderPlotly(daily.acceptance)
                                                output$weekly.acceptance.plot <- renderPlotly(weekly.acceptance.plot)
                                                
                                                output$rejection.by.sc <- renderPlotly(rejection.by.sc)
                                } else if (input$sidebarmenu == "util"){
                                                source("02_psw_utilization.R")
                                                
                                                output$under.utilized <- renderDataTable(under.utilized)
                                                output$utilized <- renderDataTable(utilized)
                                                output$target.plot <- renderPlotly(target)
                                                output$cohort.comparison <- renderPlotly(cohort.comparison)
                                                output$monthly.resignations <- renderPlotly(monthly.resignations)
                                                
                                                
                                } else {
                                                source("06_communications.R") 
                                                output$sm.likes <- renderPlotly(sm.likes)
                                                output$sm.posts <- renderPlotly(sm.posts)
                                                output$sm.imp <- renderPlotly(sm.imp)
                                                output$sm.comments <- renderPlotly(sm.comments)
                                                output$sm.views <- renderPlotly(sm.views)
                                                output$sm.clicks <- renderPlotly(sm.clicks)
                                                output$sm.shares <- renderPlotly(sm.shares)
                                }
                })
                
                
                
}


shinyApp(ui, server)

