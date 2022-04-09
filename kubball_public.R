library(extrafont)
library(shiny)
library(tidyverse)
library(ncaahoopR)

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("KU Dashboard"),

    # Sidebar with a checkbox input for game selection 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("dateBoxes", 
                               label = "Games to Include:",
                                choices = c("11-9 vs. Michigan St." = 401372138,
                                    "11-12 vs. Tarleton" = 401372250,
                                    "11-18 vs. Stony Brook" = 401370985,
                                    "11-25 vs. North Texas" = 401370152,
                                    "11-26 vs. Dayton" = 401376716,
                                    "11-28 vs. Iona" = 401376720,
                                    "12-3 at St. John's" = 401372016,
                                    "12-7 vs. UTEP" = 401372006,
                                    "12-11 vs. Missouri" = 401371875,
                                    "12-18 vs. SFA" = 401372251,
                                    "12-29 vs. Nevada" = 401398463,
                                    "1-1 vs. George Mason" = 401398910,
                                    "1-4 at Oklahoma St." = 401368359,
                                    "1-8 vs. at Texas Tech" = 401368364,
                                    "1-11 vs. Iowa St." = 401368368,
                                    "1-15 vs. West Virginia" = 401368372,
                                    "1-18 at Oklahoma" = 401368377,
                                    "1-22 at Kansas St." = 401368382,
                                    "1-24 vs. Texas Tech" = 401368387,
                                    "1-29 vs. Kentucky" = 401364899,
                                    "2-1 at Iowa St." = 401368394,
                                    "2-5 vs. Baylor" = 401368398,
                                    "2-7 at Texas" = 401368402,
                                    "2-12 vs. Oklahoma" = 401368408,
                                    "2-14 vs. Oklahoma St." = 401368412,
                                    "2-19 at West Virginia" = 401368420,
                                    "2-22 vs. Kansas St." = 401368423,
                                    "2-26 at Baylor" = 401368427,
                                    "3-1 at TCU" = 401368434,
                                    "3-3 vs. TCU" = 401412470,
                                    "3-5 vs. Texas" = 401368438,
                                    "3-10 vs. West Virginia" = 401405836,
                                    "3-11 vs. TCU" = 401405840,
                                    "3-12 vs. Texas Tech" = 401405842,
                                    "3-17 vs. Texas Southern" = 401408582,
                                    "3-19 vs. Creighton" = 401408611,
                                    "3-25 vs. Providence" = 401408627,
                                    "3-27 vs. Miami" = 401408632,
                                    "4-02 vs. Villanova" = 401408634,
                                    "4-04 vs. North Carolina" = 401408636
                                     ),
                                    selected = c(401372138,401372250,401370985,
                                                 401370152,401376716,401376720,
                                                 401372016,401372006,401371875,
                                                 401372251,401372252,401398463,
                                                 401398910,401368359,401368364,
                                                 401368368,401368372,401368377,
                                                 401368382,401368387,401364899,
                                                 401368394,401368398,401368402,
                                                 401368408,401368412,401368420,
                                                 401368423,401368427,401368434,
                                                 401412470,401368438,401405836,
                                                 401405840,401405842,401408582,
                                                 401408611,401408627,401408632,
                                                 401408634,401408636)
                        ),
            
            # Radio button input for weighting 3-pointers    
            radioButtons("weightButton", 
                         label = "ASSIST NETWORK OPTIONS \n
                         Weighted 3-pointers?",
                         choices = c("Yes" = TRUE,
                                     "No" = FALSE),
                         selected = TRUE
            ),
            
            # Numeric input box for involvement threshold
            numericInput("thresholdSelector", 
                         label = "Minimum Involvement % for Player Inclusion?",
                        min = 0,
                        max = 1,
                        step = 0.01,
                        value = 0.1
                        ),
            
            #Selection input box for highlighting a player
            selectInput("playerSelector",
                        label = "Player to highlight?",
                        choices = c("None" = NA,
                                    "0 - Bobby Pettiford" = "Bobby Pettiford",
                                    "1 - Joseph Yesufu" = "Joseph Yesufu",
                                    "2 - Christian Braun" = "Christian Braun",
                                    "3 - Dajuan Harris Jr." = "Dajuan Harris Jr.",
                                    "5 - Kyle Cuffe Jr." = "Kyle Cuffe Jr.",
                                    "10 - Jalen Wilson" = "Jalen Wilson",
                                    "11 - Remy Martin" = "Remy Martin",
                                    "12 - Chris Teahan" = "Chris Teahan",
                                    "13 - Charlie McCarthy" = "Charlie McCarthy",
                                    "15 - Dillon Wilhite" = "Dillon Wilhite",
                                    "20 - Michael Jankovich" = "Michael Jankovich",
                                    "21 - Zach Clemence" = "Zach Clemence",
                                    "24 - K.J. Adams" = "K.J. Adams",
                                    "30 - Ochai Agbaji" = "Ochai Agbaji",
                                    "31 - Cam Martin" = "Cam Martin",
                                    "33 - David McCormack" = "David McCormack",
                                    "44 - Mitch Lightfoot" = "Mitch Lightfoot",
                                    "55 - Jalen Coleman-Lands" = "Jalen Coleman-Lands"),
                        selected = "Ochai Agbaji"),
            
            #Radio button input for presenting shot chart as heatmap
            radioButtons("heatmapSelector",
                         label = "SHOT CHART OPTIONS \n
                         Heatmap?",
                         choices = c("Yes" = TRUE,
                                     "No" = FALSE)
                         )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                plotOutput("assistNetworkPlot")
            ),
            fluidRow(
                plotOutput("kuShotChart"),
                plotOutput("oppShotChart")
            )
        )
    )
)

# Define server logic required to draw a plots
server <- function(input, output) {

  output$assistNetworkPlot <-  renderPlot({
    ifelse(input$playerSelector != "NA", 
           {circle_assist_net(team = "Kansas", 
                              season = input$dateBoxes,
                              three_weights = input$weightButton,
                              threshold = input$thresholdSelector,
                              highlight_player = input$playerSelector,
                              highlight_color = "#0051BA",
                              message = "ASSIST NETWORK")},
           
           {circle_assist_net(team = "Kansas", 
                              season = input$dateBoxes,
                              three_weights = input$weightButton,
                              threshold = input$thresholdSelector,
                              message = "ASSIST NETWORK"
           )}
    )
  })
    
    output$kuShotChart <- renderPlot({
        team_shot_chart(game_ids = input$dateBoxes,
                        team = "Kansas",
                        heatmap = input$heatmapSelector)
    })
    
    output$oppShotChart <- renderPlot({
        opp_shot_chart(game_ids = input$dateBoxes,
                        team = "Kansas",
                        heatmap = input$heatmapSelector)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
