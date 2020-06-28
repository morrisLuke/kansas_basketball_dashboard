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
                                choices = c("11-5 vs. Duke" = 401168155,
                                    "11-8 vs. UNCG" = 401169601,
                                     "11-15 vs. Monmouth" = 401169615,
                                    "11-19 vs. ETSU" = 401169621 ,
                                    "11-25 at Chaminade" = 401169630,
                                    "11-26 vs. BYU" = 401182627,
                                    "11-27 vs. Dayton" = 401182629,
                                    "12-7 vs. Colorado" = 401169646 ,
                                    "12-10 vs. Milwaukee" = 401169649,
                                    "12-14 vs. UMKC" = 401169657,
                                    "12-21 at Villanova" = 401169661,
                                    "12-29 at Stanford" = 401169669,
                                    "1-4 vs. West Virginia" = 401169679,
                                    "1-8 vs. at Iowa St." = 401169686,
                                    "1-11 vs. Baylor" = 401169688,
                                    "1-14 at Oklahoma" = 401169693,
                                    "1-18 at Texas" = 401169699,
                                    "1-21 vs. Kansas St." = 401169705,
                                    "1-25 vs. Tennessee" = 401169713,
                                    "1-27 at Oklahoma St." = 401169718,
                                    "2-1 vs. Texas Tech" = 401169726,
                                    "2-3 vs. Texas" = 401169729,
                                    "2-8 at TCU" = 401169733,
                                    "2-12 at West Virginia" = 401169741,
                                    "2-15 vs. Oklahoma" = 401169743,
                                    "2-17 vs. Iowa St." = 401169748,
                                    "2-22 at Baylor" = 401169753,
                                    "2-24 vs. Oklahoma St." = 401169759,
                                    "2-29 at Kansas St." = 401169764,
                                    "3-4 vs. TCU" = 401169772,
                                    "3-7 at Texas Tech" = 401169774
                                     ),
                                    selected = c(401168155, 401169601, 401169615, 
                                                 401169621, 401169630, 401182627, 
                                                 401182629, 401169646, 401169649,
                                                 401169657, 401169661, 401169669,
                                                 401169679, 401169686, 401169688,
                                                 401169693, 401169699, 401169705,
                                                 401169713, 401169718, 401169726,
                                                 401169729, 401169733, 401169741,
                                                 401169743, 401169748, 401169753,
                                                 401169759, 401169764, 401169772,
                                                 401169774)
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
                                    "0 - Marcus Garrett" = "Marcus Garrett",
                                    "1 - Devon Dotson" = "Devon Dotson",
                                    "2 - Christian Braun" = "Christian Braun",
                                    "3 - Dajuan Harris" = "Dajuan Harris",
                                    "4 - Isaiah Moss" = "Isaiah Moss",
                                    "5 - Elijah Elliott" = "Elijah Elliott",
                                    "10 - Jalen Wilson" = "Jalen Wilson",
                                    "12 - Chris Teahan" = "Chris Teahan",
                                    "13 - Tristan Enaruna" = "Tristan Enaruna",
                                    "20 - Michael Jankovich" = "Michael Jankovich",
                                    "22 - Silvio De Sousa" = "Silvio De Sousa",
                                    "30 - Ochai Agbaji" = "Ochai Agbaji",
                                    "33 - David McCormack" = "David McCormack",
                                    "35 - Udoka Azubuike" = "Udoka Azubuike",
                                    "44 - Mitch Lightfoot" = "Mitch Lightfoot"),
                        selected = "Devon Dotson"),
            
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
