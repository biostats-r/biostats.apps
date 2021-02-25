#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# TODO
# Move R2 to figure annotation?
# recode term column
# Replot diagnostic plots. Maybe combine data and facet.
# Make into function


#' @importFrom dplyr summarise bind_rows

library(tidyverse)
library(broom)
library(ggfortify)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Exploring influential and leverage points"),
    fluidRow(
        column(width = 3,
        radioButtons("choice", "diagnostic to plot", 
                     choiceNames = c("Residual vs. Fitted", "Normal Q-Q", "Scale-Location", "Cook's distance", "Residual vs Leverage", "Cook's distance vs Leverage"),
                    choiceValues = 1:6)
        ),
        column(width = 9,
            plotOutput("rawPlot", click = "plot_click")
        )
    ),
    fluidRow(
        tableOutput("effect_table"),
        tableOutput("performance_table"),
    ),
    fluidRow(
        column(width = 6, 
               plotOutput("diagnostic_black")
        ),
        column(width = 6, 
               plotOutput("diagnostic_red")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

 plot_data <- reactiveVal({
     t1 <- tibble(x = 1:10, y = x + rnorm(10), colour = "black") 
     t2 <- summarise(t1, x = mean(x), y = mean(y), colour = "red") 
     bind_rows(t2, t1)
 })
    
 observeEvent(input$plot_click, {
     old <- plot_data()
     old[1, 1:2] <- list(input$plot_click$x, input$plot_click$y)
     plot_data(old)
 })
 
 #fit models
 mods <-  reactive({
     mod_all <- lm(y ~ x, data = plot_data())
     mod_black <- lm(y ~ x, data = plot_data(), subset = colour == "black")
     list(mod_all = mod_all, mod_black = mod_black)
     }
 )
 
 #plot data
 output$rawPlot <- renderPlot({
     ggplot(plot_data(), aes(
         x = x,
         y = y,
         colour = I(colour)
     )) +
         geom_point() +
         geom_smooth(
             method = "lm",
             formula = y ~ x,
             mapping = aes(group = 1),
             colour = "red",
             fill = "red"
         ) +
         geom_smooth(
             method = "lm",
             formula = y ~ x,
             data = plot_data() %>% filter(colour != "red"),
             colour = "black"
         ) +
         scale_x_continuous(limits = c(0, 20), expand = c(0, 0)) + 
         scale_y_continuous(limits = c(0, 20), expand = c(0, 0))
     
 })
 
 #output tables
 output$effect_table <- renderTable({
     bind_rows(
       red = tidy(mods()$mod_all),
       black = tidy(mods()$mod_black),
       .id = "colour"
 ) %>% select(-statistic)
     })
 
 output$performance_table <- renderTable({
     bind_rows(
         red = glance(mods()$mod_all),
         black = glance(mods()$mod_black),
         .id = "colour"
     )
 })
 
 #diagnostic plots
 output$diagnostic_black <- renderPlot(autoplot(mods()$mod_black, which = as.numeric(input$choice)))
 output$diagnostic_red <- renderPlot(autoplot(mods()$mod_all, which = as.numeric(input$choice)))
 
 
}

# Run the application 
shinyApp(ui = ui, server = server)
