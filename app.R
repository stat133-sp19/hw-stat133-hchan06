#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(reshape2)

#' @title future_value
#' @description computes the future value of an investment
#' @param amount numeric vector of the initial invested amount
#' @param rate numeric vector of the annual rate of return
#' @param years numeric vector of the number of years
#' @return future value of an investment
future_value <- function(amount, rate, years) {
  return(amount*(1 + rate)^years)
}

#' @title annuity
#' @description computes the future value of annuity
#' @param contrib numeric vector of the contributed amount
#' @param rate numeric vector of the annual rate of return
#' @param years numeric vector of the number of years
#' @return future value of annuity
annuity <- function(contrib, rate, years) {
  return(contrib*(((1 + rate)^years - 1)/rate))
}

#' @title growing_annuity
#' @description computes the future value of growing annuity
#' @param contrib numeric vector of the contributed amount
#' @param rate numeric vector of the annual rate of return
#' @param growth numeric vector of annual growth rate
#' @param years numeric vector of the number of years
#' @return future value of growing annuity
growing_annuity <- function(contrib, rate, growth, years) {
  return(contrib*(((1+rate)^years - (1+growth)^years)/(rate-growth)))
}


ui <- fluidPage(
  
  titlePanel("Savings Simulation"),
  
  
  fluidRow(column(4,
                  sliderInput(inputId = "amount", label = "Initial Amount",
                              min = 0, max = 100000, step = 500, value = 1000),
                  sliderInput(inputId = "contrib", label = "Annual Contribution",
                              min = 0, max = 50000, step = 500, value = 2000)),
           column(4,
                  sliderInput(inputId = "return", label = "Return Rate (in %)",
                              min = 0, max = 20, step = 0.1, value = 5),
                  sliderInput(inputId = "growth", label = "Growth Rate (in %)",
                              min = 0, max = 20, step = 0.1, value = 2)),
           column(4,
                  sliderInput(inputId = "years", label = "Years",
                              min = 0, max = 50, step = 1, value = 20),
                  selectInput(inputId = "facet", label = "Facet?", 
                              choices = c("No", "Yes"), selected = "No"))),
  
  hr(),
  
  h4("Timelines"),
  plotOutput("timeline"),
  
  h4("Balances"),
  verbatimTextOutput("balance")
)

server <- function(input, output) {
  
  dat <- reactive({
    no_contrib <- rep(0, input$years)
    fixed_contrib <- rep(0, input$years)
    growing_contrib <- rep(0, input$years)
    
    
    for(i in 0:input$years) {
      no_contrib[i + 1] <- future_value(amount = input$amount, rate = input$return/100, years = i)
      fixed_contrib[i + 1] <- no_contrib[i + 1] + annuity(contrib = input$contrib, rate = input$return/100, years = i)
      growing_contrib[i + 1] <- no_contrib[i + 1] + growing_annuity(contrib = input$contrib, rate = input$return/100, growth = input$growth/100, years = i)
    }
    
    dat <- data.frame(
      year = 0:input$years,
      no_contrib = no_contrib,
      fixed_contrib = fixed_contrib,
      growing_contrib = growing_contrib
    )
    
    return(dat)
    
  })
  
  
  output$timeline <- renderPlot({
    
    if(input$facet == "No") {
      ggplot(data = dat()) +
        geom_point(aes(x = year, y = no_contrib, color = "no_contrib")) + geom_line(aes(x = year, y = no_contrib, color = "no_contrib"), size = 1) +
        geom_point(aes(x = year, y = fixed_contrib, color = "fixed_contrib")) + geom_line(aes(x = year, y = fixed_contrib, color = "fixed_contrib"), size = 1) +
        geom_point(aes(x = year, y = growing_contrib, color = "growing_contrib")) + geom_line(aes(x = year, y = growing_contrib, color = "growing_contrib"), size = 1) +
        scale_color_manual(values = c("no_contrib" = "#F8766D", "fixed_contrib" = "#068439", "growing_contrib" = "#659DFE"), breaks = c("no_contrib", "fixed_contrib", "growing_contrib")) +
        labs(title = "Three modes of investing", x = "Years", y = "Dollars", color = "Modalities")
    }else {
      dat2 <- melt(data = dat(), id = "year")
      
      ggplot(data = dat2, aes(x = year, y = value, color = variable)) +
        geom_area(aes(fill = variable), alpha = 0.4) + geom_point() + geom_line() +
        labs(title = "Three modes of investing", x = "Years", y = "Dollars", fill = "Modalities", color = "Modalities") +
        facet_wrap(.~variable)
    }
  })
  
  output$balance <- renderPrint({
    print(dat())
  })
}


# Run the application 
shinyApp(ui = ui, server = server)



