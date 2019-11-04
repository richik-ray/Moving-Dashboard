#import calls
library(shiny)
library(readxl)
library(tidyverse)
source("../gghelper.R")

#data-handling
database <- read_excel("../findata.xlsx")
database <- subset(database, !is.na(database$Value))
housingdatabase <- read_excel("../hous.xlsx")
healthDatabase <- read_excel("../hosp.xlsx")

#ui
ui <- fluidPage(
  titlePanel("What is your new area like?"),

  sidebarLayout(
    sidebarPanel(position = "left",
      helpText("Choose what you want to examine below."),

      selectInput("indic",
        label = "Which indicator?",
        choices = c("Education", "Crime Rate", "Climate", "Housing Cost", "Political", "Healthcare"),
        selected = "Education"),

      selectInput("graph",
        label = "Which graph?",
        choices = c("Histogram", "Line"),
        selected = ""),

      conditionalPanel(condition = "input.graph == 'Histogram'",
        sliderInput(inputId = "bins",
                    label = "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30)
      ),

      conditionalPanel(condition = "input.graph == 'Line' || input.graph == 'Histogram'",
        selectInput("stateOrNat",
        label = "Which Counties",
        choices = c("All Counties", "In my State"),
        selected = "In my State")
      )
    ),
    mainPanel(
      conditionalPanel(condition = "input.graph == 'Histogram' && (input.indic == 'Education' || input.indic == 'Crime Rate' || input.indic == 'Political' || input.indic == 'Housing Cost' || input.indic == 'Climate')",
        textOutput("histogramdesc"),
        plotOutput("histogram")),
      conditionalPanel(condition = "input.graph == 'Line' && input.indic == 'Housing Cost'",
        textOutput("histogramhoustext"),
        plotOutput("housingline")),
      conditionalPanel(condition = "input.graph == 'Line' && input.indic != 'Housing Cost'",
        textOutput("housingtext")),
      conditionalPanel(condition = "input.indic == 'Healthcare'",
        textOutput("Healthtext"),
        plotOutput("Healthcare"))
    )
  )
)

#server
server <- function(input, output) {


  output$histogramdesc <- renderText({
    txt <- readtext("../txt.txt")
    county <- txt$text[1]
    paste("Below, all counties in the United States are graphed. Your county, ", county, "is at ", getEdu(database, county, input$indic))
  })


  output$histogram <- renderPlot({
    txt <- readtext("../txt.txt")
    county <- txt$text[1]
    switch(input$indic,
      "Education" = database <- subset(database, database$Type == "GradRate"),
      "Crime Rate" = database <- subset(database, database$Type == "Crime"),
      "Political" = database <- subset(database, database$Type == "Political"),
      "Climate" = database <- subset(database, database$Type == "Temp"),
      "Housing Cost" = database <- subset(database, database$Type == "HousPrices")
    )
    if (input$stateOrNat == "In my State") {
      daf <- subset(database, State == rightSt2(county))
      x <- as.numeric(daf$Value)
    } else {
      x <- as.numeric(database$Value)
    }
    str <- switch(input$indic,
     "Education" = "High School Graduation Rate",
     "Crime Rate" = "Crime Rate per 100,000",
     "Political" =  "2016 Perc. Differential",
     "Climate" = "Average Temperature",
     "Housing Cost" = "Average Housing Cost"
    )
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#75AADB", border = "white",
       xlab = str,
       main = paste("Histogram of ", str))
  })


  output$housingline <- renderPlot({
    txt <- readtext("../txt.txt")
    county <- txt$text[1]
    makeLine(housingdatabase, county, input$stateOrNat == "In my State")
  })


  output$housingtext <- renderText  ({
    paste("There is no line graph available for this indicator.")
  })


  output$histogramhoustext <- renderText  ({
    paste("The blue line represents the county you are looking at.")
  })

  output$Healthtext <- renderText ({
    paste("The graph will only appear if more than one hospital exists")
  })

  output$Healthcare <- renderPlot ({
    txt <- readtext("../txt.txt")
    county <- txt$text[1]
    dof <- subset(healthDatabase, id == county)
    print(dof)
    plot <- ggplot(dof, aes(x = facname, y = rec)) +
      geom_bar(stat = "identity") +
      theme(panel.background = element_blank(),
        panel.grid = element_line(color = "gray75"),
        panel.spacing = margin(6,6,6,6, "mm"),
        plot.margin = margin(6,6,6,6, "mm"),
        axis.title.y = element_text(size=18, color = "black"),
        axis.title.x = element_text(size=18, color = "black"),
        axis.text = element_text(size = 15),
        axis.text.x = element_blank(),
        title = element_text(size = 22)) +
      labs(x = "Facility Name", y = "Survey Results")
  })

}

#finish
shinyApp(ui = ui, server = server)
