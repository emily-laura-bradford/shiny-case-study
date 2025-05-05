# Emily Bradford
# ESCI 599: App Development
# May 5, 2025

# Assignment: ER Case Study II - Updated

# ----------

# packages
library(shiny)
library(shinythemes)
library(vroom) # fast file reading
library(tidyverse) # general data analysis
library(shinyWidgets) # for line/smooth toggle
library(shinybusy) # add busy indicator

# ----------

# load data

# note: data were downloaded using the console so they don't get downloaded
# every time the app is run

# NEISS injuries data from 2017
injuries <- vroom("neiss//injuries.tsv.gz")
# NEISS products data from 2017
products <- vroom("neiss//products.tsv")
# NEISS population data from 2017
population <- vroom("neiss//population.tsv")
# capitalize sex
injuries$sex   <- as.factor(str_to_sentence(injuries$sex))
population$sex <- as.factor(str_to_sentence(population$sex))

# extract product codes
prod_codes <- setNames(products$prod_code, products$title)

# function to count top several variables, summarize others
count_top <- function(df, var, n = 7, title) {
  df2 <- df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    # convert to integer since it's population
    # results in table with two rows: category as row 1, count as row 2
    summarise(n = as.integer(sum(weight)))
  # add thousands separators for my sanity
  df2[,2] <- sapply(df2[,2], FUN = function(x) prettyNum(x, big.mark=","))
  # clearer labels for table
  colnames(df2) <- c(title, "Cases")
  return(df2)
}

# ----------

# define UI
ui <- fluidPage(
  # set theme
  theme = shinytheme("slate"),
  # busy indicator,
  add_busy_spinner(spin = "fading-circle"),
  # name and date
  "Author: Emily Bradford",
  br(),
  "Date: May 5, 2025",
  # header/title
  titlePanel("ER Case Study App"),
  br(),
  # input boxes
  fluidRow(
    # input 1: select product
    column(6,
           # input is called "code" and the label displayed to users is "Product"
           # the user can pick any of the product codes as the input
           selectInput("code", "Product",
                       choices = prod_codes,
                       width = "100%")
           ),
    # input 2: select whether to show rate or raw count
    # input is called "y" and the label displayed to users is "Y axis"
    column(2, selectInput("y", "Y axis", c("rate", "count"))),
    # input 3: select number of rows in table
    # input is called "row_no" and the label displayed to users is "Number of rows"
    column(2, numericInput("row_no", "Number of rows",
                           # default value = 7, min = 2, max = 10
                           # less than 2 is not a useful table (would just be "other" for all I think)
                           # more than 10 is not really readable or useful
                           value = 7, min = 2, max = 10))
  ),
  # three tables, each of which is 1/3 as wide as the plot
  fluidRow(
    # table 1: diagnosis
    column(4, tableOutput("diag")),
    # table 2: body part
    column(4, tableOutput("body_part")),
    # table 3: location
    column(4, tableOutput("location"))
  ),
  br(),
  fluidRow(
    column(8, strong("Plot of injuries by age and sex")),
    # allows user to choose whether the plot is a line plot or a smooth plot
    column(4, prettyToggle("smoothen", label_on = "Smooth plot", label_off = "Line plot"))
  ),
  fluidRow(
    # displays the age/sex plot
    column(12, plotOutput("age_sex"))
  ),
  # display narrative if user chooses
  fluidRow(
    # button called "story" labeled "Tell me a story"
    column(2, actionButton("story", "Tell me a story")),
    # first button
    column(1, actionButton("first", "<<")),
    # back button
    column(1, actionButton("back", "<")),
    # output narrative as plain text
    column(6, textOutput("narrative")),
    # forward button
    column(1, actionButton("forward", ">")),
    # last button
    column(1, actionButton("last", ">>"))
  )
)

# define server
server <- function(input, output, session) {
  # reactive expression; assigns reactive variable "selected" to a table
  #    which contains only those rows where the product code is the one
  #    the user selected using selectInput
  selected <- reactive(
    injuries %>%
      filter(prod_code == input$code)
    )
  
  # table of diagnoses
  # set to maximum width
  output$diag <- renderTable(count_top(selected(), diag, n = input$row_no,
                                       title = "Diagnosis"),
                             width = "100%")
  
  # table of body parts
  # set to maximum width
  output$body_part <- renderTable(count_top(selected(), body_part, n = input$row_no,
                                            title = "Body part"),
                                  width = "100%")
  
  # table of locations
  # set to maximum width
  output$location <- renderTable(count_top(selected(), location, n = input$row_no,
                                           title = "Location"),
                                 width = "100%")
  
  # --- plot section ---
  
  # reactive function to summarize data by age and sex
  summary <- reactive({
    selected() %>%
      # count weighted by U.S. pop
      count(age, sex, wt = weight) %>%
      # join count data to population data
      left_join(population, by = c("age", "sex")) %>%
      # determine rate
      mutate(rate = n / population * 1e4)
  })
  
  # create output plot
  output$age_sex <- renderPlot({
    # conditional on the user choosing to display raw count
    if (input$y == "count") {
      # conditional on the user choosing to display a smooth plot
      if (input$smoothen == TRUE) {
        summary() %>%
          # simple smoothed plot with age on x-axis, count on y-axis, colored by sex
          ggplot(aes(age, n, colour = sex)) %>%
          + stat_smooth(se = FALSE, span = 0.5) %>%
          # legend label
          + scale_color_discrete(name = "Sex") %>%
          # labels
          + labs(y = "Estimated number of injuries",
                 x = "Age") %>%
          # theme
          + theme_light()
      } else {
        # same plot as above, but line rather than smooth
        ggplot(aes(age, rate, colour = sex)) %>%
          + geom_line() %>%
          + scale_color_discrete(name = "Sex") %>%
          + labs(y = "Estimated number of injuries",
                 x = "Age") %>%
          + theme_light()
      }
    # conditional on the user choosing to display rate
    } else {
      # conditional on the user choosing to display a smooth plot
      if (input$smoothen == TRUE) {
        summary() %>%
          # same plot as above, but y is rate and y-axis title is different
          ggplot(aes(age, rate, colour = sex)) %>%
          + stat_smooth(na.rm = TRUE, se = FALSE, span = 0.5) %>%
          + scale_color_discrete(name = "Sex") %>%
          + labs(y = "Injuries per 10,000 people",
                 x = "Age") %>%
          + theme_light()
      } else {
        summary() %>%
          # same plot as above, but y is rate and y-axis title is different
          ggplot(aes(age, rate, colour = sex)) %>%
          + geom_line(na.rm = TRUE) %>%
          + scale_color_discrete(name = "Sex") %>%
          + labs(y = "Injuries per 10,000 people",
                 x = "Age") %>%
          + theme_light()
      }
    }
    # default res
  }, res = 96)
  
  # --- narrative section ---
  
  # store maximum number of stories
  max_stories <- reactive(length(selected()$narrative))
  
  # save current position in list of stories; default is 1
  story <- reactiveVal(1)
  
  # picks the first story when the user changes the product code
  observeEvent(input$code, {
    story(1)
  })
  
  # picks a new random story when the user clicks the "tell me a story" button
  observeEvent(input$story, {
    story(sample(c(1:max_stories()), 1))
  })
  
  # sets "story" to 1 when the user clicks on the first button
  observeEvent(input$first, {
    story(1)
  })
  
  # increments "story" by 1 every time the user clicks on the forward button
  observeEvent(input$forward, {
    # i don't fully understand what the modulo is doing here
    # but somehow it makes it possible for the buttons to loop,
    # i.e. hitting "previous" when the first is selected shows the last
    # and hitting "next" when the last is selected shows the first
    story((story() %% max_stories()) + 1)
  })
  
  # increments "story" by -1 every time the user clicks on the back button
  observeEvent(input$back, {
    # see above in re: modulo mystery
    story(((story() - 2) %% max_stories()) + 1)
  })
  
  # sets "story" to max value when the user clicks on the last button
  observeEvent(input$last, {
    story(max_stories())
  })
  
  # define narrative output; rendered in plain text
  output$narrative <- renderText({
    selected()$narrative[story()]
  })
}

# ----------

# run app
shinyApp(ui, server)