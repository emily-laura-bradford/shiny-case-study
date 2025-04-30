# Emily Bradford
# ESCI 599: App Development
# April 30, 2025

# Assignment: ER Case Study

# ----------

# packages
library(shiny)
library(shinythemes)
library(vroom) # fast file reading
library(tidyverse) # general data analysis

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
injuries$sex <- as.factor(str_to_sentence(injuries$sex))
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
  # name and date
  "Author: Emily Bradford",
  br(),
  "Date: April 30, 2025",
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
    column(2, selectInput("y", "Y axis", c("rate", "count")))
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
  "Plot of injuries by age and sex",
  fluidRow(
    # displays the age/sex plot
    column(12, plotOutput("age_sex"))
  ),
  # display narrative if user chooses
  fluidRow(
    # button called "story" labeled "Tell me a story"
    column(2, actionButton("story", "Tell me a story")),
    # output narrative as plain text
    column(10, textOutput("narrative"))
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
  output$diag <- renderTable(count_top(selected(), diag,
                                       title = "Diagnosis"),
                             width = "100%")
  
  # table of body parts
  # set to maximum width
  output$body_part <- renderTable(count_top(selected(), body_part,
                                            title = "Body part"),
                                  width = "100%")
  
  # table of locations
  # set to maximum width
  output$location <- renderTable(count_top(selected(), location,
                                           title = "Location"),
                                 width = "100%")
  
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
      summary() %>%
        # simple line plot with age on x-axis, count on y-axis, colored by sex
        ggplot(aes(age, n, colour = sex)) %>%
        + geom_line() %>%
        # legend label
        + scale_color_discrete(name = "Sex") %>%
        # labels
        + labs(y = "Estimated number of injuries",
               x = "Age")
    # conditional on the user choosing to display rate
    } else {
      summary() %>%
        # same plot as above, but y is rate and y-axis title is different
        ggplot(aes(age, rate, colour = sex)) %>%
        + geom_line(na.rm = TRUE) %>%
        + scale_color_discrete(name = "Sex") %>%
        + labs(y = "Injuries per 10,000 people",
               x = "Age")
    }
    # default res
  }, res = 96)
  
  # reactive function: grabs a narrative from the data
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selected() %>%
      # pull from narrative column
      pull(narrative) %>%
      # grabs a single random narrative
      sample(1)
  )
  # define narrative output; rendered in plain text
  output$narrative <- renderText(narrative_sample())
}

# ----------

# run app
shinyApp(ui, server)