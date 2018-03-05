#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load required libraries

library(shiny)
library(tidyverse)
library(googlesheets)
library(jsonlite)
library(magrittr)


# Read current list of people attending the "Happy New YeaR!" CRUG meetup
# To make the app read an actusal API, uncomment the readJSON and delete the readRDS line

# listFromAPI<-read_json(" ")

listFromAPI <- readRDS("C:/Users/CMC/Documents/GitHub/Jan2017MeetupShiny/Workshop/ll.rds")

# extract member info for people attending event (will return both attending and not attending)
# Returns a list of lists
memberInfo <- listFromAPI %>% map("member")

# Turn list of lists into tibble (tidyverse version of a data frame)
memberInfo %<>% map(~unlist(., recursive = F)) %>% bind_rows()

memberInfo <- bind_cols(photo.photo_link=NA, name=NA)



# Define UI for application
ui <- fluidPage(

  # Application title
  titlePanel("Making Small Working Groups"),
  h3(tags$strong("Purpose:")),
  h5("This Shiny App groups survey responses from a Google Form,
     and tries to associate respones by name to a Meetup.com profile picture"),
  h5("API calls and number of groups are dynamic"),
  h3("Challenge:"),
  h5("Can you add a button that will shuffle groups?"),
  h5("Can you add skill-summary graph within each group column?"),
  h5("Can you use clustering instead of simple cutoffs?"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      uiOutput("splitGroupsUI"),
      uiOutput("binUI")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      # Use imageOutput to place the image on the page
          #tableOutput('mytable')
      uiOutput("htmlTable"),
      plotOutput("pp")
    )
  )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {

  responses <- reactive({
    # Read in fake data
     readRDS("responses.RDS")

    # Real data would be accessed with code like that commented out below and deleting the readRDS
    # surveyResponses <- gs_key(" Insert Key Here ")
    # surveyResponses %>%  gs_read(ws = "Form Responses 1")

  })


  output$splitGroupsUI <- renderUI({

      sliderInput("splitGroups",
                    "Experience Level at which to Split Groups",
                    min = 1,
                    max = max(responses()$`Experience with Shiny`),
                    value = 1,
                    step=1)
  })


  maxBins <- reactive ({
      responses() %>% filter(`Experience with Shiny`>= input$splitGroups) %>% nrow
  })


  output$binUI <- renderUI({
      sliderInput("bins",
                "Number of Groups to Make:",
                min = 1,
                max = maxBins(),
                value = 1,
                step=1)
  })


  namesAndPhotos <- reactive({

     # Fuzzy string match, but non-matches are returned as "integer(0)"
           # Change "integer(0)" to NA
      fuzzyGrep <- responses()$`What is your name? (First and Last)` %>% map(~agrep(., memberInfo$name)) %>% map_int(1,.null=NA)

      # We needed NA instead of NULL, to keep the vector length consistent
      photosVector <- memberInfo$photo.photo_link[fuzzyGrep]
      namesVector <- responses()$`What is your name? (First and Last)`

      # This is what a call to namesAndPhotos() will return. A list of length 2, containing a vector
      # of names and a vector of photos, in matching order
      bind_cols("What is your name? (First and Last)" = namesVector, photos=photosVector)
  })


  output$htmlTable <- renderUI({

    # Combine the two tibbles (google form data and meetup API data) by participant name
    namesAndPhotosJoined <- left_join(namesAndPhotos(),responses(), by="What is your name? (First and Last)")

    # If the user selects to split into more than one group (bin) then make make groups
    if(input$bins > 1){

      # For simplicity we only recognize two groups of participants (those above the cutoff andd those below the cutoff)
      aboveSplit <- namesAndPhotosJoined %>% filter(`Experience with Shiny` >= input$splitGroups)
      belowSplit <- namesAndPhotosJoined %>% filter(`Experience with Shiny` <  input$splitGroups)

      # Create random vectors representing groups (ie c(1,2,3) if 3-groups are wanted) and repeat this vector
      # as long as there are participants in a skill level (aboveSplit and belowSplit)
      # Lastly, randomize the vector so that we aren't just doing alphabetical order by people's names
      groupIndices <- c(1:input$bins)
      randomizedAboveSplit <- sample(rep(groupIndices,times=ceiling(nrow(aboveSplit)/length(groupIndices))))
      randomizedAboveSplit <- sample(randomizedAboveSplit[1:nrow(aboveSplit)])

      randomizedBelowSplit <- rep(groupIndices,times=ceiling(nrow(belowSplit)/length(groupIndices)))
      randomizedBelowSplit <- sample(randomizedBelowSplit[1:nrow(belowSplit)])

      # This modifies belowSplit into a list of tibbles, with each list element representing a group
      belowSplit %<>%  mutate(Group=randomizedBelowSplit) %>% split(.$Group)
      # This modifies aboveSplit into a list of tibbles, with each list element representing a group
      aboveSplit %<>% mutate(Group=randomizedAboveSplit) %>% split(.$Group)

      # Combines aboveSplit and belowSplit for each group
      groups <- lapply(1:length(aboveSplit),function(x){
                  bind_rows(aboveSplit[x],belowSplit[x])
               })

    }else{
      # If there is only one group to be made:
      groups <- list(namesAndPhotosJoined)
    }


    # Unecessary, but simplify groups to only contain name and photo columns
    groups %<>%  map(~.[1:2])


   # htmlTableData() and htmlTableHead() are functions could totally live before the ui (ie outside the reactive environment)
   # However, when apps become larger, I like to kee related functions together unless used widely throughout an app,
   # This is personal preference

    # This one might be intimidating... What it does is loop through each member of a group, creating an HTML table with
    # a row, per member, and two columns: name in one column and photo in the second.
    # First we create a bunch of tables each containing a group of participants in the "htmlTableData" function
    # Then we add table headers for each group ("Names", "Photos")
    # Then  we insert the each HTML table of groups into the columns of a big HTML table to organize the groups on the screen

    # htmlTableData output is a list of HTML tables, each element/table is a group of participants
    htmlTableData <- function(inputGroup){lapply(1:nrow(inputGroup), function(x){
                                           # Initiate a row object of an HTML table
                                          tags$tr(style = "border: 1px solid black",
                                                   # First column is participant name
                                                  tags$td(pull(inputGroup, "What is your name? (First and Last)")[x]),
                                                   # Second column is photo if found, if not "whoAmI.jpg" is inserted,
                                                   # which lives in www directory whichitself is in the same directory as app.R
                                                  tags$td(tags$img(src=if(is.na(pull(inputGroup,photos)[x])){
                                                          "whoAmI.jpg"   }else{
                                                      pull(inputGroup,photos)[x]
                                                    }
                                                    , width = "100px", height = "100px"))
                                          )
                                        })}


    # This  function adds headers to the group HTMLtables
    htmlTableHead <- function(inputDataHTML){
                          tags$td(
                            tags$table(style = "border: thin solid; padding: 1%; width: 100%;",
                                       tags$td(
                                         tags$tr(style = "border: 1px solid black",
                                                 tags$th("Names"),
                                                 tags$th("Photo")
                                         ),
                                         # Insert group table (this is loopes )
                                         htmlTableData(inputDataHTML)
                          )))
                    }


    # This is the function that initiates all the crazy HTML table stuff above. The output is a list.
    # Each list element contains the HTML table for a group
    innerHtmlTables <- lapply(groups,htmlTableHead)

    # Create Group headers for the main "organizing" HTML table
    groupHeaders <- lapply(1:input$bins, function(x){ tags$th(paste0("Group ", x) )})

    # Create the main "organizing" HTML table, which places each group HTML table into a column with header of "Group XX"
    mainHtmlTable <- tags$table(style = "border: thin solid; padding: 1%; width: 100%;",
                  tags$tr(style = "border: 1px solid black",
                          groupHeaders
                          ),

                  innerHtmlTables
                  )
    # The output of htmlTable() is the value of "mainHtmlTable", as created above
    mainHtmlTable
  })


  output$pp <- renderPlot(
                  plot(jitter(x=responses()$`Experience with R`,0.5), y=responses()$`Experience with Shiny`,
                       xlim=c(min(responses()$`Experience with R`)-1,max(responses()$`Experience with R`)+1),
                       ylim=c(min(responses()$`Experience with Shiny`)-1,max(responses()$`Experience with Shiny`)+1))
               )




}

# Run the application
shinyApp(ui = ui, server = server)

