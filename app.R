#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(tidyverse)
library(shiny)

s_df <- read_csv('esm244_students_w24.csv') %>%
  mutate(img = ifelse(is.na(pic), 'blank.jpg', paste0(sis_user_id, '.jpg')))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ESM244 Student Rand-O-matic!"),

    sidebarLayout(
        sidebarPanel(
          # checkbox to randomize or sort
          actionButton(inputId = 'dice',
                       label = 'Randomize!', icon = icon('dice')),
          checkboxInput(inputId = 'show_pic',
                        label = 'Show pic?', value = FALSE),
          # radio buttons to select a student - updated according to checkbox
          radioButtons(inputId = 'student_button',
                       label = 'Students',
                       choices = rep('octocat!', 5))
        ),

        mainPanel(
          htmlOutput('display_text'),
          imageOutput('display_pic')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ### observeEvent to update UI when action button is clicked
  observeEvent(input$dice, {
    ### Randomly select 5 students
    s5 <- s_df %>%
      sample_n(5) %>% pull(student)
    ### then update the radiobutton choices!
    updateRadioButtons(inputId = 'student_button', choices = s5)
  })
  
  ### reactive to react to radio buttons - print info and pic
  s_info <- reactive({
    s_df %>% filter(student == input$student_button)
  })
  
  ### generate name/pronouns text for main panel
  output$display_text <- renderUI({
    ### clean up name
    name_raw <- s_info()$student
    nickname <- s_info()$nickname
    first_name <- ifelse(is.na(nickname), str_remove_all(name_raw, '.+, '), nickname)
    last_name  <- str_remove_all(name_raw, ', .+')
    name_out <- paste(first_name, last_name)
    ### clean up pronouns
    pronouns <- s_info()$pronouns; pronouns <- ifelse(is.na(pronouns), '', pronouns)
    ### now stick 'em together!
    html_out <- paste('<h3>', name_out, '</h3>',
                      '<p>', pronouns, '</p>')
    return(HTML(html_out))
  })
  
  output$display_pic <- renderImage({
    if(!input$show_pic) return(list(src = 'img/blank.jpg'))
    if(input$student_button == 'octocat!') {
      img_src <- 'img/blank.jpg'
    } else {
      img_src <- paste0('img/', s_info()$img)
    }
    return(list(src = img_src))
  }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
