library(shiny)
library(shinyjs)
library(tidyverse)

loading_done <- function(null_test) {
    hide(id = "loading_page", anim = TRUE, animType = "slide")
    show("main_content")
}

ui <- tagList(
    useShinyjs(),
    tags$head(
        tags$link(href = "style.css", rel="stylesheet")
    ),
    div(
        id = "loading_page",
        img(src = "psyteachr_hex.png", class = "center-fit"),
        tags$br(), tags$br(),
        icon("spinner", class="fa-spin")
    ),
    hidden(
        div(
            id = "main_content",
            
            fluidPage(
                
                titlePanel("The Normal Distribution"),
                
                sidebarLayout(
                    sidebarPanel(
                        width = 4,
                        tags$p("This web app is designed to help you understand how the pnorm() and qnorm() functions work. Play around with the parameters to see how the arguments you give to the function are being used to calculate the output."),
                        tags$br(),
                        selectInput("dist_function", "Function", c("pnorm()", "qnorm()")),
                        uiOutput("arg1_ui"),
                        numericInput("arg2", "mean = ", value = 170),
                        numericInput("arg3", "sd = ", value = 7.2),
                        selectInput("arg4", "lower.tail =", c("TRUE", "FALSE"), selected = "FALSE")
                    ),
                    
                    mainPanel(
                        width = 8,
                        shiny::HTML("<H4><b>Selected R Code</b></H4>"),
                        tags$p("Your selections produce the following R code:"),
                        verbatimTextOutput("rcode_text"),
                        tags$p("You can copy-paste this into RStudio to get the same result."),
                        tags$br(),
                        shiny::HTML("<H4><b>Plain English Translation</b></H4>"),
                        uiOutput("plain_eng_trans"),
                        tags$br(),
                        shiny::HTML("<H4><b>Result</b></H4>"),
                        verbatimTextOutput("rcode_text_output"),
                        tags$br(),
                        plotOutput("dist_plot")
                    )
                )
            )
        )
    )
)

server <- function(input, output) {
    
    output$arg1_ui <- renderUI({
        if (input$dist_function == "pnorm()") {
            numericInput("arg1", "q = ", value = 180, step = 0.5)
        } else if (input$dist_function == "qnorm()") {
            numericInput("arg1", "p = ", value = 0.15, step = 0.05, min = 0, max = 1)
        }
    })
    
    rcode_react <- reactive({
        func <- if (input$dist_function == "pnorm()") "pnorm" else if (input$dist_function == "qnorm()") "qnorm"
        arg1_name <- if (input$dist_function == "pnorm()") "q" else if (input$dist_function == "qnorm()") "p"
        sprintf("%s(%s = %g, mean = %g, sd = %g, lower.tail = %s)", func, arg1_name, input$arg1, input$arg2, input$arg3, input$arg4)
    })
    
    output$rcode_text <- renderText({
        rcode_react()
    })
    
    output$rcode_text_output <- renderText({
        eval(parse(text=rcode_react()))
    })
    
    output$plain_eng_trans <- renderUI({
        if (input$dist_function == "pnorm()") {
            shiny::HTML(sprintf("In plain English, your R code says:<br><br>Under a normal distribution, with a mean of <b>%g</b> and a standard deviation of <b>%g</b>, what is the probability of observing a value of <b>%g</b> or <b>%s</b>?", input$arg2, input$arg3, input$arg1, ifelse(input$arg4 == "TRUE", "less", "more")))
        } else {
            shiny::HTML(sprintf("In plain English, your R code says:<br><br>Under a normal distribution, with a mean of <b>%g</b> and a standard deviation of <b>%g</b>, what is the <b>%s</b> value in the <b>%s</b> <b>%g%%</b>?", input$arg2, input$arg3, ifelse(input$arg4=="TRUE", "highest", "lowest"), ifelse(input$arg4=="TRUE", "bottom", "top"), input$arg1*100))
        }
    })
    
    output$dist_plot <- renderPlot({
        tryCatch({
            i_mean <- input$arg2
            i_sd <- input$arg3
            
            sd_scope <- 4
            
            i_samples <- seq(
                i_mean-sd_scope*i_sd, i_mean+sd_scope*i_sd,
                abs((i_mean+sd_scope*i_sd) - (i_mean-sd_scope*i_sd))/1000
            )
            
            i_dat <- tibble(
                x = i_samples,
                density = dnorm(i_samples, mean = i_mean, sd = i_sd)
            )
            
            target_val <- if (input$dist_function == "pnorm()") {
                input$arg1
            } else {
                eval(parse(text=rcode_react()))
            }
            
            i_dat_shade <- if (input$arg4) {
                filter(i_dat, x<=target_val)
            } else {
                filter(i_dat, x>=target_val)
            }
            
            i_dat %>%
                ggplot(aes(x = x, y = density)) +
                geom_area(data = i_dat_shade, fill = "red") +
                geom_line(size = 1.2) +
                labs(x = NULL, y = NULL) +
                theme_minimal() +
                scale_y_continuous(breaks = NULL) +
                theme(axis.text.x = element_text(size = 16))
        }, error = function(e) {
            cat("Plot loading...")
        })
        
    }, bg = "transparent")
    
    loading_done()
}

# Run the application 
shinyApp(ui = ui, server = server)
