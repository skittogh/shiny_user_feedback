
# Place this UI module where you want it in the UI
# feedback_button_ui(id = "feedback")

# Place this server module anywhere in the server code
# feedback_button_server(id = "feedback", parent = session)



#' Send email using Mailgun forwarding API service.
#'
#' @param email email address of recipient.
#'
#' @param mail_message message body for email, in this case in HTML format.
#'
#' @param subject_text test in the email subject header. Usually app title.
#'
#' @return
#' @export
#'
#' @examples
#' sendEmail(email = xyz_at_mail.com, mail_message = HMTL("this is the message), subject_text = "test message")
#'
#' @details An email function suitable for use for apps hosted on shinyapps.io.
#' Uses the Mailgun service so that email content is sent in an API request
#' and Mailgun compiles it and adds it to a queue's for delivery to the specified recipient.
#' https://www.mailgun.com/
#' Note that this function requires an .Renviron file to be added to the app directory
#' which contains the API connection details. Example loaded without the HEX keys
#' send mail function if you are not in an interactive session.
#' In this case we are using HTML formatted message text
#' library("httr")
#' modified from: https://gist.github.com/MarkEdmondson1234/ddcac436cbdfd4557639522573bfc7b6

sendEmail <- function(email, mail_message, subject_text){

    url <- Sys.getenv("MAILGUN_ACCESS_KEY_URL")

    api_key <- Sys.getenv("MAILGUN_SECRET_ACCESS_KEY")

    the_body <-
        list(
            from = Sys.getenv("MAILGUN_ACCESS_FROM"),
            to = email,
            subject = subject_text,
            html = mail_message
        )

    post_email <- httr::POST(url,
                             httr::authenticate("api", api_key),
                             encode = "form",
                             body = the_body)
    post_email

    # print so that status of email sending is printed or logged in shinyapps.io log file
    print(paste0("Feedback email send status: ", httr::http_status(post_email)))
}







# to run: feedback_button_ui(id = "feedback")
# library(shiny)
# use_package("shiny")
#' UI shiny module for feedback button
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
feedback_button_ui <- function(id) {
    ns <- NS(id)
    shiny::tagList(
        shiny::tags$div(
            class = 'tooltip_feedback',
            shiny::actionButton(ns("web_button"),
                                label = "Feedback",
                                icon = icon(
                                    name = NULL,
                                    id = "feedbackbtn"
                                )
            ),
            shiny::tags$span(class = 'tooltiptext_feedback',
                             "Please click here to provide feedback on this web tool")
        )
    )
}




#' Server shiny module for feedback button
#'
#' @param id
#' @param label
#' @param parent
#'
#' @details
#' This is the server counterpart to the UI 'feedback_button_ui'
#' When input$web_button is clicked from the UI, a model box is opened,
#' When input$submit_feedback is clicked, the details from the user inputs are
#' sent as email using the sendEmail() function
#' requires metadata$app_title from app which is the title of the application (change this to app name)
#' use_package("shiny", "shinyWidgets", "emojifont")
#'
#' @examples
#' feedback_button_server(id = "feedback", parent = session)
feedback_button_server <- function(id, label = "feedback_button_server", parent) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        shiny::observeEvent(input$web_button, {
            shiny::showModal(modalDialog(
                title = "A few quick questions...",

                shiny::radioButtons(inputId = ns("get_required_info"),
                                    "Did you find the information you wanted?",
                                    choices = c("Yes", "No"),
                                    selected = character(0)),

                shiny::radioButtons(
                    inputId = ns("experience"),
                    label = "Rate your general experience with this web tool:",
                    choiceNames  = c(
                        emojifont::emoji('frowning_face'),
                        emojifont::emoji('slightly_frowning_face'),
                        emojifont::emoji('neutral_face'),
                        emojifont::emoji('slightly_smiling_face'),
                        emojifont::emoji('grinning')
                        # HTML(shiny::icon("face-tired", class = "fa-solid icon_face")),
                        # HTML(shiny::icon("face-frown", class = "fa-solid icon_face")),
                        # HTML(shiny::icon("face-meh", class = "fa-solid icon_face")),
                        # HTML(shiny::icon("face-smile", class = "fa-solid icon_face")),
                        # HTML(shiny::icon("face-grin-hearts", class = "fa-solid icon_face"))
                    ),
                    choiceValues = c("terrible", "bad", "meh", "good", "great"),
                    inline = TRUE,
                    selected = character(0)
                ),

                shiny::radioButtons(
                    inputId= ns("layout"),
                    label = "Is the layout of the web tool sensible and easy to navigate?",
                    choiceNames = c(
                        emojifont::emoji('frowning_face'),
                        emojifont::emoji('slightly_frowning_face'),
                        emojifont::emoji('neutral_face'),
                        emojifont::emoji('slightly_smiling_face'),
                        emojifont::emoji('grinning')
                        # HTML(shiny::icon("face-tired", class = "fa-solid icon_face")),
                        # HTML(shiny::icon("face-frown", class = "fa-solid icon_face")),
                        # HTML(shiny::icon("face-meh", class = "fa-solid icon_face")),
                        # HTML(shiny::icon("face-smile", class = "fa-solid icon_face")),
                        # HTML(shiny::icon("face-grin-hearts", class = "fa-solid icon_face"))
                    ),
                    choiceValues = c("terrible", "bad", "meh", "good", "great"),
                    inline = TRUE,
                    selected = character(0)
                ),

                shiny::radioButtons(
                    inputId = ns("data_vis"),
                    label = "Is the data displayed in plots and tables clear and understandable?",
                    choiceNames = c(
                        emojifont::emoji('frowning_face'),
                        emojifont::emoji('slightly_frowning_face'),
                        emojifont::emoji('neutral_face'),
                        emojifont::emoji('slightly_smiling_face'),
                        emojifont::emoji('grinning')
                        # HTML(shiny::icon("face-tired", class = "fa-solid icon_face")),
                        # HTML(shiny::icon("face-frown", class = "fa-solid icon_face")),
                        # HTML(shiny::icon("face-meh", class = "fa-solid icon_face")),
                        # HTML(shiny::icon("face-smile", class = "fa-solid icon_face")),
                        # HTML(shiny::icon("face-grin-hearts", class = "fa-solid icon_face"))
                    ),
                    choiceValues = c("terrible", "bad", "meh", "good", "great"),
                    inline = TRUE,
                    selected = character(0)
                ),


                shiny::radioButtons(inputId = ns("background"),
                                    "What is your background?",
                                    choices = c("Health professional", "Data related field", "Planning/policy", "Media", "Interested member of public", "Other"),
                                    selected = character(0)
                ),


                shiny::renderUI({
                    req(!is.null(input$background), input$background)
                    if(input$background == "Other") {
                        shiny::textInput(inputId = ns("background_other"),
                                         label = "If you feel like sharing, please type your background with one or two words")
                    }
                }),


                shiny::tags$div( id = "feedback_button_success",
                                 # shinyWidgets::actionBttn(inputId =ns("submit_feedback"),
                                 shiny::actionButton(inputId = ns("submit_feedback"),
                                                     label = "Submit"),

                                 shiny::renderUI({
                                     if(input$submit_feedback != 0) {
                                         # shiny::HTML(paste0(emojifont::emoji('white_check_mark'), " Thanks!"))
                                         shiny::HTML(paste0(shiny::icon("check-circle", class = "blue_check-circle")))
                                     }
                                 })
                ),

                shiny::br(),
                shiny::br(),

                shiny::HTML("If you would like to provide further feedback, or if you require information not included in this web tool,
                 please email the <a href='mailto:further_info_mail_address.com'>XYZ team</a>
                 at XYZ."),


                size = "m",
                easyClose = TRUE,
                footer = tagList(
                    shiny::modalButton("Close")
                )
            )
            )
        })


        shiny::observeEvent(input$submit_feedback, {

            background <- ifelse(input$background == "Other", input$background_other, input$background)

            text <- shiny::tagList(
                "Feedback:",
                shiny::tags$ul(
                    shiny::tags$li(
                        paste0("Found the information wanted?: ", input$get_required_info)
                    ),
                    shiny::tags$li(
                        paste0("General experience with web tool: ", input$experience)
                    ),
                    shiny::tags$li(
                        paste0("Layout sensible and easy to navigate?: ", input$layout)
                    ),
                    shiny::tags$li(
                        paste0("Data displayed in plots and tables clear and understandable?: ", input$data_vis)
                    ),
                    shiny::tags$li(
                        paste0("User backgound: ", background)
                    )
                )
            )
            print(text)

            # run send mail function if you are not in an interactive session
            if(!interactive()){
                sendEmail(email = "xyz_at_mail.com", mail_message = text, subject_text = metadata$app_title)
            }
        })


    })
}
