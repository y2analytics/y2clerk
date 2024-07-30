#' Upload survey to Qualtrics
#'
#' Upload a survey from docx format to the Qualtrics library. The questionnaire doc itself will need to be in the Y2 specified format.
#'
#' @param file_path A string path to the questionnaire docx file.
#' @param api_token A string that is an API token to related the account you want the survey to be uploaded to.
#' @return A series of statements updating the user on the converter's progress.
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' qnr_converter(
#'   file_path = '~/Desktop/example.docx'
#'   api_token = 'xxx'
#' )
#' }
#' @export


qnr_converter <- function(
    file_path,
    api_token
    ) {

  # set api variables -------------------------------------------------------

  apiToken <- api_token

  baseUrl <- 'https://iad1.qualtrics.com/API/v3/survey-definitions'

  # set up questionnaire doc ------------------------------------------------
  print("Loading Questionnaire")

  doc_raw <- officer::read_docx (file_path)

  doc <- officer::docx_summary(doc_raw)

  doc <- doc %>%
    dplyr::filter(.data$text != "")

  # Get survey name ---------------------------------------------------------

  survey_name <- doc$text[1]

  # load survey name ---------------------------------------------------------

  request_body <- list(
    SurveyName = stringr::str_c("Survey Uploader - ", survey_name),
    Language = "EN",
    ProjectCategory = "CORE"
  )

  request_body_json <- jsonlite::toJSON(request_body, auto_unbox = TRUE)

  # create new survey -------------------------------------------------------
  print("Creating new survey")

  response = httr::POST(
    baseUrl,
    body = request_body_json,
    encode = "raw",
    httr::add_headers(
      `x-api-token` = apiToken,
      `content-type`= 'application/json',
      `Accept` = 'application/json'
    )
  )

  survey_id <- httr::content(response)[2]$result$SurveyID

  # set survey id -----------------------------------------------------------

  getSurveyQuestionUrl <- stringr::str_c(response$url, "/", survey_id, "/questions")
  getSurveyBlockUrl <- stringr::str_c(response$url, "/", survey_id, "/blocks")

  # split up question parts -------------------------------------------------

  get_text <- function(line) {

    line_text <- doc$text[line]
    next_line <- doc$text[line + 1]
    overall_text <- ""

    if (stringr::str_detect(next_line, "_")) {
      overall_text <- line_text
    }
    else {

      while(!stringr::str_detect(line_text, "\\[")) {

        line <- line + 1

        if (overall_text == "") {
          overall_text <- line_text
        }
        else {
          overall_text <- stringr::str_c(overall_text, "\n", line_text)
        }

        line_text <- doc$text[line]

      }
    }

    return(overall_text)

  }

  looking_for <- "question"
  row_vector_set <- list()
  choice_vector <- NULL
  row_type <- ""
  row_value <- ""
  row_label <- ""

  data_for_api <- dplyr::tibble(type = character(),
                                value = character(),
                                vector = list(),
                                label = character(),
                                iteration = numeric())

  for (line in 1:nrow(doc)) {

    # add to api data
    if (line > 1 & (looking_for == "question" | looking_for == "answers" | looking_for == "statements")) {
      data_for_api <- data_for_api %>%
        dplyr::add_row(type = row_type,
                       value = row_value,
                       vector = row_vector_set,
                       label = row_label,
                       iteration = line
        )
    }

    if (looking_for == "answers") {
      looking_for <- "answer set"
    }

    if (looking_for == "statements") {
      looking_for <- "statement set"
    }

    if (line > 1) {
      prev_text <- doc$text[line - 1]
    }
    line_text <- doc$text[line]
    next_line <- doc$text[line + 1]


    if (looking_for == "question") {
      if (stringr::str_detect(line_text, "_") & !stringr::str_detect(line_text, "\\[")) {
        looking_for <- "question text"
      }
      if (stringr::str_detect(line_text, "_") & stringr::str_detect(line_text, "\\[")) {
        looking_for <- "question"
        row_type <- "logic statement"
        row_value <- line_text
        row_vector_set <- NULL
        row_label <- ""
      }
      if (stringr::str_detect(line_text, "\\]") & stringr::str_detect(line_text, "\\[")) {
        looking_for <- "question"
        row_type <- "logic statement"
        row_value <- line_text
        row_vector_set <- NULL
        row_label <- ""
      }
      if (stringr::str_detect(stringr::str_to_lower(line_text), "intro")) {
        looking_for <- "intro text"
      }
      if (stringr::str_detect(stringr::str_to_lower(line_text), "text_")) {
        looking_for <- "text block"
      }
    }
    else if (looking_for == "statement set") {

      if (is.null(choice_vector)) {
        choice_vector <- list(list(Display = line_text))
      }
      else {
        choice_vector <- append(choice_vector, list(`1` = list(Display = line_text)))
      }

      # check for end of statement set
      if (grepl("ANSWERS", next_line, fixed = TRUE)) {
        looking_for <- "answers"

        choice_vector <- choice_vector %>%
          purrr::set_names(c(1:length(choice_vector)))

        row_type <- "statement set"
        row_value <- ""
        row_vector_set <- c(choice_vector)
        row_label <- ""

        choice_vector <- NULL
      }

    }
    else if (looking_for == "answer set") {

      if (is.null(choice_vector)) {
        choice_vector <- list(list(Display = line_text))
      }
      else {
        choice_vector <- append(choice_vector, list(`1` = list(Display = line_text)))
      }

      # check for end of answer set
      if ((grepl("_", next_line, fixed = TRUE) |
          grepl("INTRO", next_line, fixed = TRUE))) {
        looking_for <- "question"

        choice_vector <- choice_vector %>%
          purrr::set_names(c(1:length(choice_vector)))

        row_type <- "answer set"
        row_value <- ""
        row_vector_set <- c(choice_vector)
        row_label <- ""

        choice_vector <- NULL
      }

    }
    else if (looking_for == "intro text") {

      line_text <- get_text(line)

      row_type <- "intro text"
      row_value <- line_text
      row_vector_set <- NULL
      row_label <- prev_text

      looking_for <- "question"
    }
    else if (looking_for == "text block") {

      line_text <- get_text(line)

      row_type <- "text block"
      row_value <- line_text
      row_vector_set <- NULL
      row_label <- prev_text

      looking_for <- "question"
    }
    else if (looking_for == "question text") {

      q_text <- get_text(line)

      q_text <- stringr::str_remove(line_text, "\\[.+")
      extra_text <- stringr::str_remove(line_text, ".+\\?")

      if (stringr::str_detect(extra_text, "SINGLE")) {
        row_type <- "SINGLE-SELECT"
        looking_for <- "answers"
      }
      if (stringr::str_detect(extra_text, "INSERT LIST")) {
        row_type <- "SINGLE-SELECT"
        looking_for <- "question"
      }
      if (stringr::str_detect(extra_text, "DROPDOWN")) {
        row_type <- "SINGLE-SELECT"
        looking_for <- "question"
      }
      if (stringr::str_detect(extra_text, "MULTIPLE")) {
        row_type <- "MULTIPLE-SELECT"
        looking_for <- "answers"
      }
      if (stringr::str_detect(extra_text, "MATRIX")) {
        row_type <- "MATRIX"
        looking_for <- "statements"
      }
      if (stringr::str_detect(extra_text, "OPEN-ENDED")) {
        row_type <- "OPEN-ENDED"
        looking_for <- "question"
      }
      if (stringr::str_detect(extra_text, "OPEN ENDED")) {
        row_type <- "OPEN-ENDED"
        looking_for <- "question"
      }
      if (stringr::str_detect(extra_text, "NUMERIC TEXT ENTRY")) {
        row_type <- "NUMERIC TEXT ENTRY"
        looking_for <- "question"
      }

      row_label <- prev_text
      row_value <- q_text
      row_vector_set <- NULL

    }
  }

  # loop through questions --------------------------------------------------
  print("Pushing questions to Qualtrics API")

  data_for_api <- data_for_api %>%
    dplyr::filter(.data$type != "other") %>%
    dplyr::filter(.data$type != "logic statement") %>%
    dplyr::filter(.data$vector != 'list(Display = "[ANSWERS]")')

  numbers_list <- list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)


  for (line in 1:nrow(data_for_api)) {

    line_type <- data_for_api$type[line]
    line_text <- data_for_api$value[line]
    line_label <- data_for_api$label[line]
    line_answers <- list()
    line_statements <- list()

    if ((stringr::str_detect(line_type, "text block")) | (stringr::str_detect(line_type, "intro text"))) {

      questionDef <- list(
        QuestionText = line_text,
        DataExportTag = line_label,
        QuestionType = "DB",
        Selector = "TB",
        Configuration = list(
          QuestionDescriptionOption = "UseText"
        ),
        Validation = list(
          Settings = list(
          )
        ),
        Language = list(),
        NextChoiceId = 3,
        NextAnswerId = 1,
        QuestionID = "QID1",
        QuestionText_Unsafe = "Text?"
      )

      questionDef_json <- jsonlite::toJSON(questionDef, auto_unbox = TRUE)

      response = httr::POST(
        getSurveyQuestionUrl,
        body = questionDef_json,
        httr::add_headers(
          `x-api-token` = apiToken,
          `content-type`= 'application/json',
          `Accept` = 'application/json'
        )
      )

    }

    if (stringr::str_detect(line_type, "E-SELECT")) {

      answer_line_number <- line + 1

      while((data_for_api$type[answer_line_number] == "answer set") & (answer_line_number <= nrow(data_for_api))) {

        line_answers <- append(line_answers, data_for_api$vector[answer_line_number])

        answer_line_number <- answer_line_number + 1
      }

      line <- answer_line_number

      question_type <- ifelse(line_type == "SINGLE-SELECT", "SAVR", "MAVR")

      questionDef <- list(
        QuestionText = line_text,
        DataExportTag = line_label,
        QuestionType = "MC",
        Selector = question_type,
        SubSelector = "TX",
        Configuration = list(
          QuestionDescriptionOption = "UseText"
        ),
        QuestionDescription = "IDK?",
        Choices = line_answers,
        Validation = list(
          Settings = list(
            ForceResponse = "OFF",
            ForceResponseType = "ON",
            Type = "None"
          )
        ),
        Language = list(),
        NextChoiceId = 3,
        NextAnswerId = 1,
        QuestionID = "QID1",
        QuestionText_Unsafe = "Color?"
      )

      questionDef_json <- jsonlite::toJSON(questionDef, auto_unbox = TRUE)

      response = httr::POST(
        getSurveyQuestionUrl,
        body = questionDef_json,
        httr::add_headers(
          `x-api-token` = apiToken,
          `content-type`= 'application/json',
          `Accept` = 'application/json'
        )
      )

    }
    else if (stringr::str_detect(line_type, "OPEN-ENDED")) {

      questionDef <- list(
        QuestionText = line_text,
        DataExportTag = line_label,
        QuestionType = "TE",
        Selector = "SL",
        Configuration = list(
          QuestionDescriptionOption = "UseText"
        ),
        QuestionDescription = "?",
        Validation = list(
          Settings = list(
            ForceResponse = "ON",
            ForceResponseType = "ON",
            Type = "None"
          )
        ),
        Language = list(),
        QuestionText_Unsafe = "?"
      )

      questionDef_json <- jsonlite::toJSON(questionDef, auto_unbox = TRUE)

      response = httr::POST(
        getSurveyQuestionUrl,
        body = questionDef_json,
        httr::add_headers(
          `x-api-token` = apiToken,
          `content-type`= 'application/json',
          `Accept` = 'application/json'
        )
      )

    }
    else if (stringr::str_detect(line_type, "MATRIX")) {
      statements_line_number <- line + 1

      while((data_for_api$type[statements_line_number] == "statement set") & (statements_line_number <= nrow(data_for_api))) {

        line_statements <- append(line_statements, data_for_api$vector[statements_line_number])

        statements_line_number <- statements_line_number + 1
      }

      answer_line_number <- statements_line_number

      while((data_for_api$type[answer_line_number] == "answer set") & (answer_line_number <= nrow(data_for_api))) {

        line_answers <- append(line_answers, data_for_api$vector[answer_line_number])

        answer_line_number <- answer_line_number + 1
      }

      line <- answer_line_number

      questionDef <- list(
        QuestionText = line_text,
        DataExportTag = line_label,
        QuestionType = "Matrix",
        Selector = "Likert",
        SubSelector = "SingleAnswer",
        Configuration = list(
          QuestionDescriptionOption = "UseText"
        ),
        QuestionDescription = "?",
        Choices = line_statements,
        ChoiceOrder = numbers_list[1:length(line_statements)],
        Validation = list(
          Settings = list(
            ForceResponse = "OFF",
            ForceResponseType = "ON",
            Type = "None"
          )
        ),
        Answers = line_answers,
        AnswerOrder = numbers_list[1:length(line_answers)],
        Language = list(),
        QuestionText_Unsafe = "Color?"
      )

      questionDef_json <- jsonlite::toJSON(questionDef, auto_unbox = TRUE)

      response = httr::POST(
        getSurveyQuestionUrl,
        body = questionDef_json,
        httr::add_headers(
          `x-api-token` = apiToken,
          `content-type`= 'application/json',
          `Accept` = 'application/json'
        )
      )
    }
  }

  print("Converter finished!")

}
