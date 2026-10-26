# ============================================================
# app.R
# Urban Rainfall Analysis and Flood-Risk Prediction Using R
# Dashboard for the two supplied datasets
# ============================================================

library(shiny)
library(ggplot2)

# Run with the working directory set to this extracted project folder.
project_folder <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

pred_file <- file.path(project_folder, "district_risk_predictions.csv")
rain_candidates <- list.files(
  project_folder,
  pattern = "^rainfall_districtwise_daily_imd.*\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)
metrics_file <- file.path(project_folder, "model_metrics.csv")
class_metrics_file <- file.path(project_folder, "class_metrics.csv")
importance_file <- file.path(project_folder, "feature_importance.csv")
flood_candidates <- list.files(project_folder, pattern = "^District_FloodedArea.*\\.csv$",
                                full.names = TRUE, ignore.case = TRUE)
flood_file <- if (length(flood_candidates)) flood_candidates[1] else NA_character_

if (!file.exists(pred_file)) {
  stop("Run 01_prepare_train.R first to create district_risk_predictions.csv")
}
if (!length(rain_candidates)) {
  stop("Rainfall CSV not found in project folder.")
}

predictions <- read.csv(pred_file, stringsAsFactors = FALSE, check.names = FALSE)
# Normalize prediction output headers and validate the district field.
names(predictions) <- gsub("[^A-Za-z0-9]+", "_", trimws(names(predictions)))
if (!("District" %in% names(predictions))) {
  district_candidates <- which(tolower(gsub("[^a-z0-9]", "", names(predictions))) %in%
                                 c("district", "districtname", "districtkey"))
  if (length(district_candidates)) {
    predictions$District <- as.character(predictions[[district_candidates[1]]])
  } else {
    stop("The predictions file has no District/DistrictKey column. Found columns: ",
         paste(names(predictions), collapse = ", "))
  }
}
predictions$District <- trimws(as.character(predictions$District))
if (all(is.na(predictions$District) | predictions$District == "") && "DistrictKey" %in% names(predictions)) {
  predictions$District <- trimws(as.character(predictions$DistrictKey))
}
if (nrow(predictions) > 0 && length(predictions$District) != nrow(predictions)) {
  stop("District column length does not match the predictions rows.")
}
rain_raw <- read.csv(rain_candidates[1], stringsAsFactors = FALSE,
                     check.names = FALSE, na.strings = c("", "NA", "NaN"))
names(rain_raw) <- gsub("[[:space:]]+", " ", trimws(gsub("[\r\n]+", " ", names(rain_raw))))
# Resolve key fields safely so headers containing embedded newlines/spaces work.
find_col <- function(df, candidates) {
  norm <- function(x) tolower(gsub("[^a-z0-9]", "", enc2utf8(as.character(x))))
  fix_common <- function(x) {
    x <- norm(x)
    x <- gsub("distict", "district", x, fixed = TRUE)
    x <- gsub("dailynomal", "dailynormal", x, fixed = TRUE)
    x
  }
  header_norm <- fix_common(names(df))
  candidate_norm <- fix_common(candidates)
  for (candidate in candidate_norm) {
    idx <- which(header_norm == candidate)
    if (length(idx)) return(names(df)[idx[1]])
  }
  for (candidate in candidate_norm) {
    distances <- as.vector(adist(candidate, header_norm))
    if (!length(distances) || all(is.na(distances))) next
    best_distance <- min(distances, na.rm = TRUE)
    # Also tolerate headers where several characters were lost in the CSV.
    allowed <- max(4L, ceiling(0.35 * nchar(candidate)))
    if (best_distance <= allowed) {
      idx <- which(distances == best_distance)
      if (length(idx) == 1) return(names(df)[idx])
      lens <- abs(nchar(header_norm[idx]) - nchar(candidate))
      return(names(df)[idx[which.min(lens)]])
    }
  }
  NA_character_
}

state_col <- find_col(rain_raw, "State")
district_col <- find_col(rain_raw, c("District", "District Name"))
date_col <- find_col(rain_raw, c("Date", "Daily Date"))
actual_col <- find_col(rain_raw, c("Daily Actual", "DailyActual"))
if (anyNA(c(state_col, district_col, date_col, actual_col))) {
  stop("Rainfall columns could not be identified. Found: ", paste(names(rain_raw), collapse = " | "))
}
rain_raw$State_clean <- trimws(as.character(rain_raw[[state_col]]))
rain_raw$District_clean <- trimws(as.character(rain_raw[[district_col]]))
rain_raw$Date_clean <- as.Date(as.character(rain_raw[[date_col]]),
                               tryFormats = c("%Y-%m-%d", "%d-%m-%Y", "%m/%d/%Y"))
rain_raw$DailyActual <- suppressWarnings(as.numeric(rain_raw[[actual_col]]))

if (file.exists(metrics_file)) {
  model_metrics <- read.csv(metrics_file, stringsAsFactors = FALSE)
} else {
  model_metrics <- data.frame(Metric = character(), Value = numeric())
}
if (file.exists(class_metrics_file)) {
  class_metrics <- read.csv(class_metrics_file, stringsAsFactors = FALSE)
} else {
  class_metrics <- data.frame()
}
if (file.exists(importance_file)) {
  importance_data <- read.csv(importance_file, stringsAsFactors = FALSE)
} else {
  importance_data <- data.frame()
}

risk_levels <- c("Low", "Medium", "High")
predictions$Predicted_Flood_Risk <- factor(
  predictions$Predicted_Flood_Risk,
  levels = risk_levels
)
predictions$Flood_Risk_Label <- factor(
  predictions$Flood_Risk_Label,
  levels = risk_levels
)

states <- sort(unique(predictions$State))
states <- states[!is.na(states)]

ui <- fluidPage(
  titlePanel("Urban Rainfall Analysis and Flood-Risk Prediction"),

  tags$div(
    style = "padding:10px; margin-bottom:15px; border:1px solid #d8a23a; border-radius:6px;",
    strong("Important interpretation: "),
    "The model predicts relative district flood-susceptibility tiers based on flooded-area percentage. ",
    "It is not a validated prediction of a flood on a future date. The rainfall input covers a short period in 2026."
  ),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        "state", "Select state/territory",
        choices = c("All States" = "ALL", states),
        selected = "ALL"
      ),
      selectInput(
        "risk", "Filter predicted risk",
        choices = c("All categories" = "ALL", risk_levels),
        selected = "ALL"
      ),
      textInput("district_search", "Search district name", value = ""),
      downloadButton("download_predictions", "Download filtered predictions")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          "Flood-Risk Overview",
          h3("District-Level Risk Classification"),
          fluidRow(
            column(4, wellPanel(h4("Districts shown"), textOutput("district_count"))),
            column(4, wellPanel(h4("High-risk predictions"), textOutput("high_count"))),
            column(4, wellPanel(h4("Mean maximum daily rain"), textOutput("mean_max_rain")))
          ),
          plotOutput("risk_bar", height = "350px"),
          h4("District Risk Predictions"),
          tableOutput("prediction_table")
        ),

        tabPanel(
          "Rainfall Analysis",
          h3("Daily Rainfall Pattern"),
          selectInput("rain_state", "Rainfall state", choices = sort(unique(rain_raw$State_clean))),
          selectInput("rain_district", "Rainfall district", choices = character(0)),
          plotOutput("daily_rain_plot", height = "350px"),
          h4("Highest rainfall observations"),
          tableOutput("wettest_table")
        ),

        tabPanel(
          "Flood Exposure",
          h3("Flooded-Area Exposure vs Rainfall"),
          p("Flooded-area percentage is a district exposure/susceptibility proxy, not a dated flood event label."),
          plotOutput("exposure_scatter", height = "400px")
        ),

        tabPanel(
          "Model Evaluation",
          h3("Test-set metrics"),
          tableOutput("model_metrics_table"),
          h3("Per-class performance"),
          tableOutput("class_metrics_table"),
          h3("Feature importance"),
          if (nrow(importance_data) > 0) {
            plotOutput("importance_plot", height = "400px")
          } else {
            p("Feature importance file not found. Run 01_prepare_train.R.")
          }
        ),

        tabPanel(
          "Data Notes",
          h4("How risk categories were created"),
          p("The flooded-area dataset does not contain event dates or daily flood/no-flood labels."),
          p("Corrected_Percent_Flooded_Area was divided into three relative groups using its one-third and two-thirds quantiles. These groups are labelled Low, Medium and High district susceptibility."),
          p("The Random Forest learns these proxy tiers from rainfall summary features. Because the target is a proxy and the rainfall period is short, the result must not be presented as a validated future flood forecast."),
          h4("Source data"),
          p("Daily IMD district rainfall and District_FloodedArea.csv.")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  filtered_predictions <- reactive({
    d <- predictions

    if (!identical(input$state, "ALL")) {
      d <- d[d$State == input$state, ]
    }
    if (!identical(input$risk, "ALL")) {
      d <- d[as.character(d$Predicted_Flood_Risk) == input$risk, ]
    }
    if (nzchar(trimws(input$district_search))) {
      d <- d[grepl(trimws(input$district_search), d$District, ignore.case = TRUE), ]
    }
    d
  })

  output$district_count <- renderText(nrow(filtered_predictions()))

  output$high_count <- renderText({
    sum(filtered_predictions()$Predicted_Flood_Risk == "High", na.rm = TRUE)
  })

  output$mean_max_rain <- renderText({
    x <- filtered_predictions()$Rain_Max_Daily_mm
    if (!length(x) || all(is.na(x))) return("No data")
    paste0(round(mean(x, na.rm = TRUE), 2), " mm")
  })

  output$risk_bar <- renderPlot({
    d <- filtered_predictions()
    validate(need(nrow(d) > 0, "No districts match the selected filters."))

    counts <- as.data.frame(table(
      factor(d$Predicted_Flood_Risk, levels = risk_levels)
    ))
    names(counts) <- c("Risk", "Districts")

    ggplot(counts, aes(Risk, Districts, fill = Risk)) +
      geom_col() +
      scale_fill_manual(values = c("Low" = "#65A765",
                                   "Medium" = "#E4B44C",
                                   "High" = "#D65A5A")) +
      labs(title = "Predicted District Susceptibility Categories",
           x = "Predicted category", y = "Number of districts") +
      theme_minimal() +
      theme(legend.position = "none")
  })

  output$prediction_table <- renderTable({
    d <- filtered_predictions()
    validate(need(nrow(d) > 0, "No matching districts."))
    d <- d[order(match(d$Predicted_Flood_Risk, risk_levels),
                 -d$High_Probability), ]
    cols <- c("State", "District", "Predicted_Flood_Risk",
              "Flood_Risk_Label", "Corrected_Percent_Flooded_Area",
              "Rain_Mean_mm", "Rain_Max_Daily_mm", "Rain_7Day_Max_mm",
              "Low_Probability", "Medium_Probability", "High_Probability")
    d <- d[, cols, drop = FALSE]
    names(d) <- c("State", "District", "Predicted Risk", "Reference Proxy Tier",
                  "Corrected Flooded Area (%)", "Mean Rain (mm/day)",
                  "Max Daily Rain (mm)", "Max 7-Day Rain (mm)",
                  "Low Probability", "Medium Probability", "High Probability")
    d
  }, striped = TRUE, bordered = TRUE, digits = 3)

  observeEvent(input$rain_state, {
    district_choices <- sort(unique(
      rain_raw$District_clean[rain_raw$State_clean == input$rain_state]
    ))
    updateSelectInput(
      session, "rain_district",
      choices = district_choices,
      selected = if (length(district_choices)) district_choices[1] else character(0)
    )
  }, ignoreInit = FALSE)

  selected_rain <- reactive({
    req(input$rain_state, input$rain_district)
    d <- rain_raw[
      rain_raw$State_clean == input$rain_state &
        rain_raw$District_clean == input$rain_district &
        !is.na(rain_raw$Date_clean) &
        is.finite(rain_raw$DailyActual),
    ]
    d[order(d$Date_clean), ]
  })

  output$daily_rain_plot <- renderPlot({
    d <- selected_rain()
    validate(need(nrow(d) > 0, "No rainfall observations for this district."))
    ggplot(d, aes(Date_clean, DailyActual)) +
      geom_line(color = "steelblue", linewidth = 0.7) +
      geom_point(color = "steelblue", size = 1) +
      labs(title = paste("Daily Rainfall:", input$rain_district),
           x = "Date", y = "Daily rainfall (mm)") +
      theme_minimal()
  })

  output$wettest_table <- renderTable({
    d <- selected_rain()
    d <- d[order(-d$DailyActual), ]
    d <- head(d, 10)
    data.frame(
      Date = format(d$Date_clean, "%d-%m-%Y"),
      Rainfall_mm = round(d$DailyActual, 2)
    )
  }, striped = TRUE, bordered = TRUE)

  output$exposure_scatter <- renderPlot({
    d <- filtered_predictions()
    validate(need(nrow(d) > 0, "No matching districts."))
    ggplot(d, aes(Rain_Max_Daily_mm, Corrected_Percent_Flooded_Area,
                  color = Predicted_Flood_Risk)) +
      geom_point(alpha = 0.75, size = 2) +
      scale_color_manual(values = c("Low" = "#65A765",
                                    "Medium" = "#E4B44C",
                                    "High" = "#D65A5A")) +
      labs(x = "Maximum daily rainfall in available period (mm)",
           y = "Corrected flooded area (%)",
           color = "Predicted tier",
           title = "Rainfall Features and Historical Flooded-Area Exposure") +
      theme_minimal()
  })

  output$model_metrics_table <- renderTable({
    model_metrics
  }, striped = TRUE, bordered = TRUE, digits = 4)

  output$class_metrics_table <- renderTable({
    validate(need(nrow(class_metrics) > 0, "Class metrics unavailable."))
    class_metrics
  }, striped = TRUE, bordered = TRUE, digits = 3)

  output$importance_plot <- renderPlot({
    validate(need(nrow(importance_data) > 0, "Importance data unavailable."))
    importance_data$Importance <- if ("MeanDecreaseGini" %in% names(importance_data)) {
      importance_data$MeanDecreaseGini
    } else if ("MeanDecreaseAccuracy" %in% names(importance_data)) {
      importance_data$MeanDecreaseAccuracy
    } else {
      importance_data[[1]]
    }
    importance_data$Feature <- as.character(importance_data$Feature)
    ggplot(importance_data, aes(reorder(Feature, Importance), Importance)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(x = "Feature", y = "Importance", title = "Random Forest Feature Importance") +
      theme_minimal()
  })

  output$download_predictions <- downloadHandler(
    filename = function() "district_flood_risk_predictions.csv",
    content = function(file) {
      write.csv(filtered_predictions(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
