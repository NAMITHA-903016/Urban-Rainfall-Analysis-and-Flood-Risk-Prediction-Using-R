# ============================================================
# 01_prepare_train.R
# Urban Rainfall Analysis and Flood-Risk Prediction Using R
#
# IMPORTANT:
# With the two supplied files, the target is a DISTRICT-LEVEL
# FLOOD-SUSCEPTIBILITY PROXY based on corrected flooded-area %.
# This is not a validated prediction of a flood on a future date.
# The rainfall file currently covers only 2026-08-19 to 2026-10-14.
# ============================================================

# Install once if needed:
# install.packages("randomForest")

required <- c("randomForest")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop(
    paste0(
      "Please install required package(s) first: ",
      paste(missing, collapse = ", "),
      "\nRun: install.packages(c(",
      paste(sprintf('"%s"', missing), collapse = ", "),
      "))"
    )
  )
}
library(randomForest)

# 1. Use the current working directory (set it to this extracted folder in RStudio).
# This avoids hard-coded Windows user paths and works after extracting the ZIP anywhere.
project_folder <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)


# Find the rainfall CSV even if Windows added (1) to the filename.
rain_candidates <- list.files(
  project_folder,
  pattern = "^rainfall_districtwise_daily_imd.*\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)
flood_candidates <- list.files(
  project_folder,
  pattern = "^District_FloodedArea.*\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (!length(rain_candidates)) {
  stop("Could not find rainfall_districtwise_daily_imd*.csv in ", project_folder,
       "\nSet the working directory to the extracted project folder.")
}
if (!length(flood_candidates)) {
  stop("Could not find District_FloodedArea*.csv in ", project_folder,
       "\nSet the working directory to the extracted project folder.")
}
rain_file <- rain_candidates[1]
flood_file <- flood_candidates[1]

cat("Rainfall file:", rain_file, "\n")
cat("Flood-area file:", flood_file, "\n")

# 2. Read files
rain_raw <- read.csv(
  rain_file, stringsAsFactors = FALSE,
  check.names = FALSE, na.strings = c("", "NA", "NaN")
)
flood_raw <- read.csv(
  flood_file, stringsAsFactors = FALSE,
  check.names = FALSE, na.strings = c("", "NA", "NaN")
)

# Normalize headers safely: remove embedded newlines, repeated whitespace and BOM.
clean_headers <- function(x) {
  x <- gsub("\ufeff", "", x, fixed = TRUE)
  x <- gsub("[\\r\\n]+", " ", x)
  x <- gsub("[[:space:]]+", " ", trimws(x))
  x
}
names(rain_raw) <- clean_headers(names(rain_raw))
names(flood_raw) <- clean_headers(names(flood_raw))

# Resolve column names without relying on exact punctuation/case.
find_col <- function(df, candidates) {
  # Robustly match source headers even when the CSV contains damaged names
  # such as "Dist ict" or "Daily No mal".
  norm <- function(x) tolower(gsub("[^a-z0-9]", "", enc2utf8(as.character(x))))
  fix_common <- function(x) {
    x <- norm(x)
    # Known spelling/character-loss variants in the supplied rainfall file.
    x <- gsub("distict", "district", x, fixed = TRUE)
    x <- gsub("dailynomal", "dailynormal", x, fixed = TRUE)
    x <- gsub("dailyactual", "dailyactual", x, fixed = TRUE)
    x <- gsub("mo thly", "monthly", x, fixed = TRUE)
    x
  }
  header_norm <- fix_common(names(df))
  candidate_norm <- fix_common(candidates)

  # Exact normalized match first.
  for (candidate in candidate_norm) {
    idx <- which(header_norm == candidate)
    if (length(idx)) return(names(df)[idx[1]])
  }

  # Fuzzy fallback for headers with several characters missing (for example,
  # "Pe ce t_Flooded_A ea"). Keep a relative distance limit and choose the
  # closest match; this handles damaged CSV headers without requiring edits.
  for (candidate in candidate_norm) {
    distances <- as.vector(adist(candidate, header_norm))
    if (!length(distances) || all(is.na(distances))) next
    best_distance <- min(distances, na.rm = TRUE)
    allowed <- max(4L, ceiling(0.35 * nchar(candidate)))
    if (best_distance <= allowed) {
      idx <- which(distances == best_distance)
      if (length(idx) == 1) return(names(df)[idx])
      # Break ties by similarity in length, then use the first best candidate.
      lens <- abs(nchar(header_norm[idx]) - nchar(candidate))
      return(names(df)[idx[which.min(lens)]])
    }
  }
  NA_character_
}

# Explicitly identify the key rainfall fields; stop with useful diagnostics if absent.
rain_col_map <- c(
  State = find_col(rain_raw, c("State")),
  District = find_col(rain_raw, c("District", "District Name")),
  Date = find_col(rain_raw, c("Date", "Daily Date")),
  DailyActual = find_col(rain_raw, c("Daily Actual", "Daily Actual mm", "DailyActual")),
  DailyNormal = find_col(rain_raw, c("Daily Normal", "DailyNormal"))
)
if (anyNA(rain_col_map)) {
  stop("Could not identify required rainfall columns. Found headers: ",
       paste(names(rain_raw), collapse = " | "),
       "\nRequired: State, District, Date, Daily Actual, Daily Normal.")
}
flood_col_map <- c(
  Dist_Name = find_col(flood_raw, c("Dist_Name", "District", "District Name")),
  Percent_Flooded_Area = find_col(flood_raw, c("Percent_Flooded_Area")),
  Parmanent_Water = find_col(flood_raw, c("Parmanent_Water", "Permanent_Water")),
  Corrected_Percent_Flooded_Area = find_col(flood_raw, c("Corrected_Percent_Flooded_Area"))
)
if (anyNA(flood_col_map)) {
  stop("Could not identify required flooded-area columns. Found headers: ",
       paste(names(flood_raw), collapse = " | "))
}

required_rain <- c("State", "District", "Date", "Daily Actual", "Daily Normal")
required_flood <- c(
  "Dist_Name", "Percent_Flooded_Area",
  "Parmanent_Water", "Corrected_Percent_Flooded_Area"
)


# 3. Normalize names conservatively and harmonize common spelling variants.
normalize_district <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("&", " AND ", x, fixed = TRUE)
  x <- gsub("[^A-Z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)

  # Common spelling/renaming differences. Review unmatched_names.csv later.
  aliases <- c(
    "AMARAVATI" = "AMRAVATI",
    "ARARIYA" = "ARARIA",
    "ANANTAPURAMU" = "ANANTAPUR",
    "BANGALORE RURAL" = "BENGALURU RURAL",
    "BANGLORE URBAN" = "BENGALURU",
    "BANGALORE" = "BENGALURU",
    "BELGAUM" = "BELAGAVI",
    "BELLARY" = "BALLARI",
    "BAGALKOT" = "BAGALKOTE",
    "BARGARH" = "BARGARH",
    "BAUDA" = "BAUDH",
    "BADAUN" = "BUDAUN",
    "BULANDSAHAR" = "BULANDSHAHR",
    "BULDHANA" = "BULDANA",
    "CHAMARAJANAGAR" = "CHAMARAJANAGARA",
    "CHARKHIDADRI" = "CHARKI DADRI",
    "CHIKBALLAPUR" = "CHIKKABALLAPURA",
    "CHIKMAGALUR" = "CHIKKAMAGALURU",
    "CHHOTA UDEPUR" = "CHOTA UDAIPUR",
    "DADAR NAGAR HAVELI" = "DADRA NAGAR HAVELI",
    "DAKSHIN KANNADA" = "DAKSHINA KANNADA",
    "DANTEWARA" = "DAKSHIN BASTAR DANTEWADA",
    "DARJEELING" = "DARJILING",
    "DAVANGERE" = "DAVANAGERE",
    "DEOGARH" = "DEBAGARH",
    "DHOLPUR" = "DHAULPUR",
    "DANGS" = "DANG",
    "DRABHANGA" = "DARBHANGA",
    "GURGAON" = "GURUGRAM",
    "HARDWAR" = "HARIDWAR",
    "HOSHANGABAD" = "NARMADAPURAM",
    "FAIZABAD" = "AYODHYA",
    "B KOTHAGUDEM" = "BHADRADRI KOTHAGUDEM"
  )

  idx <- match(x, names(aliases))
  replace <- !is.na(idx)
  x[replace] <- unname(aliases[idx[replace]])
  x
}

rain_raw$DistrictKey <- normalize_district(rain_raw[[rain_col_map[["District"]]]])
flood_raw$DistrictKey <- normalize_district(flood_raw[[flood_col_map[["Dist_Name"]]]])

# 4. Clean rainfall columns
rain <- data.frame(
  State = trimws(as.character(rain_raw[[rain_col_map[["State"]]]])),
  District = trimws(as.character(rain_raw[[rain_col_map[["District"]]]])),
  DistrictKey = rain_raw$DistrictKey,
  Date = as.Date(as.character(rain_raw[[rain_col_map[["Date"]]]]),
                  tryFormats = c("%Y-%m-%d", "%d-%m-%Y", "%m/%d/%Y")),
  DailyActual = suppressWarnings(as.numeric(rain_raw[[rain_col_map[["DailyActual"]]]])),
  DailyNormal = suppressWarnings(as.numeric(rain_raw[[rain_col_map[["DailyNormal"]]]]))
)

rain <- rain[
  !is.na(rain$Date) &
    !is.na(rain$DistrictKey) & rain$DistrictKey != "" &
    is.finite(rain$DailyActual) & rain$DailyActual >= 0,
]
rain <- rain[order(rain$DistrictKey, rain$Date), ]

# Rolling 3- and 7-day totals are calculated within each district.
roll_sum <- function(x, width) {
  out <- rep(NA_real_, length(x))
  if (length(x) >= width) {
    for (i in seq.int(width, length(x))) {
      w <- x[(i - width + 1):i]
      if (all(is.finite(w))) out[i] <- sum(w)
    }
  }
  out
}

rain_parts <- split(rain, rain$DistrictKey)
rain_parts <- lapply(rain_parts, function(d) {
  d <- d[order(d$Date), ]
  d$Rain3Day <- roll_sum(d$DailyActual, 3)
  d$Rain7Day <- roll_sum(d$DailyActual, 7)
  d
})
rain <- do.call(rbind, rain_parts)
rownames(rain) <- NULL

# 5. Create district-level rainfall features from the available period.
safe_max <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else max(x)
}
safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else mean(x)
}

district_parts <- split(rain, rain$DistrictKey)
rain_features <- do.call(rbind, lapply(district_parts, function(d) {
  data.frame(
    DistrictKey = d$DistrictKey[1],
    State = d$State[1],
    District = d$District[1],
    Rain_Days = nrow(d),
    Rain_Mean_mm = safe_mean(d$DailyActual),
    Rain_Max_Daily_mm = safe_max(d$DailyActual),
    Rain_3Day_Max_mm = safe_max(d$Rain3Day),
    Rain_7Day_Max_mm = safe_max(d$Rain7Day),
    Wet_Days_2_5mm = sum(d$DailyActual >= 2.5, na.rm = TRUE),
    Rain_Normal_Mean_mm = safe_mean(d$DailyNormal),
    Rain_Excess_Mean_mm = safe_mean(d$DailyActual - d$DailyNormal),
    stringsAsFactors = FALSE
  )
}))
rownames(rain_features) <- NULL

# 6. Clean flood-area data. Multiple rows per district are averaged.
flood <- data.frame(
  Dist_Name = trimws(as.character(flood_raw[[flood_col_map[["Dist_Name"]]]])),
  DistrictKey = flood_raw$DistrictKey,
  Percent_Flooded_Area = suppressWarnings(as.numeric(flood_raw[[flood_col_map[["Percent_Flooded_Area"]]]])),
  Permanent_Water = suppressWarnings(as.numeric(flood_raw[[flood_col_map[["Parmanent_Water"]]]])),
  Corrected_Percent_Flooded_Area = suppressWarnings(
    as.numeric(flood_raw[[flood_col_map[["Corrected_Percent_Flooded_Area"]]]])
  )
)
flood <- flood[
  !is.na(flood$DistrictKey) & flood$DistrictKey != "" &
    is.finite(flood$Corrected_Percent_Flooded_Area),
]

flood_parts <- split(flood, flood$DistrictKey)
flood_features <- do.call(rbind, lapply(flood_parts, function(d) {
  data.frame(
    DistrictKey = d$DistrictKey[1],
    Flood_Dataset_District = d$Dist_Name[1],
    Percent_Flooded_Area = mean(d$Percent_Flooded_Area, na.rm = TRUE),
    Permanent_Water = mean(d$Permanent_Water, na.rm = TRUE),
    Corrected_Percent_Flooded_Area = mean(
      d$Corrected_Percent_Flooded_Area, na.rm = TRUE
    ),
    stringsAsFactors = FALSE
  )
}))
rownames(flood_features) <- NULL

# 7. Merge by district. No date-level flood labels exist in the flood-area file.
combined <- merge(
  rain_features, flood_features,
  by = "DistrictKey",
  all = FALSE,
  sort = TRUE
)

if (nrow(combined) < 100) {
  warning("Fewer than 100 districts matched. Review district name matching.")
}

# 8. Define a transparent PROXY target based on relative corrected flooded area.
# These are relative susceptibility tiers, NOT official event-risk thresholds.
q <- as.numeric(quantile(
  combined$Corrected_Percent_Flooded_Area,
  probs = c(1/3, 2/3), na.rm = TRUE, names = FALSE, type = 7
))
# Quantile cut points can be identical in small/rounded datasets. Use rank-based
# thirds as a fallback so the project does not crash on duplicate breaks.
if (length(unique(q)) < 2) {
  ranks <- rank(combined$Corrected_Percent_Flooded_Area, ties.method = "average", na.last = "keep")
  pct <- ranks / sum(is.finite(ranks))
  combined$Flood_Risk_Label <- ifelse(pct <= 1/3, "Low",
                                     ifelse(pct <= 2/3, "Medium", "High"))
  q <- as.numeric(quantile(combined$Corrected_Percent_Flooded_Area,
                           probs = c(1/3, 2/3), na.rm = TRUE, names = FALSE))
} else {
  combined$Flood_Risk_Label <- cut(
    combined$Corrected_Percent_Flooded_Area,
    breaks = c(-Inf, q[1], q[2], Inf),
    labels = c("Low", "Medium", "High"),
    include.lowest = TRUE, right = TRUE
  )
}
combined$Flood_Risk_Label <- factor(
  combined$Flood_Risk_Label,
  levels = c("Low", "Medium", "High")
)

# Save merged data and district matching diagnostics.
write.csv(
  combined,
  file.path(project_folder, "district_flood_risk_dataset.csv"),
  row.names = FALSE
)

rain_keys <- unique(rain_features$DistrictKey)
flood_keys <- unique(flood_features$DistrictKey)
unmatched_rain <- rain_features[!rain_features$DistrictKey %in% flood_keys, ]
unmatched_flood <- flood_features[!flood_features$DistrictKey %in% rain_keys, ]
write.csv(unmatched_rain, file.path(project_folder, "unmatched_rainfall_districts.csv"),
          row.names = FALSE)
write.csv(unmatched_flood, file.path(project_folder, "unmatched_flood_area_districts.csv"),
          row.names = FALSE)

cat("\n--- DATA CHECK ---\n")
cat("Rainfall date range:", as.character(min(rain$Date)), "to",
    as.character(max(rain$Date)), "\n")
cat("Rainfall districts:", length(unique(rain$DistrictKey)), "\n")
cat("Flood-area districts:", length(unique(flood_features$DistrictKey)), "\n")
cat("Matched districts:", nrow(combined), "\n")
cat("Flood-risk proxy class counts:\n")
print(table(combined$Flood_Risk_Label))
cat("\nSaved unmatched district lists for review.\n")

# 9. Train/test split by district (not by daily rows).
# Target = relative flood-area susceptibility tier.
# Predictors = rainfall features only. Flood-area columns are not predictors.
feature_names <- c(
  "Rain_Days", "Rain_Mean_mm", "Rain_Max_Daily_mm",
  "Rain_3Day_Max_mm", "Rain_7Day_Max_mm",
  "Wet_Days_2_5mm", "Rain_Normal_Mean_mm", "Rain_Excess_Mean_mm"
)

model_data <- combined[, c("DistrictKey", "State", "District",
                           "Flood_Risk_Label", feature_names)]
model_data <- model_data[complete.cases(model_data[, c("Flood_Risk_Label", feature_names)]), ]
model_data$Flood_Risk_Label <- droplevels(model_data$Flood_Risk_Label)

if (nrow(model_data) < 30 || nlevels(model_data$Flood_Risk_Label) < 2) {
  stop("Not enough matched complete districts/classes to train a model.")
}

# Stratified split: sample within each class to keep class representation in test.
set.seed(42)
train_index <- unlist(lapply(levels(model_data$Flood_Risk_Label), function(cl) {
  ids <- which(model_data$Flood_Risk_Label == cl)
  if (length(ids) <= 1) return(ids)
  n_train <- min(length(ids) - 1, max(1, floor(0.80 * length(ids))))
  sample(ids, n_train)
}), use.names = FALSE)
train_index <- sort(unique(train_index))
test_index <- setdiff(seq_len(nrow(model_data)), train_index)
if (!length(test_index)) stop("Test set is empty; more matched districts are needed.")
train <- model_data[train_index, , drop = FALSE]
test <- model_data[test_index, , drop = FALSE]

# Ensure class factor levels remain consistent.
train$Flood_Risk_Label <- factor(train$Flood_Risk_Label,
                                 levels = c("Low", "Medium", "High"))
test$Flood_Risk_Label <- factor(test$Flood_Risk_Label,
                                levels = c("Low", "Medium", "High"))

set.seed(42)
rf_model <- randomForest(
  x = train[, feature_names],
  y = train$Flood_Risk_Label,
  ntree = 500,
  importance = TRUE
)

pred <- predict(rf_model, newdata = test[, feature_names], type = "class")
pred <- factor(pred, levels = c("Low", "Medium", "High"))
actual <- factor(test$Flood_Risk_Label, levels = c("Low", "Medium", "High"))

conf <- table(Actual = actual, Predicted = pred)
write.csv(as.data.frame.matrix(conf),
          file.path(project_folder, "confusion_matrix.csv"))

# Per-class precision, recall and F1.
metric_rows <- lapply(c("Low", "Medium", "High"), function(cl) {
  tp <- sum(actual == cl & pred == cl)
  fp <- sum(actual != cl & pred == cl)
  fn <- sum(actual == cl & pred != cl)
  precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
  recall <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
  f1 <- if (is.na(precision) || is.na(recall) || precision + recall == 0) {
    NA_real_
  } else {
    2 * precision * recall / (precision + recall)
  }
  data.frame(Class = cl, Precision = precision, Recall = recall, F1_Score = f1)
})
class_metrics <- do.call(rbind, metric_rows)

accuracy <- mean(actual == pred)
balanced_accuracy <- mean(class_metrics$Recall, na.rm = TRUE)

model_metrics <- data.frame(
  Metric = c("Test Accuracy", "Balanced Accuracy", "Matched districts",
             "Train districts", "Test districts",
             "Low/Medium boundary (corrected flooded area %)",
             "Medium/High boundary (corrected flooded area %)"),
  Value = c(
    accuracy, balanced_accuracy, nrow(model_data), nrow(train), nrow(test),
    q[1], q[2]
  )
)
write.csv(model_metrics, file.path(project_folder, "model_metrics.csv"),
          row.names = FALSE)
write.csv(class_metrics, file.path(project_folder, "class_metrics.csv"),
          row.names = FALSE)

# 10. Fit final model on all matched districts and predict proxy tier.
set.seed(42)
final_model <- randomForest(
  x = model_data[, feature_names],
  y = factor(model_data$Flood_Risk_Label, levels = c("Low", "Medium", "High")),
  ntree = 500,
  importance = TRUE
)

all_predictions <- predict(final_model, newdata = model_data[, feature_names],
                           type = "class")
probabilities <- predict(final_model, newdata = model_data[, feature_names],
                         type = "prob")

predictions <- model_data[, c("DistrictKey", "State", "District")]
predictions$Flood_Risk_Label <- model_data$Flood_Risk_Label
predictions$Predicted_Flood_Risk <- factor(
  all_predictions, levels = c("Low", "Medium", "High")
)
predictions$Corrected_Percent_Flooded_Area <- combined$Corrected_Percent_Flooded_Area[
  match(predictions$DistrictKey, combined$DistrictKey)
]
predictions$Rain_Mean_mm <- model_data$Rain_Mean_mm
predictions$Rain_Max_Daily_mm <- model_data$Rain_Max_Daily_mm
predictions$Rain_3Day_Max_mm <- model_data$Rain_3Day_Max_mm
predictions$Rain_7Day_Max_mm <- model_data$Rain_7Day_Max_mm
predictions$Low_Probability <- probabilities[, "Low"]
predictions$Medium_Probability <- probabilities[, "Medium"]
predictions$High_Probability <- probabilities[, "High"]

write.csv(predictions,
          file.path(project_folder, "district_risk_predictions.csv"),
          row.names = FALSE)
saveRDS(final_model, file.path(project_folder, "flood_risk_random_forest.rds"))

# Save feature importance
importance_data <- as.data.frame(importance(final_model))
importance_data$Feature <- rownames(importance_data)
rownames(importance_data) <- NULL
write.csv(importance_data, file.path(project_folder, "feature_importance.csv"),
          row.names = FALSE)

# Plot model importance
png(file.path(project_folder, "feature_importance.png"),
    width = 1200, height = 800, res = 130)
varImpPlot(final_model, main = "Rainfall Feature Importance")
dev.off()

cat("\n--- MODEL EVALUATION ---\n")
print(conf)
print(model_metrics)
print(class_metrics)
cat("\nSaved files:\n")
cat("- district_flood_risk_dataset.csv\n")
cat("- district_risk_predictions.csv\n")
cat("- confusion_matrix.csv\n")
cat("- model_metrics.csv\n")
cat("- class_metrics.csv\n")
cat("- feature_importance.csv / feature_importance.png\n")
cat("- flood_risk_random_forest.rds\n")

cat(
  "\nSCIENTIFIC LIMITATION:\n",
  "The target labels are relative tiers created from corrected flooded-area percentage.\n",
  "This model estimates district-level susceptibility tiers, not whether a flood will happen\n",
  "on a specific future date. The rainfall data spans only a short period in 2026.\n",
  "Do not report this as validated event-based flood prediction.\n",
  sep = ""
)
