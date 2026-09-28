PROPERTY_URL <- "https://geodata.baltimorecity.gov/egis/rest/services/CityView/Realproperty_OB/FeatureServer/0"
NEIGHBOR_URL <- "https://geodata.baltimorecity.gov/egis/rest/services/CityView/Neighborhoods/FeatureServer/0"
FIELDS <- paste(c("OBJECTID", "BLOCKLOT", "FULLADDR", "NEIGHBOR", "YEAR_BUILD", "SALEDATE",
                  "PERMHOME", "USEGROUP", "VACIND", "FULLCASH"), collapse = ",")

arc_query <- function(url, params) {
  response <- httr::GET(paste0(url, "/query"), query = c(list(f = "geojson"), params),
                        httr::timeout(90), httr::user_agent("BaltimoreSuitabilityExplorer/0.1"))
  httr::stop_for_status(response)
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  error <- tryCatch(jsonlite::fromJSON(content)$error, error = function(e) NULL)
  if (!is.null(error)) stop(paste("City GIS service:", error$message), call. = FALSE)
  path <- tempfile(fileext = ".geojson")
  on.exit(unlink(path), add = TRUE)
  writeLines(content, path, useBytes = TRUE)
  sf::st_read(path, quiet = TRUE)
}

arc_json <- function(url, params) {
  response <- httr::GET(paste0(url, "/query"), query = c(list(f = "json"), params), httr::timeout(90))
  httr::stop_for_status(response)
  data <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
  if (!is.null(data$error)) stop(paste("City GIS service:", data$error$message), call. = FALSE)
  data
}

sql_names <- function(names, field) {
  stopifnot(length(names) > 0, field %in% c("Name", "NEIGHBOR"))
  paste0(field, " IN (", paste(sprintf("'%s'", gsub("'", "''", names, fixed = TRUE)), collapse = ","), ")")
}

get_neighborhood_names <- function() {
  data <- arc_json(NEIGHBOR_URL, list(where = "1=1", outFields = "Name",
                                        returnGeometry = "false", resultRecordCount = 2000))
  sort(unique(stats::na.omit(vapply(data$features$attributes$Name, as.character, ""))))
}

fetch_neighborhoods <- function(names) {
  polygons <- arc_query(NEIGHBOR_URL, list(where = sql_names(names, "Name"), outFields = "Name",
                                           returnGeometry = "true", outSR = 4326))
  if (!nrow(polygons)) stop("No neighborhood boundary was found for this selection.", call. = FALSE)
  polygons
}

fetch_properties <- function(names, max_rows = 30000L) {
  chunks <- lapply(split(names, ceiling(seq_along(names) / 10)), function(batch) {
    where <- sql_names(batch, "NEIGHBOR")
    count <- arc_json(PROPERTY_URL, list(where = where, returnCountOnly = "true"))$count
    if (count > max_rows) stop("This selection exceeds the 30,000-property limit. Select fewer neighborhoods.", call. = FALSE)
    if (!count) return(NULL)
    pages <- lapply(seq.int(0, count - 1, by = 1000), function(offset) {
      page <- arc_query(PROPERTY_URL, list(where = where, outFields = FIELDS,
                              returnGeometry = "true", outSR = 4326,
                              orderByFields = "OBJECTID ASC", resultOffset = offset,
                              resultRecordCount = 1000))
      if (!nrow(page)) stop("City GIS returned an incomplete page. Please rerun the analysis.", call. = FALSE)
      page
    })
    dplyr::bind_rows(pages)
  })
  rows <- dplyr::bind_rows(chunks)
  if (!nrow(rows)) stop("No property records were returned for the selected neighborhoods.", call. = FALSE)
  if (nrow(rows) > max_rows) stop("This selection exceeds the 30,000-property limit. Select fewer neighborhoods.", call. = FALSE)
  rows <- rows[!duplicated(rows$OBJECTID), ]
  rows
}

parse_sale <- function(value) {
  value <- gsub("[^0-9]", "", as.character(value))
  # The City's sale dates are eight-character text; accept YYYYMMDD and MMDDYYYY.
  ymd <- as.Date(value, format = "%Y%m%d")
  mdy <- as.Date(value, format = "%m%d%Y")
  candidate <- ifelse(!is.na(ymd) & ymd <= Sys.Date() & ymd >= as.Date("1800-01-01"),
                      format(ymd), format(mdy))
  as.Date(candidate)
}

score_properties <- function(rows, built_before, held_years, older, held, home) {
  build <- suppressWarnings(as.integer(rows$YEAR_BUILD))
  date <- parse_sale(rows$SALEDATE)
  permanent <- toupper(trimws(as.character(rows$PERMHOME)))
  old_flag <- !is.na(build) & build > 1700 & build <= built_before
  held_flag <- !is.na(date) & date <= (Sys.Date() - round(held_years * 365.25))
  home_flag <- !is.na(permanent) & permanent %in% c("Y", "YES", "1")
  rows$address <- as.character(rows$FULLADDR)
  rows$neighborhood <- as.character(rows$NEIGHBOR)
  rows$year_built <- build
  rows$sale_date <- date
  rows$permanent_home <- permanent
  rows$built_before_threshold <- old_flag
  rows$held_since_threshold <- held_flag
  rows$permanent_home_flag <- home_flag
  rows$score <- as.integer(older) * old_flag + as.integer(held) * held_flag + as.integer(home) * home_flag
  rows
}

property_table <- function(properties) {
  sf::st_drop_geometry(properties) |>
    dplyr::transmute(block_lot = BLOCKLOT, address, neighborhood, score,
                     built_before_threshold, held_since_threshold, permanent_home_flag,
                     year_built, sale_date, permanent_home, use_group = USEGROUP,
                     vacancy_indicator = VACIND, assessed_value = FULLCASH)
}

neighborhood_summary <- function(properties) {
  property_table(properties) |>
    dplyr::group_by(neighborhood) |>
    dplyr::summarise(properties = dplyr::n(), average_score = round(mean(score), 2),
                     older_buildings = sum(built_before_threshold),
                     older_buildings_pct = round(mean(built_before_threshold), 3),
                     earlier_sales = sum(held_since_threshold),
                     earlier_sales_pct = round(mean(held_since_threshold), 3),
                     permanent_home_flags = sum(permanent_home_flag),
                     permanent_home_pct = round(mean(permanent_home_flag), 3),
                     missing_build_year = sum(is.na(year_built)),
                     missing_sale_date = sum(is.na(sale_date)), .groups = "drop")
}

score_breakdown <- function(properties) {
  property_table(properties) |>
    dplyr::count(neighborhood, score, name = "properties") |>
    tidyr::pivot_wider(names_from = score, values_from = properties, names_prefix = "score_", values_fill = 0)
}

write_report <- function(result, file) {
  wb <- openxlsx::createWorkbook()
  sheets <- list("Neighborhood summary" = neighborhood_summary(result$properties),
                 "Score breakdown" = score_breakdown(result$properties),
                 "Property records" = property_table(result$properties),
                 "Method and sources" = data.frame(
                   item = c("Retrieved", "Built in or before", "Years since sale", "Building age counted",
                            "Years since sale counted", "Permanent home counted", "Scoring method",
                            "Property source", "Neighborhood source", "Interpretation"),
                   value = c(format(result$fetched, "%Y-%m-%d %H:%M %Z"), result$settings$built_before,
                             result$settings$held_years, result$settings$older, result$settings$held,
                             result$settings$home, "One point for each enabled condition; missing values score zero",
                             PROPERTY_URL, NEIGHBOR_URL,
                             "Screening only. Sale date is not verified tenure; permanent home indicator is not verified occupancy.")))
  for (name in names(sheets)) {
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeDataTable(wb, name, sheets[[name]], tableStyle = "TableStyleMedium2")
    openxlsx::freezePane(wb, name, firstRow = TRUE)
    openxlsx::setColWidths(wb, name, cols = seq_len(ncol(sheets[[name]])), widths = "auto")
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}
