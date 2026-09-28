library(shiny)
library(httr)
library(jsonlite)
library(sf)
library(dplyr)
library(leaflet)
library(DT)
library(openxlsx)

PROPERTY_URL <- "https://geodata.baltimorecity.gov/egis/rest/services/CityView/Realproperty_OB/FeatureServer/0"
NEIGHBOR_URL <- "https://geodata.baltimorecity.gov/egis/rest/services/CityView/Neighborhoods/FeatureServer/0"

FIELDS <- paste(
  c(
    "OBJECTID", "BLOCKLOT", "BLOCK", "LOT", "FULLADDR", "NEIGHBOR",
    "STDIRPRE", "ST_NAME", "ST_TYPE", "BLDG_NO", "ZIP_CODE",
    "MAILTOADD", "PERMHOME", "AR_OWNER", "SALEDATE", "USEGROUP",
    "OWNER_1", "OWNER_2", "OWNER_3", "VACIND", "CITY_TAX",
    "SALEPRIC", "YEAR_BUILD"
  ),
  collapse = ","
)

arc_query <- function(url, params) {
  response <- httr::GET(
    paste0(url, "/query"),
    query = c(list(f = "geojson"), params),
    httr::timeout(90),
    httr::user_agent("BaltimoreSuitabilityExplorer/0.2")
  )
  
  httr::stop_for_status(response)
  
  content <- httr::content(
    response,
    as = "text",
    encoding = "UTF-8"
  )
  
  error <- tryCatch(
    jsonlite::fromJSON(content)$error,
    error = function(e) NULL
  )
  
  if (!is.null(error)) {
    stop(
      paste("City GIS service:", error$message),
      call. = FALSE
    )
  }
  
  path <- tempfile(fileext = ".geojson")
  on.exit(unlink(path), add = TRUE)
  
  writeLines(content, path, useBytes = TRUE)
  sf::st_read(path, quiet = TRUE)
}

arc_json <- function(url, params) {
  response <- httr::GET(
    paste0(url, "/query"),
    query = c(list(f = "json"), params),
    httr::timeout(90)
  )
  
  httr::stop_for_status(response)
  
  data <- jsonlite::fromJSON(
    httr::content(response, as = "text", encoding = "UTF-8")
  )
  
  if (!is.null(data$error)) {
    stop(
      paste("City GIS service:", data$error$message),
      call. = FALSE
    )
  }
  
  data
}

sql_names <- function(names, field) {
  stopifnot(
    length(names) > 0,
    field %in% c("Name", "NEIGHBOR")
  )
  
  quoted <- sprintf(
    "'%s'",
    gsub("'", "''", names, fixed = TRUE)
  )
  
  paste0(
    field,
    " IN (",
    paste(quoted, collapse = ","),
    ")"
  )
}

get_neighborhood_names <- function() {
  data <- arc_json(
    NEIGHBOR_URL,
    list(
      where = "1=1",
      outFields = "Name",
      returnGeometry = "false",
      resultRecordCount = 2000
    )
  )
  
  sort(unique(stats::na.omit(
    as.character(data$features$attributes$Name)
  )))
}

fetch_neighborhoods <- function(names) {
  polygons <- arc_query(
    NEIGHBOR_URL,
    list(
      where = sql_names(names, "Name"),
      outFields = "Name",
      returnGeometry = "true",
      outSR = 4326
    )
  )
  
  if (nrow(polygons) == 0) {
    stop(
      "No neighborhood boundary was found for this selection.",
      call. = FALSE
    )
  }
  
  polygons
}

fetch_properties <- function(names, max_rows = 30000L) {
  batches <- split(
    names,
    ceiling(seq_along(names) / 10)
  )
  
  chunks <- lapply(batches, function(batch) {
    where <- sql_names(batch, "NEIGHBOR")
    
    count <- arc_json(
      PROPERTY_URL,
      list(
        where = where,
        returnCountOnly = "true"
      )
    )$count
    
    if (count > max_rows) {
      stop(
        "Select fewer neighborhoods. This selection exceeds 30,000 properties.",
        call. = FALSE
      )
    }
    
    if (count == 0) {
      return(NULL)
    }
    
    offsets <- seq.int(0, count - 1, by = 1000)
    
    pages <- lapply(offsets, function(offset) {
      page <- arc_query(
        PROPERTY_URL,
        list(
          where = where,
          outFields = FIELDS,
          returnGeometry = "true",
          outSR = 4326,
          orderByFields = "OBJECTID ASC",
          resultOffset = offset,
          resultRecordCount = 1000
        )
      )
      
      if (nrow(page) == 0) {
        stop(
          "City GIS returned an incomplete page. Please rerun.",
          call. = FALSE
        )
      }
      
      page
    })
    
    dplyr::bind_rows(pages)
  })
  
  rows <- dplyr::bind_rows(chunks)
  
  if (nrow(rows) == 0) {
    stop(
      "No property records were returned for the selected neighborhoods.",
      call. = FALSE
    )
  }
  
  if (nrow(rows) > max_rows) {
    stop(
      "Select fewer neighborhoods. This selection exceeds 30,000 properties.",
      call. = FALSE
    )
  }
  
  rows[!duplicated(rows$OBJECTID), ]
}

parse_sale <- function(value) {
  digits <- gsub(
    "[^0-9]",
    "",
    as.character(value)
  )
  
  digits[nchar(digits) != 8] <- NA_character_
  
  # The City's raw SALEDATE is MMDDYYYY text.
  date <- suppressWarnings(
    as.Date(digits, format = "%m%d%Y")
  )
  
  invalid <- !is.na(date) &
    (
      date < as.Date("1800-01-01") |
        date > Sys.Date()
    )
  
  date[invalid] <- as.Date(NA)
  date
}

normalize_street <- function(value) {
  value <- toupper(trimws(as.character(value)))
  value[is.na(value) | value == ""] <- NA_character_
  
  value <- sub(",.*$", "", value)
  value <- gsub("[^A-Z0-9 ]", " ", value)
  value <- gsub("\\s+", " ", trimws(value))
  
  # Treat 0745 RYAN ST and 745 RYAN ST as the same address.
  sub("^0+([0-9]+)", "\\1", value)
}

mail_zip <- function(value) {
  text <- as.character(value)
  
  # Keep one result per property, including missing ZIPs.
  matched <- !is.na(text) &
    grepl(
      "[0-9]{5}(-[0-9]{4})?\\s*$",
      text
    )
  
  result <- rep(
    NA_character_,
    length(text)
  )
  
  result[matched] <- substr(
    regmatches(
      text[matched],
      regexpr(
        "[0-9]{5}(-[0-9]{4})?\\s*$",
        text[matched]
      )
    ),
    1,
    5
  )
  
  result
}

street_block <- function(number, direction, street, type) {
  block <- suppressWarnings(as.integer(number))
  
  block <- ifelse(
    is.na(block),
    NA_character_,
    as.character((block %/% 100L) * 100L)
  )
  
  parts <- cbind(
    block,
    as.character(direction),
    as.character(street),
    as.character(type)
  )
  
  apply(parts, 1, function(row) {
    row <- trimws(row)
    
    row <- row[
      !is.na(row) &
        nzchar(row) &
        row != "NA"
    ]
    
    if (length(row) == 0) {
      NA_character_
    } else {
      paste(row, collapse = " ")
    }
  })
}

classify_properties <- function(rows) {
  sale <- parse_sale(rows$SALEDATE)
  mailing <- as.character(rows$MAILTOADD)
  
  home_zip <- sub(
    "\\.0$",
    "",
    as.character(rows$ZIP_CODE)
  )
  
  mailing_zip <- mail_zip(mailing)
  
  same_zip <- !is.na(mailing_zip) &
    !is.na(home_zip) &
    mailing_zip == home_zip
  
  property_street <- normalize_street(rows$FULLADDR)
  mailing_street <- normalize_street(mailing)
  
  same_street <- !is.na(property_street) &
    !is.na(mailing_street) &
    property_street == mailing_street
  
  flags <-
    toupper(trimws(as.character(rows$PERMHOME))) %in% c("H", "D") |
    toupper(trimws(as.character(rows$AR_OWNER))) %in% c("H", "D")
  
  matched <- same_zip & same_street & flags
  
  rows$address <- as.character(rows$FULLADDR)
  rows$neighborhood <- as.character(rows$NEIGHBOR)
  
  rows$street_block <- street_block(
    rows$BLDG_NO,
    rows$STDIRPRE,
    rows$ST_NAME,
    rows$ST_TYPE
  )
  
  rows$mail_zip <- mailing_zip
  rows$address_match <- ifelse(same_street, "YES", "NO")
  rows$zip_match <- ifelse(same_zip, "YES", "NO")
  rows$homeowner <- ifelse(flags, "YES", "NO")
  rows$sale_date <- sale
  
  rows$sale_year <- suppressWarnings(
    as.integer(format(sale, "%Y"))
  )
  
  for (years in c(10L, 15L, 20L, 25L, 30L)) {
    cutoff <- as.Date(sprintf(
      "%s-%s",
      as.integer(format(Sys.Date(), "%Y")) - years,
      format(Sys.Date(), "%m-%d")
    ))
    
    column <- paste0("legacy_", years, "y")
    
    rows[[column]] <- ifelse(
      matched &
        !is.na(sale) &
        sale <= cutoff,
      "Legacy",
      "Not Legacy"
    )
  }
  
  rows
}

property_table <- function(properties) {
  sf::st_drop_geometry(properties) |>
    dplyr::transmute(
      FULLADDR = address,
      NEIGHBOR = neighborhood,
      Street_Block = street_block,
      Address_Match = address_match,
      MailZip_Match_HomeZip = zip_match,
      Homeowner = homeowner,
      `10Y` = legacy_10y,
      `15Y` = legacy_15y,
      `20Y` = legacy_20y,
      `25Y` = legacy_25y,
      `30Y` = legacy_30y,
      BLOCKLOT,
      BLOCK,
      LOT,
      PERMHOME,
      AR_OWNER,
      USEGROUP,
      SALEDATE,
      Sale_Year = sale_year,
      Sale_Date_Parsed = sale_date,
      OWNER_1,
      OWNER_2,
      OWNER_3,
      ZIP_CODE,
      Mail_ZIP = mail_zip,
      MAILTOADD,
      STDIRPRE,
      ST_NAME,
      ST_TYPE,
      BLDG_NO,
      YEAR_BUILD,
      SALEPRIC,
      CITY_TAX,
      VACIND
    )
}

street_block_pivot <- function(properties) {
  property_table(properties) |>
    dplyr::group_by(NEIGHBOR, Street_Block) |>
    dplyr::summarise(
      Properties = dplyr::n(),
      
      Address_and_ZIP_match = sum(
        Address_Match == "YES" &
          MailZip_Match_HomeZip == "YES"
      ),
      
      Homeowners = sum(Homeowner == "YES"),
      
      `10Y_Legacy` = sum(`10Y` == "Legacy"),
      `15Y_Legacy` = sum(`15Y` == "Legacy"),
      `20Y_Legacy` = sum(`20Y` == "Legacy"),
      `25Y_Legacy` = sum(`25Y` == "Legacy"),
      `30Y_Legacy` = sum(`30Y` == "Legacy"),
      
      .groups = "drop"
    ) |>
    dplyr::arrange(NEIGHBOR, Street_Block)
}

neighborhood_summary <- function(properties) {
  street_block_pivot(properties) |>
    dplyr::group_by(NEIGHBOR) |>
    dplyr::summarise(
      dplyr::across(
        -Street_Block,
        ~ sum(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

write_report <- function(result, file) {
  workbook <- openxlsx::createWorkbook()
  
  sheets <- list(
    "Final Pivot" = street_block_pivot(result$properties),
    "Neighborhood Summary" = neighborhood_summary(result$properties),
    "Property Additions" = property_table(result$properties),
    
    "Method and Sources" = data.frame(
      item = c(
        "Retrieved",
        "Reference date",
        "Address match",
        "ZIP match",
        "Homeowner",
        "Legacy thresholds",
        "Property source",
        "Neighborhood source",
        "Interpretation"
      ),
      
      value = c(
        format(result$fetched, "%Y-%m-%d %H:%M %Z"),
        as.character(result$settings$reference_date),
        
        paste(
          "Property street address equals mailing street address",
          "after punctuation and house-number zero normalization"
        ),
        
        "ZIP_CODE equals final five-digit ZIP in MAILTOADD",
        "PERMHOME or AR_OWNER is H or D",
        
        paste(
          "10, 15, 20, 25, 30 years from reference date;",
          "all address, ZIP, and homeowner checks required"
        ),
        
        PROPERTY_URL,
        NEIGHBOR_URL,
        
        paste(
          "Screening only. Recorded sale date is a tenure proxy;",
          "verify occupancy and grant eligibility."
        )
      )
    )
  )
  
  for (name in names(sheets)) {
    openxlsx::addWorksheet(workbook, name)
    
    openxlsx::writeDataTable(
      workbook,
      name,
      sheets[[name]],
      tableStyle = "TableStyleMedium4"
    )
    
    openxlsx::freezePane(
      workbook,
      name,
      firstRow = TRUE
    )
    
    openxlsx::setColWidths(
      workbook,
      name,
      cols = seq_len(ncol(sheets[[name]])),
      widths = "auto"
    )
  }
  
  openxlsx::saveWorkbook(
    workbook,
    file,
    overwrite = TRUE
  )
}

ui <- fluidPage(
  titlePanel("HNI LEGACY HOMEOWNER SUITABILITY EXPLORER"),
  
  sidebarLayout(
    sidebarPanel(
      helpText(
        "Select one or more Baltimore neighborhoods to see the legcay statues of the property. Each run requests pulls current Baltimore City Real Property records."
      ),
      
      selectizeInput(
        "neighborhoods",
        "Neighborhoods",
        choices = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "Search for neighborhoods"
        )
      ),
      
      sliderInput(
        "map_years",
        "Map legacy threshold (years)",
        min = 5,
        max = 30,
        value = 20,
        step = 5
      ),
      
      actionButton(
        "run",
        "Run analysis",
        class = "btn-primary"
      ),
      
      br(),
      br(),
      
      downloadButton(
        "export",
        "Download Excel workbook"
      ),
      
      hr(),
      
      helpText(
        "To be labled LEGACY a property requires matching property and mailing street addresses, matching ZIP codes, a homeowner flag in PERMHOME or AR_OWNER, and a sale date before the tenure cutoff."
      ),
      
      tags$p(
        tags$a(
          "City Real Property source",
          href = PROPERTY_URL,
          target = "_blank"
        )
      )
    ),
    
    mainPanel(
      textOutput("status"),
      
      tabsetPanel(
        tabPanel(
          "MAP",
          leafletOutput("map", height = 650)
        ),
        
        tabPanel(
          "NEIGHBORHOOD SUMMARY",
          DTOutput("summary")
        ),
        
        tabPanel(
          "NEIGHBORHOOD STREET BLOCK PIVOT",
          DTOutput("pivot")
        ),
        
        tabPanel(
          "PROPERTY RECORDS",
          DTOutput("properties")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observe({
    tryCatch(
      {
        names <- get_neighborhood_names()
        
        updateSelectizeInput(
          session,
          "neighborhoods",
          choices = names,
          server = TRUE
        )
      },
      
      error = function(e) {
        showNotification(
          paste(
            "Could not load neighborhood names:",
            conditionMessage(e)
          ),
          type = "error",
          duration = NULL
        )
      }
    )
  })
  
  result <- eventReactive(input$run, {
    req(length(input$neighborhoods) > 0)
    
    withProgress(
      message = "Fetching current City property data",
      value = 0,
      {
        polygons <- fetch_neighborhoods(
          input$neighborhoods
        )
        
        incProgress(0.2)
        
        raw <- fetch_properties(
          input$neighborhoods
        )
        
        incProgress(0.6)
        
        properties <- classify_properties(raw)
        
        list(
          properties = properties,
          boundaries = polygons,
          fetched = Sys.time(),
          settings = list(
            reference_date = Sys.Date()
          )
        )
      }
    )
  })
  
  output$status <- renderText({
    analysis <- result()
    
    sprintf(
      "%s properties across %s neighborhoods. Retrieved %s.",
      format(
        nrow(analysis$properties),
        big.mark = ","
      ),
      length(unique(
        analysis$properties$neighborhood
      )),
      format(
        analysis$fetched,
        "%Y-%m-%d %H:%M %Z"
      )
    )
  })
  
  output$map <- renderLeaflet({
    analysis <- result()
    
    boundaries <- sf::st_transform(
      analysis$boundaries,
      4326
    )
    
    properties <- analysis$properties
    
    # Create the 60% black mask outside selected neighborhoods.
    projected <- sf::st_transform(
      boundaries,
      26918
    )
    
    outer_area <- sf::st_buffer(
      sf::st_as_sfc(
        sf::st_bbox(projected)
      ),
      dist = 100000
    )
    
    selected_area <- sf::st_union(
      sf::st_make_valid(
        sf::st_geometry(projected)
      )
    )
    
    mask <- sf::st_difference(
      outer_area,
      selected_area
    ) |>
      sf::st_transform(4326)
    
    map <- leaflet() |>
      addTiles() |>
      addPolygons(
        data = mask,
        color = "transparent",
        weight = 0,
        fillColor = "#000000",
        fillOpacity = 0.60
      ) |>
      addPolygons(
        data = boundaries,
        fill = FALSE,
        color = "#214e74",
        weight = 2,
        label = ~Name,
        group = "Selected neighborhoods"
      )
    
    if (nrow(properties) > 0) {
      points <- sf::st_point_on_surface(
        sf::st_transform(
          properties,
          26918
        )
      ) |>
        sf::st_transform(4326)
      
      coordinates <- sf::st_coordinates(points)
      
      # The slider changes map colors only.
      # It does not change the tables or Excel export.
      reference_date <- analysis$settings$reference_date
      
      cutoff <- as.Date(sprintf(
        "%s-%s",
        as.integer(format(reference_date, "%Y")) -
          input$map_years,
        format(reference_date, "%m-%d")
      ))
      
      qualifies <-
        properties$address_match == "YES" &
        properties$zip_match == "YES" &
        properties$homeowner == "YES" &
        !is.na(properties$sale_date) &
        properties$sale_date <= cutoff
      
      status <- ifelse(
        qualifies,
        "Legacy",
        "Not Legacy"
      )
      
      point_colors <- ifelse(
        qualifies,
        "#218a55",
        "#c43c3c"
      )
      
      sale_label <- ifelse(
        is.na(properties$sale_date),
        "Unknown",
        format(properties$sale_date, "%m/%d/%Y")
      )
      
      popup <- sprintf(
        "<strong>%s</strong><br>%s<br>Street block: %s<br>Sale date: %s<br>Address match: %s<br>ZIP match: %s<br>Homeowner: %s<br>%sY: %s",
        htmltools::htmlEscape(as.character(properties$address)),
        htmltools::htmlEscape(as.character(properties$neighborhood)),
        htmltools::htmlEscape(as.character(properties$street_block)),
        sale_label,
        properties$address_match,
        properties$zip_match,
        properties$homeowner,
        input$map_years,
        status
      )
      
      map <- map |>
        addCircleMarkers(
          lng = coordinates[, 1],
          lat = coordinates[, 2],
          radius = 4,
          stroke = FALSE,
          fillOpacity = 0.7,
          color = point_colors,
          popup = popup,
          group = "Properties"
        ) |>
        addLegend(
          "bottomright",
          colors = c(
            "#218a55",
            "#c43c3c"
          ),
          labels = c(
            paste0(
              input$map_years,
              "Y Legacy"
            ),
            "Not Legacy"
          ),
          title = paste0(
            input$map_years,
            "-year screening"
          )
        )
    }
    
    bounds <- sf::st_bbox(boundaries)
    
    map |>
      fitBounds(
        bounds[["xmin"]],
        bounds[["ymin"]],
        bounds[["xmax"]],
        bounds[["ymax"]]
      ) |>
      addLayersControl(
        overlayGroups = c(
          "Selected neighborhoods",
          "Properties"
        ),
        options = layersControlOptions(
          collapsed = FALSE
        )
      )
  })
  
  output$summary <- renderDT({
    datatable(
      neighborhood_summary(
        result()$properties
      ),
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  output$pivot <- renderDT({
    datatable(
      street_block_pivot(
        result()$properties
      ),
      options = list(
        pageLength = 25,
        scrollX = TRUE
      ),
      rownames = FALSE,
      filter = "top"
    )
  })
  
  output$properties <- renderDT({
    datatable(
      property_table(
        result()$properties
      ),
      options = list(
        pageLength = 25,
        scrollX = TRUE
      ),
      rownames = FALSE,
      filter = "top"
    )
  })
  
  output$export <- downloadHandler(
    filename = function() {
      paste0(
        "baltimore_suitability_",
        format(Sys.Date(), "%Y%m%d"),
        ".xlsx"
      )
    },
    
    content = function(file) {
      write_report(
        result(),
        file
      )
    }
  )
}

shinyApp(ui, server)