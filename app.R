ui <- bslib::page_sidebar(
  title = "Get sunrise, sunset, and day length for any location on Earth",
  sidebar = bslib::sidebar(
    bslib::card(
      bslib::card_header(
        shiny::icon("info-circle"), "About this app"
      ),
      bslib::card_body(
        shiny::markdown("
          #### Sun Calculator
          This interactive application calculates sunrise, sunset times, and daylight hours for virtually any location on Earth.

          ##### How to use:
          1. **Search** for a location using the map search bar
          2. **Pan & Zoom** (optional) on the map to refine your selection
          3. **Press** the **Run Sun Calculator** button to generate results
        ")
      )
    ),
    shiny::actionButton(
      "run_suncalc", "Run Sun Calculator",
      icon = shiny::icon("sun")
    ),
    bslib::card(
      bslib::card_header(
        shiny::icon("user"), "Author"
      ),
      bslib::card_body(
        shiny::markdown("
          **Ashirwad Barnwal**

          [<i class='fab fa-linkedin'></i> Connect on LinkedIn](https://www.linkedin.com/in/ashirwad1992)
        ")
      )
    ),
    width = 350
  ),
  bslib::layout_column_wrap(
    width = 1 / 2,
    bslib::layout_column_wrap(
      width = 1,
      bslib::card(
        shinycssloaders::withSpinner(leaflet::leafletOutput("map"), fill = TRUE)
      ),
      bslib::card(
        shinycssloaders::withSpinner(
          DT::DTOutput("daylight_info_tbl", fill = TRUE)
        )
      )
    ),
    bslib::card(
      shinycssloaders::withSpinner(
        shiny::plotOutput("plot"),
        fill = TRUE
      )
    )
  )
)


server <- function(input, output, session) {
  map_center <- shiny::reactiveValues(lng = -95.7129, lat = 37.0902)

  shiny::observeEvent(input$map_center, {
    map_center$lng <- input$map_center$lng
    map_center$lat <- input$map_center$lat
  })

  calc_trigger <- shiny::eventReactive(input$run_suncalc,
    {
      list(
        lng = map_center$lng,
        lat = map_center$lat
      )
    },
    ignoreNULL = FALSE
  )

  map_center_sf <- shiny::reactive({
    trigger <- calc_trigger()
    sf::st_sf(
      geometry = sf::st_sfc(sf::st_point(x = c(trigger$lng, trigger$lat))),
      crs = 4326
    )
  })

  tz <- shiny::reactive({
    lutz::tz_lookup(map_center_sf(), method = "accurate")
  })

  daylight_info_gg <- shiny::reactive({
    tibble::tibble(
      date_time = as.POSIXct(
        seq.Date(
          from = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
          to = as.Date(paste0(format(Sys.Date(), "%Y"), "-12-31")),
          by = 1
        ),
        tz = tz()
      ),
      sunrise_time = purrr::map_vec(
        date_time,
        ~ suntools::sunriset(
          crds = map_center_sf(),
          dateTime = .x,
          direction = "sunrise",
          POSIXct.out = TRUE
        ) |>
          dplyr::pull(time)
      ),
      sunset_time = purrr::map_vec(
        date_time,
        ~ suntools::sunriset(
          crds = map_center_sf(),
          dateTime = .x,
          direction = "sunset",
          POSIXct.out = TRUE
        ) |>
          dplyr::pull(time)
      ),
      day_length = lubridate::as.duration(sunset_time - sunrise_time)
    )
  })

  daylight_info_ggtext <- shiny::reactive({
    dplyr::bind_rows(
      dplyr::slice_min(daylight_info_gg(), day_length, with_ties = FALSE),
      dplyr::slice_max(daylight_info_gg(), day_length, with_ties = FALSE)
    ) |>
      dplyr::mutate(
        date_name = c("Shortest day", "Longest day"),
        date_pretty = purrr::map_chr(
          date_time, ~ verbaliseR::prettify_date(lubridate::as_date(.x))
        ),
        day_length_period = lubridate::as.period(day_length),
        day_length_pretty = sprintf(
          "%d:%02d", day_length_period@hour, day_length_period@minute
        ),
        sunset_time_pretty = format(sunset_time, "%l:%M %p") |> trimws(),
        vjust = c(0.75, 0.25),
        vjust = dplyr::case_when(
          sf::st_coordinates(map_center_sf())[2] < 0 ~ 1 - vjust,
          .default = vjust
        ),
        hjust = 0.5
      )
  })

  daylight_info_dt <- shiny::reactive({
    daylight_info_gg() |>
      dplyr::mutate(
        date = lubridate::as_date(date_time),
        date_long = format(date_time, "%A, %b %d, %Y"),
        sunrise_time_pretty = format(sunrise_time, "%l:%M %p") |> trimws(),
        sunset_time_pretty = format(sunset_time, "%l:%M %p") |> trimws(),
        sunrise_time = format(sunrise_time, "%I:%M %p"),
        sunset_time = format(sunset_time, "%I:%M %p"),,
        day_length_period = lubridate::as.period(day_length),
        day_length_pretty = sprintf(
          "%d:%02d", day_length_period@hour, day_length_period@minute
        )
      ) |>
      dplyr::select(
        date, date_long, sunrise_time, sunrise_time_pretty,
        sunset_time, sunset_time_pretty, day_length, day_length_pretty
      )
  })

  output$daylight_info_tbl <- DT::renderDT({
    DT::datatable(
      daylight_info_dt(),
      colnames = c(
        "Date<br>(ISO 8601)" = "date", "Date" = "date_long",
        "Sunrise Time" = "sunrise_time_pretty", "Sunset Time" = "sunset_time_pretty",
        "Length of Day (Hours:Minutes)" = "day_length_pretty"
      ),
      filter = "top",
      options = list(
        dom = "lrtip",
        pageLength = 5,
        order = list(list(1, 'asc')),
        columnDefs = list(
          list(orderData = 1, targets = 2),
          list(orderData = 3, targets = 4),
          list(orderData = 5, targets = 6),
          list(orderData = 7, targets = 8),
          list(visible = FALSE, targets = c(3, 5, 7)),
          list(searchable = TRUE, targets = 1),
          list(searchable = FALSE, targets = 2:8)
        )
      ),
      escape = FALSE
    )
  })

  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::fitBounds(-124.7844079, 24.7433195, -66.9513812, 49.3457868) |>
      leaflet.extras::addSearchOSM(
        options = leaflet.extras::searchOptions(
          collapsed = FALSE,
          minLength = 2,
          zoom = 12,
          textPlaceholder = "Search for a location..."
        )
      )
  })

  output$plot <- shiny::renderPlot({
    ggplot2::ggplot(daylight_info_gg()) +
      ggplot2::geom_col(
        ggplot2::aes(
          x = date_time,
          y = day_length
        ),
        fill = "#f9f8f3",
        linewidth = 0.2,
        color = "#316386"
      ) +
      ggplot2::coord_polar(start = -pi) +
      ggplot2::labs(
        title = "Daylight Hours Over the Year: Months Progressing Clockwise",
        subtitle = glue::glue(
          "Maximum daylight is approximately {round(max_daylight / min_daylight, 2)} times greater than the minimum",
          max_daylight = max(daylight_info_ggtext()$day_length),
          min_daylight = min(daylight_info_ggtext()$day_length)
        ),
        caption = "Heavily based on Cara Thompson's #30DayChartChallenge visualization<br>Source: github.com/cararthompson/30DayChartChallenge2023"
      ) +
      ggplot2::ylim(c(-20000, 100000)) +
      ggplot2::theme_void() +
      ggtext::geom_textbox(
        data = daylight_info_ggtext(),
        ggplot2::aes(
          x = date_time,
          y = day_length + 22000,
          label = glue::glue(
            "<span>**{date_name}**<br>{date_pretty}</span><br><br>{day_length_split} minutes<br>Dark at {sunset_time_pretty}",
            day_length_split = gsub(":", " hours ", day_length_pretty)
          ),
          vjust = vjust,
          hjust = hjust
        ),
        box.color = NA,
        fill = NA,
        color = "#DDE3E6",
        halign = 0.5,
        size = 5
      ) +
      ggplot2::theme(
        plot.title = ggtext::element_textbox_simple(
          size = 22, halign = 0.5,
          margin = ggplot2::margin(t = 1, b = 0.5, unit = "cm"),
          color = "#fafafa"
        ),
        plot.subtitle = ggtext::element_textbox_simple(
          size = 18, halign = 0.5,
          margin = ggplot2::margin(t = 0.5, b = 1.5, unit = "cm"),
          color = "#fafafa"
        ),
        plot.caption = ggtext::element_textbox_simple(
          size = 14, halign = 0.5,
          margin = ggplot2::margin(t = 0.5, b = 0.5, unit = "cm"),
          color = "#DDE3E6"
        ),
        plot.background = ggplot2::element_rect(fill = "#003352", color = "#003352"),
        plot.margin = ggplot2::margin(t = 0.5, b = 0.5, l = 1.5, r = 1.5, unit = "cm")
      )
  })
}

shiny::shinyApp(ui, server)
