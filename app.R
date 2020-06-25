#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

planet_data <- readRDS("data/planet_data.RDS")


calc_my_planet_age <- function(birthday, planet_data) {

  lifetime <- birthday %>%
    lubridate::interval(Sys.Date()) %>% 
    lubridate::int_length()
  
  lifetime_hours <- lifetime / 3600
  
  my_planet_age <- planet_data %>%
    mutate(
      planetary_age = round(lifetime_hours / orbit_in_hours, 1),
      next_birthday_in = round((((trunc(planetary_age) + 1) * orbit_in_hours) - 
        lifetime_hours)/24),
      label_position = case_when(
        planet == "Jupiter" ~ 1.25,
        planet == "Saturn" ~ 1.25,
        planet == "Uranus" ~ 1.15,
        planet == "Neptune" ~ 1.15,
        TRUE ~ 1.1
      )) %>%
    select(planet, au, radius, orbit_in_hours, planetary_age, next_birthday_in, label_position) %>%
    mutate(label = paste(planetary_age, planet, "years"))
  
  
  return(my_planet_age)
    
}

my_planet_age_plot <- function(birthday, planet_data) {
  
  my_planet_age <- calc_my_planet_age(birthday, planet_data)
  
  planet_colours <- c(
    "Mercury" = "seashell4",
    "Venus" = "orange1",
    "Earth" = "dodgerblue1",
    "Mars" = "firebrick2",
    "Jupiter" = "sienna1",
    "Saturn" = "khaki",
    "Uranus" = "cadetblue1",
    "Neptune" = "blue3",
    "Pluto" = "burlywood3"
    
  )
  
  p <- ggplot(my_planet_age, aes(x = sqrt(au), y = 1, size = radius, fill = planet, colour = planet)) + 
    geom_point(alpha = 0.5, shape = 21, stroke = 1) + 
    geom_text(aes(label = label, y = label_position), size = 5, hjust = 0, colour = "white") +
    geom_curve(aes(x = 0, y = 0, xend = 0, yend = 2), size = 0.3, curvature = 0.1, 
               colour = "darkgoldenrod2") +
    scale_radius(range = c(0.5, 25)) +
    scale_x_reverse() +
    scale_colour_manual(values = planet_colours) +
    scale_fill_manual(values = planet_colours) +
    coord_flip() + 
    theme_void() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "black"))
  
  return(p)
  
}

next_planet_birthday <- function(birthday, planet_data, what) {
  
  my_planet_age <- calc_my_planet_age(birthday, planet_data)
  
  next_birthday <- my_planet_age %>%
    filter(next_birthday_in == min(next_birthday_in))
  
  if (what == "where") {
    next_birthday_out <- next_birthday$planet
  } else if (what == "when") {
    next_birthday_out <- format(Sys.Date() + lubridate::days(next_birthday$next_birthday_in),
                                format = "%a %d %b %Y")
  } else if (what == "age") {
    next_birthday_out <- trunc(next_birthday$planetary_age) + 1
  }
  
  return(next_birthday_out)
  
}

# Define UI for application that draws a histogram
ui <- navbarPage("Interplanetary Birthday Express", tabPanel(
    emo::ji("alien"),
    fixedRow(column(
      12,
      fixedRow(column(
        4,
        fixedRow(column(
          12,
          HTML("Hello Human! I cake in peace!<br /><br />",
               "The planets in your solar system orbit the sun at different ",
               "rates, use this tool to find out how old you are on different ",
               "planets in your solar system!<br /><br />"),
          dateInput("birthday", "When is your birthday?", "2000-01-01"),
          HTML("Your next interplanetary birthday will be:<h2>"),
          textOutput("next_birthday_when", inline = TRUE),
          HTML("</h2> When you will be <strong>"),
          textOutput("next_birthday_age", inline = TRUE),
          HTML("</strong> years old on <strong>"),
          textOutput("next_birthday_where", inline = TRUE),
          HTML("</strong>")))),
        column(
          8,plotOutput("planet_plot", height = "600px"))))),
    fixedRow(column(
      12,
      HTML("<br />This tool represents the time since you were born ",
           "as years on each planet of the solar system, plus Pluto ",
           "(which isn't a planet but some of you like to think it is). ",
           "It does this by first converting the time since your birthday ",
           "into hours, this figure is then divided by the orbital period ",
           "of each planet in hours and rounded to one decimal place<br />",
           "<br />The sizes of the planets and their relative distance ",
           "from the sun are based on ",
           "<a href=\"https://nssdc.gsfc.nasa.gov/planetary/factsheet/index.html\">",
           "data from NASA</a>, but have been scaled to render in the plot.")))),
    fluid = FALSE)


server <- function(input, output) {

  output$planet_plot <- renderPlot(
        my_planet_age_plot(input$birthday, planet_data)
    )
  
  output$next_birthday_where <- renderText(
    next_planet_birthday(input$birthday, planet_data, "where")
  )
  
  output$next_birthday_when <- renderText(
    next_planet_birthday(input$birthday, planet_data, "when")
  )
  
  output$next_birthday_age <- renderText(
    next_planet_birthday(input$birthday, planet_data, "age")
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
