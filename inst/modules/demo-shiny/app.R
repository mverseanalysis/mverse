library(shiny)
library(shinyBS)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(mverse)
# accent colour
accent <- "#fc8d62"
# contents
maintitle <- "Hollywood's Exclusion of Women: Is it financially motivated?"
subtitle <- "mverse Module: Bechdel Test"
information <- div(
  h3("Background"),
  p(
    "
    In this module, we investigate whether a bigger presence of female characters
    in a movie is associate with its financial performance.
    The data set comes from
    ",
    a(
      "an article from FiveThirtyEight",
      href = "https://fivethirtyeight.com/features/the-dollar-and-cents-case-against-hollywoods-exclusion-of-women/"
      ),
    "by Walt Hickey. The data set includes Bechdel Test results for ",
    format(nrow(movies), big.mark = ","),
    " movies as well as their production budgets and gross earnings adjusted to 2013 USD."
    ),
  plotOutput("budget", height = "300px"),
  p(
    "
    We observe that Hollywood tends to allocate less budget to movies that pass
    Bechdel Test compared to those in whichfewer than two named female
    characters appear or named female characters do not talk to each other.
    We are interested in whether such budget decisions are financially motivated.
    Specifically, you are asked to make
    ", em("a statistical inference")," to compare the financial performances
    between movies that have bigger presence of women vs. those that don't.
    "
    ),
  h3("Exercise"),
  p(
    tags$ol(
      tags$li(
        "
        In ", tags$b("Define Multiverse"), " tab, identify at least 3 reasonable ways to define",
        tags$ol(
          tags$li(
            em("the comparison groups"),
            " based on the Bechdel Test results; and"
            ),
          tags$li(
            em("the metrics to compare"),
            " based on budgets and gross earnings adjusted to 2013 USD."
            )
          )
        ),
      tags$li(
        "
        A multiverse of simulation-based hypothesis tests
        will be conducted based on the definitions provided. Examine the results
        displayed in ", tags$b("Display Multiverse"), " tab.
        Conclude with a final inference and comment on the robustness of your conclusion.
        "
        )
      )
    )
  )
multiversePanel <- div("define multiverse")
resultPanel <- div("display multiverse")
# UI template with menu
ui <- mverse:::module_ui(information, multiversePanel, resultPanel, maintitle, subtitle)
server <- function(input, output, session) {
  observeEvent(input$infobtn, { shinyjs::show(id = "infobox")})
  observeEvent(input$infobtnclose, { shinyjs::hide(id = "infobox")})
  observeEvent(input$startbtn, { shinyjs::hide(id = "infobox")})
  output$budget <- renderPlot({
    movies %>%
      group_by(clean_test) %>%
      summarise(med_budget = median(budget_2013)) %>%
      ggplot(aes(x = clean_test, y = med_budget)) +
      theme_minimal() +
      geom_bar(stat = "identity", fill = accent, width = 0.5) +
      geom_text(aes(label = paste0("$", round(med_budget/10^4)/100, "M")),
                hjust = 0, nudge_y = 700000, colour = "black", size = 4) +
      geom_text(aes(y = 0, label = c(
        "Fewer than two women",
        "Women don't talk to each other",
        "Women only talk about men",
        "Passes Bechdel Test with dispute*",
        "Passes Bechdel Test"
      )), colour = "black", size = 5, hjust = 1, nudge_y = -500000) +
      coord_flip() +
      scale_y_continuous(limits = c(-1.8*10^7, 5.5*10^7)) +
      labs(
        title = "Hollywood tends to bet more money on movies with less presence of female characters.",
        subtitle = "Median budget invested in 2013 USD.",
        caption = "*Some contributors to BechdelTest.com did not agree the filims in question passed the test."
      ) +
      theme(
        plot.title = element_text(colour = "black", size = 18),
        plot.subtitle = element_text(colour = "black", size = 16),
        plot.background = element_rect(fill = "#f6f6f6", linetype = 0),
        plot.title.position = "plot",
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.spacing.x = unit(rep(0, 4), "points"),
        plot.margin = unit(rep(12, 4), "points")
      )
  })
}

if (interactive()) { shinyApp(ui, server) }



