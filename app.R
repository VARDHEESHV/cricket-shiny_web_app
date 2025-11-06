# app.R — FINAL FIXED VERSION (100% Working)

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(shinylive)
library(plotly)
library(lubridate)
library(tidyr)

to_num <- function(x){
  x |> as.character() |> str_replace_all(",", "") |> str_replace_all("\\*", "") |> str_squish() |> na_if("") |> as.numeric()
}

pdf_df     <- read_csv("CLEAN_pdf_data.csv", show_col_types = FALSE)
players_df <- read_csv("CLEAN_player_stats.csv", show_col_types = FALSE)
leader_df  <- read_csv("CLEAN_leaderboard.csv", show_col_types = FALSE)
reddit_df  <- read_csv("CLEAN_reddit_comments.csv", show_col_types = FALSE)

# PDF Clean (Fixed - No duplicate name issue)
pdf_clean <- {
  if (!nrow(pdf_df)) tibble()
  else {
    # If a column named "text" already exists, use it.
    if ("text" %in% names(pdf_df)) {
      text_col <- pdf_df$text
    } else {
      # Otherwise use the first column as text.
      text_col <- pdf_df[[1]]
    }
    
    tibble(
      text = str_squish(as.character(text_col))
    ) |>
      filter(text != "", !is.na(text)) |>
      mutate(
        char_count = nchar(text),
        word_count = str_count(text, "\\S+")
      )
  }
}


leader_clean <- leader_df |>
  mutate(
    runs        = to_num(runs),
    fours       = to_num(fours),
    sixes       = to_num(sixes),
    matches     = to_num(matches),
    innings     = to_num(innings),
    highest     = to_num(str_replace_all(highest, "\\*", "")),
    average     = to_num(average),
    strike_rate = to_num(strike_rate),
    boundary_runs = 4 * coalesce(fours,0) + 6 * coalesce(sixes,0),
    boundary_share = if_else(!is.na(runs) & runs>0, boundary_runs/runs, NA_real_)
  ) |>
  arrange(rank)

players_clean <- players_df |>
  mutate(
    runs         = to_num(runs),
    matches      = to_num(matches),
    highest      = to_num(str_replace_all(highest, "\\*", "")),
    average      = to_num(average),
    strike_rate  = to_num(strike_rate),
    fours        = to_num(fours),
    sixes        = to_num(sixes),
    hundreds     = to_num(hundreds),
    fifties      = to_num(fifties),
    runs_per_match = if_else(matches > 0, runs/matches, NA_real_)
  )

reddit_clean <- reddit_df |>
  mutate(comment = str_squish(as.character(comment))) |>
  filter(!comment %in% c("", "[deleted]", "[removed]")) |>
  mutate(ID = row_number())

ui <- dashboardPage(
  dashboardHeader(title = "Cricket Insights Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName="home", icon=icon("home")),
      menuItem("PDF Insights", tabName="pdf", icon=icon("book")),
      menuItem("Player Stats", tabName="players", icon=icon("user")),
      menuItem("Top Scorers", tabName="top", icon=icon("trophy")),
      menuItem("Reddit (CRUD)", tabName="reddit", icon=icon("comments"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("home", h2("Dashboard Loaded Successfully ✅")),
      tabItem("pdf",
              textInput("pdf_search", "Search:", ""),
              DTOutput("pdf_table"),
              plotlyOutput("pdf_words_plot", height="400px")
      ),
      tabItem("players",
              selectInput("player_pick", "Player:", sort(unique(players_clean$player))),
              DTOutput("players_table"),
              plotlyOutput("players_runs_plot", height="400px")
      ),
      tabItem("top",
              sliderInput("topN","Top N:",min=5,max=30,value=15),
              plotlyOutput("top_bar",height="450px"),
              plotlyOutput("scatter_rs",height="450px")
      ),
      tabItem("reddit",
              DTOutput("reddit_table"),
              textInput("new_author","Author"),
              textAreaInput("new_comment","Comment"),
              actionButton("add_btn","Add"),
              numericInput("del_id","Delete ID:",value=1),
              actionButton("del_btn","Delete")
      )
    )
  )
)

server <- function(input, output, session){
  
  pdf_filtered <- reactive({
    if (input$pdf_search=="") pdf_clean
    else pdf_clean |> filter(str_detect(tolower(text), tolower(input$pdf_search)))
  })
  
  output$pdf_table <- renderDT(pdf_filtered())
  
  output$pdf_words_plot <- renderPlotly({
    words <- pdf_filtered()$text |> tolower() |> str_replace_all("[^a-z ]"," ") |> str_split(" ") |> unlist()
    words <- words[nchar(words)>3]
    df <- as_tibble(words) |> count(value, sort=TRUE) |> slice_head(n=30)
    ggplotly(ggplot(df,aes(reorder(value,n),n))+geom_col()+coord_flip())
  })
  
  players_filtered <- reactive(players_clean |> filter(player==input$player_pick))
  
  output$players_table <- renderDT(players_filtered())
  
  output$players_runs_plot <- renderPlotly({
    ggplotly(ggplot(players_filtered(),aes(as.numeric(season),runs))+geom_line()+geom_point())
  })
  
  output$top_bar <- renderPlotly({
    d <- leader_clean |> slice_head(n=input$topN)
    ggplotly(ggplot(d,aes(reorder(player,runs),runs))+geom_col()+coord_flip())
  })
  
  output$scatter_rs <- renderPlotly({
    ggplotly(ggplot(leader_clean,aes(strike_rate,runs,text=player))+geom_point())
  })
  
  rv <- reactiveValues(reddit=reddit_clean)
  
  output$reddit_table <- renderDT(rv$reddit)
  
  observeEvent(input$add_btn,{
    rv$reddit <- bind_rows(rv$reddit,tibble(ID=max(rv$reddit$ID)+1,author=input$new_author,comment=input$new_comment))
    write_csv(rv$reddit,"CLEAN_reddit_comments.csv")
  })
  
  observeEvent(input$del_btn,{
    rv$reddit <- rv$reddit |> filter(ID!=input$del_id)
    write_csv(rv$reddit,"CLEAN_reddit_comments.csv")
  })
}

shinyApp(ui, server)
