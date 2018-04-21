library(shiny)
library(shinycssloaders)

suppressMessages(library(DT))
suppressMessages(library(emo))
suppressMessages(library(lubridate))
suppressMessages(library(magrittr))
suppressMessages(library(qdapRegex))
suppressMessages(library(tidytext))
suppressMessages(library(tidyverse))
suppressMessages(library(tools))
suppressMessages(library(wordcloud))
suppressMessages(library(visNetwork))


#----------------------#
#          UI          #
#----------------------#

Sys.setenv(TZ = "Europe/Berlin")

ui <- navbarPage("Whatsapp Analyser",

  tabPanel("Import",
  
  sidebarLayout(
    
    sidebarPanel(
        tags$a(href='http://blog.holnburger.com', tags$img(src='Logo.png',width='90%')),
        
        tags$hr(),
      
      # Input: Select a file ----
      fileInput("file1", "WhatsApp-Export",
                multiple = TRUE,
                accept = c(".csv", 
                           ".txt",
                           ".tsv",
                           ".zip")),
     
      helpText("Zum Export von WhatsApp-Chatverläufen die Gruppe oder das Profil in WhatsApp anklicken und den Chat ohne Medien exportieren. 
               Es werden ZIP und TXT Dateien akzeptiert. Die Chat-Verläufe werden nicht abgespeichert."),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Smartphone Betriebssystem ----
      radioButtons("os", "Smartphone OS",
                   choices = c(iOS = "iOS",
                               Android = "Android"),
                   selected = "iOS"),
      
      # Input: Sprache ----
      radioButtons("language", "Sprache",
                   choices = c("deutsch" = "deutsch"),
                   selected = "deutsch"),
      helpText("Derzeit wird nur die deutsche Sprache unterstützt."),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Chat-Auszug",
                   choices = c("chronologisch" = "head",
                               "zufällig" = "rnd"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
        
      # Output: Überschrift ----
      conditionalPanel(
          condition = "output.contents", 
          h3(textOutput("headline_table"))),
      
      # Output: Tabelle mit Loading Screen ----
      tableOutput("contents") %>% withSpinner(color="#009bc2", type = 6),
      
      # Output: Wenn Tabelle geladen, Downloadbutton und Zusammenfassung ----
      conditionalPanel(
          condition = "output.contents", 
          downloadButton("downloadData", "Download .csv"),
          h3(textOutput("headline_note")),
          uiOutput("note"),
          h3(textOutput("filter_note")),
          textOutput("filter_note_add"),
          uiOutput("choose_user"),
          uiOutput("filterDate")),
      Sys.sleep(1),
      conditionalPanel(
          condition = "output.contents",
          plotOutput("overview_plot") %>% withSpinner(color="#009bc2", type = 6))
      
   
    )
    
  )
),

tabPanel("Auswertung",
  sidebarLayout(
    sidebarPanel(h2("Allgemeine Statistiken"),
        helpText("Beiträge pro Wochentag, Uhrzeit, Nutzerverhalten etc.")
        ),
    mainPanel(
             # Output: Plots Overall ----
             h3(textOutput("headline_plot_overall")),
             plotOutput("plot_per_weekday") %>% withSpinner(color="#009bc2", type = 6),
             plotOutput("plot_per_hour"),
             # Output: Plots Indivdiual ----
             h3(textOutput("headline_plot_individual")),
             plotOutput("plot_messages"),
             plotOutput("plot_emojis"),
             plotOutput("plot_emojis_per_message"),
             plotOutput("plot_message_length")
             )
    ),
  
tabPanel("User Activity",
  sidebarLayout(
      sidebarPanel(h2("Nutzeraktivität"),
                   helpText("Aktivität der Nutzer nach Uhrzeit"),
                   sliderInput("heigth_user_activity",
                               "Höhe der Grafik in Pixel:",
                               min = 100, max = 1000, value = 400)
      ),
      mainPanel(
          plotOutput("user_activity_distinct") %>% withSpinner(color="#009bc2", type = 6)
          
      )
  )         
),
tabPanel("Wordcloud",
  sidebarLayout(
    sidebarPanel(h2("Wordcloud"),
                 helpText("Die am häufigsten genutzten Wörter im gesamten Chat. 
                          Auswertung ohne Stoppwörter (\"der, die, das, und, ich, etc.\")"),
                 sliderInput("freq",
                      "Minimale Häufigkeit:",
                      min = 1,  max = 50, value = 15),
                 sliderInput("max",
                      "Maximale Anzahl an Wörtern:",
                      min = 1,  max = 300,  value = 100)
          ),
      mainPanel(
          plotOutput("wordcloud") %>% withSpinner(color="#009bc2", type = 6)
          )
    )
  ),
tabPanel("Typische Wörter",
  sidebarLayout(
    sidebarPanel(h2("Typische Wörter"),
                 helpText("Wörter, welche hauptsächlich von einer Person (im Vergleich zum Rest der Gruppe) genutzt wurden."),
        sliderInput("top",
                    "Anzahl Top Wörter:", ticks = FALSE,
                    min = 2, max = 5, value = 3),
        sliderInput("height_distinct_word",
                    "Höhe der Grafik in Pixel:",
                    min = 100, max = 1000, value = 400)),
    mainPanel(
        #uiOutput("mod_words_distinct")
        plotOutput("words_distinct") %>% withSpinner(color="#009bc2", type = 6)
        )
    )
  )
),
tabPanel("Netzwerk",
         sidebarLayout(
             sidebarPanel(h2("Netzwerk"),
             helpText("Netzwerkdarstellung der Mentions in den WhatsApp-Nachrichten. Die Darstellung ist logischerweise nur in Gruppen sinnvoll.")),
             mainPanel(
                 visNetworkOutput("network") %>% withSpinner(color="#009bc2", type = 6)
             )
         )
)
)


#----------------------#
#        Server        #
#----------------------#

server <- function(input, output) {

  # Patterns zur Strukturerkennung ----
  whatsapp_date <- "(\\d{0,2}\\.\\d{2}\\.\\d{2}?)"
  whatsapp_time <- "([01]?[0-9]|2[0-3]):([0-5][0-9]):?([0-5][0-9])"
  whatsapp_username <- "([a-zA-Z0-9]{3,16}){1}"
  whatsapp_notice <- ".*?(hinzugefügt$|geändert$|entfernt$|verlassen$|erstellt$|Ende-zu-Ende-Verschlüsselung geschützt\\.$)"
  whatsapp_files <- ".*?(<[^>]*angehängt>|<[^>]*weggelassen>|vcf$)"
  date_format <- "%Y-%m-%d %H:%M:%S"
  
  whatsapp_chat <- reactive({
     
      # Start Progress Bar ----
      withProgress(message = 'Daten auswerten', value = 0, {
      
          # Android vs. iOS Formatierung ----
          incProgress(0.15, message = "File einlesen", detail = paste0(15, "%"))
          if(input$os == "iOS") {
              whatsapp_datetime <- "(\\[\\d{0,2}\\.\\d{2}\\.\\d{2}?\\, ([01]?[0-9]|2[0-3]):([0-5][0-9]):?([0-5][0-9])?\\])"}
          else {
              whatsapp_datetime <- "(\\d{0,2}\\.\\d{2}\\.\\d{2}? um ([01]?[0-9]|2[0-3]):([0-5][0-9]):?([0-5][0-9])? - )"
              whatsapp_date <- "(\\d{0,2}\\.\\d{2}\\.\\d{2}?)" 
              whatsapp_time <- "([01]?[0-9]|2[0-3]):([0-5][0-9])"
              date_format <- "%Y-%m-%d %H:%M" }

          # File einlesen ----
          req(input$file1)
          
          if(file_ext(input$file1$datapath) == 'zip'){
              raw <- unlist(read_file(input$file1$datapath))}
          else{
              raw <- read_file(input$file1$datapath)}

          clean <- str_replace_all(raw, "(\\n)", " ")
          clean <- str_replace_all(clean, whatsapp_datetime, "\n\\1")
      
          # Datei umwandeln ----
          whatsapp_chat <- read_lines(clean)
          whatsapp_chat <- data.frame(whatsapp_chat) %>%
                  slice(3:n())

          # Daten strukturieren ----
          incProgress(0.15, message = "Daten strukturieren", detail = paste0(30, "%"))
          whatsapp_chat %<>%
              rename(raw = whatsapp_chat) %>%
              mutate(date = str_extract(raw, whatsapp_date)) %>%
              mutate(date = as.Date(date, format = "%d.%m.%y")) %>%
              mutate(time = str_extract(raw, whatsapp_time)) %>%
              mutate(datetime = paste0(date, " ", time)) %>%
              mutate(user = str_extract(raw, whatsapp_username)) %>%
              mutate(text = str_replace(raw, whatsapp_datetime, "")) %>%
              mutate(datetime = as.POSIXct(datetime, format = date_format)) %>%
              na.omit()
      
          # Systemnachrichten löschen ----
          whatsapp_chat %<>%
              mutate(text = str_replace(text, "^[^:]*:\\s*", "")) %>%
              # deletes everything before the first colon
              mutate(file = str_detect(text, whatsapp_files)) %>%
              mutate(text = str_replace(text, whatsapp_files, "")) %>%
              filter(!str_detect(text, whatsapp_notice))
      
          # URLs und Emojis extrahieren ----
          whatsapp_chat %<>%
              mutate(url = rm_url(text, extract=TRUE)) %>%
              #mutate(emoji = emo::ji_extract_all(text)) %>%
              mutate(emoji_count = emo::ji_count(text))

          # Überflüssigen Text löschen ----
          incProgress(0.15, message = "Daten bereinigen und Plots erstellen", detail = paste0(45, "%"))  
          whatsapp_chat %<>%
              mutate(text = str_replace_all(text, "(@[0-9]{10,17})", "")) %>%
              # deletes mentions of cell-phone numbers (WhatsApp mentions)
              mutate(text = rm_url(text)) %>%
              mutate(text = rm_emoticon(text)) %>%
              select(-raw, -date, -time)
          
      })
  })

  
  #----------------------#
  #        Outputs       #
  #----------------------#
  
# Darstellung der Table ---- 
  output$headline_table <- renderText({ paste("Auszug der WhatsApp-Nachrichten") })
  
  output$contents <- renderTable({
    if(input$disp == "head") {
      return(head(whatsapp_chat() %>%
                    select(datetime, user, text) %>%
                    mutate(datetime = as.character(datetime)), 5))
    }
    else {
      return(sample_n(whatsapp_chat() %>%
                          select(datetime, user, text) %>%
                          mutate(datetime = as.character(datetime)), 5))
    }
  })
  
  output$downloadData <- downloadHandler(
      filename = function() {
          paste("whatsapp_chat", ".csv", sep = "")
      },
      content = function(file) {
          write.csv(whatsapp_chat(), file, row.names = FALSE)
  }) 

  output$headline_note <- renderText({ "Zusammenfassung" })
  
  output$note <- renderUI({ HTML(paste("Seit ", strftime(whatsapp_chat()[[1, "datetime"]], "%d.%m.%Y"),
                                    "wurden insgesamt", nrow(whatsapp_chat()),
                                    "Nachrichten von", length(unique(whatsapp_chat()$user)),
                                    "Usern geschrieben. <br/>",
                                    "Es wurden", sum(whatsapp_chat()$file, na.rm=TRUE), "Dateien (GIFs, Bilder und PDFs) verschickt und",
                                    sum(whatsapp_chat()$emoji_count), "Emojis ausgetauscht.")) })
 
  output$filter_note <- renderText({ "Filtern" }) 
  
  output$filter_note_add <- renderText({ "Es ist möglich, Nutzer aus der Erhebung auszuschließen und den Erhebungszeitraum einzugrenzen." }) 
  
  output$choose_user <- renderUI({
      user.names <- as.vector( unique(whatsapp_chat()$user) )
      selectInput("users","Nutzer aus der Auswertung ausschließen", choices = user.names, multiple=TRUE)    
  })
  
  output$filterDate <- renderUI({
      
      dateRangeInput("filterDate", 
                     label = paste("Auswertungszeitraum eingrenzen"),
                     start = whatsapp_chat()[[1, "datetime"]],
                     end = Sys.time(), 
                     separator = " - ", 
                     weekstart = 1
                     
      )
  })
  
  whatsapp_chat_filter <- reactive({ whatsapp_chat() %>%
      filter(!user %in% input$users) %>%
      filter(datetime > input$filterDate[1] & datetime < input$filterDate[2])
  })
 
  output$overview_plot <- renderPlot({
      whatsapp_chat_filter() %>%
          mutate(month = format(floor_date(datetime, unit = "month")), "%m-%Y") %>%
          select(user, text, month) %>%
          group_by(user, month) %>%
          summarise(messages = n()) %>%
          ggplot(aes(x = month, y = messages, fill = user)) + 
          geom_col()
  })
  
# Darstellung der Nutzerabhängigen Plots ----
  
  output$Summary <- renderText({ paste("Auswertung aller Nachrichten") })
  
  output$plot_per_weekday <- renderPlot({
      whatsapp_chat_filter() %>%
          mutate(wday = wday(datetime, label = TRUE, week_start = getOption("lubridate.week.start", 1))) %>%
          ggplot(aes(x = wday)) +
          geom_bar() +
          labs(title = "Anzahl der Nachrichten pro Wochentag", 
               y = "Nachrichten", x = "")
  })
  
  output$plot_per_hour <- renderPlot({
      whatsapp_chat_filter() %>%
          group_by(hour = hour(datetime), user) %>%
          summarise(messages = n()) %>%
          ggplot(aes(x = hour, y = messages)) +
          geom_col() +
          labs(title = "Anzahl der Nachrichten pro Uhrzeit",
               y = "Nachrichten", x = "")
  })
  

# Darstellung der Nutzerabhängigen Plots ----
  
  output$plot_messages <- renderPlot({
      whatsapp_chat_filter() %>%
          group_by(user) %>%
          summarise(messages = n()) %>%
          arrange(desc(messages)) %>%
          ggplot(aes(x = reorder(user, -messages), y = messages, fill = user)) +
          geom_col(show.legend = FALSE) +
          labs(title = "Anzahl der Nachrichten",
               y = "Nachrichten", x = "")
      })

  output$plot_emojis <- renderPlot({
      whatsapp_chat_filter() %>%
          group_by(user) %>%
          summarise(emojis = sum(emoji_count)) %>%
          arrange(desc(emojis)) %>%
          ggplot(aes(x = reorder(user, -emojis), y = emojis, fill = user)) +
          geom_col(show.legend = FALSE) +
          labs(title = "Anzahl der Emojis",
               y = "Nachrichten", x = "")
  })
  
  output$plot_emojis_per_message <- renderPlot({
      whatsapp_chat_filter() %>%
          group_by(user) %>%
          summarise(messages = n(), emojis = sum(emoji_count), avg_emojis = emojis / messages) %>%
          ggplot(aes(x = reorder(user, -avg_emojis), y = avg_emojis, fill = user)) +
          geom_col(show.legend = FALSE) +
          labs(title = "Emojis pro Nachricht",
               y = "Emojis pro Nachricht", x = "")
  })
  
  output$plot_message_length <- renderPlot({
      whatsapp_chat_filter() %>%
          select(user, text) %>%
          mutate(length = nchar(text)) %>%
          filter(length > 1) %>%
          ggplot(aes(x = reorder(user, -length, median), y = length, fill = user)) +
          geom_boxplot(show.legend = FALSE) + 
          labs(title = "Länge der Nachrichten",
               y = "Zeichen pro Nachricht", x = "")
  })
  
  output$wordcloud <- renderPlot({
      whatsapp_chat_filter() %>%
          unnest_tokens(word, text) %>%
          anti_join(get_stopwords("de")) %>%
          count(word, sort = TRUE) %>%
          with(wordcloud(word, n, 
                         min.freq = input$freq,
                         max.words = input$max,
                         random.order = FALSE,
                         colors=brewer.pal(8, "Dark2")))
  })
  
  top_distinct_words <- reactive({
  whatsapp_chat_filter() %>%
      select(user, text) %>%
      unnest_tokens(word, text) %>%
      count(user, word, sort = TRUE) %>%
      bind_tf_idf(word, user, n) %>%
      arrange(desc(tf_idf)) %>%
      mutate(word = factor(word, levels = rev(unique(word)))) %>% 
      group_by(user) %>% 
      top_n(input$top) %>% 
      ungroup })
  
  observe({
  output$words_distinct <- renderPlot({
      whatsapp_chat_filter() %>%
          select(user, text) %>%
          unnest_tokens(word, text) %>%
          anti_join(get_stopwords("de")) %>%
          count(user, word, sort = TRUE) %>%
          bind_tf_idf(word, user, n) %>%
          arrange(desc(tf_idf)) %>%
          mutate(word = factor(word, levels = rev(unique(word)))) %>% 
          group_by(user) %>% 
          top_n(input$top) %>% 
          ungroup %>%
          ggplot(aes(word, tf_idf, fill = user)) +
          geom_col(show.legend = FALSE) +
          labs(x = NULL, y = "tf-idf") +
          facet_wrap(~user, ncol = 2, scales = "free") +
          coord_flip() +
          labs(title = "\"Typische\" Wörter")  
     }, height = input$height_distinct_word) })

  
  observe({
  output$user_activity_distinct <- renderPlot({
      whatsapp_chat_filter() %>%
      count(hour = hour(datetime), user) %>%
      group_by(user) %>%
      mutate(freq = n / sum(n)) %>%
      ggplot(aes(x = hour, y = freq, fill = user)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~user, ncol = 1, strip.position = "left") +
      labs(title = "Nutzeraktivität pro Uhrzeit",
           x = "Uhrzeit", y = "") +
      theme(panel.grid=element_blank()) + 
      facet_wrap(~user, ncol = 2) +
      scale_y_continuous(labels = scales::percent)
  }, height = input$heigth_user_activity) })
  

  output$network <- renderVisNetwork({
      
      nodes <- whatsapp_chat_filter() %>%
          group_by(id = user) %>%
          summarise() %>%
          mutate(label = id)
      edges <- whatsapp_chat_filter() %>%
          select(user, text) %>%
          rename(from = user) %>%
          mutate(to = str_extract(text, paste(unique(whatsapp_chat_filter()$user), collapse = "|"))) %>%
          select(from, to) %>%
          na.omit() %>%
          group_by(from,to) %>%
          summarise(value = n())
      visNetwork(nodes, edges)})
  
  
  outputOptions(output, "contents", suspendWhenHidden = FALSE)
}

# Create Shiny app ----
shinyApp(ui, server)