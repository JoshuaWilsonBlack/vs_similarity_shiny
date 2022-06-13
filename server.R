# Define server logic required to draw a histogram
shinyServer(
  function(input, output, session) {
    
    high_ints <- reactive(
      if (input$interval_length == "60") {
        
        qb_interval_spaces %>%
          filter(
            Speaker == input$speaker_select, 
            interval_length == "60",
            height == "high"
          ) %>%
          pull(interval) %>%
          unique() # Unique is possibly not necessary.
        
      } else {
        
        qb_interval_spaces %>%
          filter(
            Speaker == input$speaker_select, 
            interval_length == "240",
            height == "high"
          ) %>%
          pull(interval) %>%
          unique()
        
      }
    )
    
    low_ints <- reactive(
      if (input$interval_length == "60") {
        
        qb_interval_spaces %>%
          filter(
            Speaker == input$speaker_select, 
            interval_length == "60",
            height == "low"
          ) %>%
          pull(interval) %>%
          unique()
        
      } else {
        
        qb_interval_spaces %>%
          filter(
            Speaker == input$speaker_select, 
            interval_length == "240",
            height == "low"
          ) %>%
          pull(interval) %>%
          unique()
        
      }
    )
    
    # loading relevant data given input choices.
    
    observe({
      #x <- input$interval_length
      
      # Can also set the label and select items
      updateSelectInput(
        session, 
        "speaker_select",
        choices = candidate_speakers %>% 
          filter(interval_length == input$interval_length) %>%
          pull(Speaker) %>%
          unique()
      )
      
    })
    
    observe({
      
      # Can also set the label and select items
      updateSelectInput(
        session, 
        "high_int",
        choices = high_ints()
      )
      
      # Can also set the label and select items
      updateSelectInput(
        session,
        "low_int",
        choices = low_ints()
      )
    })
    
    speaker_plot <- eventReactive(
      input$generate,
      {
        speaker_panel(
          speaker = input$speaker_select,
          high_int = input$high_int,
          low_int = input$low_int,
          int_length = input$interval_length,
          norm_method = input$norm_method,
          mean_scaled = input$mean_scaled
        )
      }
    )
    
    output$speaker_plot <- renderPlot({
      # speaker_panel(
      #   input$speaker_select,
      #   str_extract(input$high_int, "[0-9]{2,4}$"),
      #   str_extract(input$low_int, "[0-9]{2,4}$"),
      #   input$interval_length,
      #   input$norm_method,
      #   mean_scaled = TRUE
      # )
      # plot(1:as.numeric(input$interval_length))
      speaker_plot()
    })
    
  }
)