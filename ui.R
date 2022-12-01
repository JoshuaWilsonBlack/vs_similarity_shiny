shinyUI(
  fluidPage(
    markdown(intro_text),
    sidebarLayout(
      sidebarPanel( 
        selectInput(
          "interval_length", 
          label = "Interval Length", 
          choices = c("240", "60"),
          selected = initial_interval_length
        ),
        
        selectInput(
          "speaker_select", 
          label = "Speaker", 
          choices = initial_speaker_choices,
          selected = initial_speaker
        ),
        
        selectInput(
          "high_int", 
          label = "High Amplitude Interval", 
          choices = initial_high_ints,
          selected = initial_high
        ),
        
        selectInput(
          "low_int", 
          label = "Low Amplitude Interval", 
          choices = initial_low_ints,
          selected = initial_low
        ),
        
        selectInput(
          "norm_method", 
          label = "Normalisation Method", 
          choices = list(
            "Lobanov 2.0, within interval" = "lob2_int", 
            "Lobanov 2.0, whole recording" = "lob2",
            "Raw Hz, whole recording" = "raw"
          ),
          selected = "lob2"
        ),
        
        checkboxInput(
          "mean_scaled",
          label = "Scale Distances",
          value = TRUE
        ),
        
        actionButton("generate", "Generate Plot"),
        img(src="NZILBB2.png", style = "max-width: 100%; max-height: 75px; padding: 10px;"),
        width = 3
      ), 
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("speaker_plot", height="800px", width="1000px"),
        width = 9
      )
    )
  )
)