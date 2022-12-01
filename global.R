#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

### Required libraries

library(shiny) # For... Shiny.
library(tidyverse) # For data manipulation
library(glue) # To combine variables within strings.
library(here) # For file management
library(patchwork) # To combine plots together.
library(scales) # For distance function.

### Required data (output from corpus_pca.Rmd).

# Load
qb_vowel_spaces <- read_rds(
    here('data', 'qb_vowel_spaces.rds')
)

qb_interval_spaces <- read_rds(
    here('data', 'qb_interval_spaces.rds')
)

## Temp test. Remove incomplete intervals.
qb_interval_spaces <- qb_interval_spaces %>%
    group_by(interval_length, Speaker, interval) %>%
    mutate(
        n = n_distinct(Vowel)
    ) %>%
    filter(
        n == 10
    )

# Add height data to qb_interval_spaces 
qb_interval_spaces <- qb_interval_spaces %>%
    mutate(
        height = if_else(scaled_amp > 0.3, "high", NULL),
        height = if_else(scaled_amp < -0.3, "low", height)
    )

vowel_colours = c(
    DRESS = "#9590FF",
    FLEECE = "#D89000",
    GOOSE = "#A3A500",
    KIT = "#39B600",
    LOT = "#00BF7D",
    NURSE = "#00BFC4",
    START = "#00B0F6",
    STRUT = "#F8766D",
    THOUGHT = "#E76BF3",
    TRAP = "#FF62BC"
)

# Generate candidate speakers (those with intervals both high and low.)

candidate_speakers <- qb_interval_spaces %>%
    filter(
        height %in% c("high", "low")
    ) %>%
    group_by(interval_length, Speaker) %>%
    summarise(
        heights = n_distinct(height)
    ) %>%
    filter(
        heights == 2 # Speaker has both a high and low
    ) 
# %>%
#     ungroup() %>%
#     group_by(Speaker) %>%
#     mutate(
#         interval_lengths = n_distinct(interval_length)
#     ) %>%
#     filter(
#         interval_lengths == 2 # Speaker has high and low in both 60 and 240s intervals
#     )

# Define variables for initial options in app.

initial_interval_length <- "240"

initial_speaker_choices <-  candidate_speakers %>% 
    filter(interval_length == initial_interval_length) %>%
    pull(Speaker) %>%
    unique()

initial_speaker <- initial_speaker_choices[[1]]

initial_high_ints <- qb_interval_spaces %>%
    filter(
        Speaker == initial_speaker, 
        interval_length == initial_interval_length,
        height == "high"
    ) %>%
    pull(interval) %>%
    unique()

initial_high <- initial_high_ints[[1]]

initial_low_ints <- qb_interval_spaces %>%
    filter(
        Speaker == initial_speaker, 
        interval_length == initial_interval_length,
        height == "low"
    ) %>%
    pull(interval) %>%
    unique()

initial_low <- initial_low_ints[[1]]

## Helper functions

# Distance functions
speaker_distances <- function(
    space, 
    all_speaker_data, 
    norm_method, 
    int_length,
    mean_scaled = FALSE
) {
    
    # Pick correct interval lob2 version for interval length.
    if (norm_method == "lob2_int") {
        norm_method <- glue("lob2_int_{int_length}")
    }
    
    raw_distances <- all_speaker_data %>%
        filter(
            norm == norm_method,
        ) %>%
        left_join(
            space %>% 
                ungroup() %>% 
                select(Vowel, F1_50, F2_50), 
            by="Vowel"
        ) %>%
        # Euclidean distance.
        mutate(
            distance = sqrt((F1_50.x - F1_50.y)^2 + (F2_50.x - F2_50.y)^2)  
        ) 
    
    if (mean_scaled == TRUE) {
        out <- raw_distances %>%
            group_by(Vowel) %>%
            mutate(
                distance = scale(distance) 
            ) %>%
            ungroup() %>%
            group_by(Speaker) %>%
            summarise(
                distance = mean(distance)
            ) %>%
            arrange(distance)
    } else {
        out <- raw_distances %>%
            ungroup() %>%
            group_by(Speaker) %>%
            summarise(
                distance = mean(distance)
            ) %>%
            arrange(distance)
    }
}


# Plotting functions.

plot_space <- function(space, titles = TRUE) {
    # Plot vowel space assuming a row for each vowel type (in column 'Vowel') and
    # columns for F1 and F2 (named 'F1_50' and 'F2_50'). Assumes 'norm' column 
    # which tracks what kind of normalisation has been applied. Assumes 
    # "Speaker" or "speaker_interval" column to generate plot title. If titles
    # argument set to FALSE, then no titles will be printed.
    
    # Check normalisation column is consistent.
    if (n_distinct(space$norm) == 1) {
        norm_method = space$norm[[1]]
    } else {
        warning("Multiple normalisation methods detected.")
    }
    
    if (titles == TRUE) {
        # Detects whether vowel space is for interval or for entire monologue.
        if ("speaker_interval" %in% names(space)) {
            plot_title = glue("{space$speaker_interval[[1]]} vowel space")
        } else {
            plot_title = glue("{space$Speaker[[1]]} vowel space")
        }
    }
    
  if (norm_method %in% c("lob2", "lob2_int", "lob2_int_240", "lob2_int_60")) {
    
    # Set appropriate plot limits and labels
    f2_limits <- c(2, -2.5)
    f1_limits <- c(2.5, -1.5)
    f2_label <- "F2 (normalised)"
    f1_label <- "F1 (normalised)"
    
    # Set plot subtitle
    if (norm_method == "lob2") {
      plot_subtitle = "Lobanov 2.0 normalization"
    } else { # Only alternative is "lob2_int"
      plot_subtitle = "Lobanov 2.0 normalization (interval level)"
    }
    
  } else {
    
    f2_label <- "F2 (Hz)"
    f1_label <- "F1 (Hz)"
    plot_subtitle = "Non-normalized data"
    
  }
    
    out_plot <- space %>%
        ggplot(
            aes(
                x = F2_50, 
                y = F1_50, 
                label = Vowel, 
                colour = Vowel
            )
        ) +
        scale_color_manual(
            values = vowel_colours
        ) +
        geom_text(show.legend = FALSE) +
        scale_x_reverse(
            position = "top", 
            name = f2_label, 
            limits = f2_limits 
        ) +
        scale_y_reverse(
            position = "right", 
            name = f1_label, 
            limits = f1_limits
        )
    
    if (titles == TRUE) {
        out_plot <- out_plot +
            labs(
                title = plot_title,
                subtitle = plot_subtitle
            ) 
    }
    
    out_plot
}

plot_panel <- function(
    speaker, # Speaker whose interval is to be plotted
    int, # Interval name
    int_length, # Length of interval
    interval_df, # Dataframe containing original interval
    vowel_spaces_df, # Dataframe containing all vowel spaces
    norm_method,
    mean_scaled = TRUE
) {
    # Given an interval, this function returns a plot of the interval and the
    # three most similar intervals.
    interval_data <- interval_df %>%
        filter(
            Speaker == speaker,
            interval == int,
            norm == norm_method,
            interval_length == int_length
        )
    
    # Add amp score to interval plot
    interval_plot <- plot_space(interval_data, titles = FALSE) +
        geom_label(
            aes(
                label = glue(
                  "Amp: {signif(scaled_amp, 2)}, PC1: {signif(PC1_score, 2)}"
                )
            ),
            x = -Inf,
            y = -Inf,
            vjust = 0,
            hjust = 0,
            alpha = 0.5,
            na.rm = TRUE,
            inherit.aes = FALSE,
            data = interval_data %>% 
                filter(Vowel == "DRESS")
        ) +
        labs(
            title = glue("{speaker}_{int}"),
            subtitle = glue("Interval length: {int_length}")
        )
    
    closest_3 <- speaker_distances(
        interval_data,
        vowel_spaces_df,
        norm_method = norm_method,
        int_length = int_length,
        mean_scaled = mean_scaled
    ) %>%
        filter(
            Speaker != speaker # Excludes speaker from most similar speakers
        ) %>%
        slice_head(n=3) %>%
        pull(Speaker)
    
    # Change interval lob2 name for vowel space data frame.
    if (norm_method == "lob2_int") {
        vs_norm_method <- glue("lob2_int_{int_length}")
    } else {
        vs_norm_method <- norm_method
    }
    
    similar_spaces <- vowel_spaces_df %>%
        filter(
            Speaker %in% closest_3,
            norm == vs_norm_method
        ) %>%
        group_by(Speaker) %>%
        nest() %>%
        mutate(
            plot = map(data, ~ plot_space(.x, titles = FALSE)),
            plot = map2(plot, Speaker, ~ .x + labs(title = .y))
        ) %>%
        pull(plot)
    
    panel <- (
        interval_plot | 
            similar_spaces[[1]] | 
            similar_spaces[[2]] | 
            similar_spaces[[3]]
    )
}


speaker_panel <- function(
    speaker,
    high_int,
    low_int,
    int_length,
    norm_method,
    mean_scaled = TRUE
) {
    high_panel <- plot_panel(
        speaker,
        high_int,
        int_length,
        qb_interval_spaces,
        qb_vowel_spaces,
        norm_method = norm_method,
        mean_scaled = mean_scaled
    )
    low_panel <- plot_panel(
        speaker,
        low_int,
        int_length,
        qb_interval_spaces,
        qb_vowel_spaces,
        norm_method = norm_method,
        mean_scaled = mean_scaled
    )
    
    norm_title <- switch(
        norm_method,
        "lob2" = "Lobanov 2.0",
        "lob2_int_240" = "Lobanov 2.0 (by interval)",
        "lob2_int_60" = "Lobanov 2.0 (by interval)",
        "lob2_int" = "Lobanov 2.0 (by interval)",
        "raw" = "Raw frequency (Hz)",
        stop("Invalid norm_method value")
    )
    
    comp_panel <- high_panel/low_panel + plot_annotation(
        title = paste(speaker, "high and low amplitude intervals"),
        subtitle = paste(norm_title, "similar speakers")
    )
    comp_panel
}

intro_text <- "# Amplitude and Cross-Speaker Vowel Space Similarity

This Shiny interactive enables exploration of judgements of across-speaker vowel 
space similarity and their dependence on relative amplitude, measured by z-scoring
amplitiude for each speaker.

The visualisations we generate have two rows:

1. a high amplitude interval for a speaker and the three closest vowel spaces in the corpus.
2. a low amplitude interval for the same speaker, and the three closest vowel spaces in the corpus.
    
The controls are:

- **Interval length:** whether the corpus is divided into 60 or 240 second intervals.
- **Speaker:** the speaker we will select intervals from.
- **High PC interval:** a list of intervals from the selected speaker with high (>0.3) mean amplitude values.
- **Low PC interval:** a list of intervals from the selected speaker with low (<0.3) mean amplitude values.
- **Normalisation method:** method for normalising data, with options:
    - 'Raw Hz, whole recording': mean value for each monophthong in Hz across whole recording.
    - 'Lobanov 2.0, within interval': the Lobanov 2.0 normalisation method (see [Brand et al. 2021](https://www.sciencedirect.com/science/article/pii/S0095447021000711)) applied *within* each interval.
    - 'Lobanov 2.0, whole recording': same as above, but calculated across whole recording.
- **Scale distances:** allows for distances between monopthongs in each vowel space to be scaled so that vowel 
distances are relative to the distribution of distances between vowels in the corpus. i.e. the distance between 
<span style='font-variant:small-caps;'>DRESS</span> in one
vowel space and <span style='font-variant:small-caps;'>DRESS</span> 
in another will be relative to the distribution of distances between 
<span style='font-variant:small-caps;'>DRESS</span> vowels in the corpus as a whole.
This means that vowels with a lot of 'room to move' in the vowel space are 
less determinative of vowel space similarity.

The PC1 score for each interval is also given. In our PCA analysis, PC1 patterns
with amplitude.
    
This interactive was designed for the purpose of the paper 
'The overlooked effect of amplitude on within-speaker vowel variation' (under review)

Code for this interactive is available at: (blanked for anonymous review)
"