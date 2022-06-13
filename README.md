# Amplitude and Cross-Speaker Vowel Space Similarity

This Shiny interactive enables exploration of judgements of across-speaker vowel 
space similarity and their dependence on relative amplitude as measured by 
PC score.

The visualisations we generate have two rows:

1. a high amplitude interval for a speaker and the three closest vowel spaces in the corpus.
2. a low amplitude interval for the same speaker, and the three closest vowel spaces in the corpus.
    
The controls are:

- **Interval length:** whether the corpus is divided into 60 or 240 second intervals.
- **Speaker:** the speaker we will select intervals from.
- **High PC interval:** a list of intervals from the selected speaker with high (>1) PC1 values.
- **Low PC interval:** a list of intervals from the selected speaker with low (<1) PC1 values.
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
    
This interactive was designed for the purpose of the paper: 
'The overlooked effect of amplitude on within-speaker vowel variation' (under review)