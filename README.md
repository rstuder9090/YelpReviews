# YelpReviews
Jack Bressett, Rachel Studer, Ran Zhao

Project for UW-Madison STAT 628 Module 3 - Group 14

Parsing through Yelp Review data to help businesses find information on themselves and what topics patron's talk about in good reviews, bad reviews, and tips. Understanding topics and concepts between different levels of reviews will help sushi restaurants' strive for better ratings.

Created with R 4.1.1

## Code Folder
Contains any R code for cleaning, analysis, visualization, or Shiny App creation.
- [datacleaning.R](Code/datacleaning.R) code to load the json files, filter on sushi restaurants, and create new variables for analysis steps.
- [Plots.R](Code/Plots.R) code for creating EDA plots
- [word2Vec.R](Code/word2Vec.R) for completing Word2Vec in R
- [sushiMap.R](Code/sushiMap.R) creating map distribution of sushi restaurants
- [TextAnalysis.R](Code/TextAnalysis.R) plots of results from text analysis by term/phrase
- [LDA_Reviews_Tips.Rmd](Code/LDA_Reviews_Tips.Rmd) LDA analysis of reviews and tips datasets
- [N-Gram_Calculation.ipynb](Code/N-Gram_Calculation.ipynb) Jupyter Notebook of N-Gram analysis
- [Word_Counting_for_sushi_res.ipynb](Code/Word_Counting_for_sushi_res.ipynb) various distributions of ratings, words, and Word2Vec analysis
- [app.R](Code/app.R) code for Shiny App production


## Data Folder
Contains any clean or raw data
- [sushi.csv](Data/sushi.csv) cleaned business dataset
- [sushi_tips.csv](Data/sushi_tips.csv) cleaned tips dataset
- 10 [sushi_review](Data/sushi_review-1.csv) cleaned reviews dataset - split in 10 files
- 5 [Star-grams.csv](Data/1-Star-grams.csv) 5 star level split grams
- [all_grams_all_reviews.csv](Data/all_grams_all_reviews.csv) all N-grams together


## Images Folder
Contains all important images relative to analysis or EDA.


## How to use Code
Retrieve the business, reviews, and tips JSON files from Yelp.\
Download all files in Code folder into directory of choice. \
Set working directory to respective directory.
