# Author Disambiguation

This repository contains the Author Disambiguation Project.

The objective is to be able to recognize all articles written by a specific author who has written a specific title.

The folder `R` contains the following codes:

1. Signatures.R: Prepares the dataset by accessing to a local MySQL database.
2. Model_Disambiguated_CV.R: Generates a Cross validated Ensemble Model utilizing a subset of disambiguated signatures.
3. Model.R

The folder `disambiguattion-app` contains:

- The [User guide](http://rpubs.com/Saulabrm/AuthorDisambiguation) for the Application developed in Rpres.
- The [Shiny App](https://saulabrm.shinyapps.io/disambiguation-app/) code in R.
