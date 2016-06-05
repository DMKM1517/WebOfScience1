# Author Disambiguation

This repository contains the Author Disambiguation Project.

The objective is to be able to recognize all articles written by a specific author who has written a specific title.

The folder `R` contains the following codes:

1. Signatures.R: Prepares the dataset by accessing to a local MySQL database.
2. Model_Disambiguated_CV.R: Generates a Cross validated Ensemble Model utilizing a subset of disambiguated signatures.
3. Model.R

The folder `python` contains models built using scikit-learn and word2vec. `Name2Vec.bin` is a skip-gram model that was trained with following parameters:
```
min_word_count = 1
vector_size = 10
word_window = 5
worker_threads = 8
```

The `spark` folder contains code to build datapipelines that generate author,title pair signatures. All the pre-procesing functions are contained in the `PreProcessingUtil.scala`, `ADMain.scala` contains the main method and `Features.scala` containes pipeline logic. 

The folder `disambiguattion-app` contains:

- The [User guide](http://rpubs.com/Saulabrm/AuthorDisambiguation) for the Application developed in Rpres.
- The [Shiny App](https://saulabrm.shinyapps.io/disambiguation-app/) code in R.
