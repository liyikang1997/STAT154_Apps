# K_means with all quantitative variables

This app is for K_means clustering with all quantitative variables.


## Running the app

The easiest way to run an app is with the `runGitHub()` function from the `"shiny"` package:

```R
# install.packages("shiny")
library(shiny)

# Run an app from a subdirectory in the repo
runGitHub("STAT154_Apps", "liyikang1997", subdir = "k_means_allquant")
```