# Visualize PCA with Clusters

This app is for visualizing Principal Component Analysis with clusters by Hierarchical Clustering.


## Running the app

The easiest way to run an app is with the `runGitHub()` function from the `"shiny"` package:

```R
# install.packages("shiny")
library(shiny)

# Run an app from a subdirectory in the repo
runGitHub("STAT154_Apps", "liyikang1997", subdir = "PCA_hclust")
```