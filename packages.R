suppressPackageStartupMessages(
  xfun::pkg_attach(
    c(
      # reproducibility and data wrangling
      "R.utils", "here", "drake", "tidyverse", "data.table",  
      "simplerspec", "ChemometricsWithR", # spectroscopy
      "future", "furrr", "doFuture", # parallel computation
      "ggpubr", "cowplot", # graphics
      "lineup" # correlation metrics
    )
  )
)