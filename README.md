# File Merger

An R Shiny app that combines many Excel and CSV files into one table. Built for the usual case: the
same report arriving as a dozen files, each with a slightly different header.

**Live app:** https://keyurtbd.shinyapps.io/excel__csv_file_merger/ (on the free tier it sleeps when idle,
so the first load can take about 30 seconds)

## What it does

- Upload any mix of `.csv`, `.xls` and `.xlsx` files at once.
- Set which row holds the header, for exports that start with a few lines of title or notes.
- Optionally clean special characters out of the column names and values.
- Optionally add a `Source_File` column, so every merged row says which file it came from. That is the bit
  you want an hour later, when one number looks wrong.
- Preview the merged table, then download it as one Excel file.

CSV files are read with `data.table::fread`, falling back to `readr` when a file trips it up; Excel files
with `readxl`.

## Run it locally

```r
install.packages(c("shiny", "shinydashboard", "shinyjs", "DT", "data.table", "dplyr",
                   "readr", "readxl", "writexl"))
shiny::runApp("Excel & CSV File Merger.R")
```
