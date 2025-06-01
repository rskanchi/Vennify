# Vennify

A minimal R Shiny app to generate proportional Venn diagrams from pasted or uploaded gene lists.

### Requirements

-   R (\>= 4.1.0)
-   The following R packages: `shiny`, `eulerr`, `gplots`, `stringi`, `colourpicker`.

You can install the R packages with `install.packages(c("shiny", "eulerr", "gplots", "stringi", "colourpicker"))` or 
restore the project environment if you're using `renv`:

```r
install.packages("renv")
renv::restore()
```

### Running the App
In R, `setwd(<project/folder>)` and execute `shiny::runApp()`. 
In RStudio, open the project, click the Run App button in the upper right of the editor.

### Input options

#### Paste lists

- Paste values (one per line) for up to 3 sets
- Customize each set's label and color
- Optionally, specify an output folder

#### Upload file

- Upload a CSV with set columns (set1, set2, ...) and optional sign columns (sign1, sign2, ...)
- Sign columns could be the log2 fold change, correlation or any other numeric value indicating directionality of the gene/rows
- Select which columns to use as set and sign **in the same order** as set columns.
- Choose custom colors for each set

### Output

Proportional Venn diagrams (pdf format), unique/overlap set elements and frequencies (csv format).
If the sign variables are provided, the app will also generate UP and DOWN-regulated venns, lists and frequencies.

### Notes
- You can edit the output folder by typing a valid path (local to the machine running the app).
- `renv.lock` is included for reproducibility; the `renv/` folder is excluded to keep the repo light.
- If you want to use renv, install it and uncomment this line in .Rprofile: `source("renv/activate.R")`

### License
This project is licensed under the MIT License.