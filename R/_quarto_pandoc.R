# library(rmarkdown, quietly = TRUE)

md_data <- readLines("qmd/abbreviations.qmd")
infile <- tempfile(fileext=".md")
writeLines(md_data, infile)
outfile <- rmarkdown::render(infile, rmarkdown::latex_fragment(), quiet = TRUE)
test <- readLines(outfile)
cat(test, sep = "\n")
