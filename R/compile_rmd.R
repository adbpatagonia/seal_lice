## ADB
## Oct 4, 2018

## Compile Rmarkdown report

# compile_rmd function ----------------------------------------------------
compile_rmd <- function(file) {
  rmarkdown::render(
    input = here::here("rmds", paste(file, ".rmd", sep = "")),
    output_file = here::here("reports", paste(file, ".html", sep = ""))
  )
}
