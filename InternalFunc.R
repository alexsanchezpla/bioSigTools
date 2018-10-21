gen_markdown <- function(inputFile) {
  
  t <- tempfile()
  cat(inputFile, file = t)
  
  t <- render(
    input         = t,
    output_format = 'html_document')
  
  res <- readLines(t)
  unlink(sub('.html$', '*', t))
  paste(res, collapse = '\n')
  
}
require(knitr)
require(shiny)
gen_markdown(readme_file[1,1])
