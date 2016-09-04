
setwd("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Pallid-Sturgeon-Population-Model/pallid-sturgeon-population-model/analyses/2016-001/2016-001-02")

# COMPILE UP REPORT
knitr::knit2html("./2016-001-02.Rmd","index.html")	
knitr::pandoc('2016-001-02.md', format='docx')


rmarkdown::render("./2016-001-02.Rmd", pdf_document())
