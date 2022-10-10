library("quarto")
quarto_render("index.qmd") # defaults to html
quarto_render("index.qmd", output_format = "pdf")