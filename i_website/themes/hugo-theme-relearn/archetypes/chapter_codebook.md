+++
archetype = "chapter"
title = "{{ replace .Name "-" " " | title }}"
name_excel = ["Table 1.xlsx", "test"]
description = "`r paste(rmarkdown::metadata$name_excel, collapse = '.')`"
output = "html_document"
+++

paste(rmarkdown::metadata$name_excel, collapse = "<br>")
