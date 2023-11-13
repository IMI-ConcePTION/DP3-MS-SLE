+++
archetype = "codebook"
title = "{{ replace .Name "-" " " | title }}"
output = "html_document"
+++

```{r, echo = FALSE, warning = FALSE, message = FALSE}
html_codebook(rmarkdown::metadata$name_excel)
```