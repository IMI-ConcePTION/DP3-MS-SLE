+++
archetype = "codebook"
title = "{{ replace .Name "-" " " | title }}"
+++

```{r, echo = FALSE, warning = FALSE, message = FALSE}
html_codebook(rmarkdown::metadata$name_excel)
```