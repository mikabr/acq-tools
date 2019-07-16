# Emi Tanaka (@statsgen) and Garrick Aden-Buie (@grrrck) and Evangeline Reynolds (@EvaMaeRey)
# have contributed to this code
# modified by Mika Braginsky

# reveal lines up to `upto`
reveal <- function(name, upto) {
  content <- knitr::knit_code$get(name)
  content[upto] <- gsub("%>%\\s*(#.+)?$", "\\1", content[upto])
  content[upto] <- gsub("\\+\\s*(#.+)?$", "\\1", content[upto])
  content[upto] <- gsub("->\\s*(#.+)?$", "\\1", content[upto])
  content[1:upto]
}

partial_knit_chunks <- function(chunk_name, chunk_opts, pre_text) {
  if (length(chunk_opts) > 0) chunk_opts <- paste(",", chunk_opts)

  # create slide for lines 1:N for each line N in the given chunk
  idx_lines <- seq_along(knitr::knit_code$get(chunk_name))

  partial_knit_steps <- glue::glue(
    "{{pre_text}}\n\n",
    "```{r plot_{{chunk_name}}_{{idx_lines}}, eval=TRUE {{chunk_opts}}, code=reveal('{{chunk_name}}', {{idx_lines}})}",
    "```",
    .open = "{{", .close = "}}", .sep = "\n"
  )
  glue::glue_collapse(partial_knit_steps, "\n\n---\n\n")

}

apply_reveal <- function(chunk_name, chunk_opts = "", pre_text = "") {
  paste(knitr::knit(text = partial_knit_chunks(chunk_name, chunk_opts, pre_text)), collapse = "\n")
}
