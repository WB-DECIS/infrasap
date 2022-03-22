download_bttns <- function(id, lab){
  shinyWidgets::downloadBttn(
    outputId = id,
    label = lab,
    style = "gradient",
    color = "primary",
    # size = "md",
    block = FALSE,
    no_outline = TRUE
  )  
}