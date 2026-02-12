render_child <- function(template, ...) {

    # create isolated environment
    child_env <- new.env(parent = baseenv())

    # pass parameters as a list
    child_env$params <- list(...)

    # render and return markdown text
    knitr::knit_child(
        template,
        envir = child_env,
        quiet = TRUE
    )
}