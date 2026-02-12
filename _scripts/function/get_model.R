get_model <- function(crop) {
    stopifnot(is.character(crop), length(crop) == 1)
    APSIMX_DIR <- Sys.getenv("APSIMX_DIR")
    stopifnot(dir.exists(APSIMX_DIR))
    # list all crops and only keep target crops
    models <- list.files(file.path(APSIMX_DIR, "Models/Resources/"), "*.json", full.names = TRUE)
    prototypes <- list.files(file.path(APSIMX_DIR, "Prototypes/"), "*.apsimx", full.names = TRUE, recursive = TRUE)
    crops <- tibble::tibble(Model = c(models, prototypes)) |> 
        dplyr::mutate(Crop = tools::file_path_sans_ext(basename(Model)))
    crops <- crops |> 
        dplyr::filter(Crop %in% crop)

    model <- crops |> 
        dplyr::filter(Crop == crop) |> 
        dplyr::pull(Model)
    apsimx <- rapsimng::read_apsimx(model)
    return(apsimx)
}
