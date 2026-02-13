# Run apsimx files 
get_apsimx <- function(crops) {
    stopifnot(is.character(crops) && length(crops) >= 1)
    base_folder <- Sys.getenv("APSIMX_DIR")
    if (nchar(base_folder) == 0) {
        stop("Please set the APSIMX_DIR environment variable to the APSIMX directory.")
    }
    
    # List APSIMX for each crop
    all_files <- NULL
    i <- 1
    for (i in seq(along = crops)) {
        crop <- crops[i]
        crop_dirs <- c(file.path(base_folder, sprintf("Tests/Validation/%s/", crop)),
                    file.path(base_folder, sprintf("Prototypes/%s/", crop)))
        files <- list.files(crop_dirs, "\\.apsimx$", full.names = TRUE, recursive = TRUE)

        if (length(files) == 0) {
            warning(sprintf("No .apsimx files found for crop %s in %s", crop, paste(crop_dirs, collapse = ", ")))
            next
        }
        all_files[[i]] <- tibble::tibble(
            crop = crop,
            file = files
        )
    }
    all_files <- dplyr::bind_rows(all_files)
    return(all_files)
}


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



get_obs <- function(crop, crop_data_dir = "_data/_outputs") {
    file_path <- file.path(crop_data_dir, paste0(crop, ".Rds")) |> 
    here::here()
    message("Reading cached output: ", file_path)
    obs <- readRDS(file_path)
    obs <- obs |> 
        dplyr::filter(!grepl("Error$", trait)) |> 
        dplyr::mutate(trait = gsub(paste0("^", crop, "\\."), "", trait)) |> 
        dplyr::filter(!is.na(Observed), !is.na(Predicted)) 
    return(obs)
}

get_experiment_data <- function(apsimx, crop_data_dir = "_data/_outputs") {
    file_path <- file.path(crop_data_dir, paste0(crop, "_experiments.Rds")) |> 
        here::here()
    stopifnot(file.exists(file_path))
    message("Reading cached experiment data: ", file_path)
    experiment_data <- readRDS(file_path)
    
    return(experiment_data)
}


strip_base_path <- function(paths, base_folder) {
    base_folder <- normalizePath(base_folder, winslash = "/", mustWork = FALSE)
    paths <- normalizePath(paths, winslash = "/", mustWork = FALSE)
    
    ifelse(
        startsWith(paths, base_folder),
        substring(paths, nchar(base_folder) + 2),
        paths
    )
}
