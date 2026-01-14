rm(list = ls())

# Define parmeters
rerun <- TRUE # whether to rerun apsimx simulations
target_crops <- c("Barley", "Wheat", "Canola") # list of crops to process
target_crops <- "Wheat" 
APSIMX_DIR <- Sys.getenv("APSIMX_DIR")
template_cultivar_file <- "_template/cultivar.qmd" # Template for cultivar report
template_index_file <- "_template/index.qmd" # Template for index report
template_home_file <- "_template/home.qmd" # Template for home report
crop_cache_dir <- ".cache" # Directory to store cached data

# source all functions
a <- list.files("_scripts/function", full.names = TRUE) |>
    lapply(source)

# read templates
template_cultivar <- readLines(template_cultivar_file)
template_index <- readLines(template_index_file)
template_home <- readLines(template_home_file)

# create cache directory
if (!dir.exists(crop_cache_dir)) {
    dir.create(crop_cache_dir, recursive = TRUE)
}
# list all crops and only keep target crops
models <- list.files(file.path(APSIMX_DIR, "Models/Resources/"), "*.json", full.names = TRUE)
crops <- tibble::tibble(Model = models) |> 
    dplyr::mutate(Crop = tools::file_path_sans_ext(basename(Model)))
crops <- crops |> 
    dplyr::filter(Crop %in% target_crops)
i <- 1
for (i in seq(along = crops[[1]])) {
    crop <- crops$Crop[i]
    # List all apsimx files
    files <- get_apsimx(crop)
    # files <- files[7,]
    # Run apsimx files
    run_apsimx(files$file, apsimx_base = APSIMX_DIR, rerun = rerun)

    # read all reports 
    all_reports <- read_reports(files$file, crop)
    obs_file <- file.path(crop_cache_dir, paste0(crop, ".Rds"))
    saveRDS(all_reports, obs_file)
    

    apsimx <- rapsimng::read_apsimx(crops$Model[i])
    cultivars <- rapsimng::get_cultivar(apsimx, alias = TRUE) |> tibble::tibble()
    cultivars_names <- cultivars$standard_name |> unique()
    # cultivars_names <- cultivars_names[1:2]


    crop_output_dir <- paste0("crop/", tolower(crop))
    if (!dir.exists(crop_output_dir)) {
        dir.create(crop_output_dir, recursive = TRUE)
    } else {
        # remove all files in the directory
        files <- list.files(crop_output_dir, full.names = TRUE)
        a <- file.remove(files)
    }
    
    # for each cultivars
    j <- 1
    for (j in seq(along = cultivars_names)) {
        cultivar <- cultivars_names[j]
        # Render .qmd content
        output <- whisker::whisker.render(template_cultivar, list(
            title = paste(cultivar),
            crop = crop,
            cultivar = cultivar
        ))
        writeLines(output, file.path(crop_output_dir, paste0(cultivar, ".qmd")))
    }
    index_lines_i <- template_index
    
    writeLines(index_lines_i, file.path(crop_output_dir, "index.qmd"))        
        
    # # Add link to index
    # rel_link <- file.path(crop_output_dir, "index.html")
    # template_home <- c(template_home, paste0("- [", crop, "](", rel_link, ")"))    
}

# Generate table of crops for home template
crop_table <- crops |>
    dplyr::mutate(
        link = paste0("- [", Crop, "](crop/", tolower(Crop), "/index.html)")
    ) |>
    dplyr::pull(link) |>
    paste(collapse = "\n")

template_home <- whisker::whisker.render(template_home, list(
    crop_table = crop_table
))
writeLines(template_home, "index.qmd")