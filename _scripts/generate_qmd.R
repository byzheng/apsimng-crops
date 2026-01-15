rm(list = ls())


APSIMX_DIR <- Sys.getenv("APSIMX_DIR")
target_crops <- c("Barley", "Wheat", "Canola") # list of crops to process
target_crops <- "Wheat" 
template_cultivar_file <- "_template/cultivar.qmd" # Template for cultivar report
template_index_file <- "_template/index.qmd" # Template for index report
template_home_file <- "_template/home.qmd" # Template for home report
crop_output_dir <- "_data/_outputs" # Directory to store cached data

# source all functions
a <- list.files("_scripts/function", full.names = TRUE) |>
    lapply(source)

# read templates
template_cultivar <- readLines(template_cultivar_file)
template_index <- readLines(template_index_file)
template_home <- readLines(template_home_file)

# list all crops and only keep target crops
models <- sprintf("https://raw.githubusercontent.com/APSIMInitiative/ApsimX/refs/heads/master/Models/Resources/%s.json", target_crops)

crops <- tibble::tibble(Model = models) |> 
    dplyr::mutate(Crop = tools::file_path_sans_ext(basename(Model)))

i <- 1
for (i in seq(along = crops[[1]])) {
    crop <- crops$Crop[i]
    
    message("Processing crop: ", crop)
    output_file <- file.path(crop_output_dir, paste0(crop, ".Rds"))
    all_reports <- readRDS(output_file)

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
        obs_i <- all_reports |> 
            dplyr::filter(Genotype == tolower(cultivar))
        has_data <- nrow(obs_i) > 0
        # Render .qmd content
        pos <- grep("has_data:", template_cultivar)
        template_cultivar[pos] <- paste0("has_data: ", tolower(as.character(has_data)))
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