rm(list = ls())

library(tidyverse)
library(whisker)

template_cultivar_file <- "_template-cultivar.qmd"
template_index_file <- "_template-index.qmd"
crop_cache_dir <- ".cache"
template_home_file <- "_template_home.qmd"
APSIMX_DIR <- Sys.getenv("APSIMX_DIR")
if (nchar(APSIMX_DIR) == 0) {
    stop("Please set the APSIMX_DIR environment variable to the APSIMX directory.")
}

npiExtractObservations <- function(crop) {
    
    file <- file.path(APSIMX_DIR, sprintf("Tests/Validation/%s/%s.apsimx", crop, crop))
    file <- paste0(tools::file_path_sans_ext(file), ".db")
    if (!file.exists(file)) {
        stop("DB file is not found: ", file) 
    }
    library(RSQLite)
    con <- dbConnect(RSQLite::SQLite(), file)
    tbls <- dbListTables(con)
    simulations <- dbReadTable(con, "_Simulations") %>% tibble()
    sowing_report <- dbReadTable(con, "SowingReport") %>% tibble()
    harvest_report <- NULL
    if ("HarvestReport" %in% tbls) {
        harvest_report <- dbReadTable(con, "HarvestReport") %>% tibble()
    }
    factors <- dbReadTable(con, "_Factors") |> tibble()
    reports <- tbls[grepl("(Harvest.*ObsPred|PredictedObserved)$", tbls, ignore.case = TRUE)] %>% 
        map_df(function(x) {
            dbReadTable(con, x) %>% 
                select(SimulationID, starts_with("Predicted"), starts_with("Observed")) %>% 
                select(-starts_with("Pred.Obs")) %>% 
                mutate(across(c( starts_with("Predicted"), starts_with("Observed")), ~as.numeric(.x)))
        }) %>% 
        tibble() %>% 
        pivot_longer(cols = c( starts_with("Predicted"), starts_with("Observed"))) %>% 
        filter(grepl(crop, name)) |> 
        filter(!is.na(value)) %>% 
        mutate(type = gsub("^(Observed|Predicted)\\..+$", "\\1", name),
            trait = gsub("^(Observed|Predicted)\\.(.+)$", "\\2", name)) %>% 
        
        select(-name) %>% 
        group_by(SimulationID, type, trait) %>% 
        summarise(value = mean(value), .groups = "drop") %>% 
        pivot_wider(names_from = "type",
                    values_from = "value") %>% 
        filter(!is.na(Observed))
    dbDisconnect(con)
    
    # sowing_report %>%
    #     select(SimulationID, Genotype = contains('SowingData.Cultivar')) %>% 
    #     filter(is.na(Genotype))
    
    sims <- sowing_report %>%
        mutate(Clock.Today = as.Date(Clock.Today)) %>% 
        select(SimulationID, Genotype = contains('SowingData.Cultivar'),
               Sowing = Clock.Today,
               Latitude = IWeather.Latitude,
               Longitude = IWeather.Longitude) %>%
        right_join(simulations,
                   by = c("SimulationID" = "ID")) %>%
        mutate(Genotype = tolower(Genotype)) %>%
        tibble() %>% 
        right_join(reports, by = "SimulationID") %>% 
        left_join(factors %>% 
                      #select(-FolderName, -CheckpointID) %>%
                      select(SimulationID, ExperimentName) %>%
                      distinct(), by = "SimulationID") %>%
        rename(SimulationName = Name) 
    
    # Merge emregenc date
    if (!is.null(harvest_report)) {
        emergence_report <- harvest_report %>%
            select(SimulationID, contains("EmergenceDAS")) %>% 
            rename(EmergenceDAS = contains("EmergenceDAS"))
        sims <- sims %>% 
            left_join(emergence_report, by = "SimulationID")
    }
    # simple_sims <- simplifySimulation(factors)
    # sims <- simple_sims |> 
    #     distinct() |> 
    #     left_join(sims |> distinct(SimulationID, SimulationName),
    #               by = c("TargetSimulationID" = "SimulationID")) |>  
    #     select(SimulationID, TargetSimulationName = SimulationName) |> 
    #     right_join(sims, by = "SimulationID")
    sims
}


if (!dir.exists(crop_cache_dir)) {
    dir.create(crop_cache_dir, recursive = TRUE)
}

template_cultivar <- readLines(template_cultivar_file)
template_index <- readLines(template_index_file)
template_home <- readLines(template_home_file)
models <- list.files(file.path(APSIMX_DIR, "Models/Resources/"), "*.json", full.names = TRUE)
crops <- tibble(Model = models) |> 
    mutate(Crop = tools::file_path_sans_ext(basename(Model)))
crops <- crops |> 
    filter(grepl("Canola", Crop))
i <- 1
for (i in seq(along = crops[[1]])) {
    
    crop <- crops$Crop[i]
    apsimx <- rapsimng::read_apsimx(crops$Model[i])
    cultivars <- rapsimng::get_cultivar(apsimx, alias = TRUE) |> tibble()
    cultivars_names <- cultivars$name |> unique()
    cultivars_names <- cultivars_names[1:2]
    
    crop_output_dir <- paste0("crop-", tolower(crop))
    if (!dir.exists(crop_output_dir)) {
        dir.create(crop_output_dir)
    } else {
        # remove all files in the directory
        files <- list.files(crop_output_dir, full.names = TRUE)
        a <- file.remove(files)
    }
    
    # Read observations/prediction
    obs <- npiExtractObservations(crop)
    # save observations into file
    obs_file <- file.path(crop_cache_dir, paste0(crop, ".Rds"))
    saveRDS(obs, obs_file)
    
    j <- 1
    for (j in seq(along = cultivars_names)) {
        cultivar <- cultivars_names[j]
        # Render .qmd content
        output <- whisker.render(template_cultivar, list(
            title = paste(cultivar),
            crop = crop,
            cultivar = cultivar
        ))
        writeLines(output, file.path(crop_output_dir, paste0(cultivar, ".qmd")))
    }
    
    index_lines_i <- template_index
    
    writeLines(index_lines_i, file.path(crop_output_dir, "index.qmd"))        
        
    # Add link to index
    rel_link <- file.path(crop_output_dir, "index.html")
    template_home <- c(template_home, paste0("- [", crop, "](", rel_link, ")"))

}

writeLines(template_home, "index.qmd")

quarto::quarto_render()
