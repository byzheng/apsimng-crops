# Get the time of the latest git commit for a specific folder
.last_apsimx_commit <- local({
    cache <- new.env(parent = emptyenv())
    
    function(folder) {
        stopifnot(is.character(folder) && length(folder) == 1)
        stopifnot(dir.exists(folder))
        
        # Use normalized path as cache key
        cache_key <- normalizePath(folder, winslash = "/", mustWork = TRUE)
        
        if (!exists(cache_key, envir = cache)) {
            tryCatch({
                git_log <- system(
                    sprintf("git -C \"%s\" log -1 --format=%%ct", folder),
                    intern = TRUE,
                    ignore.stderr = TRUE
                )
                if (length(git_log) == 0 || nzchar(git_log) == 0) {
                    commit_time <- Sys.time()
                } else {
                    commit_time <- as.POSIXct(as.numeric(git_log), origin = "1970-01-01")
                }
                assign(cache_key, commit_time, envir = cache)
            }, error = function(e) {
                assign(cache_key, Sys.time(), envir = cache)
            })
        }
        
        get(cache_key, envir = cache)
    }
})

run_apsimx <- function(files, rerun = TRUE) {

    stopifnot(is.character(files) && length(files) >= 1)
    stopifnot(all(file.exists(files)))
    stopifnot(is.logical(rerun) && length(rerun) == 1)
    
    # Check the Models executable 
    Models <- if (.Platform["OS.type"] == "windows") {
        "Models.exe"
    } else if (.Platform["OS.type"] == "unix"){
        "Models"
    } else {
        stop("Unsupported OS type")
    }
    
    if (nzchar(Sys.which(Models)) == 0) {    
        return(invisible())
    }
    
    if (!rerun) {
        return(invisible())
    }
    # Run APSIMX for file
    i <- 1
    for (i in seq(along = files)) {
        if (!rerun) {
            next
        }
        # Remove db files to force re-run
        db_files <- gsub("\\.apsimx$", ".db", files[i])
        aa <- db_files |> lapply(function(x) {
            if (file.exists(x)) {
                file.remove(x)
            }
        })
        Sys.sleep(1)  # Wait for file removal to complete
        # Run simulations
        message(sprintf("Re-running simulation(s) for %s...", length(files[i])))
        cmd <- paste0(Models, " \"",  files[i], "\"")
        system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
        Sys.sleep(1)  # Wait for simulations to complete
    }
}
