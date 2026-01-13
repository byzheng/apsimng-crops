# Run apsimx files 
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
