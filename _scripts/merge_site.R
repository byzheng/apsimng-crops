merge_site <- function(site_dir = "_site", 
    parts_dir = "site-parts",
    output_dir = "final_site") {
    is_github <- Sys.getenv("GITHUB_ACTIONS") == "true"
    if (!is_github) {
        message("Not running on GitHub Actions, skipping site merge.")
        return(invisible(NULL))
    }

    if (!dir.exists(site_dir)) {
        stop("Global site directory not found: ", site_dir)
    }

    unlink(output_dir, recursive = TRUE, force = TRUE)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    global_files <- list.files(site_dir, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
    global_files <- global_files[file.info(global_files)$isdir == FALSE]

    for (src in global_files) {
        rel <- sub(paste0("^", normalizePath(site_dir, winslash = "/", mustWork = TRUE), "/"), "", normalizePath(src, winslash = "/", mustWork = TRUE))
        dst <- file.path(output_dir, rel)
        dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
        file.copy(src, dst, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    }

    part_dirs <- Sys.glob(file.path(parts_dir, "site-*"))
    collision_count <- 0L

    for (dir in part_dirs) {
        if (!dir.exists(dir)) next

        nested <- Sys.glob(file.path(dir, "site-*"))
        source_root <- if (length(nested) > 0) nested[[1]] else dir

        source_files <- list.files(source_root, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
        source_files <- source_files[file.info(source_files)$isdir == FALSE]

        source_root_norm <- normalizePath(source_root, winslash = "/", mustWork = TRUE)

        for (src in source_files) {
            src_norm <- normalizePath(src, winslash = "/", mustWork = TRUE)
            rel <- sub(paste0("^", source_root_norm, "/"), "", src_norm)

            if (identical(rel, "index.html")) {
                message("Skipping fragment root index.html: ", src)
                next
            }

            dst <- file.path(output_dir, rel)
            dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)

            if (file.exists(dst)) {
                collision_count <- collision_count + 1L
                message("Overwriting existing file: ", dst, " (from ", src, ")")
            }

            file.copy(src, dst, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
        }
    }

    merged_files <- list.files(output_dir, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
    merged_files <- merged_files[file.info(merged_files)$isdir == FALSE]

    message("Merged file count in ", output_dir, ": ", length(merged_files))
    message("Overwritten files: ", collision_count)

    if (length(merged_files) > 0) {
        sample_files <- head(sort(sub(paste0("^", normalizePath(output_dir, winslash = "/", mustWork = TRUE), "/"), "", normalizePath(merged_files, winslash = "/", mustWork = TRUE))), 30)
        message("Sample merged files:")
        for (f in sample_files) message(" - ", f)
    }

    invisible(list(total = length(merged_files), overwritten = collision_count))
}

merge_site()