pkgs <- c("here", "purrr", "magick", "pdftools")
purrr::walk(pkgs, library, character.only = TRUE)

path_to_convert <- here::here("pub", "figs", "figs-submission")

(files_to_convert <- list.files(path = path_to_convert, full.names = TRUE))

# Read and convert to eps
imgs <-
  files_to_convert %>%
  map(~ image_read_pdf(.x, density = 1200))

# Create new file names
names_new <- tools::file_path_sans_ext(files_to_convert)

fs::dir_create(path = "pub/figs/figs-submission-eps")
dir_out <- here::here("pub", "figs", "figs-submission-eps")

files_out_pdf <- paste0(
  dir_out,
  "/",
  paste0(
    tools::file_path_sans_ext(list.files(path = path_to_convert)),
    ".eps"
  )
)

# wrtie out
walk2(
  .x = imgs,
  .y = files_out_pdf,
  .f = ~ image_write(image = .x, path = .y, density = 1200)
)