## As in github.com/roryspanton/batch-magick
## Preparation steps:
# * deb: libpoppler-cpp-dev (Debian, Ubuntu, etc)
#  * On Ubuntu 16.04 or 18.04 use this PPA:
#     sudo add-apt-repository -y ppa:cran/poppler
#     sudo apt-get update
#     sudo sudo apt-get install -y libpoppler-cpp-dev
#  * rpm: poppler-cpp-devel (Fedora, CentOS, RHEL)
#  * csw: poppler_dev (Solaris)
#  * brew: poppler (Mac OSX)

pkgs <- c("purrr", "magick", "pdftools")
purrr::walk(pkgs, library, character.only = TRUE)

path_to_crop <- here::here("pub", "figs", "figs-original")

(files_to_crop <- list.files(path = path_to_crop, full.names = TRUE))

# Read and crop
imgs_cropped <-
  files_to_crop %>%
  map(~ image_read_pdf(.x)) %>%
  map(~ image_scale(.x, geometry = "1000"))

# Create new file names
names_new <- tools::file_path_sans_ext(files_to_crop)

dir_out <- here::here("pub", "figs", "figs-cropped")

files_out_png <- paste0(
  dir_out,
  "/",
  paste0(
    tools::file_path_sans_ext(list.files(path = path_to_crop)),
    ".png"
  )
)
files_out_pdf <- paste0(
  dir_out,
  "/",
  list.files(path = path_to_crop)
)

## Write out
# raster: .png
walk2(
  .x = imgs_cropped,
  .y = files_out_png,
  .f = ~ image_write(image = .x, path = .y)
)
# vector: .pdf
walk2(
  .x = imgs_cropped,
  .y = files_out_pdf,
  .f = ~ image_write(image = .x, path = .y)
)

