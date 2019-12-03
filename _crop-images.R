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
  map(~ image_scale(.x, geometry = "500"))

# Create new file names
names_new <- tools::file_path_sans_ext(files_to_crop)

dir_out <- here::here("pub", "figs", "figs-cropped")
files_out <- paste0(
  dir_out,
  "/",
  list.files(path = path_to_crop)
)

# Write out
walk2(
  .x = imgs_cropped,
  .y = files_out,
  .f = ~ image_write(image = .x, path = .y)
)
  

