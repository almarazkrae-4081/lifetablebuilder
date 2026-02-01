pkgs <- c(
  "shiny",
  "DT",
  "ggplot2",
  "readxl",
  "gridExtra"
)

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

message("Done. Now run: shiny::runApp('src/app.R')")
