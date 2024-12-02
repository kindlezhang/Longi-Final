# install package
install.packages("devtools")
install.packages("knitr")

# reader
# windows: Rtools

devtools::has_devel(debug = TRUE)
pkgbuild::check_build_tools(debug = TRUE)
