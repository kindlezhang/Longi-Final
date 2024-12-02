# install package
install.packages("devtools")
install.packages("knitr")

# reader
# windows: Rtools

devtools::has_devel(debug = TRUE)
pkgbuild::check_build_tools(debug = TRUE)

# create package

setwd("D:\\Study\\master lecture\\longi data\\r_package")
library(devtools)
create_package("linmodel")
