# For reproducibility, create a project-specific library

# Set the default repository option
options(repos = c(CRAN = "https://cloud.r-project.org"))

# The following code creates a local directory for your projects packages and
# removes the users home package directory to stop issues with using packages
# from other projects. It is only executed if neither packrat nor renv are present
# as this approach is not compatible with these packages
if(!(dir.exists("./renv") | dir.exists("./packrat"))){

  if(!dir.exists(".rpkg")){

    dir.create(".rpkg")

    .libPaths(c(".rpkg", .libPaths()[!grepl("/home/", .libPaths())]))

  }

  # set lib paths
  .libPaths(c(".rpkg", .libPaths()))
  Sys.setenv(R_LIBS_SITE = .libPaths()[1])

}



