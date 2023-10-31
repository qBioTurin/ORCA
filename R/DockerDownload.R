#' @title Download for the first time all containers embedded in the workflows
#' @description This is a function that prepares the docker environment to be used for the first time the application is installed.
#' @param containers.file, a character string with the name of the file which indicate which are
#'  the initial set of containers to be downloaded. If NULL then the set is given by a
#'   file called "containersNames.txt" located in the folder inst/Containers of ORCA package.
#' @author Pernice Simone
#'
#' @examples
#'\dontrun{
##'     #running runDocker
#'      downloadContainers()
#'
#' }
#' @export

downloadContainers <- function(containers.file=NULL, tag = "latest"){
  if (is.null(containers.file))
  {
    containers.file = paste(path.package(package="OCA"),"Containers/containersNames.txt",sep="/")
    containers <- read.table(containers.file,
                             header = TRUE,
                             row.names = 1)
  } else {
    containers <- read.table(containers.file,
                             header = TRUE,
                             row.names = 1)
  }
  

    curr.tag <- gsub(pattern = "([[:alpha:]]+){1}(/ORCA){1}(-[[:alpha:]]+:){1}",
                     replacement = "",
                     x = containers$names)
    curr.tag <- unique(curr.tag)
    containers$names <- gsub(pattern = curr.tag,
                             replacement = tag,
                             x = containers$names)
  
  userid=system("id -u", intern = TRUE)
  username=system("id -un", intern = TRUE)
  
  for (i in dim(containers)[1]:1)
  {
    status <- system(paste("docker pull ",containers[i,1],
                           sep = ""))
    if (status)
    {
      containers <- containers[-i]
    }
    else
    {
        return(status)
    }
  }
  
  write.table(containers,
              paste(path.package(package = "ORCA"),"Containers/containersNames.txt",
                    sep = "/"))

  }
