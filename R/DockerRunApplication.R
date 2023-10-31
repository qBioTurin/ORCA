#'
#' @title Run docker container
#'
#' @description This is the function to run the application through its docker image
#' @author Pernice Simone
#'
#' @examples
#'\dontrun{
##'     #running runDocker
#'      docker.run(params=NULL, changeUID=TRUE, debug=FALSE)
#'
#' }

docker.application.run <- function(){


  # to check the Docker ID by file
  if (file.exists("dockerID")){
    cat("\n\nDocker does not start, there is already a docker container running che dockerID file!!!\n\n")
    system("echo 2 > ExitStatusFile 2>&1")
    return(2)
  }
  
  ## to execute docker
  cat(paste("docker run -it --rm -p 3838:3838 qbioturin/OCA-application"))
  system(paste("docker run -it --rm -p 3838:3838 qbioturin/OCA-application") )

  ## Get the Docker ID from file
  dockerid=readLines("dockerID", warn = FALSE)
  cat("\nDocker ID is:\n",substr(dockerid,1, 12),"\n")
  
  ## Check the Docker container status
  dockerStatus=system(paste("docker inspect -f {{.State.Running}}",dockerid),intern= T)
  while(dockerStatus=="true"){
    Sys.sleep(10);
    dockerStatus=system(paste("docker inspect -f {{.State.Running}}",dockerid),intern= T)
    cat(".")
  }
  cat(".\n\n")
  ## Check the Docker container exit status
  dockerExit <- system(paste("docker inspect -f {{.State.ExitCode}}",dockerid),intern= T)
  
  if(as.numeric(dockerExit)!=0){
    cat("\nExecution is interrupted\n")
    cat(paste("\nDocker container ", substr(dockerid,1,12), " had exit different from 0\n", sep=""))
    system(paste("docker logs ", substr(dockerid,1,12), " &> ", substr(dockerid,1,12),"_error.log", sep=""))
    # cat("The container's log is saved at: ")
    # system(paste0("docker inspect --format=","'{{.LogPath}}' ",dockerid))
    cat(paste("Please send to simone.pernice@unito.it this error: Docker failed exit 0,\n the description of the function you were using and the following error log file,\n which is saved in your working folder:\n", substr(dockerid,1,12),"_error.log\n", sep=""))
    system("echo 3 > ExitStatusFile 2>&1")
    return(3)
  }else{
    cat("\nDocker exit status:",dockerExit,"\n\n")
    file.remove("dockerID")
    if(debug==TRUE){
      system(paste("docker logs ", substr(dockerid,1,12), " &> ", substr(dockerid,1,12),".log", sep=""))
      cat("The container's log is saved at: ")
      system(paste0("docker inspect --format=","'{{.LogPath}}' ",dockerid))
    }
    system(paste("docker rm -f ",dockerid),intern= T)
    #Normal Docker execution
    system("echo 0 > ExitStatusFile 2>&1")
    return(0)
  }
}
