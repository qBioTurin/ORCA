#' @title Run docker container
#'
#' @description This function runs the Shiny app using its Docker image. It pulls the image if not available,
#' opens the browser, and monitors for errors.
#' @author Pernice Simone
#'
#' @examples
#' \dontrun{
#'   docker.application.run()
#' }
#' @export
docker.application.run <- function(port = 3838, image = "qbioturin/orca-shiny-application:latest", debug = FALSE) {
  
  # Check if Docker is installed
  if (system("docker --version", ignore.stdout = TRUE, ignore.stderr = TRUE) != 0) {
    cat("Docker is not installed or not available in PATH.\n")
    system("echo 1 > ExitStatusFile 2>&1")
    return(1)
  }
  
  # Check if Docker image exists locally
  image_check_cmd <- paste("docker images -q", image)
  image_id <- system(image_check_cmd, intern = TRUE)
  
  if (length(image_id) == 0 || image_id == "") {
    cat("\nDocker image not found locally. Pulling from DockerHub...\n")
    pull_status <- system(paste("docker pull", image))
    if (pull_status != 0) {
      cat("\nFailed to pull the image from DockerHub.\n")
      system("echo 2 > ExitStatusFile 2>&1")
      return(2)
    }
  } else {
    cat("\nDocker image found locally.\n")
  }
  
  # Run Docker container in detached mode
  run_cmd <- paste("docker run -d -p", paste0(port, ":3838"), image)
  container_id <- system(run_cmd, intern = TRUE)
  
  if (length(container_id) == 0 || container_id == "") {
    cat("\nFailed to start Docker container.\n")
    system("echo 3 > ExitStatusFile 2>&1")
    return(3)
  }
  
  # Open app in browser
  browse_url <- paste0("http://0.0.0.0:", port)
  cat("\nOpening Shiny app in your browser: ", browse_url, "\n")
  utils::browseURL(browse_url)
  
  # Monitor container
  cat("Waiting for container to finish...\n")
  cat("\nType 's' and press ENTER to stop the container...\n")
  repeat {
    input <- tryCatch(readLines(n = 1, warn = FALSE), error = function(e) "")
    if (tolower(input) == "s") {
      cat("\n Stopping container...\n")
      system(paste("docker stop", container_id))
      break
    }
    
    # Check if container is still running
    status <- system(paste("docker inspect -f {{.State.Running}}", container_id), intern = TRUE)
    if (status != "true") {
      cat("\n️  Container has stopped.\n")
      break
    }
    
    Sys.sleep(5)
  }
  
  # Get exit code
  exit_code <- system(paste("docker inspect -f '{{.State.ExitCode}}'", container_id), intern = TRUE)
  
  if (as.numeric(exit_code) != 0) {
    cat("\nDocker container exited with error. Exit code:", exit_code, "\n")
    log_file <- paste0(substr(container_id, 1, 12), "_error.log")
    system(paste("docker logs", container_id, ">", log_file))
    cat("Log saved to:", log_file, "\n")
    cat("Please send the log to simone.pernice@unito.it with a description of the error.\n")
    system("echo 4 > ExitStatusFile 2>&1")
    return(4)
  }
  
  if (debug) {
    log_file <- paste0(substr(container_id, 1, 12), ".log")
    system(paste("docker logs", container_id, ">", log_file))
    cat("\nDebug log saved to:", log_file, "\n")
  }
  
  # Clean up (optional since container is started with `-d`, not `--rm`)
  system(paste("docker rm", container_id), ignore.stdout = TRUE, ignore.stderr = TRUE)
  
  system("echo 0 > ExitStatusFile 2>&1")
  cat("\nShiny app ran successfully.\n")
  
  
  return(0)
}
