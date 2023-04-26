# item_file_download('62793493d34e8d45aa6e3ba9', names='gagesII_metadata_study.csv', destinations='gagesII_metadata_study.csv')
# item_file_download('627974ccd34e8d45aa6e3c81', names='annual_stats_weibull_site.csv', destinations='annual_stats_weibull_site.csv')

local_item_file_download = function(sb_id, ..., names, destinations, 
                              dest_dir = getwd(), session=current_session(), 
                              overwrite_file = FALSE){
  
  sb_id = as.sbitem(sb_id)
  
  if(!session_validate(session))
    stop('Session state is invalid, please re-authenticate')
  
  flist <- item_list_files(sb_id, ..., session=session)
  
  if(nrow(flist) < 1)
    stop(sb_id$id, ':Item has no attached files')
  
  if(missing(names)) {
    
    names <- flist$fname
    
  } else {
    
    if(!missing(destinations) & length(names) != length(destinations))
      stop('Length of names and destinations must be identical')
    
  }
  
  if(!all(names %in% flist$fname)) stop(sb_id$id, 'Item does not contain all requested files')
  
  if(!exists("destinations") | missing(destinations)) {
    destinations <- file.path(dest_dir, names)
  }
  browser()
  flist <- merge(cbind(flist, do.call(rbind.data.frame, attr(flist, "cloud"))), 
                 data.frame(fname=names, dest=destinations))
  
  for(i in seq_len(nrow(flist))) {
    tryCatch({
      
      if(flist[i, ]$cuid != "") {
        
        if(!exists("gql")) gql <- httr::handle(url = "https://api-beta.staging.sciencebase.gov/graphql")
        
        message("retrieving S3 URL")
        
        flist[i, ]$url <- local_get_cloud_download_url(flist[i, c("cuid", "key", "title", "useForPreview")], 
                                                 gql)[[1]]$getS3DownloadUrl$downloadUri[1]
        
      }
      
      message(paste("downloading file", flist[i,]$dest))
      
      httr::GET(url=flist[i,]$url, ..., 
          httr::write_disk(flist[i,]$dest, overwrite = overwrite_file), 
          handle=session, timeout = httr::timeout(10),
          httr::progress())
      
    }, error = function(e) {
      if(file.exists(flist[i,]$dest)) {
        warning(paste(basename(flist[i,]$dest), "exists, and overwrite is false. Skipping."))
      } else {
        stop(paste("Error downloading", flist[i,]$dest, "Original error: \n", e))
      }
    })
  }
  
  return(path.expand(flist$dest))
}


local_get_cloud_download_url <- function(cr, gql) {
  
  query <- "query getS3DownloadUrl($input: SaveFileInputs){ getS3DownloadUrl(input: $input){ downloadUri }}"
  
  variables <- sprintf('{"input": {"selectedRows": {"cuid": "%s", "key": "%s", "title": "%s", "useForPreview": %s}}}',
                       cr$cuid, cr$key, cr$title, cr$useForPreview)
  
  variables <- list(input = list(selectedRows = list(cuid = cr$cuid, key = cr$key, title = cr$title, useForPreview = FALSE)))
  
  json <- jsonlite::toJSON(list(query = query, variables = variables), auto_unbox = TRUE)
  
  run_gql_query(query, gql, json = json)
}

#' @noRd
#' @param q character gql query to embed into json body
#' @param gql handle to pass to POST
#' @param json character json to pass -- shoul include gql query and additional content. 
#' json is optional - it will default to just the query.
run_gql_query <- function(q, gql, json = jsonlite::toJSON(list(query = q), auto_unbox = TRUE)) {
  out <- httr::POST("https://api-beta.staging.sciencebase.gov/graphql", get_gql_header(), 
                    body = json,  
                    handle = gql)
  
  if(out$status_code == 200) {
    jsonlite::fromJSON(rawToChar(out$content))
  } else {
    stop(paste("Error making multipart session.\n code:", out$status_code,
               "\n content:", rawToChar(out$content)))		
  }
  
}

get_gql_header <- function() {
  httr::add_headers(
    .headers = c(`content-type` = "application/json", 
                 accept = "application/json", 
                 authorization = paste("Bearer", 
                                       get_access_token())))
}

get_access_token <- function() {
  token <- pkg.env$keycloak_token$access_token
  
  if(is.null(token)) {
    stop("no token found, must call athenticate_sb()")
  }
  
  token
}
