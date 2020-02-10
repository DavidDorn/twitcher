

#' Helper function to turn requests from the Twitch API into dataframes
#' @export twitch_request_into_df
#' @param request A request returned from a call to the Twitch API
#' @return A dataframe containing the data returned from a call to the Twitch API
twitch_request_into_df <- function(request) {
  content <- content(request)
  dt_list <- map(content$data, as.data.table)
  dt <- rbindlist(dt_list, fill = TRUE)
  #data <- dt[!duplicated(dt),]
  return(dt)
}


#' gets the top current streams on Twitch by viewers
#' @export get_streams
#' @param client_id A Twitch API client id
#' @param pages The number of pages of streams returned, each page has around 20 observations
#' @return A dataframe containing the top current streams on Twitch by viewers
get_streams <- function(client_id, pages = 10) {

  request <- GET("https://api.twitch.tv/helix/streams", add_headers("Client-ID"=client_id))
  data <- twitch_request_into_df(request)
  pagination = content(request)$pagination[[1]]

  for (i in 2:pages) {
    request2 <- GET("https://api.twitch.tv/helix/streams", add_headers("Client-ID"=client_id),
                    query= list(
                      after = pagination
                    ))
    data2 <- twitch_request_into_df(request2)
    pagination = content(request2)$pagination[[1]]
    if (!length(pagination) == 0) {
      data <- bind_rows(data,data2)
    } else {
      break
    }

  }
  data <- data[,-11]
  return(data[!duplicated(data),])
}

#' gets the current information for a list of Twitch users identified by user-ids
#' @export get_users
#' @param client_id A Twitch API client id
#' @param user_ids A vector of user ids
#' @return A dataframe containing the current information for a list of Twitch users.
get_users <- function(client_id, user_ids) {

  if (length(user_ids)>100) {
    stop("Maximum number of simultaneous requests is 100!")
  }
  request_string = paste(
    paste("https://api.twitch.tv/helix/users?id=", user_ids[1],sep=""),
    paste(user_ids[2:100], collapse = "&id="), sep="&id=")


  request <- GET(request_string, add_headers("Client-ID"=client_id))

  data <- twitch_request_into_df(request)
  return(data)
}


#' gets the current information for a list of games featured on Twitch identified by game-ids
#' @export get_games
#' @param client_id A Twitch API client id
#' @param game_ids A vector of game ids
#' @return A dataframe containing the current information for a list of game titles on Twitch.
get_games <- function(client_id, game_ids) {

  if (length(game_ids)>100) {
    stop("Maximum number of simultaneous requests is 100!")
  }

  request_string = paste(
    paste("https://api.twitch.tv/helix/games?id=", game_ids[1],sep=""),
    paste(game_ids[2:100], collapse = "&id="), sep="&id=")

  request <- GET(request_string, add_headers("Client-ID"=client_id))
  data <- twitch_request_into_df(request)
  return(data)
}

#' gets the current followers of a given Twitch user as identified by its user-id
#' @export get_followers
#' @param client_id A Twitch API client id
#' @param user_id A Twitch user id
#' @param max_requests The max number of requests you want to send (each request returns a maxiumum of 100 followers)
#' @return A dataframe containing the current information for followers of a Twitch user.
get_followers <- function(client_id, user_id, max_requests = 10) {


  request_string =
    paste("https://api.twitch.tv/helix/users/follows?to_id=", user_id,sep="")

  request <- GET(request_string, add_headers("Client-ID"=client_id),
                 query = list(first = 100))
  data <- twitch_request_into_df(request)
  pagination = content(request)$pagination[[1]]
  total = content(request)$total
  num_requests = max_requests

  if (!is.null(total) ) {
    for (i in 2:num_requests) {
      request_string2 =
        paste("https://api.twitch.tv/helix/users/follows?to_id=", user_id,sep="")

      request2 <- GET(request_string2, add_headers("Client-ID"=client_id),
                      query = list(first = 100, after = pagination))

      data2 <- twitch_request_into_df(request2)
      pagination = content(request2)$pagination[[1]]

      if (!length(pagination) == 0) {
        data <- bind_rows(data,data2)
        print(paste("Finished loop ",i, "of ", num_requests, length(data$from_name),  "users geted."))
        Sys.sleep(1)
      } else {
        break
      }
    }

  }


  return(mutate(data, total = total))
}


