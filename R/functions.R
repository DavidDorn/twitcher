

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
  ratelimit = as.integer(request$headers$`ratelimit-limit`)
  ratelimit_remaining = as.integer(request$headers$`ratelimit-remaining`)


  for (i in 2:pages) {
    request2 <- GET("https://api.twitch.tv/helix/streams", add_headers("Client-ID"=client_id),
                    query= list(
                      after = pagination
                    ))
    data2 <- twitch_request_into_df(request2)
    pagination = content(request2)$pagination[[1]]
    ratelimit_remaining = as.integer(request$headers$`ratelimit-remaining`)

    if (!length(pagination) == 0) {
      data <- bind_rows(data,data2)
      if (pages-2 > ratelimit_remaining) {
        Sys.sleep(60/ratelimit)
      }

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
#' @param pull_all Boolean: try to collect all followers or not
#' @return A dataframe containing the current information for followers of a Twitch user.
get_followers <- function(client_id, user_id, max_requests = 10, pull_all = F) {


  request_string =
    paste("https://api.twitch.tv/helix/users/follows?to_id=", user_id,sep="")

  request <- GET(request_string, add_headers("Client-ID"=client_id),
                 query = list(first = 100))
  data <- twitch_request_into_df(request)
  pagination = content(request)$pagination[[1]]
  ratelimit = as.integer(request$headers$`ratelimit-limit`)
  ratelimit_remaining = as.integer(request$headers$`ratelimit-remaining`)

  total = content(request)$total

  if (pull_all == T) {
    num_requests = ceiling(total/100)
    print(paste("Pulling all ", total, " followers, this will take approximately",
                ceiling(total/ratelimit/60)," hours!"))
  } else {
    num_requests = max_requests
  }


  if (!is.null(total) && total > 100) {
    for (i in 2:num_requests) {
      request_string2 =
        paste("https://api.twitch.tv/helix/users/follows?to_id=", user_id,sep="")

      request2 <- GET(request_string2, add_headers("Client-ID"=client_id),
                      query = list(first = 100, after = pagination))

      data2 <- twitch_request_into_df(request2)
      pagination = content(request2)$pagination[[1]]
      ratelimit_remaining = as.integer(request$headers$`ratelimit-remaining`)

      if (!length(pagination) == 0) {
        data <- bind_rows(data,data2)
        print(paste("Finished page ",i, " of ", num_requests, ",",
                    length(data$from_name), " of ", total, " followers collected."))
        if (num_requests-2 > ratelimit_remaining) {
          Sys.sleep(60/ratelimit)
        }

      } else {
        break
      }
    }

  }

  print(paste("Done!"))
  return(mutate(data, total = total))
}

#' gets as list of tags employed by twitch
#' @export get_tags
#' @param client_id A Twitch API client id
#' @param max_requests The max number of requests you want to send (each request returns a maxiumum of 100 followers)
#' @return A dataframe containing the current tags used by twitch.tv.
get_tags <- function(client_id, max_requests = 10) {


  request_string = "https://api.twitch.tv/helix/tags/streams"

  request <- GET(request_string, add_headers("Client-ID"=client_id),
                 query = list(first = 100))

  data <- twitch_request_into_df(request)
  pagination = content(request)$pagination[[1]]
  ratelimit = as.integer(request$headers$`ratelimit-limit`)
  ratelimit_remaining = as.integer(request$headers$`ratelimit-remaining`)


  num_requests = max_requests


  for (i in 2:num_requests) {

    request2 <- GET(request_string, add_headers("Client-ID"=client_id),
                    query = list(first = 100, after = pagination))

    data2 <- twitch_request_into_df(request2)
    if (!is.null(content(request)$pagination)) {
      pagination = content(request2)$pagination[[1]]
      ratelimit_remaining = as.integer(request$headers$`ratelimit-remaining`)

      data <- bind_rows(data,data2)
      print(paste("Finished page ",i, " of ", num_requests, ",",
                  length(data$tag_id)," tags collected."))
      if (num_requests-2 > ratelimit_remaining) {
        Sys.sleep(60/ratelimit)
      }
    } else {
      break
    }


  }


  print(paste("Done!", ratelimit_remaining, " requests remaining."))
  return(as.data.table(unlist(data)))
}

#' gets the currently active tags of a given broadcast
#' @export get_stream_tags
#' @param client_id A Twitch API client id
#' @param broadcaster_id A Twitch user id which is currently broadcasting, otherwise returns NULL
#' @return A dataframe containing the current ictive tags of a given broadcast.
get_stream_tags <- function(client_id, broadcaster_id) {


  request_string = paste("https://api.twitch.tv/helix/streams/tags?broadcaster_id=",broadcaster_id, sep="")

  request <- GET(request_string, add_headers("Client-ID"=client_id))

  data <- twitch_request_into_df(request)

  return(as.data.table(data))
}


#' gets the current active extensions of a given Twitch user as identified by its user-id.
#' @export get_active_user_extensions
#' @param client_id A Twitch API client id
#' @param user_id A Twitch user id
#' @return A dataframe containing the currently active extensions for a given a Twitch user that is broadcasting, returns NULL if none are active.
get_active_user_extensions <- function(client_id, user_id) {


  request_string = paste("https://api.twitch.tv/helix/users/extensions?user_id=",user_id, sep="")

  request <- GET(request_string, add_headers("Client-ID"=client_id))

  data <- twitch_request_into_df(request)

  return(as.data.table(data))
}
