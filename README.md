

# twitcher - R wrapper functions for the [twitch.tv](http://twitch.tv) API

This package contains a bunch of functions to interact with the New [twitch.tv](http://twitch.tv) API ("helix") that I wrote for my PhD research. It is currently limited to endpoints useful for data gathering purposes (i.e. collection of current top streams, retreiving game and user info, collecting followers of a given account etc.), as this is what I'm using for my own research.


## Authentification

All functions require authentification via a valid twitch client id, such as an app access token (you can learn how to obtain one [here](https://dev.twitch.tv/docs/authentication#registration)). This limits the number of requests to 30 per minute, which is enough to look up stream, game and user_info but struggles with pulling followers which can reach tens of millions.

## Endpoints currently implemented:

### get_streams
Fetches the current top stream by viewers. [API reference](https://dev.twitch.tv/docs/api/reference#get-streams)

### get_games
Fetches game info from twitch (if you already have game-ids, i.e. es returned by the get_streams function). [API reference](https://dev.twitch.tv/docs/api/reference#get-games)

### get_users
Fetches user info for given userids (as returned by the get_streams function). [API reference](https://dev.twitch.tv/docs/api/reference#get-users)

### get_followers
Fetches a given user's followers. [API reference](https://dev.twitch.tv/docs/api/reference#get-users-follows)

### get_tags
Fetches all tags used by twitch.tv. [API reference](https://dev.twitch.tv/docs/api/reference#get-all-stream-tags)

### get_stream_tags
Fetches the tags of a given stream (as identified by user-id of a user currently broadcasting). [API reference](https://dev.twitch.tv/docs/api/reference#get-stream-tags)

### get active_user_extensions

[API reference](https://dev.twitch.tv/docs/api/reference#get-user-active-extensions)

## Endpoints not yet implemented

### get_videos
[API reference](https://dev.twitch.tv/docs/api/reference#get-videos)
