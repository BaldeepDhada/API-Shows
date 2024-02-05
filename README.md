# TVmaze API Wrapper

This an interactive application which uses TVmaze's REST API to enable users to access television data held within the TVMaze database. Specifically, the application prompts users to:

1. Enter a television show
2. Select the television show from the returned menu
3. Select the television show season
4. Select the type of plot the user wants visualized

This application utilizes 10 functions to handle the user work flow:

1. **get_shows()**
    - An API call is made to the TVmaze database with the television show the user inputed as a parameter. The function returns a dataframe of all television shows similar to what the user inputed, as well as all associated data found within the database. NULL is returned if the television show is not found within the database.

2. **format_show_name()**
    - This function formats the dataframe returned in the **get_shows()** function, returning only the television show name, premier date, end date, and genres.

3. **get_seasons()**
    - A second API call is made to the TVmaze database with television show id as a parameter. This function returns a dataframe of all available data pertaining to a specific television show.

4. **format_season_name()**
    - This function formats the dataframe returned in the **get_seasons()** function, returning only season numbers, season names, premier dates, end dates, and number of episodes.

5. **get_episodes_of_season()**
    - A third API call is made to the TVMaze database with season id as a parameter. This function returns a dataframe of all available data pertaining to the season of a television show.

6. **format_episode_name()**
    - This function formats the dataframe returned in the **get_episodes_of_season()** function, returning only episode numbers, episode names, and episode ratings.

7. **get_all_episodes()**
    - A fourth API call is made to the TVMaze database with television show id as a parameter. This function returns a dataframe of all available television show episode data.

8. **format_all_episodes()**
    - This function formats the dataframe returned in the **get_all_episodes()** function, returning only episode numbers, episode names, and episode ratings.

9. **generate_ratings_plot()**
    - This function utilizes the data generated from the **format_episode_name()** function to produce a visualization of a television shows average rating per season plotted over season.

10. **generate_season_ratings_plot()**
    - This function utilizes the data generated from the **format_all_episodes()** function to produce a visualization of a television shows average rating per episode plotted over episode.

One run of this application requires 4 API calls to be made and the free version of the TVMaze API can be called a maximum of 20 times per minute. As such, this application should only be run a maximum of 5 times per minute.
