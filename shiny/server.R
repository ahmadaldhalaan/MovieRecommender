### Revised from https://github.com/pspachtholz/BookRecommender

## server.R

# define functions
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
  
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings$Timestamp = NULL

# create table for genre recommendations
rankings <- ratings %>% left_join(movies, by = "MovieID")

# read in UBCF model
rec <- readRDS('model_ubcf.rds')

shinyServer(function(input, output, session) {
  
  output$genre <- renderTable({
    # count number of ratings per user
    ratings_dist <- rankings %>% count(UserID)
    # select regular users
    rankings <- subset(ratings_dist, n>=96, select = UserID) %>% 
      inner_join(rankings, by = "UserID")
    
    # filter for movies with the chosen genre
    genre_rankings <- filter(rankings, grepl(input$variable, Genres))   
    
    # calculate parameters  
    C <- mean(genre_rankings$Rating)
    R <- genre_rankings %>% group_by(MovieID) %>% summarise(mean_rating = mean(Rating))
    v <- genre_rankings %>% group_by(MovieID) %>% summarise(n = n())
    m <- 60 # 1% of users
    
    # find weighted rating and bind with movie id  
    W <- data.frame(cbind(R$MovieID, (R$mean_rating*v$n + C*m) / (v$n+m)))
    colnames(W) <- c("MovieID","Rating")
    
    # find top 10 rated movies  
    top_10 <- top_n(W, 10, Rating)
    
    # create output
    genre_recom <- top_10 %>% 
      inner_join(genre_rankings, by = "MovieID") %>% 
      distinct(Title, Rating.x) %>%
      rename(Rating = Rating.x) %>%
      arrange(-Rating)

  })

  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the button is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      
      # create dummy user ID
      user_ratings$UserID <- 9999
      user_ratings <- user_ratings[,c(3,1,2)]
      
      # bind user ratings with all ratings and convert to sparse real Rating Matrix
      ratings <- rbind(user_ratings, ratings)
      
      i = paste0('u', ratings$UserID)
      j = paste0('m', ratings$MovieID)
      x = ratings$Rating
      tmp = data.frame(i, j, x, stringsAsFactors = T)
      Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
      rownames(Rmat) = levels(tmp$i)
      colnames(Rmat) = levels(tmp$j)
      Rmat = new('realRatingMatrix', data = Rmat)
      
      # predict top 10 for user and save in a table
      p <- as(predict(rec, Rmat[6041], n=10),"list")
      movie_index <- as.integer(sub('m','',p[[1]]))
      
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = movie_index,
                                  Title = subset(movies, MovieID %in% movie_index)$Title)
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function
