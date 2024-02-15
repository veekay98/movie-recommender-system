library(text2vec) # For text processing and vector space models.

# Read the dataset
movies <- read.csv("/Users/vigneshkrishnan/Downloads/final-movies.csv", stringsAsFactors = FALSE)

calculate_cosine_similarity <- function(text1, text2) {
  
  # Combine the two input texts into a single vector, treating each as a separate document
  texts = c(text1, text2)
  
  # Creates a document-term matrix (DTM) from the combined texts
  
  # 'itoken' tokenizes the texts, 'tolower' converts to lowercase, 'word_tokenizer' splits into words
  it = itoken(texts, tolower, word_tokenizer)
  
  # Create a vocabulary from the tokenized texts
  v = create_vocabulary(it)
  
  # Vectorize the vocabulary to prepare for DTM creation
  vectorizer = vocab_vectorizer(v)
  
  # Generate the DTM using the vectorized vocabulary
  dtm = create_dtm(it, vectorizer)
  
  # Initialize a TF-IDF (Term Frequency-Inverse Document Frequency) model
  tfidf = TfIdf$new()
  
  # Apply the TF-IDF transformation to the DTM
  dtm_tfidf = fit_transform(dtm, tfidf)
  
  # Calculate the cosine similarity between the two TF-IDF vectors
  # 'sim2' function computes similarity, '[1, 2]' selects the similarity score between the first and second document
  similarity_score = sim2(dtm_tfidf, dtm_tfidf, method = "cosine", norm = "l2")[1, 2]
  
  # Return the calculated cosine similarity score
  return(similarity_score)
}




# Calculate Jaccard similarity for genres
calculate_jaccard_similarity <- function(list1, list2) {
  # Calculate the intersection of the two lists
  intersection <- length(intersect(list1, list2))
  
  # Calculate the union of the two lists
  union <- length(union(list1, list2))
  
  # If the union is zero (i.e., both lists are empty), return a similarity of 0
  if (union == 0) return(0)
  
  # Return the Jaccard similarity: size of intersection divided by size of union
  return(intersection / union)
}

convert_to_vector <- function(array_string) {
  # Remove square brackets and split the string by comma
  number_strings <- strsplit(gsub("\\[|\\]", "", array_string), ", ")[[1]]
  
  # Convert to numeric and create a vector
  vector <- as.numeric(number_strings)
  
  return(vector)
}

calculate_average_genres <- function(genre_lists) {
  # Initialize an empty list to store the converted vectors
  genre_vectors <- list()
  
  # Iterate through the genre lists and convert each to a vector
  for (i in 1:length(genre_lists)) {
    genre_vectors[[i]] <- convert_to_vector(genre_lists[i])
  }
  
  # Combine the genre vectors into a matrix (each vector as a row)
  combined_matrix <- do.call(rbind, genre_vectors)
  
  # Calculate the average for each column
  average_values <- colMeans(combined_matrix)
  
  return(average_values)
}

# A list of target movie titles for analysis
target_movie_titles <- c("Inception", "The Dark Knight", "Batman Begins")

# Filter the 'movies' dataframe to only include rows where the original title is in the target movie titles list
target_movies <- movies[movies$original_title %in% target_movie_titles,]

# Concatenate all overviews of the target movies into a single string to create an average overview
average_overview <- paste(target_movies$overview, collapse = " ")

# Calculate the average genres for all movies in target_movies using one hot encodings
average_genres <- calculate_average_genres(as.list(target_movies$encoded_genres))

# Extract unique director names from the target movies
average_director <- unique(target_movies$Directors)

# List containing the average director names
name_list <- list(average_director)

# Function to clean each string
clean_string <- function(s) {
  # Remove [' at the start and '] at the end of the string
  cleaned = gsub("^\\['", "", s)
  cleaned = gsub("'\\]$", "", cleaned)
  return(cleaned)
}

# Apply 'clean_string' function to each director's name in the list and create a unique vector of cleaned names
director_list <- unique(unlist(lapply(name_list, clean_string)))

# Function to check if a vector contains a specific string
check_string_in_vector <- function(vector, string_to_check) {
  # Check if any element in string_to_check is in vector
  if (any(string_to_check %in% vector)) {
    return(1)
  } else {
    return(0)
  }
}

# Returns the main cast member of the movie
extract_lead_actor <- function(cast_string){
  # Remove brackets and split by comma
  cast_list <- strsplit(gsub("\\['|'\\]", "", cast_string), "', '")[[1]]
  
  # Extract the first cast member
  first_cast_member <- cast_list[1]
  
  return(first_cast_member)
}

# To store the names of the main cast members
first_cast_names <- vector("character", length = nrow(target_movies))

# Iterate through the target movie rows and extract the main cast members
for (i in 1:nrow(target_movies)) {
  first_cast_names[i] <- extract_lead_actor(target_movies$Cast[i])
}

# Removes duplicates
first_cast_names <- unique(first_cast_names)

# Function to calculate similarity to the average profile
calculate_similarity_to_average <- function(movie) {
  # If the movie is in the target list, then return 0 similarity
  if (movie$title %in% target_movie_titles) {
    return(0)
  }
  # If any of the columns considered are empty, then return 0 similarity
  if (is.na(movie$overview) | is.na(movie$encoded_genres) | 
      is.na(movie$Directors) | is.na(movie$Cast)) {
    return(0)
  } else {
    # Similarity based on movie overview
    overview_sim <- calculate_cosine_similarity(movie$overview, average_overview)
    # Similarity based on genres they belong to
    genre_sim <- calculate_jaccard_similarity(convert_to_vector(movie$encoded_genres), average_genres)
    # Similarity based on director
    director_sim <- check_string_in_vector(unique(unlist(lapply(movie$Directors, clean_string))), director_list)
    # Similarity based on main cast member
    lead_actor_sim <- check_string_in_vector( extract_lead_actor(movie$Cast), first_cast_names)
    
    # Calculates weighted sum of similarity between the two movies
    weighted_sum <- 0.4 * overview_sim + 0.2 * genre_sim + 0.2 * director_sim + 0.2 * lead_actor_sim
    return(weighted_sum)
  }
}

# Iterates through all the movies, finds similarity between each of them and our target movies
# and then stores it back to a new column called 'similarity'
movies$similarity <- sapply(1:nrow(movies), function(i) {
  calculate_similarity_to_average(movies[i, , drop = FALSE])
})

# Find the most similar movie based on max value in similarity column and returns the title
most_similar_movie <- movies[which.max(movies$similarity), "title"]

# This makes sure that the recommended movie has a voter rating of 5 or above. Keeps looping until that's satisfied
while (movies[which.max(movies$similarity), "vote_average"]<5.0){
  # Find the index of the current most similar movie with rating <5
  index_of_most_similar_movie <- which(movies$title == most_similar_movie)
  
  # Set the similarity score of the  movie to 0
  movies$similarity[index_of_most_similar_movie] <- 0
  
 # This is done so that the already explored and ignored movies are not compared again
  target_movie_titles <- c(target_movie_titles, most_similar_movie)

  # Finding the movie with the highest similarity
  most_similar_movie <- movies[which.max(movies$similarity), "title"]
}

print(most_similar_movie)