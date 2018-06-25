engine <- function(word){
  breakdown <- strsplit(word, '')[[1]]
  message(word)
  s <- Sys.time()
  for(i in breakdown){
    x <- FALSE
    while(!x) x <- ((y <- keypress()) == i)
	cat(y)
  }
  e <- Sys.time()
  cat('\n')
  return(e-s)
}

newGame <- function(nwords = 10, difficulty = c('easy', 'medium', 'hard'), seed = NULL){
    if(!has_keypress_support()) stop('woRdsmith is not supported by GUIs, please run this game in terminal (windows: Rterm.exe, Max/Linux run R from terminal).')
    difficulty <- match.arg(difficulty)
    if(!length(nwords) == 1L & !as.numeric(nwords) > 0) stop('Please ensure you provide a single number larger than 0!)')
    if(!is.null(seed) & (!length(seed) == 1L | !is.numeric(seed))) stop('Provide a single, numeric, input to seed')
    message('Loading game data')
    data(words)
    # Generate a seed
    if(is.null(seed)){
	seed <- 2147483648
        while(seed > 2147483647) seed <- as.numeric(paste0(sample(1:1000, 3), collapse=''))
    }
    set.seed(seed)
    # Select sample words
    wordset <- switch(difficulty,
                'easy' = {sample(tolower(words), nwords)},
		'medium' ={sample(words, nwords)},
		'hard' = {
			hardwords <- sample(words, nwords*2)
			paste(hardwords[c(T,F)], hardwords[c(F,T)])
			}
		)
    game <- sapply(wordset, engine)
	results <- list('Difficulty' = difficulty,
	                'Number of Words' = nwords,
			'Seed' = seed,
			'Total Time' = sum(game),
			'Details' = data.frame(Words = wordset, Time = game, stringsAsFactors=FALSE))
	class(results) <- 'results'
	results
}

print.results <- function(x){
    message(sprintf('You completed the game in %s [seconds]', x[['Total Time']]))
	message(sprintf('The seed used was: %s on %s difficulty', x[['Seed']], x[['Difficulty']]))
	message(sprintf('The average time per word was %s seconds', mean(x[['Details']][,2], na.rm = TRUE)))
	words <- x[['Details']][,1]
	message(sprintf('The longest and shortest words were: %s and %s. \nThe average length of each word was %s characters.\n',
	    words[which.max(nchar(words))],words[which.min(nchar(words))],mean(nchar(words),na.rm = T)))
    invisible(x)
}
