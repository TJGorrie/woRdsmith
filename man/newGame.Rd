\name{newGame}
\alias{newGame}
\title{
Start a new game of woRdsmith
}
\description{
Starts a new game of woRdsmith, timing begins as soon as word is printed.
}
\usage{
newGame(nwords = 10, difficulty=c('easy', 'medium', 'hard'))
}
\arguments{
\item{nwords}{
The number of words to play with. Default is 10.
}
\item{difficulty}{
The difficulty of the game, a choice of easy, medium or hard. Easy contains only lower case letters, medium includes upper case and hard contains both and will additionally join two words together.
}
\item{seed}{
An integer specify the seed to use if desired
}
}

\value{
Invisibly returns a result class object that contains information about the game that has been played.
}

\author{
Tyler Gorrie-Stone
Who to contact: <tgorri@essex.ac.uk>
}

\examples{
\dontrun{
newGame()
}
}
