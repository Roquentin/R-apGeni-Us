# ###############################################################
#
#                  Get functions for genius API
#
# ###############################################################

packages <- c( 'jsonlite', 'httr', 'XML' )

# Auto-install packages
newPackages <- packages[!( packages %in% installed.packages()[, 'Package'] )]

if ( length( newPackages ) ) {
	install.packages( newPackages )
}

# Loading libraries
for ( p in seq( length( packages ) ) ) {
	library( packages[p], character.only = TRUE )
}

# --------------------------------------
# Retrieve JSON data from genius.com API
# --------------------------------------

getSongsWithKeyword <- function ( keyword = '', nbResults = 100, token = '' ) {
	# JSON API search call
	baseURL <- 'https://api.genius.com/search?q=' 
	requestURL <- paste0(baseURL, keyword,
						 '&per_page=', nbResults,
						 '&access_token=', token)
	message( 'Genius API search call for: ', keyword )
	response <- fromJSON( requestURL )

	# Retrieving the data if success
	if ( response$meta$status == 200 ) {
		message ( 'API call was sucessful !' )
		songs <- response$response$hits[1]$result;
	} else {
		message( 'Something went wrong... check your token ;-)' )
		songs <- c()
	}

	return( songs )
}

# --------------------------------------
# Deal with the JSON data to export to data.frame
# with lyrics and artist filtering (both optional)
# --------------------------------------

getSongs <- function ( artists = c(), getLyrics = FALSE, jsonData = c() ) {
	# Quick data check
	if ( !is.data.frame ( jsonData ) ) {
		message( 'jsonData is not a dataframe !')
		return()
	}

	message( 'Crunching metadata...' )

	# Get the index of matched songs
	matchedSongs = seq( nrow( jsonData ) )

	if ( length( artists ) > 0 ) {
		# Make it a fair comparison
		artists <- tolower( artists )
		jsonData$primary_artist$name <- tolower( jsonData$primary_artist$name )

		# Now, get matching indexes
		matchedSongs <- which( jsonData$primary_artist$name %in% artists )
	}
	
	# Getting matching subset
	songsMeta <- jsonData[matchedSongs, c( 'url', 'title' )]

	# Now, we deal with the lyrics
	lyrics = rep( '', nrow( songsMeta ) ) 

	if ( getLyrics ) {
		message( 'Crunching lyrics (go grab some coffee, this might take a while)...' )
		for (song in seq( length( lyrics ) ) ) {
			if ( !is.na( songsMeta$url[song] ) ) {
				lyrics[song] <- getLyrics( songsMeta$url[song] )
			}
		}
	}

	# Put it all together and go !
	songs <- data.frame( title = songsMeta$title,
						 artist = jsonData$primary_artist$name[matchedSongs],
						 lyricsURL = songsMeta$url,
						 lyricsText = lyrics )

	return( songs )
}

# --------------------------------------
# Get Lyrics of a song for a single URL
# --------------------------------------

getLyrics <- function ( lyricsURL ) {
	# Unfortunately, genius has no JSON API for songs, so we have
	# to access the webpage directly
	message( 'Retrieving lyrics for ', lyricsURL, '...' )
	response <- GET( lyricsURL )
	html <- content( response, type = 'text/html', encoding = 'UTF-8' )

	# XMLPath to get the lyrics div
	div <- html["//div[@class='lyrics']//text()"]

	# Concatenate and clean div and children nodes
	divText <- paste( sapply( div,
							 function( x ) { xmlValue( x ) }),
					 collapse = " " )
	divTextInline <- gsub( "\\s{2}", " ", divText ) 

	return( divTextInline )
}
