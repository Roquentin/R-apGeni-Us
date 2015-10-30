# ###############################################################
#
#                  Retrieving rap songs from genius
#
# ###############################################################

source('genius.R')

# WARNING: add your own token below
token <- '4bexxufUNjk7gM-JjEUsDxH1jLBbkEw51sWakjqcTAfrEKMqse83YzU4-eXLZPIU'

# 1. Booba

boobaJson = getSongsWithKeyword( keyword = 'booba', 
								 nbResults = 100,
								 token = token )

boobaSongs = getSongs( artists = c( 'booba', 'lunatic' ),
					   getLyrics = TRUE,
					   jsonData = boobaJson )

write.csv2( boobaSongs, file = 'booba.csv')

# 2. Rap conscient

rapConscJson = getSongsWithKeyword( keyword = 'rap+conscient',
								    nbResults = 100,
									token = token )

rapConscSongs = getSongs( getLyrics = TRUE,
						  jsonData = rapConscJson )

write.csv2( rapConscSongs, file = 'rapConscient.csv' )
