
library(rvest)
library(stringi)
library(magrittr)
library(tidyverse)
library(stringr)
library(tidytext)

# Download from Wikipedia
########################################################################################################################
pageURL <- "https://en.wikipedia.org/wiki/List_of_Shark_Tank_episodes"
htmlContent <- read_html(pageURL)

dfRaw <- map_df(1:8, function(Z) html_nodes(htmlContent, "table") %>% 
                    .[[Z+2]] %>% 
                    html_nodes("td") %>% 
                    html_text() %>% 
                    matrix(nrow = length(.)/6, byrow = TRUE) %>% 
                    as.data.frame() %>% 
                    set_colnames(c("Episode", "Title", "AirDate", "ProdCode", "Viewers", "Details")) %>% 
                    mutate(Season = Z) %>% 
                    select(Season, Episode, Title, AirDate, Viewers, Details))

getFullNames <- function(vField) {
    dfMapping <- frame_data(~pattern, ~replacement,
                            "Mark", "Mark Cuban",
                            "Daymond", "Daymond John",
                            "Kevin", "Kevin O'Leary",
                            "Robert", "Robert Herjavec",
                            "Barbara", "Barbara Corcoran",
                            "Lori", "Lori Greiner")
    for(i in 1:nrow(dfMapping)) {
        vField <- str_replace(vField, dfMapping$pattern[i], dfMapping$replacement[i])
    }
    vField
}

season1_sharks <- "Kevin Harrington, Daymond John, Kevin O'Leary, Barbara Corcoran, Robert Herjavec"

getSharks <- function(detailString) {
    lSharks <- qdapRegex::rm_between(detailString, "Sharks: ", "\\n", extract = TRUE)
}

getPitches <- function(detailString) {
    detailString <- stri_trans_general(detailString, "NFD; [:Nonspacing Mark:] Remove; NFC")
    lPitches <- qdapRegex::rm_between(detailString, '"', '"', extract = TRUE)
    lFirstPitches <- lapply(lPitches, "[", 1)
    lOtherPitches <- qdapRegex::rm_between_multiple(detailString, c('; "','. "',', "',"; '\""), c('"','"','"','"'), 
                                                    extract = TRUE)
    return(mapply(c, lFirstPitches, lOtherPitches, SIMPLIFY = FALSE))
}

getPitchInfo <- function(detailString) {
    cat("\n", detailString)
    detailString <<- detailString
    detailString <- stri_trans_general(detailString, "NFD; [:Nonspacing Mark:] Remove; NFC")
    
    regexPattern <- "\\)[[:space:]]*;|\\)[[:space:]]*:|\\)[[:space:]]*,|\\)[[:space:]]*\\.|\n|Update on"
    vPitchInfo <- stri_split_regex(detailString, pattern = regexPattern) %>% unlist() %>% stri_trim()
    
    # vValid <- stri_endswith_fixed(vPitchInfo, "YES)") | stri_endswith_fixed(vPitchInfo, "NO)") |
    #     stri_endswith_fixed(vPitchInfo, "(YES") | stri_endswith_fixed(vPitchInfo, "(NO")
    vValid <- str_detect(vPitchInfo, "\\(YES*|\\(NO*|YES\\)|NO\\)")
    vPitchInfo <- vPitchInfo[vValid]
    
    lProducts <- qdapRegex::rm_between(vPitchInfo, '"', '"', extract = TRUE) %>% lapply("[", 1) 
    vProducts <- lProducts %>% unlist() %>% str_trim() %>% tolower()
    
    if("java" %in% vProducts & "coverplay" %in% vProducts) {
        vProducts <- str_replace(vProducts, "java", "coffee brand gifts")
    }
    
    lDeals <- str_extract_all(vPitchInfo, "YES|NO")
    vDeals <- lDeals %>% unlist()
    
    tibble(Product = vProducts, Deal = vDeals)
}

getDeals <- function(detailString) {
    lDeals <- str_extract_all(detailString, "YES|NO")
}

dfEpisodes <- dfRaw %>% tbl_df() %>% 
    mutate(Episode = as.integer(Episode)) %>% 
    mutate(AirDate = sub("\\(.*\\)", "", AirDate)) %>% 
    mutate(AirDate = as.Date(AirDate, format = "%B %d, %Y")) %>% 
    mutate(Viewers = stri_sub(Viewers, 1, 4)) %>% 
    mutate(Viewers = as.numeric(Viewers)) %>% 
    drop_na() %>% 
    mutate(Sharks = getSharks(Details)) %>% 
    mutate(Sharks = as.character(Sharks)) %>% 
    mutate(Sharks = ifelse(Season == 1, season1_sharks, Sharks)) %>% 
    mutate(Sharks = ifelse(Season >= 4, getFullNames(Sharks), Sharks)) %>% 
    mutate(Pitch = getPitches(Details)) %>% 
    mutate(NumPitches = map_int(Pitch, length)) %>% 
    mutate(Deals = getDeals(Details)) %>% 
    mutate(NumDeals = map_int(Deals, length)) %>% 
    mutate(pitch_info = Details %>% map(. %>% map_df(getPitchInfo))) %>% 
    mutate(NumDeals2 = map_int(pitch_info, nrow)) %>% 
    select(Season, Episode, AirDate, Viewers, Pitch, NumPitches, Deals, NumDeals, NumDeals2, pitch_info, Sharks, Details)

dfPitches <- dfEpisodes %>% 
    select(Season:Viewers, pitch_info, Sharks) %>% 
    unnest() %>% 
    select(Season:Viewers, Product, Deal, Sharks)

# All Shark Tank products from Season 1 to Season 8
########################################################################################################################
getProductURL <- function(link){
    link <- gsub("<a href=.", "", link)
    link <- gsub("\".*", "", link)
    return(link)
}

getProductLinks <- function(pageURL) {
    htmlContent <- read_html(pageURL)
    
    lProductLinks <- html_nodes(htmlContent, "ul") %>% .[[7]] %>% html_nodes("a")
    vProductNames <- lProductLinks %>% html_node("h2") %>% html_text(lProductLinks)
    
    vProductURLs <- map_chr(lProductLinks, getProductURL)
    vProductURLs <- unique(vProductURLs)
    
    df <- tibble(Product = vProductNames, URL = vProductURLs)
}

getProductInfo <- function(productURL) {
    cat("\nReading content from ", productURL)
    Sys.sleep(0.5)
    htmlContent <- read_html(productURL)
    vProductInfo <- html_nodes(htmlContent, "div table tr") %>% html_text()
    return(vProductInfo)
}

homepageURL <- "http://www.productsofsharktank.com/full-list-shark-tank-products/"
dfProduct_URL <- tibble(Page = 1:11) %>% 
    mutate(PageURL = paste0(homepageURL, Page)) %>% 
    mutate(products = map(PageURL, getProductLinks)) %>% 
    unnest() %>% 
    select(-Page, -PageURL)

dfProducts <- dfProduct_URL %>% mutate(data = map(URL, getProductInfo))

# save(dfProducts_old, dfProducts, file = "sharktank.Rda")

# Parse product data
########################################################################################################################
load(file = "sharktank.Rda")

extractField <- function(vProductData, fieldName) {
    fieldValue <- NA_character_
    idx <- which(stri_detect_fixed(vProductData, fieldName))
    if(length(idx) > 0) {
        fieldValue <- stri_replace_all_fixed(vProductData[idx], fieldName, "")
    }
    return(fieldValue)
}

parseProductData <- function(vProductData) {
    vProductData <- gsub("\n+|\t+", " ", vProductData)
    numFields <- length(vProductData)

    vSeasonInfo <- stri_extract_all(vProductData[1], regex = "([0-9]+/[0-9]+/[0-9]+)|[0-9]+") %>% unlist()
    broadcastDate <- stri_trim(gsub(".*Air Date:(.*)", "\\1", vProductData[1]))
    df <- tibble(Season = vSeasonInfo[1], Episode = vSeasonInfo[2], AirDate = broadcastDate) %>% 
        mutate(Owners = NA_character_,
               Asking = NA_character_,
               GotDeal = NA,
               Investors = NA_character_,
               DealTerms = NA_character_)
    
    idxOwner <- which(stri_detect_fixed(vProductData, "Entrepreneurs"))
    idxAsking <- which(stri_detect_fixed(vProductData, "Asking"))
    if(ifelse(length(idxAsking) == 0, FALSE, idxOwner == idxAsking)) {             # information in next line
        dealString <- vProductData[idxOwner+1]
        df$Owners <- stri_replace_all_fixed(stri_extract(dealString, regex = "([A-Z]+[a-z]+ ).*[$]"), "$", "")
        df$Asking <- qdapRegex::rm_between(dealString, "$", "%", fixed = T, extract = T, include.markers = T) %>% unlist()
        df$GotDeal <- stri_extract(dealString, regex = " No| Yes")
        
        idxInvestor <- which(stri_detect_fixed(vProductData, "Investor"))
        idxInvested <- which(stri_detect_fixed(vProductData, "Invested"))
        if(ifelse(length(idxInvestor) == 0 | length(idxInvested) == 0, FALSE, idxInvestor == idxInvested)) {
            termString <- vProductData[idxInvestor+1]
            df$Investors <- stri_replace_all_fixed(stri_extract(termString, regex = "([A-Z]+[a-z]+ ).*[$]"), "$", "")
            df$DealTerms <- stri_replace_all_fixed(termString, df$Investors, "")
        }
    } else {
        df$Owners <- extractField(vProductData, "Entrepreneurs")
        df$Asking <- extractField(vProductData, "Deal Proposed")
        df$GotDeal <- extractField(vProductData, "Deal Reached?")
        df$Investors <- extractField(vProductData, "Shark Investor(s)")
        df$DealTerms <- extractField(vProductData, "Deal Agreement")
    }
    return(df)
}

# Get rid of empty strings in data
dfProducts <- dfProducts %>% 
    mutate(data = map(data, function(Z) Z[lapply(stri_trim(Z), stri_length) %>% unlist() > 0])) %>% 
    mutate(data = map(data, parseProductData))

df <- dfProducts %>% 
    unnest() %>% 
    mutate_if(is.character, stri_trim_both) %>% 
    mutate_if(is.character, function(Z) ifelse(Z == "", NA, Z)) %>% 
    mutate(Season = as.integer(Season),
           Episode = as.integer(Episode),
           Product = tolower(Product)) %>% 
    select(-URL, -Owners, -AirDate) %>% 
    arrange(Season, Episode, Product) %>% 
    select(Season, Episode, Product, everything())

# write.table(df, file = "sharktank.tsv", quote = F, sep = "\t", row.names = F)
# write.table(dfPitches, file = "sharktank_wiki.tsv", quote = F, sep = "\t", row.names = F)

df <- read_tsv("sharktank.tsv")
dfPitches <- read_tsv("sharktank_wiki.tsv")

dfMissing <- dfPitches %>% anti_join(df, by = c("Season","Episode","Product")) %>% 
    arrange(Season, Episode, Product)

dfMissing2 <- df %>% anti_join(dfPitches, by = c("Season","Episode","Product")) %>% 
    arrange(Season, Episode, Product)

# combine data from both sources when there's no missing data
# dfCombined <- dfPitches %>% inner_join(df, by = c("Season","Episode","Product")) %>% 
#     select(Season:Product, Deal, GotDeal, Asking, DealTerms, everything()) %>% 
#     arrange(Season, Episode, Product)

# write.table(dfCombined, file = "sharktank_final.tsv", quote = F, sep = "\t", row.names = F)

dfCombined <- read_tsv("sharktank_final.tsv")

# Download Closed Captions from YouTube
########################################################################################################################
library(magrittr)
library(tidyverse)
library(stringr)
library(rvest)
# library(youTubeDataR)

getVideoIDs <- function(playlist_id) {
    dfItems <- getPlaylistItems(token, playlist.id = playlist_id)
    return(dfItems[["snippet.resourceId.videoId"]] %>% as.character())
}

downloadCaptions <- function(videoID) {
    Sys.sleep(1)
    cat("\nDownloading captions for videoID:", videoID)
    getCaptions(token, video.id = videoID)
}

# Authenticate with YouTube credentials
token <- youOAuth(client.id = Sys.getenv("YOUTUBE_CLIENT_ID"), 
                  client.secret = Sys.getenv("YOUTUBE_CLIENT_SECRET"))

dfPlaylists <- frame_data(~season, ~url,
                          1, "https://www.youtube.com/watch?v=ucqAfRCb00E&list=ELE1PGOJ1VJ8Q",
                          2, "https://www.youtube.com/watch?v=5NOMNHUZbfY&list=ELqy3kIR_XzZw",
                          3, "https://www.youtube.com/watch?v=rI7-iHB5o5o&list=ELxve2GOPbL3Y",
                          4, "https://www.youtube.com/watch?v=sHnKj7tjCKE&list=ELFXU0-5N6J2Q",
                          5, "https://www.youtube.com/watch?v=yxakAMN55XU&list=EL2tBoX_3qrJQ",
                          6, "https://www.youtube.com/watch?v=2VrnVjPHlpk&list=ELIgUAQm-c26HEOC5xcroaCQ",
                          7, "https://www.youtube.com/watch?v=uDGLM0FmueI&list=ELkYq2oXDxbbkv9O75ba7UQg",
                          8, "https://www.youtube.com/watch?v=vmMTwF5D1kI&list=ELmhI5cQvIJIwqP4-k7Btb0w")

dfPlaylists <- dfPlaylists %>% 
    mutate(playlist_id = sub(".*list=(*)", "\\1", url)) %>% 
    mutate(video_id = map(playlist_id, function(Z) getVideoIDs(Z))) %>% 
    unnest() %>% 
    select(-url) %>% 
    group_by(playlist_id) %>% 
    mutate(episode = row_number()) %>% 
    select(season, episode, everything())

dfCaptions <- dfPlaylists %>% 
    dplyr::filter(season == 7) %>% 
    mutate(captions = map(video_id, function(Z) downloadCaptions(Z) %>% html_nodes("text") %>% html_text())) %>% 
    mutate(captions = map_chr(captions, function(Z) paste(Z, collapse = " ")))

save(dfPlaylists, dfCaptions, file = "sharktank_captions.Rda")

vLines <- dfCaptions$captions[1]
vLines <- str_replace_all(vLines, "&#39;", "'")
vLines <- str_replace_all(vLines, '&quot;', '"')
write_lines(vLines, path = "temp.txt")
