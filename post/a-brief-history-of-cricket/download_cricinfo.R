library(magrittr)
library(tidyverse)
library(rvest)

lCodes <- list(FORMATCODE = 1:3, COUNTRYCODE = c(1:9, 25))
names(lCodes$FORMATCODE) <- c("Tests", "ODIs", "T20Is")
names(lCodes$COUNTRYCODE) <- c("England","Australia","South Africa","West Indies","New Zealand",
                              "India","Pakistan","Sri Lanka","Zimbabwe","Bangladesh")

# URL template to get records of all teams (matches, won, etc.)
url_template_team <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=FORMATCODE;template=results;type=team"

# URL template to get records of a team by year (matches, won, etc.)
url_template_team_byYear <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=FORMATCODE;team=COUNTRYCODE;template=results;type=team;view=year"

# URL template to get player records (matches, won, etc.)
url_template_player <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=FORMATCODE;filter=advanced;groupby=players;orderby=won;page=PAGENUM;size=200;template=results;type=team"

# URL template to get overall batting statistics for all formats
url_template_batting <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=FORMATCODE;filter=advanced;orderby=runs;page=PAGENUM;size=200;team=COUNTRYCODE;template=results;type=batting"

# URL template to get overall batting statistics for tests including strike rates
url_template_batting_sr <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=FORMATCODE;filter=advanced;orderby=runs;page=PAGENUM;size=200;spanmin1=01+Jan+STARTYEAR;spanval1=span;team=COUNTRYCODE;template=results;type=batting"

# URL template to get overall bowling statistics for all formats
url_template_bowling <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=FORMATCODE;filter=advanced;orderby=wickets;page=PAGENUM;size=200;team=COUNTRYCODE;template=results;type=bowling"

# Function to download team data from ESPNcricinfo Statsguru -------------------------------------------------------------
getTeamRecords <- function(url_template, formatCode, countryCode = NA) {
    initURL <- sub("FORMATCODE", formatCode, url_template, fixed = T)
    if(!is.na(countryCode)) {
        initURL <- sub("COUNTRYCODE", countryCode, initURL, fixed = T)
    }
    
    dfResults <- NULL
    pageNum <- 0
    while(TRUE) {
        pageNum <- pageNum + 1
        downloadURL <- sub("PAGENUM", pageNum, initURL, fixed = T)
        
        htmlContent <- read_html(downloadURL)
        dfTemp <- htmlContent %>% html_nodes("table") %>% .[[3]] %>% html_table() %>% select(-(ncol(.)))
        
        dfResults <- rbind(dfResults, dfTemp)
        if(nrow(dfTemp) < 200) break
    }
    return(dfResults)
}

# Function to download player batting/bowling data from ESPNcricinfo Statsguru ------------------------------------------
getBattingBowlingRecords <- function(url_template, formatCode, countryCode) {
    initURL <- sub("FORMATCODE", formatCode, url_template, fixed = T)
    initURL <- sub("COUNTRYCODE", countryCode, initURL, fixed = T)

    # change the download year for Sri Lanka    
    if(url_template == url_template_batting_sr) {
        startYear <- 1994
        if(names(lCodes$COUNTRYCODE[which(lCodes$COUNTRYCODE == 8)]) == "Sri Lanka") {
            startYear <- 1997
        }
        initURL <- sub("STARTYEAR", startYear, initURL, fixed = T)
    }
    
    dfResults <- NULL
    pageNum <- 0
    while(TRUE) {
        pageNum <- pageNum + 1
        downloadURL <- sub("PAGENUM", pageNum, initURL, fixed = T)
        
        htmlContent <- read_html(downloadURL)
        dfTemp <- htmlContent %>% html_nodes("table") %>% .[[3]] %>% html_table() %>% select(c(1:(ncol(.)-1)))
        
        dfResults <- rbind(dfResults, dfTemp)
        if(nrow(dfTemp) < 200) break
    }
    return(dfResults)
}

# Download team records data --------------------------------------------------------------------------------------------
dfTeamsRaw <- tibble(Format = names(lCodes$FORMATCODE)) %>% 
    mutate(data = pmap(list(url_template_team, lCodes$FORMATCODE[Format]), getTeamRecords))

# Download team records by year------------------------------------------------------------------------------------------
dfTeamsRaw_byYear <- tibble(Country = names(lCodes$COUNTRYCODE)) %>% 
    expand(Country, Format = names(lCodes$FORMATCODE)) %>% 
    mutate(data = pmap(list(url_template_team_byYear,
                            lCodes$FORMATCODE[Format], 
                            lCodes$COUNTRYCODE[Country]),
                       getTeamRecords))

# Download player records data ------------------------------------------------------------------------------------------
dfPlayersRaw <- tibble(Format = names(lCodes$FORMATCODE)) %>% 
    mutate(data = pmap(list(url_template_player, lCodes$FORMATCODE[Format]), getTeamRecords))

# Download batting data -------------------------------------------------------------------------------------------------
dfBattingRaw <- tibble(Country = names(lCodes$COUNTRYCODE)) %>% 
    expand(Country, Format = names(lCodes$FORMATCODE)) %>% 
    mutate(data = pmap(list(url_template_batting,
                            lCodes$FORMATCODE[Format], 
                            lCodes$COUNTRYCODE[Country]),
                       getBattingBowlingRecords))

# Download batting data with strike rates -------------------------------------------------------------------------------
dfBattingRaw_withSR <- tibble(Country = names(lCodes$COUNTRYCODE),
                              Format = "Tests") %>% 
                        mutate(data = pmap(list(url_template_batting_sr,
                                                lCodes$FORMATCODE[Format], 
                                                lCodes$COUNTRYCODE[Country]),
                                           getBattingBowlingRecords))

# Download bowling data -------------------------------------------------------------------------------------------------
dfBowlingRaw <- tibble(Country = names(lCodes$COUNTRYCODE)) %>% 
    expand(Country, Format = names(lCodes$FORMATCODE)) %>% 
    mutate(data = pmap(list(url_template_bowling,
                            lCodes$FORMATCODE[Format], 
                            lCodes$COUNTRYCODE[Country]),
                       getBattingBowlingRecords))

# Save downloaded data --------------------------------------------------------------------------------------------------
save(dfTeamsRaw, dfTeamsRaw_byYear, dfPlayersRaw, dfBattingRaw, dfBattingRaw_withSR, dfBowlingRaw, file = "cricketRaw.Rda")

#########################################################################################################################
# URL template to search PLAYERCODE by Player as SEARCHTERM: spaces in player name should be replaced by +
url_template_search <- "http://stats.espncricinfo.com/ci/engine/stats/analysis.html?search=SEARCHTERM;template=analysis"

# URL template to get statistics of Player by matches won
url_template_player_wins <- "http://stats.espncricinfo.com/ci/engine/player/PLAYERCODE.html?class=FORMATCODE;result=1;template=results;type=batting"

# Manually download player win records for dfTopBatsmen -----------------------------------------------------------------
getPlayerCode <- function(playerName) {
    playerName <- stringi::stri_replace(playerName, replacement = "", regex = " \\(.*\\)")
    searchTerm <- gsub(" ", "+", playerName, fixed = T)
    
    downloadURL <- sub("SEARCHTERM", searchTerm, url_template_search, fixed = T)
    
    htmlContent <- read_html(downloadURL)
    url_playercode <- htmlContent %>% html_nodes("table") %>% .[[1]] %>% html_nodes("td") %>% .[[3]] %>% 
        html_children() %>% .[[1]] %>% html_attrs() %>% .[1]
    
    playerCode <- stringi::stri_extract_first(url_playercode, regex = "[0-9]+")
    return(playerCode)
}

getPlayerWinRecords <- function(playerCode, formatCode) {
    downloadURL <- sub("PLAYERCODE", playerCode, url_template_player_wins, fixed = T)
    downloadURL <- sub("FORMATCODE", formatCode, downloadURL, fixed = T)
    
    # Put a 3 seconds delay before requesting next set of records
    Sys.sleep(3)
    htmlContent <- read_html(downloadURL)
    dfResults <- htmlContent %>% html_nodes("table") %>% .[[3]] %>% html_table() 
    colnames(dfResults)[1] <- "ResultType"
    dfResults <- dfResults %>% select(-(ncol(.)))
    
    return(dfResults)
}

# Map player code to top players
vTopBatsmen <- unique(as.character(dfTopBatsmen$Player))
names(vTopBatsmen) <- map_chr(vTopBatsmen, getPlayerCode)

# Get player win records
dfTopBatsmenRaw_wins <- dfTopBatsmen %>% 
    select(Format, Country, Player) %>% 
    mutate(PlayerCode = map_chr(Player, function(Z) names(vTopBatsmen[vTopBatsmen == Z]))) %>% 
    mutate(data = pmap(list(PlayerCode, lCodes$FORMATCODE[as.character(Format)]), getPlayerWinRecords))

# Save downloaded data --------------------------------------------------------------------------------------------------
save(vTopBatsmen, dfTopBatsmenRaw_wins, file = "topBatsmenRawWins.Rda")
