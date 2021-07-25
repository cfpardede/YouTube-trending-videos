#### Cesar Pardede ####
# Final Project
# Math 533
# 2020-11-19

library(lubridate)
library(chron)
library(e1071)
library(plyr)

#### Analysis of YouTube Data #### 
youtube_CA = read.csv('Fall 2020/Math 533/Final/archive/CAvideos.csv', stringsAsFactors = F)
youtube_DE = read.csv('Fall 2020/Math 533/Final/archive/DEvideos.csv', stringsAsFactors = F)
youtube_FR = read.csv('Fall 2020/Math 533/Final/archive/FRvideos.csv', stringsAsFactors = F)
youtube_GB = read.csv('Fall 2020/Math 533/Final/archive/GBvideos.csv', stringsAsFactors = F)
youtube_IN = read.csv('Fall 2020/Math 533/Final/archive/INvideos.csv', stringsAsFactors = F)
youtube_JP = read.csv('Fall 2020/Math 533/Final/archive/JPvideos.csv', stringsAsFactors = F)
youtube_KR = read.csv('Fall 2020/Math 533/Final/archive/KRvideos.csv', stringsAsFactors = F)
youtube_MX = read.csv('Fall 2020/Math 533/Final/archive/MXvideos.csv', stringsAsFactors = F)
youtube_RU = read.csv('Fall 2020/Math 533/Final/archive/RUvideos.csv', stringsAsFactors = F)
youtube_US = read.csv('Fall 2020/Math 533/Final/archive/USvideos.csv', stringsAsFactors = F)
youtube_list = list(youtube_CA, youtube_DE, youtube_FR, youtube_GB, youtube_JP, youtube_KR, youtube_MX, youtube_RU, youtube_US)

# add region column
youtube_CA['region'] = 'CA'
youtube_DE['region'] = 'DE'
youtube_FR['region'] = 'FR'
youtube_GB['region'] = 'GB'
youtube_IN['region'] = 'IN'
youtube_JP['region'] = 'JP'
youtube_KR['region'] = 'KR'
youtube_MX['region'] = 'MX'
youtube_RU['region'] = 'RU'
youtube_US['region'] = 'US'
regions = c('CA', 'DE', 'FR', 'GB', 'IN', 'JP', 'KR', 'MX', 'RU', 'US')
youtube = rbind(youtube_CA, youtube_DE, youtube_FR, youtube_GB, youtube_IN, youtube_JP, youtube_KR, youtube_MX, youtube_RU, youtube_US)
youtube = data.frame(youtube)
youtube[, 'region'] = as.factor(youtube[, 'region'])

## DATA CLEANING ##

# formatting true and false entries
tf_columns = c("comments_disabled", "ratings_disabled", "video_error_or_removed")
for (columns in tf_columns){
  youtube[, columns] = gsub('FALSE', 'False', youtube[, columns])
  youtube[, columns] = gsub('TRUE', 'True', youtube[, columns])
  youtube[, columns] = as.factor(youtube[, columns])
}

# read category_id as factors
youtube[, 'category_id'] = as.factor(youtube[, 'category_id'])
cat_key = rep(NA, 44)
cat_key[1] = 'Film & Animation'
cat_key[2] = 'Autos & vehicles'
cat_key[10] = 'Music'
cat_key[15] = 'Pets'
cat_key[17] = 'Sports'
cat_key[18] = 'Short Movies'
cat_key[19] = 'Travel & Events'
cat_key[20] = 'Gaming'
cat_key[21] = 'Videoblogging'
cat_key[22] = 'People & Blogs'
cat_key[23] = 'Comedy'
cat_key[24] = 'Entertainments'
cat_key[25] = 'News & Politics'
cat_key[26] = 'Howto & Style'
cat_key[27] = 'Education'
cat_key[28] = 'Science & Technology'
cat_key[29] = 'Nonprofits & Activism'
cat_key[30] = 'Movies'
cat_key[31] = 'Anime/Animation'
cat_key[32] = 'Action/Adventure'
cat_key[33] = 'Classics'
cat_key[34] = 'Comedy'
cat_key[35] = 'Documentary'
cat_key[36] = 'Drama'
cat_key[37] = 'Family'
cat_key[38] = 'Foreign'
cat_key[39] = 'Horror'
cat_key[40] = 'Sci-Fi/Fantasy'
cat_key[41] = 'Thriller'
cat_key[42] = 'Shorts'
cat_key[43] = 'Shows'
cat_key[44] = 'Trailers'

# extract publish date, edit publish time
publish_raw = ymd_hms(youtube[, "publish_time"])
youtube[, 'publish_date'] = as.Date(publish_raw)
youtube[, 'publish_time'] = times(gsub(".* ", '', publish_raw))

# format trending date and video id
youtube[, "trending_date"] = as.Date(ydm(youtube[, "trending_date"]))
youtube[, "video_id"] = as.character(youtube[, "video_id"])

# drop unneeded columns, rearrange columns
youtube = subset(youtube, select = -c(thumbnail_link)) # keep video_id to make querying videos easier
reorder_columns = c("trending_date", "video_id", "title", "channel_title", "category_id", "publish_date", "publish_time", "tags", 
                    "views", "likes", "dislikes", "comment_count", "comments_disabled", "ratings_disabled", "video_error_or_removed", 
                    "region","description")
youtube = youtube[, reorder_columns]

#### EXPLORATORY DATA ANALYSIS (EDA) ####

# get quick summaries
summary(youtube)
i = 1
summaries = list()
for(r in regions){
  summaries[[i]] = summary(youtube[which(youtube['region'] == r), ])
  i = i + 1
}
summaries

# most viewed, liked, disliked, and commented vidoes from each region
i = 1
region_yt_max = list()
for (r in regions){
  yt = youtube[youtube[, 'region'] == r, ]
  region_max_index = apply(yt[, c('views', 'likes', 'dislikes', 'comment_count')], 2, which.max)
  region_max_title = yt[region_max_index, 'title']
  region_max_channel = yt[region_max_index, 'channel_title']
  region_max_count = apply(yt[, c('views', 'likes', 'dislikes', 'comment_count')], 2, max)
  region_yt_max[[i]] = list(region = r, 
                            'most viewed' = c(
                              'title' = region_max_title[1], 'channel' = region_max_channel[1], region_max_count[1]),
                            'most liked' = c(
                              'title' = region_max_title[2], 'channel' = region_max_channel[2], region_max_count[2]),
                            'most disliked' = c(
                              'title' = region_max_title[3], 'channel' = region_max_channel[3], region_max_count[3]),
                            'most commented' = c(
                              'title' = region_max_title[4], 'channel' = region_max_channel[4], region_max_count[4]))
  i = i + 1
}
region_yt_max

# most viewed, liked, disliked, and commented vidoes globally
max_index = apply(youtube[, c('views', 'likes', 'dislikes', 'comment_count')], 2, which.max)
max_title = youtube[max_index, 'title']
max_channel = youtube[max_index, 'channel_title']
max_count = apply(youtube[, c('views', 'likes', 'dislikes', 'comment_count')], 2, max)
yt_max = list(region = 'Global',
              'most viewed' = c(
                'title' = max_title[1], 'channel' = max_channel[1], max_count[1]),
              'most liked' = c(
                'title' = max_title[2], 'channel' = max_channel[2], max_count[2]),
              'most disliked' = c(
                'title' = max_title[3], 'channel' = max_channel[3], max_count[3]),
              'most commented' = c(
                'title' = max_title[4], 'channel' = max_channel[4], max_count[4]))
yt_max

# many metrics are approximately lognormal? what does it mean?
# par(mfrow = c(2, 2))
# hist(youtube[, "views"], main = 'Views', xlab = 'Views')
# hist(youtube[, "likes"], main = 'Likes', xlab = 'Likes')
# hist(youtube[, "dislikes"], main = 'Dislikes', xlab = 'Dislikes')
# hist(youtube[, "comment_count"], main = 'Comments', xlab = 'Comments')
# 
# hist(log(youtube[, "views"]), main = 'Views', xlab = 'Views')
# hist(log(youtube[, "likes"]), main = 'Likes', xlab = 'Likes')
# hist(log(youtube[, "dislikes"]), main = 'Dislikes', xlab = 'Dislikes')
# hist(log(youtube[, "comment_count"]), main = 'Comments', xlab = 'Comments')
# par(mfrow = c(1,1))

# trending quantiles
trending_CI = apply(youtube[, c('views', 'likes', 'dislikes', 'comment_count')], 2, quantile, probs = c(0, 0.50, 0.95))
trending_CI
views_CI = quantile(youtube[, 'views'], probs = c(0, 0.50, 0.95))
views_CI

# how many days to trend after publishing?
days_to_trend = as.numeric(youtube[, "trending_date"] - youtube[, "publish_date"])
days_CI = quantile(days_to_trend, probs = c(0, 0.50, 0.95))
whazzup = list(video = youtube[which.max(days_to_trend), ],
               days = max(days_to_trend))
days_CI

# the whazzup video resurfaced after 4215 days to trend for 1 day on 05 February 
# in the US - corresponding with the Philadelphia Eagles win over the New England 
# Patriots in Super Bowl LII the day before. I suspect this is a cyclical occurrence.
dim(youtube[youtube['title'] == 'Budweiser - Original Whazzup? ad', ])[1]

## MOST POPULAR VIDEOS PER CATEGORY PER REGION??? ##
cat_max = list()
i = 1
for (r in regions){
  yt = youtube[which(youtube['region'] == r), ]
  for (category in which(!is.na(cat_key))){
    yt_cat = yt[which(yt['category_id'] == category), ]
    max_view_cat = which.max(yt_cat[, 'views'])
    cat_max[[i]] = list('region' = r,
                        'category' = cat_key[category],
                        'category id' = category,
                        'video id' = yt[max_view_cat, "video_id"],
                        yt_cat[max_view_cat, c('title', 'channel_title', 'views', 'likes', 'dislikes', 'comment_count')])
    i = i + 1
  }
}
# cat_max

trending_counts = list()
i = 1
for (r in regions){
  yt = youtube[which(youtube['region'] == r), ]

  counts_title = count(yt, c('title'))
  counts_by_id = count(yt[which(yt['video_id'] != '#NAME?'), ], c('video_id'))
  max_counts_title = as.character(counts_title[which.max(counts_title[,'freq']), 'title'])    
  max_counts_id = counts_by_id[which.max(counts_by_id[,'freq']), 'video_id']
  
  trending_title = yt[which(yt[, 'title'] == max_counts_title), c("trending_date", "video_id", "title", "channel_title", "publish_date", 
                                                 "views", "likes", "dislikes", "comment_count")]
  trending_by_id = yt[which(yt[, 'video_id'] == max_counts_id), c("trending_date", "video_id", "title", "channel_title", "publish_date", 
                                                 "views", "likes", "dislikes", "comment_count")]
  trending_counts[[i]] = list('region' = r,
                      'most trending by title' = trending_title[1, ], '# trending by title' = dim(trending_title)[1],
                      'channel names' = unique(trending_title[, "channel_title"]),
                      'most trending by ID' = trending_by_id[1, ], '# trending by ID ' = dim(trending_by_id)[1]
                      )
  i = i + 1
}
trending_counts

global_trending_counts = list()
counts_title = count(youtube, c('title'))
counts_by_id = count(youtube[which(youtube['video_id'] != '#NAME?'), ], c('video_id'))
max_counts_title = as.character(counts_title[which.max(counts_title[,'freq']), 'title'])
max_counts_id = as.character(counts_by_id[which.max(counts_by_id[,'freq']), 'video_id'])

trending_title = youtube[which(youtube[, 'title'] == max_counts_title), c("trending_date", "video_id", "title", "channel_title", "publish_date",
                                                                "views", "likes", "dislikes", "comment_count", 'region')]
trending_by_id = youtube[which(youtube[, 'video_id'] == max_counts_id), c("trending_date", "video_id", "title", "channel_title", "publish_date",
                                                                "views", "likes", "dislikes", "comment_count", 'region')]
global_trending_counts = list('most trending by title' = trending_title[1, ], '# trending by title' = dim(trending_title)[1],
                              'channel names' = unique(trending_title[, "channel_title"]),
                              'most trending by ID' = trending_by_id[1, ], '# trending by ID ' = dim(trending_by_id)[1])
global_trending_counts


# par(mfrow = c(3, 1))
par(mfrow = c(1, 1))
hist(youtube[,'views'], main = 'Views', xlab = 'Views')
hist(youtube[,'likes'], main = 'Likes', xlab = 'Likes')
hist(youtube[,'dislikes'], main = 'Dislikes', xlab = 'Dislikes')
hist(youtube[,'comment_count'], main = 'Comments', xlab = 'Comments')

hist(log(youtube[,'views']), main = 'Views', xlab = 'Views')
hist(log(youtube[,'likes']), main = 'Likes', xlab = 'Likes')
hist(log(youtube[,'dislikes']), main = 'Dislikes', xlab = 'Dislikes')
hist(log(youtube[,'comment_count']), main = 'Comments', xlab = 'Comments')
par(mfrow = c(1, 1))

plot(youtube[,'views'], youtube[,'likes'], main = 'Views vs Likes', xlab = 'Views', ylab = 'Likes')
plot(youtube[,'views'], youtube[,'dislikes'], main = 'Views vs Dislikes', xlab = 'Views', ylab = 'Dislikes')
plot(youtube[,'likes'], youtube[,'dislikes'], main = 'Likes vs Dislikes', xlab = 'Likes', ylab = 'Dislikes')

plot(log(youtube[,'views']), log(youtube[,'likes']), main = 'Views vs Likes', xlab = 'Views', ylab = 'Likes')
plot(log(youtube[,'views']), log(youtube[,'dislikes']), main = 'Views vs Dislikes', xlab = 'Views', ylab = 'Dislikes')
plot(log(youtube[,'likes']), log(youtube[,'dislikes']), main = 'Likes vs Dislikes', xlab = 'Likes', ylab = 'Dislikes')

# top 3 dislikes/likes
youtube_dislikes = youtube[order(youtube[,'dislikes'], decreasing = T),]
unique(youtube_dislikes[, c('title', "channel_title")])[1:3, ]

youtube_likes = youtube[order(youtube[,'likes'], decreasing = T),]
unique(youtube_likes[, c('title', "channel_title")])[1:3, ]

youtube[which(youtube[,'likes'] == sort(youtube[,'likes'], decreasing = T)[1]), ]
youtube[which(youtube[,'likes'] == sort(youtube[,'likes'], decreasing = T)[2]), ]

#### STATISTICAL LEARNING AND MODELING ####
n = dim(youtube)[1]
library(ggfortify)
pca_youtube = youtube[, c("views", "likes", "dislikes", "comment_count")]
pca = prcomp(pca_youtube, scale. = T)
autoplot(pca, data = youtube, colour = 'region', loadings = TRUE, loadings.colour = 'blue', 
         loadings.label = TRUE, loadings.label.size = 3, main = 'All Regions')
pca
# autoplot(pcl, data = pca_logtube) # fails because pcl fails

# for some reason looping through region names doesn't display plots. manually do pca for all regions.
r = 'CA'
youtube_region = youtube[which(youtube['region'] == r), ]
pca_youtube = youtube_region[, c("views", "likes", "dislikes", "comment_count")]
pca = prcomp(pca_youtube, scale. = T)
pca
autoplot(pca, data = youtube_region, colour = 'grey', loadings = TRUE, 
         loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3,
         main = r)
r = 'DE'
youtube_region = youtube[which(youtube['region'] == r), ]
pca_youtube = youtube_region[, c("views", "likes", "dislikes", "comment_count")]
pca = prcomp(pca_youtube, scale. = T)
pca
autoplot(pca, data = youtube_region, colour = 'grey', loadings = TRUE, 
         loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3,
         main = r)
r = 'FR'
youtube_region = youtube[which(youtube['region'] == r), ]
pca_youtube = youtube_region[, c("views", "likes", "dislikes", "comment_count")]
pca = prcomp(pca_youtube, scale. = T)
pca
autoplot(pca, data = youtube_region, colour = 'grey', loadings = TRUE, 
         loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3,
         main = r)
r = 'GB'
youtube_region = youtube[which(youtube['region'] == r), ]
pca_youtube = youtube_region[, c("views", "likes", "dislikes", "comment_count")]
pca = prcomp(pca_youtube, scale. = T)
pca
autoplot(pca, data = youtube_region, colour = 'grey', loadings = TRUE, 
         loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3,
         main = r)
r = 'IN'
youtube_region = youtube[which(youtube['region'] == r), ]
pca_youtube = youtube_region[, c("views", "likes", "dislikes", "comment_count")]
pca = prcomp(pca_youtube, scale. = T)
pca
autoplot(pca, data = youtube_region, colour = 'grey', loadings = TRUE, 
         loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3,
         main = r)
r = 'JP'
youtube_region = youtube[which(youtube['region'] == r), ]
pca_youtube = youtube_region[, c("views", "likes", "dislikes", "comment_count")]
pca = prcomp(pca_youtube, scale. = T)
pca
autoplot(pca, data = youtube_region, colour = 'grey', loadings = TRUE, 
         loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3,
         main = r)
r = 'KR'
youtube_region = youtube[which(youtube['region'] == r), ]
pca_youtube = youtube_region[, c("views", "likes", "dislikes", "comment_count")]
pca = prcomp(pca_youtube, scale. = T)
pca
autoplot(pca, data = youtube_region, colour = 'grey', loadings = TRUE, 
         loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3,
         main = r)
r = 'MX'
youtube_region = youtube[which(youtube['region'] == r), ]
pca_youtube = youtube_region[, c("views", "likes", "dislikes", "comment_count")]
pca = prcomp(pca_youtube, scale. = T)
pca
autoplot(pca, data = youtube_region, colour = 'grey', loadings = TRUE, 
         loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3,
         main = r)
r = 'RU'
youtube_region = youtube[which(youtube['region'] == r), ]
pca_youtube = youtube_region[, c("views", "likes", "dislikes", "comment_count")]
pca = prcomp(pca_youtube, scale. = T)
pca
autoplot(pca, data = youtube_region, colour = 'grey', loadings = TRUE, 
         loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3,
         main = r)
r = 'US'
youtube_region = youtube[which(youtube['region'] == r), ]
pca_youtube = youtube_region[, c("views", "likes", "dislikes", "comment_count")]
pca = prcomp(pca_youtube, scale. = T)
pca
autoplot(pca, data = youtube_region, colour = 'grey', loadings = TRUE, 
         loadings.colour = 'blue', loadings.label = TRUE, loadings.label.size = 3,
         main = r)
