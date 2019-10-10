library(readxl)
library(dplyr)
library(reshape2)
library(readr)
library(ggplot2)

###############################################################################
# Standardize country names. When possible, names should match the Natural
# Earth shapefiles so that I can make maps as needed.
#   fix_adm0() assumes that it's getting a data frame with country names stored
#   in a column called 'country'.
###############################################################################
fix_adm0 <- function(df) {
  rename_df <- data_frame(country=c('Bahamas, The','Bahamas','Burma/Myanmar','Burma','Cape Verde',
                                    'Congo, Democratic Republic of the Congo',
                                    'Democratic Republic of Congo',
                                    'Dem. Rep. Congo',
                                    'Congo, Dem. Rep.',
                                    'Congo, Democratic Republic',
                                    'Congo, Republic of','Congo, Rep.','Congo',
                                    'Côte d\'Ivoire','Cote d\'Ivoire',
                                    'Democratic Republic of Vietnam','Viet Nam',
                                    'Egypt, Arab Rep.','Gambia, The','The Gambia','Hong Kong SAR',
                                    'Hong Kong SAR, China','Iran, Islamic Rep.',
                                    'Korea, Dem. People’s Rep.','Korea, North','Korea, Rep.','Korea, South',
                                    'Lao PDR','Lao P.D.R.','Macao SAR, China',
                                    'Macedonia, FYR','Micronesia, Fed. Sts.',
                                    'Russian Federation','São Tomé and Príncipe',
                                    'Syrian Arab Republic','United States','Venezuela, RB',
                                    'Yemen, Rep.','Kyrgyz Republic','St. Vincent and The Grenadines',
                                    'Slovak Republic','Bolivia (Plurinational State of)',
                                    'Bosnia Herzegovina','China, Taiwan Province of',
                                    'Micronesia','Indonesia (...2002)',
                                    'Iran (Islamic Republic of)','Korea, Dem. People\'s Rep. of',
                                    'Korea, Republic of','Lao People\'s Dem. Rep.',
                                    'Libyan Arab Jamahiriya','Micronesia (Federated States of)',
                                    'Republic of Korea','Republic of Moldova','Sudan (...2011)',
                                    'Taiwan, China','TFYR of Macedonia','The former Yugoslav Rep. of Macedonia',
                                    'United Rep. of Tanzania','Tanzania','United States Virgin Islands',
                                    'Venezuela (Bolivarian Rep. of)','China, Hong Kong SAR',
                                    'China, Macao SAR','Dem. Rep. of the Congo','Timor-Leste','Kazakstan',
                                    'Palestinian Territory, Occupied','State of Palestine','Palestine/Gaza','Palestine/West Bank',
                                    'Brunei Darussalam','Swaziland','Serbia','Sao Tome and Principe',
                                    'Czech Republic','Bosnia','Central AfR',
                                    'Congo, Democratic Republic of','DominicanRep',
                                    'Unitd Kingdm','GuineaBiss','Equa Guinea','Kyrgyz',
                                    'Korea North','Korea South','UAE','USA','SierraLeo',
                                    'Palestine','Sudan South','Papua NG','Slovak Rep',
                                    'Trinidad','Central African Rep.','Congo (Rep. of the)',
                                    'Democratic People\'s Republic of Korea','Dem. People\'s Rep. of Korea',
                                    'Dominican Rep.','Korea (Rep. of)','Lao People\'s Democratic Republic',
                                    'Nepal (Republic of)','Taiwan, Province of China','North Macedonia',
                                    'The Republic of North Macedonia','Hong Kong, China','Macao, China',
                                    'Eswatini','Lao People’s Democratic Republic','The Democratic Republic Of The Congo',
                                    'Islamic Republic of Iran','United Kingdom of Great Britain and Northern Ireland',
                                    "China, People's Republic of","Egypt, Arab Republic of",'PNG',"Yemen, Republic of",
                                    'Hong Kong, SAR China',"Congo Brazzaville","D.R. Congo","Congo, DR",
                                    "Venezuela (Bolivarian Republic Of)","DRC",
                                    "Hong Kong","Afghanistan, Islamic Republic of","Armenia, Republic of","Azerbaijan, Republic of",
                                    "China, P.R.: Hong Kong","China, P.R.: Macao","China, P.R.: Mainland",
                                    "Eswatini, Kingdom of","Iran, Islamic Republic of","Kosovo, Republic of",
                                    "Marshall Islands, Republic of","Micronesia, Federated States of",
                                    "North Macedonia, Republic of","Serbia, Republic of","Timor-Leste, Dem. Rep. of",
                                    "Venezuela, Republica Bolivariana de","United Republic Of Tanzania"),
                          country_new=c('The Bahamas','The Bahamas','Myanmar','Myanmar','Cabo Verde',
                                        'Democratic Republic of the Congo',
                                        'Democratic Republic of the Congo',
                                        'Democratic Republic of the Congo',
                                        'Democratic Republic of the Congo',
                                        'Democratic Republic of the Congo',
                                        'Republic of the Congo','Republic of the Congo','Republic of the Congo',
                                        'Ivory Coast','Ivory Coast',
                                        'Vietnam','Vietnam',
                                        'Egypt','Gambia','Gambia','Hong Kong',
                                        'Hong Kong','Iran',
                                        'North Korea','North Korea','South Korea','South Korea',
                                        'Laos','Laos','Macau',
                                        'Macedonia','Federated States of Micronesia',
                                        'Russia','Sao Tome and Principe',
                                        'Syria','United States of America','Venezuela',
                                        'Yemen','Kyrgyzstan','St. Vincent and the Grenadines',
                                        'Slovakia','Bolivia',
                                        'Bosnia and Herzegovina','Taiwan',
                                        'Federated States of Micronesia','Indonesia',
                                        'Iran','North Korea',
                                        'South Korea','Laos',
                                        'Libya','Federated States of Micronesia',
                                        'South Korea','Moldova','Sudan',
                                        'Taiwan','Macedonia','Macedonia',
                                        'United Republic of Tanzania','United Republic of Tanzania','Virgin Islands (U.S.)',
                                        'Venezuela','Hong Kong','Macau','Democratic Republic of the Congo','East Timor','Kazakhstan',
                                        'West Bank and Gaza','West Bank and Gaza','West Bank and Gaza','West Bank and Gaza',
                                        'Brunei','eSwatini','Republic of Serbia','São Tomé and Principe',
                                        'Czechia','Bosnia and Herzegovina','Central African Republic',
                                        'Democratic Republic of the Congo','Dominican Republic',
                                        'United Kingdom','Guinea-Bissau','Equatorial Guinea',
                                        'Kyrgyzstan','North Korea','South Korea','United Arab Emirates',
                                        'United States of America','Sierra Leone','West Bank and Gaza',
                                        'South Sudan','Papua New Guinea','Slovakia',
                                        'Trinidad and Tobago','Central African Republic','Republic of the Congo',
                                        'North Korea','North Korea','Dominican Republic','South Korea',
                                        'Laos','Nepal','Taiwan','Macedonia','Macedonia','Hong Kong','Macao',
                                        'eSwatini','Laos','Democratic Republic of the Congo','Iran',
                                        'United Kingdom','China','Egypt','Papua New Guinea','Yemen',
                                        'Hong Kong',"Republic of the Congo","Democratic Republic of the Congo",
                                        "Democratic Republic of the Congo","Venezuela","Democratic Republic of the Congo",
                                        "Hong Kong","Afghanistan","Armenia","Azerbaijan",
                                        "Hong Kong","Macao","China","eSwatini","Iran","Kosovo",
                                        "Marshall Islands","Federated States of Micronesia",
                                        "Macedonia","Republic of Serbia","East Timor","Venezuela",
                                        "United Republic of Tanzania"))
                                       
  tmp <- df %>%
    mutate(country=sub('Saint\\.','Saint',country),
           country=sub('St\\.','Saint',country)) %>%
    left_join(rename_df,by='country') %>%
    mutate(country = ifelse(is.na(country_new),country,country_new)) %>%
    select(-country_new)
  # So hard to get Cote d'Ivoire right
  tmp$country[grep('voire',tmp$country)] <- 'Ivory Coast'
  tmp
}

###############################################################################
# Plot world map
###############################################################################
library(rgdal)
library(maptools)
library(ggplot2)
library(dplyr)

load_world_shp <- function() {
  world = readOGR(dsn="~/Projects/GIS/Natural_Earth_quick_start/50m_cultural", 
                  layer="ne_50m_admin_0_countries")
  world@data$id = rownames(world@data)
  world.points = fortify(world, region="id")
  plyr::join(world.points, world@data, by="id") %>%
    mutate(SOVEREIGNT=as.character(SOVEREIGNT))
}

world_map <- function(d,title) {
  world.df <- load_world_shp()
  plot <- d %>%
    rename(SOVEREIGNT=country) %>%
    right_join(world.df,by='SOVEREIGNT') %>%
    ggplot(aes(long,lat,group=group,fill=value)) + 
      geom_polygon() +
      geom_path(color='#6C6463',size=0.1) +
      coord_equal() +
      #scale_fill_gradient(low='#FFFFFF',high='#0067B9',na.value='#CFCDC9') +
      theme_void() +
      theme(legend.title=element_blank(),
            axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank()) +
      ggtitle(title) +
      theme(text=element_text(family="Liberation Sans"))
  plot
}

###############################################################################
# Country lists
###############################################################################
world.df <- load_world_shp()
all_countries <- unique(world.df$SOVEREIGNT)
rm(world.df)

usaid_countries <- c('Afghanistan','Albania','Angola','Armenia','Azerbaijan',
                     'Bangladesh','Belarus','Benin','Bosnia and Herzegovina',
                     'Botswana','Brazil','Burkina Faso','Myanmar','Burundi',
                     'Cambodia','Cameroon','Central African Republic','Chad',
                     'China','Colombia','Ivory Coast','Cuba','Cyprus',
                     'Democratic Republic of the Congo','Djibouti',
                     'Dominican Republic','Ecuador','Egypt',
                     'El Salvador','eSwatini','Ethiopia','Georgia','Ghana',
                     'Guatemala','Guinea',
                     'Guyana','Haiti','Honduras','India','Indonesia','Iraq',
                     'Jamaica','Jordan','Kazakhstan',
                     'Kenya','Kosovo','Kyrgyzstan','Laos','Lebanon','Lesotho',
                     'Liberia','Libya','Macedonia','Madagascar','Malawi','Maldives',
                     'Mali','Mauritania','Mexico','Moldova','Mongolia','Montenegro',
                     'Morocco','Mozambique','Namibia','Nepal','Nicaragua','Niger',
                     'Nigeria','Pakistan','Panama','Paraguay','Peru','Philippines',
                     'Republic of the Congo','Rwanda','Senegal','Republic of Serbia','Sierra Leone',
                     'Somalia','South Africa','South Sudan','Sri Lanka','Sudan','Syria',
                     'Tajikistan','United Republic of Tanzania','Thailand','East Timor','Tunisia','Turkmenistan',
                     'Uganda','Ukraine','Uzbekistan','Venezuela','Vietnam',
                     'West Bank and Gaza','Yemen','Zambia','Zimbabwe')

non_usaid_countries <- setdiff(all_countries,usaid_countries)

# USAID bureau groupings
africa <- c('Angola','Benin','Botswana','Burkina Faso','Burundi','Cameroon',
            'Central African Republic','Chad','Ivory Coast',
            'Democratic Republic of the Congo','Djibouti','eSwatini','Ethiopia',
            'Ghana','Guinea','Kenya','Lesotho','Liberia','Madagascar','Malawi',
            'Mali','Mauritania','Mozambique','Namibia','Niger','Nigeria',
            'Republic of the Congo','Rwanda','Senegal','Sierra Leone','Somalia',
            'South Africa','South Sudan','Sudan','United Republic of Tanzania',
            'Uganda','Zambia','Zimbabwe')
asia <- c('Bangladesh','Myanmar','Cambodia','China','India','Indonesia',
          'Kazakhstan','Kyrgyzstan','Laos','Maldives','Nepal','Philippines',
          'Sri Lanka','Tajikistan','Thailand','East Timor','Turkmenistan',
          'Uzbekistan','Vietnam','Afghanistan','Pakistan','Mongolia')
e_and_e <- c('Albania','Armenia','Azerbaijan','Belarus','Bosnia and Herzegovina',
             'Cyprus','Georgia','Kosovo','Macedonia','Moldova','Montenegro',
             'Russia','Republic of Serbia','Ukraine')
lac <- c('Bolivia','Brazil','Colombia','Cuba','Dominican Republic','Ecuador',
         'El Salvador','Guatemala','Haiti','Honduras','Jamaica','Mexico',
         'Nicaragua','Panama','Paraguay','Peru','Venezuela','Guyana')
me <- c('Egypt','Iraq','Jordan','Lebanon','Libya','Morocco','Syria',
        'Tunisia','West Bank and Gaza','Yemen')

# WB lending groups
low_income <- c('Afghanistan','Guinea-Bissau','Sierra Leone','Benin','Haiti',
                'Somalia','Burkina Faso','North Korea','South Sudan','Burundi',	
                'Liberia','Syria','Central African Republic','Madagascar','Tajikistan',
                'Chad','Malawi','United Republic of Tanzania','Comoros','Mali',
                'Togo','Democratic Republic of the Congo','Mozambique','Uganda',
                'Eritrea','Nepal','Yemen','Ethiopia','Niger','Zimbabwe','Gambia',
                'Rwanda','Guinea','Senegal')
lmic <- c('Angola','Indonesia','Papua New Guinea','Bangladesh','Kenya','Philippines',
          'Bhutan','Kiribati','São Tomé and Principe','Bolivia','Kosovo','Solomon Islands',
          'Cabo Verde','Kyrgyzstan','Sri Lanka','Cambodia','Laos','Sudan',
          'Cameroon','Lesotho','eSwatini','Republic of the Congo','Mauritania',
          'East Timor','Ivory Coast','Federated States of Micronesia','Tunisia','Djibouti','Moldova',
          'Ukraine','Egypt','Mongolia','Uzbekistan','El Salvador','Morocco','Vanuatu',
          'Georgia','Myanmar','Vietnam','Ghana','Nicaragua','West Bank and Gaza',
          'Honduras','Nigeria','Zambia','India','Pakistan')
umic <- c('Albania','Fiji','Namibia','Algeria','Gabon','Nauru','American Samoa',
          'Grenada','Paraguay','Armenia','Guatemala','Peru','Azerbaijan',
          'Guyana','Romania','Belarus','Iran','Russia','Belize','Iraq','Samoa',
          'Bosnia and Herzegovina','Jamaica','Republic of Serbia','Botswana',
          'Jordan','South Africa','Brazil','Kazakhstan','Saint Lucia','Bulgaria',
          'Lebanon','Saint Vincent and the Grenadines','China','Libya','Suriname',
          'Colombia','Macedonia','Thailand','Costa Rica','Malaysia','Tonga',
          'Cuba','Maldives','Turkey','Dominica','Marshall Islands','Turkmenistan',
          'Dominican Republic','Mauritius','Tuvalu','Equatorial Guinea','Mexico',
          'Venezuela','Ecuador','Montenegro')
high_income <- c('Andorra','Germany','Oman','Antigua and Barbuda','Gibraltar','Palau',
                 'Argentina','Greece','Panama','Aruba','Greenland','Poland','Australia',
                 'Guam','Portugal','Austria','Hong Kong','Puerto Rico','The Bahamas','Hungary',
                 'Qatar','Bahrain','Iceland','San Marino','Barbados','Ireland','Saudi Arabia',
                 'Belgium','Isle of Man','Seychelles','Bermuda','Israel','Singapore',
                 'British Virgin Islands','Italy','Sint Maarten (Dutch part)',
                 'Brunei','Japan','Slovakia','Canada','South Korea',
                 'Slovenia','Cayman Islands','Kuwait','Spain','Channel Islands','Latvia',
                 'Saint Kitts and Nevis','Chile','Liechtenstein','St. Martin (French part)',
                 'Croatia','Lithuania','Sweden','Curaçao','Luxembourg','Switzerland',
                 'Cyprus','Macao','Taiwan','Czechia','Malta','Trinidad and Tobago','Denmark',
                 'Monaco','Turks and Caicos Islands','Estonia','Netherlands',
                 'United Arab Emirates','Faroe Islands','New Caledonia','United Kingdom',
                 'Finland','New Zealand','United States of America','France',
                 'Northern Mariana Islands','Uruguay','French Polynesia','Norway',
                 'Virgin Islands (U.S.)')
setdiff(all_countries,c(low_income,lmic,umic,high_income)) # only some debatable countries

# why do I need a list of non-countries? Because they are in a lot of WB datasets.
non_countries <- c("Arab world","Developing","East Asia & Pacific",
                   "East Asia & Pacific (excluding high income)","Euro area",
                   "Europe & Central Asia","Europe & Central Asia (excluding high income)",
                   "High income","High income: OECD","Latin America & Caribbean",
                   "Latin America & Caribbean (excluding high income)",
                   "Lower middle income","Low income","Middle East & North Africa",
                   "Middle East & North Africa (excluding high income)",
                   "Middle income","North America","South Asia","Sub-Saharan Africa",
                   "Sub-Saharan Africa (excluding high income)","Upper middle income",
                   "World",'BCEAO',"Eastern Caribbean","Eastern Caribbean Currency Union")

###############################################################################
# Plotting functions
###############################################################################
approved_colors <-c(
  '#002F6C', # USAID blue
  '#BA0C2F',  # USAID red
  '#6C6463',   # dark gray
  '#A7C6ED', # light blue
  '#651D32', # dark red
  '#CFCDC9' # light gray
) 

colors_USAID <- scale_color_manual(values=approved_colors)
fill_USAID <- scale_fill_manual(values=approved_colors)

theme_USAID <- theme( 
  axis.ticks = element_blank(), 
  axis.ticks.length = unit(0, units = "points"), 
  
  strip.text = element_blank(),
  strip.background = element_blank(),
  
  panel.border = element_blank(), 
  panel.grid = element_blank(), 
  panel.background = element_blank(), 
  plot.background = element_blank(),
  legend.key = element_blank(),
  text=element_text(family="Liberation Sans")) 



