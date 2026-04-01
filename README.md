# bachelor-project

# Will you accept this rose?

Project Summary
Will You Accept This Rose? is an interactive R Shiny web application built to explore data from ABC's long-running reality dating shows, The Bachelor and The Bachelorette. The app combines data visualization, statistical analysis, and a healthy dose of reality TV humor to let users explore contestant demographics, relationship outcomes, and cultural patterns — and even find out their own chances of finding love on the show.
The app features an interactive splash screen, side-by-side show selection, geographic maps of contestant hometowns, a personality survey that predicts your casting odds, a quote-based rose game, deep-dive data analysis, and a cross-cultural analysis of relationship outcomes from international versions of the franchise.
We would like to thank Professor Whitworth for his advice and support throughout our process. We would also like to acknowledge our use of Claude.ai during the coding of this app
Objectives
Map contestant origins — Visualize where Bachelor and Bachelorette contestants come from across the United States, with interactive filtering by state and circle sizes representing contestant counts.
Predict casting likelihood — Allow users to input their age, state, and occupation to calculate how likely they would be to be cast on the show and how far they might go.
Analyze contestant demographics — Explore age distributions and the most common occupations among contestants across both shows.
Measure show success — Compare proposal rates and relationship longevity across seasons of both shows.
Explore cross-cultural patterns — Use data from international versions of the franchise to examine how cultural background relates to relationship outcomes, age patterns, and wedding rates.
Gamify the experience — Let users play as a contestant through the Rose Game, choosing quotes on dates and seeing how their communication style would affect their chances of receiving the final rose.
About the Creators
This app was created as a final project for BIOL-185: Biostatistics at Washington & Lee University.
Caroline Natwick
Caroline is a Senior at Washington & Lee University studying Neuroscience. A self-described Bachelor superfan, The Bachelor is her favorite reality show — which made her the team's resident subject matter expert. In her own quiz results, Caroline achieved a 19% chance of being cast and, in the Rose Game, got the final rose — described by the app as "perfect, amazing, and absolutely destined for love." She is from North Carolina.
Abby Krouse
Abby is a Junior at Washington & Lee University double majoring in Biology and Politics. Her favorite reality show is Secret Lives of Mormon Wives. Abby contributed to the app's data pipeline and analysis features. Her quiz results gave her a 15% chance of being cast, and the Rose Game determined she was "too funny for the final rose" — which tracks. She is from Indiana.
Cecilia Hartford
Cecilia is a Sophomore at Washington & Lee University studying Biology. A Survivor fan at heart, Cecilia brought a competitive analytical edge to the project. Like Abby, her quiz results showed a 15% chance of being cast, and the Rose Game also found her too funny for the final rose. She is from Maryland.

AI Tools Used
Claude (Anthropic) was used throughout the development of this project as a coding assistant. Specific uses included:
Writing and debugging R Shiny server.R and ui.R code
Building the interactive leaflet maps with state filtering and custom popups
Designing and implementing the contestant survey and casting likelihood calculator
Developing the Rose Game logic, including quote classification, scoring, and result generation
Creating the Data Analysis tab with ggplot2 visualizations for age, occupation, and success rate
Building the Culture Analysis tab, including pie charts, a ggplot2 world map with floating annotation boxes and leader lines, and mean age cards
Styling the app with custom CSS including the greyscale map tile filter, Lobster font integration, and the split-screen title layout
Writing and formatting this README

Acknowledgements & Bibliography
Fivethirtyeight. (n.d.). data/bachelorette/bachelorette.csv at master · fivethirtyeight/data. GitHub. https://github.com/fivethirtyeight/data/blob/master/bachelorette/bachelorette.csv
Lenhard, A., Minten, M. P., & Lenhard, W. (2023). When biology takes over: TV formats like The Bachelor and The Bachelorette confirm evolutionary theories of partner selection. Frontiers in psychology, 14, 1219915. https://doi.org/10.3389/fpsyg.2023.1219915
The Bachelor & Bachelorette contestants. (2017, March 8). Kaggle. https://www.kaggle.com/datasets/brianbgonz/the-bachelorette-contestants/data
The Bachelor VS the Bachelorette. (2020, November 14). Kaggle. https://www.kaggle.com/datasets/rachelleperez/the-bachelor-vs-the-bachelorette
Wikipedia contributors. (2026, March 29). The Bachelor (American TV series). Wikipedia. https://en.wikipedia.org/wiki/The_Bachelor_%28American_TV_series%29#Seasons
Wikipedia contributors. (2026, March 29). The Bachelorette (American TV series). Wikipedia. https://en.wikipedia.org/wiki/The_Bachelorette_%28American_TV_series%29

