# CS-424-Project1-Chicago-L-Subway
Interactive dashboard showcasing ridership data for all stations in the Chicago subway system (CTA)
<img width="648" alt="webImage" src="https://user-images.githubusercontent.com/55251811/171336613-9d674a4d-266f-4474-8002-38ce5d8db29c.png">

The Chicago Transit Authority (CTA) records the number of daily entries at each turnstiles for each ‘L’ station. The recorded data for the last 20 years is available in the Chicago Data Portal website, https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f.
The data set contains records of total entries for every ‘L’ station for each day from January 1st, 2001 to November 30th, 2021. Each record contains the following features: station ID, station name, date of entry, day type, and total ridership.

Using the R programming language, data records for UIC-Halsted station, O’Hare station and Racine station were isolated and grouped by year, month and day of the week. Bar charts and tables are available as an interactive website using the Shiny R package and services that host standalone applications.

In this interactive website, I divided the page into a top and bottom region. Each region contains four plots dedicated to one station that the user can choose from the three available. The first plot shows total ridership for each year from 2001 to 2021. The second bar chart displays the daily ridership for a specific year chosen by the user. The third bar chart shows total ridership by month for a specific year chosen by the user. The fourth bar chart displays the ridership by the day of the week. Each region has its independent station and year controls that make it easy to compare ridership at different stations and/or year. In addition to the bar charts, the data used to make the plot is also available as a table format.

