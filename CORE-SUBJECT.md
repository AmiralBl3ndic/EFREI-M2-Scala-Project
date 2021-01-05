CORE SUBJECT (for every one to reach 10)

 C.1) For this project you'll write a program parsing 3 CSV files. The files contain data for countries, airports and runway information.

   You will create an sbt scala project with unit test. Feel free to use any scala library/framework as to write your test.

   You must not use «var», «for», «return» keywords (unless you're writing a optional part, in that case it may be allowed if you ask first).

 If you don't chose the database optional task, you're allowed to use mutables collection instead. The sole purpose of the mutable collections must be to replaces a database.
 
C.2.1) You're program will be a command line program that will ask the user for two options - Query or Reports.

 C.2.2 Query Option will ask the user for the country name or code and display the airports & runways at each airport. The input can be country code or country name.

 C.2.3 Choosing Reports will display the following (possibly through a menu):

 • 10 countries with highest number of airports (with count) and countries  with lowest number of airports.

 • Type of runways (as indicated in "surface" column) per country

 • The top 10 most common runway latitude (indicated in "le_ident" column)
