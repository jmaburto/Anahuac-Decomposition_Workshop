# Anahuac 2023: Course on decomposition techniques

Start: 22/05/2023
End: 215/05/2023

Classes will start at 3.00pm (CDMX time)

Instructor: José Manuel Aburto
## Course description

The course will present essential methods for demographic analysis with a focus on decomposition techniques. The students will improve their skills in applying these methods in R. 

## Schedule (updated daily)
### Day 1
Origins of Decomposition

Decomposition 1 (Direct vs. compositional)

Kitagawa (1955)

Survival function decomposition

### Day 2
Quick review

Prevalence based lifetables

Decomposition of life expectancy

### Day 3
Quick review

Cause-deleted lifetables

Lifespan variation

Linear integral decomposition

Stepwise decomposition method

### Day 4
Quick review

Competing risks review

Spread-Allocation-Timing decomposition

Decomposing healthy life expectancy

Formal Demography

### Challenge 1
Show that (1) can be expressed as (3) (step by step) (slides day 1)

### Challenge 2
With data on fertility (e.g. HFD or WPP) select 5 countries and analyze the change in their crude fertility rate (CFR) in a recent period (10 years) and decompose these changes following Kitagawa’s decomposition and describe your results.

Then for the most recent period select the two countries (among the 5) with the highest and lowest CFR and decompose their diﬀerence and describe your results.

### Challenge 3
Get (any source) age-specific prevalences and apply the Sullivan method in R. 

### Challenge 4
Use the stepwise decompose the change in the standard deviation of the age-at-death distribution and life expectancy by age and cause of death for 2 countries you might be interested in (over time or between them). Interpret the results of life expectancy alongside standard deviation. Make it interesting. You can use data from HCoD, HMD, WHO. 

### Challenge 5
Using one of the countries in challenge 4, use the septwise decomposition method to decompose life expectancy over time. Do the results vary of you decompose time1 - time2 vs time2 - time1? If you start the replacement from the oldest age to the youngest, are they different? if you randomise them?

## Readings
#### Day 1
Kitagawa, Evelyn M. ‘Components of a Difference Between Two Rates’. Journal of the American Statistical Association 50, no. 272 (1955): 1168–94. https://doi.org/10.2307/2281213.
 
Vaupel, James W., and Vladimir Canudas-Romo. ‘Decomposing Demographic Change into Direct vs. Compositional Components’. Demographic Research 7, no. 1 (2 July 2002): 1–14. https://doi.org/10.4054/DemRes.2002.7.1.
 
Vaupel, James W., and Vladimir Canudas-Romo. ‘Decomposing Change in Life Expectancy: A Bouquet of Formulas in Honor of Nathan Keyfitz’s 90th Birthday’. Demography 40, no. 2 (2003): 201–16. https://doi.org/10.1353/dem.2003.0018.

Berrington, Ann, Juliet Stone, and Eva Beaujouan. ‘Educational Differences in Timing and Quantum of Childbearing in Britain: A Study of Cohorts Born 1940−1969’. Demographic Research 33, no. 26 (6 October 2015): 733–64. https://doi.org/10.4054/DemRes.2015.33.26.

#### Day 2

Sullivan, D. F. ‘A Single Index of Mortality and Morbidity.’ HSMHA Health Reports 86, no. 4 (April 1971): 347–54.

Arriaga, Eduardo E. ‘Measuring and Explaining the Change in Life Expectancies’. Demography 21, no. 1 (1984): 83–96. https://doi.org/10.2307/2061029.

Yang, Yang. ‘Long and Happy Living: Trends and Patterns of Happy Life Expectancy in the U.S., 1970–2000’. Social Science Research 37, no. 4 (1 December 2008): 1235–52. https://doi.org/10.1016/j.ssresearch.2007.07.004.

Health Expectancy Calculation by the Sullivan Method: A Practical Guide (2007). Available at https://webgate.ec.europa.eu/chafea_pdb/assets/files/pdb/2006109/2006109_d5sullivan_guide_final_jun2007.pdf

Raalte, Alyson A. van, and Marília R. Nepomuceno. ‘Decomposing Gaps in Healthy Life Expectancy’. In International Handbook of Health Expectancies, edited by Carol Jagger, Eileen M. Crimmins, Yasuhiko Saito, Renata Tiene De Carvalho Yokota, Herman Van Oyen, and Jean-Marie Robine, 107–22. International Handbooks of Population. Cham: Springer International Publishing, 2020. https://doi.org/10.1007/978-3-030-37668-0_7.

Canudas-Romo, Vladimir, José Manuel Aburto, Victor Manuel García-Guerrero, and Hiram Beltrán-Sánchez. ‘Mexico’s Epidemic of Violence and Its Public Health Significance on Average Length of Life’. Journal of Epidemiology and Community Health 71, no. 2 (February 2017): 188–93. https://doi.org/10.1136/jech-2015-207015.

Canudas-Romo, Vladimir, Víctor Manuel García-Guerrero, and Carlos Javier Echarri-Cánovas. ‘The Stagnation of the Mexican Male Life Expectancy in the First Decade of the 21st Century: The Impact of Homicides and Diabetes Mellitus’. Journal of Epidemiology and Community Health 69, no. 1 (January 2015): 28–34. https://doi.org/10.1136/jech-2014-204237.

Aburto, José Manuel, and Alyson van Raalte. ‘Lifespan Dispersion in Times of Life Expectancy Fluctuation: The Case of Central and Eastern Europe’. Demography 55, no. 6 (December 2018): 2071–96. https://doi.org/10.1007/s13524-018-0729-9.

Preston, Samuel H., Patrick Heuveline, and Michel Guillot. Demography, Measuring and Modeling Population Processes, 2001. Chapters on decompostion.

#### Day 3

Preston, Samuel H., Patrick Heuveline, and Michel Guillot. Demography, Measuring and Modeling Population Processes, 2001. Chapter 4.

Colchero, Fernando, Roland Rau, Owen R. Jones, Julia A. Barthold, Dalia A. Conde, Adam Lenart, Laszlo Nemeth, et al. ‘The Emergence of Longevous Populations’. Proceedings of the National Academy of Sciences 113, no. 48 (29 November 2016): E7681–90. https://doi.org/10.1073/pnas.1612191113.

Raalte, Alyson A. van, and Hal Caswell. ‘Perturbation Analysis of Indices of Lifespan Variability’. Demography 50, no. 5 (October 2013): 1615–40. https://doi.org/10.1007/s13524-013-0223-3.

Raalte, Alyson A. van, Isaac Sasson, and Pekka Martikainen. ‘The Case for Monitoring Life-Span Inequality’. Science 362, no. 6418 (30 November 2018): 1002–4. https://doi.org/10.1126/science.aau5811.

Horiuchi, Shiro, John R. Wilmoth, and Scott D. Pletcher. ‘A Decomposition Method Based on a Model of Continuous Change’. Demography 45, no. 4 (1 November 2008): 785–801. https://doi.org/10.1353/dem.0.0033.

García, Jenny, and José Manuel Aburto. ‘The Impact of Violence on Venezuelan Life Expectancy and Lifespan Inequality’. International Journal of Epidemiology 48, no. 5 (1 October 2019): 1593–1601. https://doi.org/10.1093/ije/dyz072.

Aburto, José Manuel, and Hiram Beltrán-Sánchez. ‘Upsurge of Homicides and Its Impact on Life Expectancy and Life Span Inequality in Mexico, 2005–2015’. American Journal of Public Health 109, no. 3 (March 2019): 483–89. https://doi.org/10.2105/AJPH.2018.304878.

#### Day 4
Nau, Claudia, and Glenn Firebaugh. ‘A New Method for Determining Why Length of Life Is More Unequal in Some Populations Than in Others’. Demography 49, no. 4 (November 2012): 1207–30. https://doi.org/10.1007/s13524-012-0133-9.
 
Alvarez, Jesús-Adrián, José Manuel Aburto, and Vladimir Canudas-Romo. ‘Latin American Convergence and Divergence towards the Mortality Profiles of Developed Countries’. Population Studies 74, no. 1 (2 January 2020): 75–92. https://doi.org/10.1080/00324728.2019.1614651.

Keyfitz, Nathan. ‘What Difference Would It Make If Cancer Were Eradicated? An Examination of the Taeuber Paradox’. Demography 14, no. 4 (November 1977): 411. https://doi.org/10.2307/2060587.

Goldman, Noreen, and Graham Lord. ‘A New Look at Entropy and the Life Table’. Demography 23, no. 2 (May 1986): 275. https://doi.org/10.2307/2061621.

Aburto, José Manuel, Francisco Villavicencio, Ugofilippo Basellini, Søren Kjærgaard, and James W. Vaupel. ‘Dynamics of Life Expectancy and Life Span Equality’. Proceedings of the National Academy of Sciences 117, no. 10 (10 March 2020): 5250–59. https://doi.org/10.1073/pnas.1915884117.

Aburto, José Manuel, Jesús-Adrián Alvarez-Martínez, Francisco Villavicencio, and James W. Vaupel. ‘The Threshold Age of the Lifetable Entropy’. Demographic Research 41 (9 July 2019): 83–102. https://doi.org/10.4054/DemRes.2019.41.4.

van Raalte, Alyson A., and Marília R. Nepomuceno. ‘Decomposing Gaps in Healthy Life Expectancy’. In International Handbook of Health Expectancies, edited by Carol Jagger, Eileen M. Crimmins, Yasuhiko Saito, Renata Tiene De Carvalho Yokota, Herman Van Oyen, and Jean-Marie Robine, 107–22. International Handbooks of Population. Cham: Springer International Publishing, 2020. https://doi.org/10.1007/978-3-030-37668-0_7.

Students should be familiar with basic life table construction. Ideally, students should also have a basic understanding of decomposition methods. It is recommended that students read Preston et al. (2001) Chapters 2-4 prior to the commencement of the course (or an equivalent textbook covering life tables and decomposition). A basic command of R, including data handling, for-loops, and writing basic functions, is a prerequisite. 
