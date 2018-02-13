-- ******************
-- Import data
-- ******************
-- PolicyID, statecode, county, eq_site_limit, hu_site_limit, fl_site_limit, tiv_2011, tiv_2012, eq_site_deductible
-- hu_448094,FL,CLAY COUNTY,1322376.3,1322376.3,1322376.3,1322376.3,1322376.3,1438163.57,0,0,0,0,30.063936,-81.707664,
.print ' '
.print 'Importing data'
-- First, create the table that the CSV will be stored in
CREATE TABLE "Florida" (
PolicyID CHAR(50),
statecode CHAR(5),
county CHAR(50),
eq_site_limit INT,
hu_site_limit INT,
  fl_site_limit INT,
tiv_2011 INT,
  tiv_2012 INT,
eq_site_deductible INT,
hu_site_deductible INT,
fl_site_deductible INT,
fr_site_deductible INT,
point_latitude INT,
linee INT,
construction INT,
);

-- Tell SQL to expect a CSV file by declaring CSV mode
.mode csv

-- Next, import the CSV following the directions on the sqlitetutorial website
.import FL_insurance_sample.csv Florida

-- Drop the header
DELETE FROM Florida WHERE PolicyID = 'PolicyID';



-- View 10 observations
.print ' '
.print 'View first 10 observations'
-- View first 10 observations
SELECT * FROM Florida LIMIT 10;


-- ******************
-- How many unique values of a certain variable?
-- ******************
.print ' '
.print 'Unique values'
-- Number of unique counties in the data (lists a number) below
--SELECT count(distinct county) from Florida;
-- or lists each one in a separate line
SELECT DISTINCT county FROM Florida;
-- or lists each one in a separate line with counts below
--SELECT Florida, COUNT(*) FROM basketball GROUP BY Season;


-- ******************
.print ' '
.print 'Property appreciation'
SELECT AVG(tiv_2012) FROM Florida;
SELECT AVG(tiv_2011) FROM Florida;
SELECT  AVG(tiv_2012-tiv_2011) FROM Florida;



-- Distribution of categories

.print ' '
.print 'Categorical distribution'
-- Frequency table of NumOT
SELECT construction, COUNT(*) FROM Florida GROUP BY construction;


-- ******************
-- Save as text file
-- ******************
.output Florida.sqlite3
.dump
