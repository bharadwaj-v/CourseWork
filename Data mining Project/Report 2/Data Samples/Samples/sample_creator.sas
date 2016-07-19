proc import datafile='C:\Users\ra0373\Desktop\Project - data\THCIC Hopsital Data - Q2-2011\PUDF_base1_2q2011_tab.txt'
     out=class
     dbms=dlm
     replace;
     delimiter='09'x;
     datarow=5;
run;

proc surveyselect data = Class method = SRS rep = 1 
  sampsize = 2000 seed = 58444 out = Hosp_data3;
  id _all_;
run;

proc export 
  data=Hosp_data3
  dbms=xlsx 
  outfile="C:\Users\ra0373\Desktop\Samples\Sample3" 
  replace;
run;

proc surveyselect data = Class method = SRS rep = 1 
  sampsize = 2000 seed = 58444 out = Hosp_data4;
  id _all_;
run;

proc export 
  data=Hosp_data4 
  dbms=xlsx 
  outfile="C:\Users\ra0373\Desktop\Samples\Sample4" 
  replace;
run;
proc surveyselect data = Class method = SRS rep = 1 
  sampsize = 2000 seed = 58444 out = Hosp_data5;
  id _all_;
run;

proc export 
  data=Hosp_data5
  dbms=xlsx 
  outfile="C:\Users\ra0373\Desktop\Samples\Sample5" 
  replace;
run;
