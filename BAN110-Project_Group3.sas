libname Clean '/folders/myfolders/lectures';

/*importing data from .csv file*/
data clean.auto;
	length name $40.;
	infile "/folders/myfolders/lectures/Project/auto-mpg.csv" dlm=',';
	input mpg cylinders$ displacement horsepower weight acceleration 
		year$ origin$ name$;
	Car_Num+1;

proc print data=clean.auto;
proc contents data=clean.auto;
	/*analyzing target variables 'MPG' */
proc means data=clean.auto n nmiss max min mean stddev;
	Var mpg;
	ods select Moments ExtremeObs Quantiles;

proc univariate data=clean.auto;
	var mpg;
	histogram/normal;

	/*missing value for character variables*/
proc format;
	value $charmisscheck ' '='Missing' other='Non Missing';

proc freq data=clean.auto;
	tables _character_/ nocum nopercent missing;
	format _character_ $charmisscheck.;

	/*invalid values in character variables*/
data _null_;
	set clean.auto;
	file print;

	if notdigit(trim(year)) then
		put year="is not digit, obs "_n_;

	if notdigit(trim(cylinders)) then
		put cylinders="is not digit, obs " _n_;

	if notdigit(trim(origin)) then
		put origin="is not digit, obs "_n_;

	/*frequency table for categorical variable*/
proc freq data=clean.auto;
	tables _character_/ nocum nopercent;

	/*converting name to upper case*/
data auto_ver1;
	set clean.auto;
	name=upcase(trim(name));

proc print data=auto_ver1;
proc contents data=auto_ver1;
	/*adding derived variable 'manufacturer' from categorical vairbale 'name'*/
data auto_ver2;
	set auto_ver1;
	Manufacturer=SCAN(name, 1);

proc freq data=auto_ver2;
	tables Manufacturer/ nocum nopercent;

	/*missing value for numeric variables */
proc means data=auto_ver2 n nmiss max min mean stddev range;
	/*imputing mean value where horsepower is missing, 5 out of 397*/
proc standard data=auto_ver2 replace out=auto_ver3;
	var horsepower;

	/*descriptive stats for numerical variables  */
proc means data=auto_ver3 n nmiss max min mean stddev range;
	ods select Moments ExtremeObs Quantiles;

proc univariate data=auto_ver3;
	var displacement horsepower weight acceleration;
	histogram/normal;

	/*box plots for numerical varibales*/
proc sgplot data=auto_ver3(keep=displacement horsepower weight acceleration);
	vbox horsepower;

	/*data for accelaration seems to be normally distributed
	thus, we can use SD method to detect the outliers on base of 2 SD */
proc means data=auto_ver3 noprint;
	var acceleration;
	output out=accel_out(drop=_freq_ _type_) mean=StdDev=/Autoname;

data _null_;
	file print;
	set auto_ver3;

	if _n_=1 then
		set accel_out;

	if acceleration<acceleration_Mean-2*acceleration_StdDev and not 
		missing(acceleration) or acceleration>acceleration_Mean+2*acceleration_StdDev 
		then
			put acceleration="is an outlier for car " name " with CarNumber " Car_Num;

	/*deleting outliers for acceleration on base of 2 SD*/
data accel_out_del;
	set auto_ver3;

	if _n_=1 then
		set accel_out;

	if acceleration<acceleration_Mean-2*acceleration_StdDev and not 
		missing(acceleration) or acceleration>acceleration_Mean+2*acceleration_StdDev 
		then
			acc_outier=acceleration;
	drop acceleration_Mean acceleration_StdDev cylinders displacement horsepower 
		weight acceleration year origin manufacturer;
run;

proc sql;
	delete from accel_out_del where acc_outier is NULL;
	run;

proc sort data=auto_ver3;
	by Car_Num;
run;

proc sort data=accel_out_del;
	by Car_Num;
run;

data auto_ver4;
	merge auto_ver3(in=T1) accel_out_del(in=T2);
	drop acc_outier;
	by Car_Num;

	if T1=1 and T2=0;
run;

proc print data=auto_ver4;
run;

/*Using IQR method to detect outliers for "Horsepower"*/
proc means data=auto_ver4 noprint;
	var horsepower;
	output out=horsepower_out(drop=_freq_ _type_) Q1=Q3=QRange=/Autoname;

data _null_;
	file print;
	set auto_ver4;

	if _n_=1 then
		set horsepower_out;

	if horsepower<horsepower_Q1-1.5*horsepower_QRange and not missing(horsepower) 
		or horsepower>horsepower_Q3+1.5*horsepower_QRange then
			put horsepower="is an outlier for car " name " with CarNumber " Car_Num;

	/*Using IQR method to delete outliers for "Horsepower"*/
proc means data=auto_ver4 noprint;
	var horsepower;
	output out=horsepower_out(drop=_freq_ _type_) Q1=Q3=QRange=/Autoname;

data horsepower_out_delete;
	set auto_ver4;

	if _n_=1 then
		set horsepower_out;

	if horsepower<horsepower_Q1-1.5*horsepower_QRange and not missing(horsepower) 
		or horsepower>horsepower_Q3+1.5*horsepower_QRange then
			horse_outlier=horsepower;
	drop mpg horsepower_Q1 horsepower_Q3 horsepower_QRange cylinders displacement 
		weight acceleration year origin manufacturer;
run;

proc sql;
	delete from horsepower_out_delete where horse_outlier is NULL;
	run;

proc sort data=auto_ver4;
	by car_num;
run;

proc sort data=horsepower_out_delete;
	by car_num;
run;

data auto_ver5;
	merge auto_ver4(in=T1) horsepower_out_delete(in=T2);
	drop horse_outlier;
	by car_num;

	if T1=1 and T2=0;
run;

proc print data=auto_ver5;
run;

/*Check skewness of varibales, plot histogram & QQ Plot for highly skewed varibale*/
ods select Moments ExtremeObs Quantiles;

proc univariate data=auto_ver5 normal plot;
	var displacement horsepower weight acceleration;

	/*horsepower is highly skewed*/
	/*Applying log/root transformation on Horsepower*/
data auto_ver6;
	set auto_ver5;
	Log_Horsepower=LOG(horsepower);
	Root_Horsepower=horsepower**0.25;
run;

proc print data=auto_ver6;
run;

/*Descriptive stats/histogram/QQPlot for Log_Horsepower*/
title 'Histogram, BoxPlot & QQ-Plot for Log_Horsepower & Root_Horsepower';

proc univariate data=auto_ver6 normal plot;
	var Log_Horsepower Root_Horsepower;

	/*Skewness has decresed significantly as compared to horsepower*/
	/*From Q-Q plot data for Log_Horsepower/Root_horsepower seems to be somewhat Normally Distributed  */