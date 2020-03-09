libname main654 "C:\Users\jhwang25\Desktop\SAS\";

libname stat654 "C:\Users\jhwang25\Desktop\SAS\";

options LS = 108;

title 'Pharmaceutical Marketing Effectiveness ';

***********************************************;
********** READ DATA         ******************;
***********************************************;

proc import 
	datafile = "C:\Users\jhwang25\Desktop\SAS\MarkEff_Pharma_Data_Raw.xls"
	out = stat654.project
	DBMS = xls
	REPLACE
	;

	sheet = "MarkEff_Pharma_Data_Raw";
	namerow  = 1;
	startrow = 2;
	getnames = YES; 
run;
proc sort data = stat654.project out=foo NODUPKEY;
	by month;
	run;
***********************************************;
********** SUMMARY of DATA  *******************;
***********************************************;
data stat654.project_v1;
	set stat654.project;
	
	Sales300 = 300*Sales;
	LN_Comp_DTC_TV = log(Comp_DTC_TV+1);
	LN_DTC_Display = log(DTC_Display+1);

run;
title2 'Summary measures of raw variables';
proc summary data = stat654.project_v1 min mean max std print;
	var 
		Sales300
		Formulary_Status
		Comp_DTC_TV
		Nov_Ind
		Dec_Ind
		TV
		PDEs
		DTC_Display
		UR
		
		;
	run;
ods PDF FILE= 'Jiwan_Hwang_SAS_OUTPUT.pdf';
***********************************************;
********** BUILD N-LIN MODELS *****************;
***********************************************;

proc NLIN data = stat654.project_v1
	BEST	= 10
	MAXITER	= 200
	METHOD	= GAUSS
	CONVERGE=1.0E-6
	LIST
	ALPHA = 0.05
	
; 
ID Month
;
	parms
		B_Int = 1376.30 
		B_form_status = 115.44 /** Formulary_Status*/
		B_UR = -45.86 /** UR*/
		B_Nov_Ind = 387.16 /** Nov_Ind*/
		B_Dec_Ind = 419.21 /** Dec_Ind*/
		B_LNComp_DTC_TV = -35.47 /** Comp_DTC_TV*/
		B_TV = 4 to 212 by 50 /** TV*/
		B_PDE = 8 to 147 by 50 /** PDEs*/
		B_Display = 161 to 402 /** DTC_Display*/
		A_TV = 0.3 to 1.0 by 0.15 /**TV power*/
		A_PDE = 0.3 to 1.0 by 0.15 /**PDEs power*/
		/**A_Display = 0.3 to 1.0 by 0.15 /**DTC_Display Power*/
	;
	model 
		Sales300 = B_Int + 
					B_form_status   * Formulary_Status + 
					B_UR            * UR + 
					B_Nov_Ind       * Nov_Ind + 
					B_Dec_Ind       * Dec_Ind + 
					B_LNComp_DTC_TV * LN_Comp_DTC_TV + 
					B_TV * (TV+0.01)**A_TV + 
					B_PDE * PDEs**A_PDE + 
					B_Display       * LN_DTC_Display
;
	    output out = stat654.project_out_3
		predicted = y_hat
		residual	= resid
		stdr	= SE_Resid			
		sse     = sse
	;	
run;	
***CSS;
proc means data= stat654.project_v1 css;
   var Sales300;
   output out=b2 css=css;
   run;
data _nuLL_;
   set stat654.project_out_3(obs=1); set b2(obs=1);
   rsq = 1 - sse/css;
   file print;
   put // +10 'R-square for the non-linear model is defined' /
          +10 'as 1 - SSE/CSS, where sse is the variance of' /
          +10 'of the full model, CSS is the variance of   ' /
          +10 'the mean model. ' //
          +10 'R-square =' +5 rsq 8.6;
   run;
***MAPE;
title2 'MAPE of the model';
data mape;
  set stat654.project_out_3;
  keep mape;
mape = 100*abs(y_hat-Sales300)/Sales300;
run;
proc summary data = mape mean print;
  var mape;
run;

title2 'Q-Q plot';
* Produce residial plots and Q-Q plot;
** Create standardized residuals by dividing the residuals by their SDs;
data stat654.project_out_3;
	set stat654.project_out_3;
	label std_residual = "standardized residuals";
	std_residual = resid / SE_Resid;
run;

Proc rank data = stat654.project_out_3 normal = blom out = stat654.project_out_3;
	var std_residual;
	ranks rresid;
run;

** produce Q-Q plot;
goptions reset = all border;
filename outgraph 'C:\Users\jhwang25\Desktop\SAS\project_NLIN_out3_QQ.pdf';
goptions gsfname=outgraph dev=PDF;

symbol1 color = blue value = dot;
symbol2 color = red interpol = join;
proc GPLOT data = stat654.project_out_3;
	plot std_residual * rresid rresid * rresid
		/	overlay
			haxis = -4 to 4 by 0.5 
			vaxis = -4 to 4 by 0.5
	;
run;	
quit;
title2 'Residuals vs Predicted Values';
** Plot resiiduals against the predicted values;
goptions reset = all border;
filename outgraph 'C:\Users\jhwang25\Desktop\SAS\project_NLIN_out3_Rhat.pdf';
goptions gsfname=outgraph dev=PDF;

symbol1 color = blue value = dot;
proc GPLOT data = stat654.project_out_3;
	plot resid * y_hat
		
			/vref = 0 
			
	;
run;	
quit;
title2 'Predicted vs Actual values';
** Plot predicted against the actual values;
goptions reset = all border;
filename outgraph 'C:\Users\jhwang25\Desktop\SAS\project_NLIN_out3_AVP.pdf';
goptions gsfname=outgraph dev=PDF;

symbol1 color = blue value = dot;
symbol2 color = red interpol = join;
proc GPLOT data = stat654.project_out_3;
	plot y_hat * Sales300 Sales300 * Sales300
		/	overlay
			vref = 0 
	;
run;	


title2 'Durbin–Watson';
proc reg data = stat654.project_out_3;
  model resid = /dw dwprob;
  run;
  quit;

  proc autoreg data=stat654.project_out_3;
  model resid =/ dw = 4 dwprob;
  run;

	proc export data = stat654.project_out_3
   		outfile="C:\Users\jhwang25\Desktop\SAS\project_NAIVE_out3.csv"
   		dbms=CSV 
    	replace;
	run;




quit;


********** Peform a Holdout Test Leaving the 3rd year out *******;
title "Project";
title2 "Non-Linear Regression in Additive Functional Form";
title3 "Holdout Test";

* to accomplish holdout  test, create a version of the DepVar that is set to missing in the 4th year;
data _4holdout;
	set stat654.project_v1;
	
	Sales300_Holdout = Sales300;
	If Month >= '01Jan2017'd then do
		Sales300_Holdout = .;
	end;
	
run;	

* Run the same model replacing  the original DepVar with Sales_Holdout; 
Proc NLIN data = _4holdout 
    BEST	= 10
	MAXITER	= 200
	METHOD	= GAUSS
	CONVERGE=1.0E-6
	LIST
	ALPHA = 0.05
	
; 
ID Month
;
	parms
		B_Int = 1376.30 
		B_form_status = 115.44 /** Formulary_Status*/
		B_UR = -45.86 /** UR*/
		B_Nov_Ind = 387.16 /** Nov_Ind*/
		B_Dec_Ind = 419.21 /** Dec_Ind*/
		B_LNComp_DTC_TV = -35.47 /** Comp_DTC_TV*/
		B_TV = 4 to 212 by 50 /** TV*/
		B_PDE = 8 to 147 by 50 /** PDEs*/
		B_Display = 161 to 402 /** DTC_Display*/
		A_TV = 0.3 to 1.0 by 0.15 /**TV power*/
		A_PDE = 0.3 to 1.0 by 0.15 /**PDEs power*/
		/**A_Display = 0.3 to 1.0 by 0.15 /**DTC_Display Power*/
	;
	model 
		Sales300_Holdout = B_Int + 
					B_form_status   * Formulary_Status + 
					B_UR            * UR + 
					B_Nov_Ind       * Nov_Ind + 
					B_Dec_Ind       * Dec_Ind + 
					B_LNComp_DTC_TV * LN_Comp_DTC_TV + 
					B_TV * (TV+0.01)**A_TV + 
					B_PDE * PDEs**A_PDE + 
					B_Display       * LN_DTC_Display
      ;
	    output out = NLIN1_W_Holdout_NLIN_1_Out
		predicted = y_hat
		residual	= resid
		stdr	= SE_Resid			
		sse     = sse
	;	
	

* Create a variable to hold holdout residuals;
data NLIN1_W_Holdout_NLIN_1_Out;
	set NLIN1_W_Holdout_NLIN_1_Out;
            
	If Month >= '01Jan2017'd then do
		Resid_Holdout = Sales300 - y_hat;
	end;
run;

proc export data = NLIN1_W_Holdout_NLIN_1_Out 
   outfile="C:\Users\jhwang25\Desktop\SAS\project_Holdout_NLIN_1_Out.csv"
   dbms=CSV
   replace;
run;


********** Calculate Durbin-Watson Stastics *******;
proc reg data = NLIN1_W_Holdout_NLIN_1_Out;
  model Resid_Holdout = /dw dwprob;
run;
quit;

proc autoreg data=NLIN1_W_Holdout_NLIN_1_Out;
  model Resid_Holdout = / dw = 4 dwprob;
run;
ODS PDF CLOSE;
