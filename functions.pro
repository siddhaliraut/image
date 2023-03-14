-- This function converts a case series to time series 
-- Arguments: DTINDEX - Name of corresponding case series having date indexes.
--			  OBJNAME - Name of case series			  
FUNCTION $CASE_SUM
ARGUMENT DTINDEX, OBJNAME
    
    OVER ON
    IGNORE ADDITION ON
	IGNORE ON
	IF (lastvalue(DTINDEX) NE NC)
		LOCAL SCALAR CUR_FREQ : STRING = REPLACE(TYPE(DTINDEX),"DATE:","",FIRST)
		EXECUTE "FREQUENCY " + CUR_FREQ

		LOCAL SERIES RESULT: PRECISION BY DATE
		NEW UNIQ_DATES = MERGE(SORTDATA(DTINDEX,UP,OMIT,NORANGE))
 
		LOOP FOR X=FIRSTVALUE(UNIQ_DATES) TO LASTVALUE(UNIQ_DATES)
    	CASE *
     	WHICH DTINDEX EQ UNIQ_DATES[X]
      	SET RESULT[UNIQ_DATES[X]] = SUM(OBJNAME)
    END LOOP
	ELSE 
	    RETURN ND
    END IF
	    RETURN RESULT
END FUNCTION


-- This function converts a case series to quarterly time series and then converts it to monthly time series
-- Arguments: DTINDEX - Name of corresponding case series having date indexes.
--			  OBJNAME - Name of case series			  
FUNCTION $CASE_SUM_M
ARGUMENT DTINDEX, OBJNAME
    
    OVER ON
    IGNORE ADDITION ON
    
    LOCAL SCALAR CUR_FREQ : STRING = REPLACE(TYPE(DTINDEX),"DATE:","",FIRST)
    EXECUTE "FREQUENCY " + CUR_FREQ
     
    LOCAL SERIES RESULT: PRECISION BY DATE

    IF NOT MISSING(FIRSTVALUE(DTINDEX))
    
    	NEW UNIQ_DATES = MERGE(SORTDATA(DTINDEX,UP,OMIT,NORANGE))
     
    	LOOP FOR X=FIRSTVALUE(UNIQ_DATES) TO LASTVALUE(UNIQ_DATES)
        	CASE *
         	WHICH DTINDEX EQ UNIQ_DATES[X]
          	SET RESULT[UNIQ_DATES[X]] = SUM(OBJNAME)
    	END LOOP
	END IF

	FREQ MONTHLY
 	LOCAL SERIES M_RESULT: PRECISION BY DATE
	SET M_RESULT = CONVERT(RESULT, MONTHLY, DISCRETE, AVERAGED,DAILY,ON)

	RETURN M_RESULT
END FUNCTION


-- This function returns values of a case series for a particular date
-- Arguments: DTINDEX - Name of corresponding case series having date indexes.
--			  DATEVAL - Date index for which you want case series values
--			  OBJNAME - Name of case series			  
FUNCTION $CASE_FILTER
ARGUMENT DTINDEX, DATEVAL, OBJNAME
    
    OVER ON
    IGNORE ADDITION ON
    
    LOCAL SCALAR CUR_FREQ : STRING = REPLACE(TYPE(DTINDEX),"DATE:","",FIRST)
    EXECUTE "FREQUENCY " + CUR_FREQ
	CASE *
    WHICH DTINDEX EQ DATEVAL
    
	--IF Type of Object is date else set type as is
	IF NOT MISSING(LOCATION(TYPE(OBJNAME), "DATE")) 
		EXECUTE "FREQUENCY " +SUBSTRING(TYPE(OBJNAME), LOCATION(TYPE(OBJNAME),":")+1, LENGTH(TYPE(OBJNAME)))
		LOCAL SERIES RESULT:DATE BY CASE
	ELSE 
		EXECUTE "LOCAL SERIES RESULT:" +TYPE(OBJNAME)+" BY CASE"	     
	END IF    
	
    SET RESULT[caseorder] = OBJNAME
    
	RETURN <CASE *> RESULT
END FUNCTION

-- This function returns values of a case series for a particular date, in a case series starting from index 1
-- Arguments: DTINDEX - Name of corresponding case series having date indexes.
--			  DATEVAL - Date index for which you want case series values
--			  OBJNAME - Name of case series			  
FUNCTION $CASE_FILTER_ORDERED
ARGUMENT DTINDEX, DATEVAL, OBJNAME
    
    OVER ON
    IGNORE ADDITION ON

    
    LOCAL SCALAR CUR_FREQ : STRING = REPLACE(TYPE(DTINDEX),"DATE:","",FIRST)
    EXECUTE "FREQUENCY " + CUR_FREQ
    LOCAL SERIES RESULT: PRECISION BY CASE
    
    CASE *
    WHICH DTINDEX EQ DATEVAL
    
    IF @CASE NE "NULL"
		SET RESULT[CASEORDER] = OBJNAME
	END IF   
    
	RETURN <CASE *> RESULT
END FUNCTION

-- Returns first index for which the expression has a value
-- Arguments: EXPRESSION - Fame 4GL expression
FUNCTION $GET_FIRST
ARGUMENT EXPRESSION
    CASE *
    DATE *
    RETURN FIRSTVALUE(EXPRESSION)       
END FUNCTION

-- Returns last index for which the expression has a value
-- Arguments: EXPRESSION - Fame 4GL expression
FUNCTION $GET_LAST
ARGUMENT EXPRESSION
    CASE *
    DATE *
    RETURN LASTVALUE(EXPRESSION)       
END FUNCTION

-- Returns annualized values for an expression
-- Arguments: EXPRESSION - Fame 4GL expression
FUNCTION $ANNUALIZE
ARGUMENT INP_EXPR
    FREQ Q
    CONVERT ON
    LOCAL SERIES INP_SER : PRECISION BY DATE
    LOCAL SERIES RESULT : PRECISION BY DATE
    DATE *
    DATE FIRSTVALUE(INP_EXPR) TO LASTVALUE(INP_EXPR)
    SET INP_SER = INP_EXPR
    
    WHICH MONTH(T) EQ 3
    SET RESULT = INP_SER * 4
    SET RESULT[T+1] = (INP_SER[T] + INP_SER[T+1]) * 2
    SET RESULT[T+2] = (INP_SER[T] + INP_SER[T+1] + INP_SER[T+2]) / 3 * 4
    SET RESULT[T+3] = INP_SER[T]+INP_SER[T+1]+INP_SER[T+2]+INP_SER[T+3]
    
    RETURN<DATE *> RESULT
END FUNCTION

-- Returns non missing values for an object in given date range
-- Arguments: obj_name - Time series object name of daily frequency
--			  DateFrom - Start of date range
--			  DateTo - End of date range
FUNCTION $NotMissing
ARGUMENT obj_name , DateFrom, DateTo

	over on
	ignore add on
	ignore mult on

	freq daily

	Date DateFrom to DateTo

	Which Not Missing(obj_name)

	Series temp : precision index by case

	set temp[0] = 0;

	Loop For X in Date
			set temp[lastvalue(temp) +1] = obj_name[x]
	End Loop

	Date *
	case *
	set temp[0] = ND
	return temp
   
END FUNCTION

-- Returns previous 4 Wednesday values (for use in some reports)
-- Arguments: obj_name - Time series object name of daily frequency
--			  DateFrom - Start of date range
--			  DateTo - End of date range
FUNCTION $wednesday_monthend
ARGUMENT obj_name , DateFrom, DateTo

over on
ignore add on
ignore mult on

FREQ DAILY
Date DateFrom to DateTo
WHICH MONTH(T) NE MONTH(T+1) OR WEEKDAY(T) EQ 4

case 1 to lengthdate

Local SERIES result : precision by case
set result[dateorder]=obj_name

return <case *> result
END FUNCTION

-- Returns previous 4 Wednesday date indexes (for use in some reports)
--			  DateFrom - Start of date range
--			  DateTo - End of date range
FUNCTION $wednesday_monthend_index
ARGUMENT DateFrom, DateTo

over on

FREQ DAILY
Date DateFrom to DateTo
WHICH MONTH(T) NE MONTH(T+1) OR WEEKDAY(T) EQ 4

case 1 to lengthdate

Local SERIES result : DATE by case
set result[dateorder]=T

return <case *> result
END FUNCTION

-- Returns Wednesday values in given month
-- Arguments: obj_name - Time series object name
--			  month_year - Monthly date value
FUNCTION $wednesday_values
ARGUMENT obj_name , month_year

over on
ignore add on
ignore mult on

FREQ MONTHLY
Date month_year
FREQ DAILY
WHICH WEEKDAY(T) EQ 4

case 1 to lengthdate

Local SERIES result : precision by case
set result[dateorder]=obj_name

return <case *> result
END FUNCTION

-- Returns Wednesday date indexes in given month
--			  month_year - Monthly date value
FUNCTION $wednesday_indexes
ARGUMENT month_year

over on

FREQ MONTHLY
Date month_year
FREQ DAILY
WHICH WEEKDAY(T) EQ 4

case 1 to lengthdate

Local SERIES result : date by case
set result[dateorder]=T

return <case *> result
END FUNCTION

-- Returns 4 Wednesday values from previous month
-- Arguments: obj_name - Time series object name
--			  month_year - Monthly date value
FUNCTION $four_wednesdays
ARGUMENT obj_name, month_year

over on
ignore add on
ignore mult on

FREQ MONTHLY
Date month_year-1
FREQ DAILY
WHICH WEEKDAY(T) EQ 4

Local SCALAR last_wednesday : DATE = LASTVALUE(T)
Local SERIES result : precision by case

date last_wednesday - 28
set result[1]=obj_name
date last_wednesday - 21
set result[2]=obj_name
date last_wednesday - 14
set result[3]=obj_name
date last_wednesday - 7
set result[4]=obj_name

return <case *> result
END FUNCTION

-- Returns 4 Wednesday date indexes from previous month
--			  month_year - Monthly date value
FUNCTION $four_wednesdays_index
ARGUMENT month_year

over on

FREQ MONTHLY
Date month_year-1
FREQ DAILY
WHICH WEEKDAY(T) EQ 4

Local SERIES result : date by case

set result[1]=LASTVALUE(T)-28
set result[2]=LASTVALUE(T)-21
set result[3]=LASTVALUE(T)-14
set result[4]=LASTVALUE(T)-7

return <case *> result
END FUNCTION


FUNCTION $first_four_wednesdays_last_month
ARGUMENT obj_name, month_year

over on
ignore add on
ignore mult on

FREQ MONTHLY
Date month_year-1
FREQ DAILY
WHICH WEEKDAY(T) EQ 4

Local SERIES result : precision by case
CASE 1 to 5
SET result[dateorder]=obj_name

return <case 1 to 4> result
END FUNCTION

FUNCTION $first_four_wednesdays_last_month_index
ARGUMENT month_year

over on

FREQ MONTHLY
Date month_year-1
FREQ DAILY
WHICH WEEKDAY(T) EQ 4

Local SERIES result : date by case

CASE 1 to 5
SET result[dateorder]=T

return <case 1 to 4> result
END FUNCTION

FUNCTION $wednesday_values_prev_three
ARGUMENT obj_name , month_year

over on
ignore add on
ignore mult on

Local SERIES result : precision by case
FREQ MONTHLY
Date month_year-3
FREQ DAILY
WHICH WEEKDAY(T) EQ 4
SET result[1]=LAST(obj_name)

FREQ MONTHLY
Date month_year-2
FREQ DAILY
WHICH WEEKDAY(T) EQ 4
SET result[2]=LAST(obj_name)

FREQ MONTHLY
Date month_year-1
FREQ DAILY
WHICH WEEKDAY(T) EQ 4
SET result[3]=LAST(obj_name)

FREQ MONTHLY
Date month_year
FREQ DAILY
WHICH WEEKDAY(T) EQ 4
set result[3+dateorder]=obj_name

return <case *> result
END FUNCTION


FUNCTION $wednesday_indexes_prev_three
ARGUMENT month_year

over on

FREQ DAILY
Local SERIES result : date by case

FREQ MONTHLY
Date month_year-3
FREQ DAILY
WHICH WEEKDAY(T) EQ 4
SET result[1]=LAST(T)

FREQ MONTHLY
Date month_year-2
FREQ DAILY
WHICH WEEKDAY(T) EQ 4
SET result[2]=LAST(T)

FREQ MONTHLY
Date month_year-1
FREQ DAILY
WHICH WEEKDAY(T) EQ 4
SET result[3]=LAST(T)

FREQ MONTHLY
Date month_year
FREQ DAILY
WHICH WEEKDAY(T) EQ 4     
set result[3+dateorder]=T

return <case *> result
END FUNCTION


-- Return the SUM of observations from a CASE SERIES
-- filtered by a single parameter/Obect
--
-- Usage:
-- COMPILE CBAFunctions
-- LOAD CBAFunctions
-- OR
-- CLOAD "CBAFunctions"
-- CASESUM_FILTER_1(FILTER_ON_OBJECT_NAME, FILTER_ON_OBJECT_VALUE, NAME_OF_OBJECT_TO_RETURN_SUM_OF)
-- Example:
-- CASESUM_FILTER_1(EXSE.PIR.DATEINDEX.C, "2009", EXSE.PIR.DETA.CLBA.C) 
-- Note Argument #2 is in quotes
--
-- Date format examples to be followed:
-- Daily/Business : MMMTXTYEAR : "01JAN2017"
-- Annual : YEAR : "2017"
-- Quarterly : YEAR:Q : "2017:1"
--
FUNCTION CASESUM_FILTER_1
	ARGUMENTS %FILTER_OBJECT_NAME, %FILTER_VALUE, %OBJECT_TO_RETURN_SUM_OF
	TRY
		CASE  *
		IF TYPE(%OBJECT_TO_RETURN_SUM_OF) EQ "PRECISION" OR (TYPE(%OBJECT_TO_RETURN_SUM_OF) NE "NUMERIC")
			EXECUTE "CASE "+$GENERATE_CASE_RANGE(%FILTER_OBJECT_NAME, %FILTER_VALUE)
		ELSE	
			SIGNAL ERROR : NAME(%OBJECT_TO_RETURN_SUM_OF) +" is not PRECISION or NUMERIC case series to calculate the sum."
		END IF --IF TYPE(%OBJECT_TO_RETURN_SUM_OF) EQ "PRECISION" OR (TYPE(%OBJECT_TO_RETURN_SUM_OF) NE "NUMERIC")
 		RETURN SUM(%OBJECT_TO_RETURN_SUM_OF)
	OTHERWISE
		SIGNAL ERROR: ERRORTEXT
	END TRY
END FUNCTION

-- Return the SUM of observations from a CASE SERIES
-- filtered by two parameters/Obects
--
-- Usage:
-- COMPILE CBAFunctions
-- LOAD CBAFunctions
-- OR
-- CLOAD "CBAFunctions"
-- CASESUM_FILTER_2(FILTER_ON_OBJECT_NAME_1, FILTER_ON_OBJECT_VALUE_1, FILTER_ON_OBJECT_NAME_2, FILTER_ON_OBJECT_VALUE_2 , NAME_OF_OBJECT_TO_RETURN_SUM_OF)
-- Example:
-- CASESUM_FILTER_2(EXSE.DIIN.PIR.ASRE.C,"TRUE", EXSE.FBFI.BALA.DATEINDEX.C, "2009", EXSE.DIIN.PIR.clba.C)
-- Note Argument #2 and #4 are in quotes
--
-- Date format examples to be followed:
-- Daily/Business : MMMTXTYEAR : "01JAN2017"
-- Annual : YEAR : "2017"
-- Quarterly : YEAR:Q : "2017:1"
--
FUNCTION CASESUM_FILTER_2
	ARGUMENTS %FILTER_OBJECT_NAME_1, %FILTER_VALUE_1, %FILTER_OBJECT_NAME_2, %FILTER_VALUE_2, %OBJECT_TO_RETURN_SUM_OF
	TRY
		CASE *      
		IF TYPE(%OBJECT_TO_RETURN_SUM_OF) EQ "PRECISION" OR (TYPE(%OBJECT_TO_RETURN_SUM_OF) NE "NUMERIC")
			--For Filter ONE
			LOCAL NEW filt_obj_type_1 = TYPE(%FILTER_OBJECT_NAME_1)	
			--BOOLEAN FILTER 
			IF LOCAL'filt_obj_type_1 EQ "BOOLEAN" 
				WHICH %FILTER_OBJECT_NAME_1 EQ MAKE(BOOLEAN, %FILTER_VALUE_1)
	
			--NUMERIC FILTER
			ELSE IF LOCAL'filt_obj_type_1 EQ "NUMERIC"
				WHICH %FILTER_OBJECT_NAME_1 EQ MAKE(NUMERIC, %FILTER_VALUE_1)
	
			--PRECISION FILTER
			ELSE IF LOCAL'filt_obj_type_1 EQ "PRECISION"
				WHICH %FILTER_OBJECT_NAME_1 EQ MAKE(PRECISION, %FILTER_VALUE_1)
	
			--DATE FILTER
			ELSE IF NOT MISSING(LOCATION(LOCAL'filt_obj_type_1, "DATE"))
				LOCAL NEW filt_obj_freq = SUBSTRING(LOCAL'filt_obj_type_1, LOCATION(LOCAL'filt_obj_type_1,":")+1, LENGTH(LOCAL'filt_obj_type_1))
				EXECUTE "FREQUENCY "+LOCAL'filt_obj_freq
				EXECUTE "LOCAL NEW date_filter = MAKE(DATE("+LOCAL'filt_obj_freq+"), "+QUOTE+%FILTER_VALUE_1+QUOTE+")"

				IF MISSING(LOCAL'date_filter)
					SIGNAL ERROR : %FILTER_VALUE_1 + " is invaid in terms of data type of object "+NAME(%FILTER_OBJECT_NAME_1)
				END IF
				WHICH %FILTER_OBJECT_NAME_1 EQ LOCAL'date_filter

			--STRING FILTER
			ELSE
				WHICH TRIM(%FILTER_OBJECT_NAME_1) EQ %FILTER_VALUE_1
			END IF	
			
			--For Filter TWO
			LOCAL NEW filt_obj_type_2 = TYPE(%FILTER_OBJECT_NAME_2)	
			--BOOLEAN FILTER 
			IF LOCAL'filt_obj_type_2 EQ "BOOLEAN" 
				WHICH %FILTER_OBJECT_NAME_2 EQ MAKE(BOOLEAN, %FILTER_VALUE_2)
	
			--NUMERIC FILTER
			ELSE IF LOCAL'filt_obj_type_2 EQ "NUMERIC"
				WHICH %FILTER_OBJECT_NAME_2 EQ MAKE(NUMERIC, %FILTER_VALUE_2)
	
			--PRECISION FILTER
			ELSE IF LOCAL'filt_obj_type_2 EQ "PRECISION"
				WHICH %FILTER_OBJECT_NAME_2 EQ MAKE(PRECISION, %FILTER_VALUE_2)
	
			--DATE FILTER
			ELSE IF NOT MISSING(LOCATION(LOCAL'filt_obj_type_2, "DATE"))
				LOCAL NEW filt_obj_freq = SUBSTRING(LOCAL'filt_obj_type_2, LOCATION(LOCAL'filt_obj_type_2,":")+1, LENGTH(LOCAL'filt_obj_type_2))
				EXECUTE "FREQUENCY "+LOCAL'filt_obj_freq
				EXECUTE "LOCAL NEW date_filter = MAKE(DATE("+LOCAL'filt_obj_freq+"), "+QUOTE+%FILTER_VALUE_2+QUOTE+")"

				IF MISSING(LOCAL'date_filter)
					SIGNAL ERROR : %FILTER_VALUE_2 + " is invaid in terms of data type of object "+NAME(%FILTER_OBJECT_NAME_2)
				END IF
				WHICH %FILTER_OBJECT_NAME_2 EQ LOCAL'date_filter

			--STRING FILTER
			ELSE
				WHICH TRIM(%FILTER_OBJECT_NAME_2) EQ %FILTER_VALUE_2
			END IF	
			--EXECUTE "CASE "+$GENERATE_CASE_RANGE(%FILTER_OBJECT_NAME_1, %FILTER_VALUE_1)
			--EXECUTE "CASE "+$GENERATE_CASE_RANGE(%FILTER_OBJECT_NAME_2, %FILTER_VALUE_2)
		ELSE	
			SIGNAL ERROR : NAME(%OBJECT_TO_RETURN_SUM_OF) +" is not PRECISION or NUMERIC case series to calculate the sum."
		END IF --IF TYPE(%OBJECT_TO_RETURN_SUM_OF) EQ "PRECISION" OR (TYPE(%OBJECT_TO_RETURN_SUM_OF) NE "NUMERIC")
 		RETURN SUM(%OBJECT_TO_RETURN_SUM_OF)
	OTHERWISE
		SIGNAL ERROR: ERRORTEXT
	END TRY
END FUNCTION


-- Return the SUM of observations from a CASE SERIES
-- filtered by three parameters/Obects
--
-- Usage:
-- COMPILE CBAFunctions
-- LOAD CBAFunctions
-- OR
-- CLOAD "CBAFunctions"
-- CLOAD <channel warn none>"CBAFunctions" ;DISP CASESUM_FILTER_3(NO_FILT, "", EXSE.VACO.PRIMKEY.C, "ADP", EXSE.VACO.CUCO.C, "ADP", EXSE.FOAC.FLOW.TRAN.C,"1460,5455", EXSE.EDSS.INST.REID.C)
-- CLOAD <channel warn none>"CBAFunctions" ;DISP CASESUM_FILTER_3(NO_FILT, "", NO_FILT, "", NO_FILT, "", EXSE.FOAC.FLOW.TRAN.C,"{1460,5455}", EXSE.EDSS.INST.REID.C)
FUNCTION CASESUM_FILTER_4
	ARGUMENTS %FILTER_OBJECT_NAME_1=NO_FILT, %FILTER_VALUE_1="", %FILTER_OBJECT_NAME_2=NO_FILT, %FILTER_VALUE_2="", %FILTER_OBJECT_NAME_3=NO_FILT, %FILTER_VALUE_3="", &&
	%FILTER_OBJECT_NAME_LIST_4=NO_FILT, %FILTER_LIST_VALUE_4="", %OBJECT_TO_RETURN_SUM_OF
	TRY
		CASE *   
		LOCAL SCALAR objsum:PRECISION = 0
		IF TYPE(%OBJECT_TO_RETURN_SUM_OF) EQ "PRECISION" OR (TYPE(%OBJECT_TO_RETURN_SUM_OF) NE "NUMERIC")
			--For Filter ONE			
			IF NAME(%FILTER_OBJECT_NAME_1) NE "NO_FILT" OR %FILTER_VALUE_1 NE ""
			  LOCAL NEW filt_obj_type_1 = TYPE(%FILTER_OBJECT_NAME_1)	
			  --BOOLEAN FILTER 
			  IF LOCAL'filt_obj_type_1 EQ "BOOLEAN" 
			  	WHICH %FILTER_OBJECT_NAME_1 EQ MAKE(BOOLEAN, %FILTER_VALUE_1)
	      
			  --NUMERIC FILTER
			  ELSE IF LOCAL'filt_obj_type_1 EQ "NUMERIC"
			  	WHICH %FILTER_OBJECT_NAME_1 EQ MAKE(NUMERIC, %FILTER_VALUE_1)
	      
			  --PRECISION FILTER
			  ELSE IF LOCAL'filt_obj_type_1 EQ "PRECISION"
			  	WHICH %FILTER_OBJECT_NAME_1 EQ MAKE(PRECISION, %FILTER_VALUE_1)
	      
			  --DATE FILTER
			  ELSE IF NOT MISSING(LOCATION(LOCAL'filt_obj_type_1, "DATE"))
			  	LOCAL NEW filt_obj_freq = SUBSTRING(LOCAL'filt_obj_type_1, LOCATION(LOCAL'filt_obj_type_1,":")+1, LENGTH(LOCAL'filt_obj_type_1))
			  	EXECUTE "FREQUENCY "+LOCAL'filt_obj_freq
			  	EXECUTE "LOCAL NEW date_filter = MAKE(DATE("+LOCAL'filt_obj_freq+"), "+QUOTE+%FILTER_VALUE_1+QUOTE+")"
        
			  	IF MISSING(LOCAL'date_filter)
			  		SIGNAL ERROR : %FILTER_VALUE_1 + " is invaid in terms of data type of object "+NAME(%FILTER_OBJECT_NAME_1)
			  	END IF
			  	WHICH %FILTER_OBJECT_NAME_1 EQ LOCAL'date_filter
        
			  --STRING FILTER
			  ELSE
			  	WHICH TRIM(%FILTER_OBJECT_NAME_1) EQ %FILTER_VALUE_1
			  END IF	
			END IF
			
			
			
			--For Filter TWO			
			IF NAME(%FILTER_OBJECT_NAME_2) NE "NO_FILT" OR %FILTER_VALUE_2 NE ""
			  LOCAL NEW filt_obj_type_2 = TYPE(%FILTER_OBJECT_NAME_2)	
			  --BOOLEAN FILTER 
			  IF LOCAL'filt_obj_type_2 EQ "BOOLEAN" 
			  	WHICH %FILTER_OBJECT_NAME_2 EQ MAKE(BOOLEAN, %FILTER_VALUE_2)
	      
			  --NUMERIC FILTER
			  ELSE IF LOCAL'filt_obj_type_2 EQ "NUMERIC"
			  	WHICH %FILTER_OBJECT_NAME_2 EQ MAKE(NUMERIC, %FILTER_VALUE_2)
	      
			  --PRECISION FILTER
			  ELSE IF LOCAL'filt_obj_type_2 EQ "PRECISION"
			  	WHICH %FILTER_OBJECT_NAME_2 EQ MAKE(PRECISION, %FILTER_VALUE_2)
	      
			  --DATE FILTER
			  ELSE IF NOT MISSING(LOCATION(LOCAL'filt_obj_type_2, "DATE"))
			  	LOCAL NEW filt_obj_freq = SUBSTRING(LOCAL'filt_obj_type_2, LOCATION(LOCAL'filt_obj_type_2,":")+1, LENGTH(LOCAL'filt_obj_type_2))
			  	EXECUTE "FREQUENCY "+LOCAL'filt_obj_freq
			  	EXECUTE "LOCAL NEW date_filter = MAKE(DATE("+LOCAL'filt_obj_freq+"), "+QUOTE+%FILTER_VALUE_2+QUOTE+")"
        
			  	IF MISSING(LOCAL'date_filter)
			  		SIGNAL ERROR : %FILTER_VALUE_2 + " is invaid in terms of data type of object "+NAME(%FILTER_OBJECT_NAME_2)
			  	END IF
			  	WHICH %FILTER_OBJECT_NAME_2 EQ LOCAL'date_filter
        
			  --STRING FILTER
			  ELSE
			  	WHICH TRIM(%FILTER_OBJECT_NAME_2) EQ %FILTER_VALUE_2
			  END IF	
			END IF
			
			
			
			
			
		--For Filter THREE			
			IF NAME(%FILTER_OBJECT_NAME_3) NE "NO_FILT" OR %FILTER_VALUE_3 NE ""
			  LOCAL NEW filt_obj_type_3 = TYPE(%FILTER_OBJECT_NAME_3)	
			  --BOOLEAN FILTER 
			  IF LOCAL'filt_obj_type_3 EQ "BOOLEAN" 
			  	WHICH %FILTER_OBJECT_NAME_3 EQ MAKE(BOOLEAN, %FILTER_VALUE_3)
	      
			  --NUMERIC FILTER
			  ELSE IF LOCAL'filt_obj_type_3 EQ "NUMERIC"
			  	WHICH %FILTER_OBJECT_NAME_3 EQ MAKE(NUMERIC, %FILTER_VALUE_3)
	      
			  --PRECISION FILTER
			  ELSE IF LOCAL'filt_obj_type_3 EQ "PRECISION"
			  	WHICH %FILTER_OBJECT_NAME_3 EQ MAKE(PRECISION, %FILTER_VALUE_3)
	      
			  --DATE FILTER
			  ELSE IF NOT MISSING(LOCATION(LOCAL'filt_obj_type_3, "DATE"))
			  	LOCAL NEW filt_obj_freq = SUBSTRING(LOCAL'filt_obj_type_3, LOCATION(LOCAL'filt_obj_type_3,":")+1, LENGTH(LOCAL'filt_obj_type_3))
			  	EXECUTE "FREQUENCY "+LOCAL'filt_obj_freq
			  	EXECUTE "LOCAL NEW date_filter = MAKE(DATE("+LOCAL'filt_obj_freq+"), "+QUOTE+%FILTER_VALUE_3+QUOTE+")"
        
			  	IF MISSING(LOCAL'date_filter)
			  		SIGNAL ERROR : %FILTER_VALUE_3 + " is invaid in terms of data type of object "+NAME(%FILTER_OBJECT_NAME_3)
			  	END IF
			  	WHICH %FILTER_OBJECT_NAME_3 EQ LOCAL'date_filter
        
			  --STRING FILTER
			  ELSE
			  	WHICH TRIM(%FILTER_OBJECT_NAME_3) EQ %FILTER_VALUE_3
			  END IF	
			END IF
			
			--For Filter FOUR	
			--Filtering only precision list values	
			IF NAME(%FILTER_OBJECT_NAME_LIST_4) NE "NO_FILT" OR %FILTER_LIST_VALUE_4 NE ""
				LOCAL NEW nlist_values = make(NAMELIST, %FILTER_LIST_VALUE_4 )
				--LOCAL NEW setrange = @CASE
				LOOP FOR nlcnt IN nlist_values
					BLOCK
						WHICH %FILTER_OBJECT_NAME_LIST_4 EQ MAKE(PRECISION,NAME(LOCAL'nlcnt))
						IF LENGTHCASE NE 0
							SET LOCAL'objsum=LOCAL'objsum + SUM(%OBJECT_TO_RETURN_SUM_OF)
						ELSE
							--EXECUTE "CASE "+LOCAL'setrange
							NEXT
						END IF
					END BLOCK
				END LOOP
			END IF
			
		ELSE	
			SIGNAL ERROR : NAME(%OBJECT_TO_RETURN_SUM_OF) +" is not PRECISION or NUMERIC case series to calculate the sum."
		END IF --IF TYPE(%OBJECT_TO_RETURN_SUM_OF) EQ "PRECISION" OR (TYPE(%OBJECT_TO_RETURN_SUM_OF) NE "NUMERIC")
 		RETURN LOCAL'objsum
	OTHERWISE
		SIGNAL ERROR: ERRORTEXT
	END TRY
END FUNCTION


-- Returns the CASE RANGE
-- filtered by a specific value
--
-- Usage:
-- COMPILE CBAFunctions
-- LOAD CBAFunctions
-- OR
-- CLOAD "CBAFunctions"
-- $GENERATE_CASE_RANGE(FILTER_ON_OBJECT_NAME, FILTER_ON_OBJECT_VALUE)
-- Example:
-- $GENERATE_CASE_RANGE((EXSE.DIIN.PIR.ASRE.C, "TRUE", EXSE.DIIN.PIR.clba.C)
-- Note Argument #2 is in quotes
--
-- Date format examples to be followed:
-- Daily/Business : MMMTXTYEAR : "01JAN2017"
-- Annual : YEAR : "2017"
-- Quarterly : YEAR:Q : "2017:1"
--
FUNCTION $GENERATE_CASE_RANGE
	ARGUMENTS %FILTER_OBJECT_NAME, %FILTER_VALUE

	LOCAL NEW filt_obj_type = TYPE(%FILTER_OBJECT_NAME)
	--BOOLEAN FILTER 
	IF LOCAL'filt_obj_type EQ "BOOLEAN" 
		WHICH %FILTER_OBJECT_NAME EQ MAKE(BOOLEAN, %FILTER_VALUE)
	
	--NUMERIC FILTER
	ELSE IF LOCAL'filt_obj_type EQ "NUMERIC"
		WHICH %FILTER_OBJECT_NAME EQ MAKE(NUMERIC, %FILTER_VALUE)
	
	--PRECISION FILTER
	ELSE IF LOCAL'filt_obj_type EQ "PRECISION"
		WHICH %FILTER_OBJECT_NAME EQ MAKE(PRECISION, %FILTER_VALUE)
	
	--DATE FILTER
	ELSE IF NOT MISSING(LOCATION(LOCAL'filt_obj_type, "DATE"))
		LOCAL NEW filt_obj_freq = SUBSTRING(LOCAL'filt_obj_type, LOCATION(LOCAL'filt_obj_type,":")+1, LENGTH(LOCAL'filt_obj_type))
		EXECUTE "FREQUENCY "+LOCAL'filt_obj_freq
		EXECUTE "LOCAL NEW date_filter = MAKE(DATE("+LOCAL'filt_obj_freq+"), "+QUOTE+%FILTER_VALUE+QUOTE+")"

		IF MISSING(LOCAL'date_filter)
			SIGNAL ERROR : %FILTER_VALUE + " is invaid in terms of data type of object "+NAME(%FILTER_OBJECT_NAME)
		END IF
		WHICH %FILTER_OBJECT_NAME EQ LOCAL'date_filter

	--STRING FILTER
	ELSE
		WHICH TRIM(%FILTER_OBJECT_NAME) EQ %FILTER_VALUE
	END IF	
	
	IF LENGTHCASE GT 0
		RETURN @CASE
	ELSE 
		RETURN NUMFMT(LENGTHCASE)
	END IF
END FUNCTION




FUNCTION $GET_DATA
ARGUMENTS %DT, %DT_VALUE="",%CURRENCY_OBJ,%TRAN_CODE_OBJ,%TRAN_CODE_LIST="",%ISRANGE,%SUMMED_VALUE_OBJ
    	TRY
           OVER ON
           Ignore ON,ADD ON,MUL ON
           
    	   CASE *   
        
      	   LOCAL NEW filt_obj_type_1 = TYPE(%DT)	
           LOCAL NEW filt_obj_freq = SUBSTRING(LOCAL'filt_obj_type_1, LOCATION(LOCAL'filt_obj_type_1,":")+1, LENGTH(LOCAL'filt_obj_type_1))
           LOCAL SCALAR objsum:PRECISION = 0
           LOCAL SCALAR codeList : STRING = ""
           
           EXECUTE "FREQUENCY "+LOCAL'filt_obj_freq
           EXECUTE "DATE "+%DT_VALUE
          
       	   WHICH %DT GE first(T) AND %DT LE last(T)

           IF %ISRANGE EQ TRUE
       	    EXECUTE "WHICH "+%TRAN_CODE_LIST 
		   ELSE 
			LOCAL NEW nlist_values = make(NAMELIST, %TRAN_CODE_LIST )
   
    		LOOP FOR nlcnt IN LOCAL'nlist_values
            	SET codeList = codeList + NAME(%TRAN_CODE_OBJ) +" EQ " +NAME(nlcnt)+ " OR "
        	END LOOP
            
            SET codeList = LEFT(codeList,LENGTH(codeList)-3)
            
            EXECUTE "WHICH "+codeList
            
           END IF    	   		
          
           LOOP FOR caseNumber IN CASE 
               
		      LOCAL NEW extRateDate = MAKE(DATE(MONTHLY),datefmt(%DT[caseNumber], "<mtxt><YEAR>"))
              
              LOCAL NEW tranCurrency = %CURRENCY_OBJ[caseNumber]
              IF UPPER(tranCurrency) EQ "AFL" OR tranCurrency EQ ND
                  SET tranCurrency = "AWG"
              END IF
              SCALAR exchangedAmount:precision= ROUND(CONVERT(ID("EXSE.FEB.EXRA."+tranCurrency+".MIDD.D"),M,CONSTANT,AVERAGED),2) * %SUMMED_VALUE_OBJ[caseNumber]
              SET LOCAL'objsum = LOCAL'objsum + exchangedAmount
		   END LOOP

    	   RESET
         	RETURN LOCAL'objsum
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION
           
FUNCTION $SORT_DATA
ARGUMENTS %DATE_OBJ,%OBJ_NAME,%FILTER
    	TRY
           
           OVER ON
           Ignore ON,ADD ON,MUL ON
     
           CASE *
           LOCAL NEW filt_obj_type_1 = TYPE(%DATE_OBJ)	
           
           LOCAL NEW filt_obj_freq = SUBSTRING(LOCAL'filt_obj_type_1, LOCATION(LOCAL'filt_obj_type_1,":")+1, LENGTH(LOCAL'filt_obj_type_1))
           EXECUTE "FREQUENCY "+LOCAL'filt_obj_freq
    
           EXECUTE "WHICH "+%FILTER 
           
    	   IF LENGTHCASE GT 0
	           LOCAL NEW SORTED_DATA = MERGE(SORTDATA(STRING(%OBJ_NAME)))
		   ELSE
				LOCAL NEW SORTED_DATA = ND
		   END IF	    
  
         	RETURN <CASE *>LOCAL'SORTED_DATA
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION


FUNCTION $SORT_DATA_WITH_CLOSING_ENTRY
ARGUMENTS %DATE_OBJ,%PERIOD,%OBJ_NAME,%FILTER
    	TRY
           
           OVER ON
           Ignore ON,ADD ON,MUL ON
     
           CASE *
           LOCAL NEW filt_obj_type_1 = TYPE(%DATE_OBJ)	
           
           LOCAL NEW filt_obj_freq = SUBSTRING(LOCAL'filt_obj_type_1, LOCATION(LOCAL'filt_obj_type_1,":")+1, LENGTH(LOCAL'filt_obj_type_1))
           EXECUTE "FREQUENCY "+LOCAL'filt_obj_freq
    
           EXECUTE "WHICH "+%FILTER 
           
    	   IF LENGTHCASE GT 0
	           LOCAL NEW SORTED_DATA = MERGE(SORTDATA(STRING(%OBJ_NAME)))
		   ELSE
				SERIES WORK'SORTED_DATA:STRING BY CASE
		   END IF	 

			CASE *
   			FREQ Q
       
            EXECUTE "WHICH EXSE.FBFI.BALA.DATEINDEX.C EQ "+%PERIOD
            
            IF LENGTHCASE GT 0
	           LOCAL NEW SORTED_DATA1 = MERGE(SORTDATA(STRING(PRECISION(NUMBER(EXSE.FBFI.BALA.FATY.C)))))
		   ELSE
				SERIES WORK'SORTED_DATA1:STRING BY CASE
		   END IF

			case *
   
   		  LOCAL NEW SORTED_DATA2 = MERGE(SORTED_DATA,SORTED_DATA1)
  	
         	RETURN <CASE *>LOCAL'SORTED_DATA2
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION



FUNCTION $FILTER_DATA
ARGUMENTS %DATE_OBJ,%FILTER_STMT,%SUMMED_VALUE_OBJECT
    	TRY
           BLOCK
           OVER ON
           Ignore ON,ADD ON,MUL ON
           
            
           CASE *
           LOCAL NEW filt_obj_type_1 = TYPE(%DATE_OBJ)	
           
           LOCAL NEW filt_obj_freq = SUBSTRING(LOCAL'filt_obj_type_1, LOCATION(LOCAL'filt_obj_type_1,":")+1, LENGTH(LOCAL'filt_obj_type_1))
           EXECUTE "FREQUENCY "+LOCAL'filt_obj_freq
    
           EXECUTE "WHICH "+%FILTER_STMT 
    	  
           -/TEMP_SUM= SUM(%SUMMED_VALUE_OBJECT)

	END BLOCK
	RETURN TEMP_SUM
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION

FUNCTION getData
ARGUMENT OBJECT_NAME,DATE1,DATE2
    
    OVER ON
    
    FREQ M
    
    SERIES RESULT : NUMERIC INDEX BY DATE
    
    DATE DATE1 TO DATE2
    
    SET RESULT = OBJECT_NAME[T] - OBJECT_NAME[T-12]
    
    RESET
    
    RETURN RESULT
    
END FUNCTION


FUNCTION getCaseSeriesData
ARGUMENT OBJECT_NAME,DATE1
    
    OVER ON
    
    FREQ DAILY
    
    SERIES RESULT : NUMERIC INDEX BY CASE
    
    WHICH OBJECT_NAME EQ DATE1
    
    SET RESULT = EXSE.FEB.FLOW.AWGC.C
    
    RESET
    
    RETURN RESULT
    
END FUNCTION
      
FUNCTION CASE_FILTER
	ARGUMENTS %FILTER_OBJECT_NAME, %FILTER_VALUE, %OBJECT_TO_RETURN_CASES
	TRY
		CASE  *
		IF TYPE(%OBJECT_TO_RETURN_CASES) EQ "PRECISION" OR (TYPE(%OBJECT_TO_RETURN_CASES) NE "NUMERIC")
			EXECUTE "CASE "+$GENERATE_CASE_RANGE(%FILTER_OBJECT_NAME, %FILTER_VALUE)
		ELSE	
			SIGNAL ERROR : NAME(%OBJECT_TO_RETURN_CASES) +" is not PRECISION or NUMERIC case series to calculate the sum."
		END IF --IF TYPE(%OBJECT_TO_RETURN_CASES) EQ "PRECISION" OR (TYPE(%OBJECT_TO_RETURN_CASES) NE "NUMERIC")
 		RETURN %OBJECT_TO_RETURN_CASES
	OTHERWISE
		SIGNAL ERROR: ERRORTEXT
	END TRY
END FUNCTION


FUNCTION $GET_MONTHLY_BOP_DATA_SPLIT
ARGUMENTS %DATE_OBJ,%CODE_OBJECT,%FILTER_STMT,%FILTER_STMT1,%SUMMED_VALUE_OBJECT
    	TRY
           
           OVER ON
           Ignore ON,ADD ON,MUL ON
          
         
           LOCAL NEW SORTED_DATA = $SORT_DATA_SPLIT(%DATE_OBJ,%CODE_OBJECT,%FILTER_STMT,%FILTER_STMT1)
           
          IF INDEX(SORTED_DATA) EQ "CASE" AND LASTVALUE(SORTED_DATA) GT 0
          
           CASE FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
           LOCAL SERIES RESULT : PRECISION BY CASE
           
           
           LOOP FOR CODE=FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
              
              SET RESULT[CODE] = $FILTER_DATA_SPLIT(%DATE_OBJ,%FILTER_STMT,%FILTER_STMT1+" AND  "+NAME(%CODE_OBJECT)+" EQ "+string(SORTED_DATA[CODE]),%SUMMED_VALUE_OBJECT)
              
             	
	       END LOOP
          ELSE
          	 LOCAL NEW RESULT = ND
          END IF
     
     	RETURN RESULT
       
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION

FUNCTION FILTER_CODES
	ARGUMENTS %FILTER_OBJECT_NAME, %FILTER_LIST
	TRY
		LOCAL NEW nlist_values = make(NAMELIST, %FILTER_LIST )
        LOCAL SCALAR codeList : STRING = ""
		
    		LOOP FOR nlcnt IN LOCAL'nlist_values
            	SET codeList = codeList + NAME(%FILTER_OBJECT_NAME) +" EQ """ +NAME(nlcnt)+ """ OR "
        	END LOOP
            
            SET codeList = LEFT(codeList,LENGTH(codeList)-3)
			
 		RETURN codeList;
	OTHERWISE
		SIGNAL ERROR: ERRORTEXT
	END TRY
END FUNCTION


FUNCTION CaseToTimeSeries
ARGUMENTS %Date_Series, %Obs_Series,%Filter
	TRY
		   OVER ON
           Ignore ON,ADD ON,MUL ON
           
            
           CASE *
           LOCAL NEW filt_obj_type_1 = TYPE(%Date_Series)	
           
           
           LOCAL NEW filt_obj_freq = SUBSTRING(LOCAL'filt_obj_type_1, LOCATION(LOCAL'filt_obj_type_1,":")+1, LENGTH(LOCAL'filt_obj_type_1))
           EXECUTE "FREQUENCY "+LOCAL'filt_obj_freq
           
           LOCAL SERIES ResulSet : PRECISION BY DATE
           LOCAL SCALAR DateIndex : DATE
    
           EXECUTE "WHICH "+%Filter
           
           LOOP FOR caseNumber IN CASE 
              SET Local'DateIndex = %Date_Series[caseNumber]
              SEt Local'ResulSet[DateIndex] = ResulSet[DateIndex] + %Obs_Series[caseNumber]
		   END LOOP
		   
    RETURN ResulSet
             
	OTHERWISE
		SIGNAL ERROR: ERRORTEXT
	END TRY
END FUNCTION   


FUNCTION CaseToTimeSeriesMultiCodes
ARGUMENTS %Date_Series, %Obs_Series,%Filter,%Code_Obj,%Code_List
                TRY
                                   OVER ON
           Ignore ON,ADD ON,MUL ON
           
               CASE *
               LOCAL NEW filt_obj_type_1 = TYPE(%Date_Series)          
               
               
               LOCAL NEW filt_obj_freq = SUBSTRING(LOCAL'filt_obj_type_1, LOCATION(LOCAL'filt_obj_type_1,":")+1, LENGTH(LOCAL'filt_obj_type_1))
               
               EXECUTE "FREQUENCY "+LOCAL'filt_obj_freq
               
               LOCAL SERIES ResulSet : PRECISION BY DATE
               LOCAL SCALAR DateIndex : DATE
               
                                                LOCAL NEW nlist_values = make(NAMELIST, %Code_List )
      
               LOCAL SCALAR codeList : STRING = ""
                                  
                               LOOP FOR nlcnt IN LOCAL'nlist_values
              
               -- SET codeList = codeList + NAME(%Code_Obj) +" EQ " +NAME(nlcnt)+ " OR "
                    SET codeList = codeList+"STRING("+ NAME(%Code_Obj) +") EQ """ +LOWER(NAME(nlcnt))+ """ OR "
                    
               END LOOP
        
                
                SET codeList = LEFT(codeList,LENGTH(codeList)-3)  
               
                IF(LENGTH(TRIM(codeList)) GT 0)
                              EXECUTE "WHICH "+%Filter+" AND ("+codeList+")"
                            ELSE
                                                                                EXECUTE "WHICH "+%Filter
                                        END IF
          
               
                LOOP FOR caseNumber IN CASE 
                   
                  SET Local'DateIndex = %Date_Series[caseNumber]
              
                  SEt Local'ResulSet[DateIndex] = ResulSet[DateIndex] + %Obs_Series[caseNumber]
                                    END LOOP
                                   
    RETURN ResulSet
             
                OTHERWISE
                                SIGNAL ERROR: ERRORTEXT
                END TRY
END FUNCTION



FUNCTION FILTER_CODES1
	ARGUMENTS %FILTER_OBJECT_NAME, %FILTER_LIST
	TRY
		LOCAL NEW nlist_values = make(NAMELIST, %FILTER_LIST )
        LOCAL SCALAR codeList : STRING = ""
		
    		LOOP FOR nlcnt IN LOCAL'nlist_values
            	SET codeList = codeList + NAME(%FILTER_OBJECT_NAME) +" EQ " +NAME(nlcnt)+ " OR "
        	END LOOP
            
            SET codeList = LEFT(codeList,LENGTH(codeList)-3)
			
 		RETURN codeList;
	OTHERWISE
		SIGNAL ERROR: ERRORTEXT
	END TRY
END FUNCTION

FUNCTION $SORT_MULTIPLE_OBJECTDATA
ARGUMENTS %DATE_OBJ,%EXP,%FILTER
    	TRY
           
           OVER ON
           Ignore ON,ADD ON,MUL ON
           case *
           LOCAL SERIES caseObject:STRING by case
           
           LOCAL NEW filt_obj_type_1 = TYPE(%DATE_OBJ)	
           
           LOCAL NEW filt_obj_freq = SUBSTRING(LOCAL'filt_obj_type_1, LOCATION(LOCAL'filt_obj_type_1,":")+1, LENGTH(LOCAL'filt_obj_type_1))
           EXECUTE "FREQUENCY "+LOCAL'filt_obj_freq
    
           EXECUTE "WHICH "+%FILTER 
           
		   IF LENGTHCASE GT 0
         	 EXECUTE " LOCAL NEW SORTED_DATA ="+%EXP
		   ELSE
		      LOCAL NEW SORTED_DATA = ND
		   END IF 
    	    
         	RETURN<case *> SORTED_DATA
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION


--EX: CASE_SERIES_OPERATION(RESE.CPI.ARUB.INFL.5DIGICN.C,"{04.04.01.01.01,04.05.01.01.01,07.02.02.01.01,07.02.02.01.03}",RESE.CPI.ARUB.INFL.WESEP2000.C,"{RESE.CPI.ARUB.INFL.0404010101SU.M,RESE.CPI.ARUB.INFL.405010101.M,RESE.CPI.ARUB.INFL.702020101.M,RESE.CPI.ARUB.INFL.702020103.M}",T)

FUNCTION CASE_SERIES_OPERATION
	ARGUMENTS %FILTER_OBJECT_NAME, %FILTER_VALUE, %CASE_OBJECT, %OBJECT_LIST, %DATEFEILD
	TRY
   
     	CASE *
  		freq m
		LOCAL NEW seriesList = make(NAMELIST, %OBJECT_LIST )
		LOCAL NEW filter_values = make(NAMELIST, %FILTER_VALUE )
        LOCAL SCALAR filterList : STRING = ""
        LOCAL SCALAR sumvalue: PRECISION = 0
        LOCAL SCALAR count:NUMERIC = 1
        LOCAL SERIES casenumber:PRECISION by case
        LOCAL SERIES<FREQ M> calculateValue:PRECISION by date
		
    		LOOP FOR nlcnt IN LOCAL'filter_values
            	SET filterList = filterList + "UPPER("+NAME(%FILTER_OBJECT_NAME) +") EQ """ + name(nlcnt) + """ OR "
        	END LOOP
   
            SET filterList = LEFT(filterList,LENGTH(filterList)-3)
		
			EXECUTE "WHICH "+filterList
   			SET casenumber[CASEORDER]=%CASE_OBJECT
			LOCAL SCALAR divValue:PRECISION=sum(%CASE_OBJECT)
      		case *
        	
      		IF LASTVALUE(casenumber) NE NC
			 LOOP FOR dt=firstvalue(%DATEFEILD) to lastvalue(%DATEFEILD)
	   			LOOP FOR srcnt IN LOCAL'seriesList
           
    	       		SET sumvalue=sumvalue+(srcnt[dt]* casenumber[COUNT])
        	   		SET COUNT = COUNT +1
     		      
				END LOOP
				set COUNT =1
			
         		set calculateValue[dt]=sumvalue/divValue
           		SET sumvalue=0
    		END LOOP
      
   			END IF
			
			RETURN calculateValue
		OTHERWISE
			SIGNAL ERROR: ERRORTEXT
			
	END TRY
END FUNCTION

FUNCTION $eval_opt
ARGUMENT expression, opt_settings
    OVER ON
    EXECUTE opt_settings
    EXECUTE "LOCAL FORMULA frm = " + expression    
RETURN frm
END FUNCTION 

FUNCTION $EVAL_OPT_PRUD
ARGUMENT expression, opt_settings
    OVER ON
    EXECUTE opt_settings    
RETURN <CASE *>expression
END FUNCTION 

--Below function will extract the case series records based on the
--Date range passed using the DATEINDEX parallel case series
--How to run?
-- disp $CBA_GET_CASE_DATA(EXSE.FEB.FLOW.DATEINDEX.C,EXSE.FEB.FLOW.NONA.C, "1Jan1999", "1Jan1999")
FUNCTION $CBA_GET_CASE_DATA
ARGUMENTS %dtindex, %objname, %startdate, %enddate
TRY
	--image date daily "<dz>-<mz>-<year>"
	IF NOT MISSING( LOCATION(STRING(TYPE(%objname)), "DATE:") )
		EXECUTE "LOCAL SERIES resultantseries:"+REPLACE(TYPE(%objname),":","(")+")  BY CASE"
	ELSE
		EXECUTE "LOCAL SERIES resultantseries:"+TYPE(%objname)+"  BY CASE"
	END IF
		
	--Set the freq as per the %dtindex object
	LOCAL NEW freqtype = SUBSTRING(STRING(TYPE(%dtindex)), LOCATION(STRING(TYPE(%dtindex)),":")+1,  LENGTH(STRING(TYPE(%dtindex))) )
	EXECUTE "FREQUENCY "+LOCAL'freqtype
	CASE *
	--WHICH %dtindex GE MAKE(DATE,%startdate)
	--WHICH %dtindex LE MAKE(DATE,%enddate)
	WHICH %dtindex GE %startdate AND %dtindex LE %enddate
	RESET ONLY FREQ
	SET LOCAL'resultantseries[caseorder] = %objname
	--disp <case *> resultantseries
	--whats resultantseries
OTHERWISE
	SIGNAL ERROR : ERRORTEXT
END TRY
RETURN <case *>resultantseries
END FUNCTION



FUNCTION $WHICH_FILTER
ARGUMENTS %DATE_OBJ,%EXP,%FILTER
    	TRY
           
           OVER ON
           Ignore ON,ADD ON,MUL ON
           case *
           LOCAL SERIES caseObject:STRING by case
           
           IF NOT MISSING( LOCATION(STRING(TYPE(ID(%EXP))), "DATE:") )
				EXECUTE "LOCAL SERIES SORTED_DATA:"+REPLACE(TYPE(ID(%EXP)),":","(")+")  BY CASE"
			ELSE
				EXECUTE "LOCAL SERIES SORTED_DATA:"+TYPE(ID(%EXP))+"  BY CASE"
			END IF
           
           
        
           
           LOCAL NEW filt_obj_type_1 = TYPE(%DATE_OBJ)	
           
           LOCAL NEW filt_obj_freq = SUBSTRING(LOCAL'filt_obj_type_1, LOCATION(LOCAL'filt_obj_type_1,":")+1, LENGTH(LOCAL'filt_obj_type_1))
           EXECUTE "FREQUENCY "+LOCAL'filt_obj_freq
    
           EXECUTE "WHICH "+%FILTER 
           
		   IF LENGTHCASE GT 0
         	 
           
         	 EXECUTE " SET SORTED_DATA[CASEORDER] ="+%EXP
           
		   ELSE
		      EXECUTE " SET SORTED_DATA[CASEORDER] =ND"
		   END IF 
    	    
         	RETURN<case *> SORTED_DATA
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION

FUNCTION $FILTER_DATA_SPLIT
ARGUMENTS %DATE_OBJ,%FILTER_STMT,%FILTER_STMT1,%SUMMED_VALUE_OBJECT
    	TRY
           BLOCK
           OVER ON
           Ignore ON,ADD ON,MUL ON
           
           CASE *
           LOCAL NEW filt_obj_type_1 = TYPE(%DATE_OBJ)	
           
           FREQ DAILY
               
    
           EXECUTE "WHICH "+%FILTER_STMT 
           
               EXECUTE "WHICH "+%FILTER_STMT1
               -/SPLIT_SUM= SUM(%SUMMED_VALUE_OBJECT)
   
    	  
           
	END BLOCK
	RETURN SPLIT_SUM
	   
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION

FUNCTION $SORT_DATA_SPLIT
ARGUMENTS %DATE_OBJ,%OBJ_NAME,%FILTER,%FILTER1
    	TRY
           
           OVER ON
           Ignore ON,ADD ON,MUL ON
     
           CASE *
           LOCAL NEW filt_obj_type_1 = TYPE(%DATE_OBJ)	
           
           LOCAL NEW filt_obj_freq = SUBSTRING(LOCAL'filt_obj_type_1, LOCATION(LOCAL'filt_obj_type_1,":")+1, LENGTH(LOCAL'filt_obj_type_1))
           EXECUTE "FREQUENCY "+LOCAL'filt_obj_freq
    
           EXECUTE "WHICH "+%FILTER 
           
    	   IF LENGTHCASE GT 0
            	EXECUTE "which " +%filter1
	           LOCAL NEW SORTED_DATA = MERGE(SORTDATA(STRING(%OBJ_NAME)))
		   ELSE
				LOCAL NEW SORTED_DATA = ND
		   END IF	    
  
         	RETURN <CASE *>LOCAL'SORTED_DATA
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION

FUNCTION GET_MONTHLY_BOP_DATA
ARGUMENTS %DATE_OBJ,%CODE_OBJECT,%FILTER_STMT,%SUMMED_VALUE_OBJECT
    	TRY
           
           OVER ON
           Ignore ON,ADD ON,MUL ON
           
         
           LOCAL NEW SORTED_DATA = $SORT_DATA(%DATE_OBJ,%CODE_OBJECT,%FILTER_STMT)
           
          IF INDEX(SORTED_DATA) EQ "CASE" AND LASTVALUE(SORTED_DATA) GT 0
          
           CASE FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
           LOCAL SERIES RESULT : PRECISION BY CASE
           
           
           LOOP FOR CODE=FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
              
              SET RESULT[CODE] = $FILTER_DATA(%DATE_OBJ,%FILTER_STMT+" AND  "+NAME(%CODE_OBJECT)+" EQ "+string(SORTED_DATA[CODE]),%SUMMED_VALUE_OBJECT)
             	
	       END LOOP
          ELSE
          	 LOCAL NEW RESULT = ND
          END IF
     
     	RETURN RESULT
       
         	RETURN<case *> SORTED_DATA
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION

FUNCTION GET_MONTHLY_BOP_DATA_WITH_CLOSING_ENTRY
ARGUMENTS %DATE_OBJ,%CODE_OBJECT,%FILTER_STMT,%SUMMED_VALUE_OBJECT,%PERIOD,%TRAN_TYPE
    	TRY
           
           OVER ON
           Ignore ON,ADD ON,MUL ON
           
         
           LOCAL NEW SORTED_DATA =  $SORT_DATA_WITH_CLOSING_ENTRY(%DATE_OBJ,%PERIOD,%CODE_OBJECT,%FILTER_STMT)
           
           IF INDEX(SORTED_DATA) EQ "CASE" AND LASTVALUE(SORTED_DATA) GT 0
          
           CASE FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
           LOCAL SERIES RESULT : PRECISION BY CASE
           
           
           LOOP FOR CODE=FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
              
              SET RESULT[CODE] = $FILTER_DATA(%DATE_OBJ,%FILTER_STMT+" AND  "+NAME(%CODE_OBJECT)+" EQ "+string(SORTED_DATA[CODE]),%SUMMED_VALUE_OBJECT) + $GET_CLOSING_ENTRY_BALA(%PERIOD,SORTED_DATA[CODE],%TRAN_TYPE)
             	
	       END LOOP
          ELSE
          	 LOCAL NEW RESULT = ND
          END IF
     
     	RETURN RESULT
       
         	RETURN<case *> SORTED_DATA
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION


FUNCTION $GET_CLOSING_ENTRY_BALA
ARGUMENTS %PERIOD,%CODE,%TRAN_TYPE
    	TRY
           
           OVER ON
           Ignore ON,ADD ON,MUL ON
	   CASE *
           DATE *

           LOCAL SCALAR RESULT  : PRECISION 
         
           EXEC "WHICH EXSE.FBFI.BALA.DATEINDEX.C EQ "+%PERIOD+" AND NUMBER(EXSE.FBFI.BALA.FATY.C) EQ "+%CODE
           
           IF LENGTHCASE GT 0
               
               SET RESULT = SUM(EXSE.FBFI.BALA.TODE.C) - SUM(EXSE.FBFI.BALA.TOCR.C)
               
               IF NOT MISSING(RESULT) AND %TRAN_TYPE EQ "CR" AND RESULT GT 0
                      RETURN RESULT
               ELSE IF NOT MISSING(RESULT) AND %TRAN_TYPE EQ "DR" AND RESULT LT 0
					  RETURN (RESULT * -1)
               ELSE  
               		RETURN 0
               END IF
               
	       ELSE

           	RETURN 0
           
           END IF

    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION
    

-------------------------------Added on 10-May2018----Santosh Walsetwar-----BOP with Exchange rate-----------------------------------------


FUNCTION $GET_MONTH_BOP_NEW
ARGUMENTS %DATE_OBJ,%CODE_OBJECT,%FREQ,%DATE,%CURRENCY_OBJ,%FILTER_STMT,%SUMMED_VALUE_OBJECT
    	TRY

           OVER ON
           IGNORE ON,ADD ON,MUL ON
          
           LOCAL NEW SORTED_DATA = $SORT_DATA(%DATE_OBJ,%CODE_OBJECT,%FILTER_STMT)
           LOCAL NEW SORTED_CUCO  = $SORT_DATA(%DATE_OBJ,%CURRENCY_OBJ,%FILTER_STMT)
          
		   LOCAL SERIES EXCH_RATES : PRECISION BY CASE
		   
		   IF LASTVALUE(SORTED_CUCO) GT 0
           CASE FIRSTVALUE(SORTED_CUCO) TO LASTVALUE(SORTED_CUCO)
		    SET EXCH_RATES = $GET_EXCH_RATE(SORTED_CUCO,%DATE,%FREQ)
     	   CASE *
		   END IF
          
          IF INDEX(SORTED_DATA) EQ "CASE" AND LASTVALUE(SORTED_DATA) GT 0
          
           CASE FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
          
           LOCAL SERIES RESULT : PRECISION BY CASE
           
           
           LOOP FOR CODE=FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
		   
		    LOOP FOR CURR = FIRSTVALUE(SORTED_CUCO) TO LASTVALUE(SORTED_CUCO)
        
          LOCAL SCALAR RS : PRECISION
        
              SET RS = ($FILTER_DATA(%DATE_OBJ,%FILTER_STMT+";WHICH "+NAME(%CODE_OBJECT)+" EQ "+string(SORTED_DATA[CODE])+";WHICH "+NAME(%CURRENCY_OBJ)+" EQ """+SORTED_CUCO[CURR]+"""",%SUMMED_VALUE_OBJECT)   
            
              IF RS NE ND AND RS NE NC
            	  SET RESULT[CODE] = RESULT[CODE]+(RS * EXCH_RATES[CURR])
			  END IF					              
			 END LOOP
	       END LOOP
      ELSE
          	 LOCAL NEW RESULT = ND
          END IF
     
     	RETURN RESULT
       
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION

FUNCTION $GET_EXCH_RATE
ARGUMENTS %CURR_LIST,%DATE,%FREQ
    	TRY
		
			OVER ON
			IGNORE ON,ADD ON,MUL ON,FUNC ON
		
				IF INDEX(%CURR_LIST) EQ "CASE" AND LASTVALUE(%CURR_LIST) GT 0
          
					CASE FIRSTVALUE(%CURR_LIST) TO LASTVALUE(%CURR_LIST)
					LOCAL SERIES EXCH_RATE : PRECISION BY CASE
   				
					EXECUTE "FREQ "+%FREQ
     
					DATE %DATE
  
					
					LOOP FOR CURR=FIRSTVALUE(%CURR_LIST) TO LASTVALUE(%CURR_LIST)	
         	
					IF UPPER(%CURR_LIST[CURR]) EQ "AFL"
                       LOCAL SCALAR RS = 1
                    ELSE
         				IF %CURR_LIST[CURR] NE "ND"
							EXECUTE "LOCAL SCALAR RS = CONVERT(EXSE.FEB.EXRA."+%CURR_LIST[CURR]+".MIDD.D,"+STRING(%FREQ)+",DISCRETE,AVERAGED)"
						ELSE 
							LOCAL SCALAR RS = 1
						END IF
            		END IF 
    	        
       				IF RS NE ND AND RS NE NC
               			
               			IF UPPER(%CURR_LIST[CURR])  EQ "ITL"
                      		SET RS = RS/1000
	                    ELSE IF UPPER(%CURR_LIST[CURR])  EQ "JPY"
    						SET RS = RS/10000	                    
						ELSE IF UPPER(%CURR_LIST[CURR])  EQ "AWG" OR UPPER(%CURR_LIST[CURR])  EQ "ANG" OR UPPER(%CURR_LIST[CURR])  EQ "GBP" OR UPPER(%CURR_LIST[CURR])  EQ "CAD" OR UPPER(%CURR_LIST[CURR])  EQ "USD" OR UPPER(%CURR_LIST[CURR])  EQ "AFL"
                      		SET RS = RS/1
                    	ELSE
                        	SET RS = RS/100

                        END IF
               
						SET EXCH_RATE[CURR] = RS
      				END IF
		            
		            END LOOP

              
					RETURN EXCH_RATE
				ELSE
					RETURN ND
				END IF
       
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION


----------
--FUNCTION TO RETURN CONSOLIDATED FROM FEBFLOW SPLIT SERIES
--Developed for edit case series forms
--$get_febflow_cons_series("EXSE.FEB.FLOW.aflc.C", "1jan2018", "31Mar2018", "AUA")
FUNCTION $get_febflow_cons_series
ARGUMENTS %series_name, %sdate, %edate, %institue,%filter1,%filter2,%filter3,%filter4,%filter5,%filter6
OVER ON
FREQ DAILY
TRY
    
	IF NOT MISSING(location(TYPE(ID(%series_name)),"DATE"))  
		LOCAL SERIES cons_series:DATE(DAILY) BY CASE
	ELSE
		EXECUTE "LOCAL SERIES cons_series:"+TYPE(ID(%series_name))+" BY CASE"
	END IF
	LOCAL NEW temp_str = SUBSTRING(MIRROR(%series_name), LOCATION(MIRROR(%series_name), ".") +1, LENGTH(MIRROR(%series_name)))
	LOCAL NEW series_dim = MIRROR(SUBSTRING(LOCAL'temp_str, 0, LOCATION(LOCAL'temp_str, ".")-1))
	LOCAL SCALAR !tempcs : NUMERIC 
	LOCAL SCALAR !queryStr : STRING
	
	--IF sdate and edate are present create series ignore CASE range
	IF (LENGTH(TRIM(%sdate)) NE 0) AND (LENGTH(TRIM(%edate)) NE 0)
		BLOCK
			DATE MAKE(DATE(DAILY),%sdate) TO MAKE(DATE(DAILY),%edate)
			FREQ MON
    
			SET LOCAL'tempcs = 1
			LOOP FOR dt IN DATE
				--DISP dt
				IF MONTH(dt) LE 9
					LOCAL NEW monstr = "0"+STRING(MONTH(dt))
				ELSE
					LOCAL NEW monstr = STRING(MONTH(dt))
				END IF
				--TYPE "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C"
				set local'queryStr=""

				IF EXISTS(ID("EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C")) AND &&
					NOT MISSING(FIRSTVALUE(ID("EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C")))
					CASE *
					IF (NOT MISSING(LENGTH(TRIM(%filter1))) AND LENGTH(TRIM(%filter1)) NE 0)
						SET local'queryStr=local'queryStr+"EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".AFLD.C GT "+ %filter1+" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(%filter2))) AND LENGTH(TRIM(%filter2)) NE 0)
						SET local'queryStr=local'queryStr+"EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".AFLC.C GT "+%filter2+" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(%filter3))) AND LENGTH(TRIM(%filter3)) NE 0)
						SET local'queryStr=local'queryStr+"EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".TRCO.C GT "+%filter3+" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(%filter4))) AND LENGTH(TRIM(%filter4)) NE 0)
						SET local'queryStr=local'queryStr+"EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".TRCO.C LT "+%filter4+" AND "
					END IF
					
					SET local'queryStr= SUBSTRING(local'queryStr, 0,LENGTH(local'queryStr)-4)
					      				
					EXECUTE "WHICH "+local'queryStr
					
					IF LENGTHCASE GT 0
						SET LOCAL'cons_series[CASEORDER] = ID("EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C")
					END IF
					
					
					set local'queryStr=""
					CASE *
					IF (NOT MISSING(LENGTH(TRIM(%filter1))) AND LENGTH(TRIM(%filter1)) NE 0)
						SET local'queryStr=local'queryStr+"EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".AFLD.C GT "+ %filter1+" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(%filter2))) AND LENGTH(TRIM(%filter2)) NE 0)
						SET local'queryStr=local'queryStr+"EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".AFLC.C GT "+%filter2+" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(%filter5))) AND LENGTH(TRIM(%filter5)) NE 0)
						SET local'queryStr=local'queryStr+"EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".TRCO.C GT "+%filter5+" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(%filter6))) AND LENGTH(TRIM(%filter6)) NE 0)
						SET local'queryStr=local'queryStr+"EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".TRCO.C LT "+%filter6+" AND "
					END IF
					
					SET local'queryStr= SUBSTRING(local'queryStr, 0,LENGTH(local'queryStr)-4)
					
					EXECUTE "WHICH "+local'queryStr
					
					IF NOT MISSING(LASTVALUE(cons_series))
						set tempcs=lastvalue(cons_series)+1
						IF LENGTHCASE GT 0
							LOOP FOR cs IN CASE
								SET LOCAL'cons_series[LOCAL'tempcs] = ID("EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C")[cs]
								SET LOCAL'tempcs = LOCAL'tempcs + 1
							END LOOP
						END IF
					ELSE 
						IF LENGTHCASE GT 0
							SET LOCAL'cons_series[CASEORDER] = ID("EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C")
						END IF
					END IF
					     	
				ELSE
					SIGNAL CONTINUE : ERRORTEXT
				END IF --EXISTS(ID("EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C")) AND &&
				
			END LOOP
				
		END BLOCK
		
	ELSE
		SIGNAL ERROR : "$get_febflow_cons_series() : INVALID DATE RANGE"
	END IF	--(LENGTH(TRIM(%sdate)) NE 0) AND (LENGTH(TRIM(%edate)) NE 0)
		
OTHERWISE
	SIGNAL ERROR:ERRORTEXT
END TRY	
	CASE * 
	RETURN <case *>cons_series
END FUNCTION




FUNCTION $SORTED_CODE_QUAR
ARGUMENTS %DATE_OBJS
TRY
	FREQ Q
	OVER on
	EXECUTE "DATE "+%DATE_OBJS
	IGNORE ADD ON
	freq m
	CASE *
	LOCAL series returnCode:PRECISION by case
	--LOCAL series returnCode:PRECISION by case
	LOCAL SCALAR flag:boolean = TRUE
	LOCAL SCALAR lastvalCs:NUMERIC
	set lastvalCs=1
	LOOP FOR X IN DATE
	
		--disp string(datefmt(X,"<dz><mtxt><year>")),string(datefmt(X,"<dz><mtxt><year>"))
		LOCAL NEW temp = sortdata($SORT_DATA_FEBFLOWSPLIT_CBA("01"+string(datefmt(X,"<mtxt><year>")),string(datefmt(X,"<dz><mtxt><year>"))))
		--whats temp, returnCode
		CASE lastvalCs to lastvalCs+lastvalue(returnCode)+lastvalue(temp)
		SET returnCode = MERGE(returnCode, temp)
		set lastvalCs=lastvalCs+lastvalue(returnCode)+lastvalue(temp)
	END LOOP
	case *
	--disp returnCode
	
  RETURN <CASE *> returnCode
  
OTHERWISE
	SIGNAL ERROR: ERRORTEXT
END TRY

END FUNCTION

FUNCTION $GET_MONTHLY_BOP_DATA_FEBFLOWSPLIT_CBA_QUAR
ARGUMENTS %DATE_OBJS,  %SUMMED_VALUE_OBJECT_DIM
TRY

	FREQ Q
	OVER on
	EXECUTE "DATE "+%DATE_OBJS
	IGNORE ADD ON
	freq m
	CASE *
	LOCAL series returnCode:PRECISION by case
	--LOCAL series returnCode:PRECISION by case
	LOCAL SCALAR flag:boolean = TRUE
	LOCAL SCALAR lastvalCs:NUMERIC
	set lastvalCs=1
	LOOP FOR X IN DATE
	
		--disp string(datefmt(X,"<dz><mtxt><year>")),string(datefmt(X,"<dz><mtxt><year>"))
		LOCAL NEW temp = sortdata($GET_MONTHLY_BOP_DATA_FEBFLOWSPLIT_CBA("01"+string(datefmt(X,"<mtxt><year>")),string(datefmt(X,"<dz><mtxt><year>")),%SUMMED_VALUE_OBJECT_DIM))
		--whats temp, returnCode
		CASE lastvalCs to lastvalCs+lastvalue(returnCode)+lastvalue(temp)
		SET returnCode = MERGE(returnCode, temp)
		set lastvalCs=lastvalCs+lastvalue(returnCode)+lastvalue(temp)
	END LOOP
	case *
	--disp returnCode
	
  RETURN <CASE *> returnCode

OTHERWISE
	SIGNAL ERROR: ERRORTEXT
END TRY
END FUNCTION


---------------------------ANIKET-------------------
----------
--FUNCTION TO RETURN CONSOLIDATED FROM FEBFLOW SPLIT SERIES
--Developed for edit case series forms
--disp $get_febflow_cons_series_mul_filters("EXSE.FEB.FLOW.RECO.C","01Apr2018 AND 30Apr2018","AUA"," GT ""100"" AND LT  ""1000"""," EQ ""BV"" OR EQ ""LNB"" OR EQ ""BKA""","EQ ""DIVERSE INGEZETENEN"" OR EQ ""BANCO DI CARIBE ARUBA NV"" OR EQ ""BANCO DI CARIBE ARUBA NV""","EQ ""VASA RECEIVABLE CC PAYMENT ACCOUNT"" OR EQ ""BANCO DI CARIBE ARUBA NV""","LT 4000 AND GT 3000","EQ 5510 OR MISSING()","EQ ""AN"" OR EQ ""US""","EQ ""ANG"" OR EQ ""USD""","GT 1000 AND LT 50000","GT 1000 AND LT 50000","GT 1000 AND LT 50000","GT 1000 AND LT 50000","EQ 0 AND LT 2","EQ ""79"" OR MISSING()","EQ ""DIFFERENT CONSTRUCTION INVOICES"" OR EQ ""$$/MARCH AND APRIL INTEREST CORRESPOND""","EQ ""39120""","MISSING()")
--disp $get_febflow_cons_series_mul_filters("EXSE.FEB.FLOW.RECO.C","01Apr2018 AND 30Apr2018","AUA"," GT ""100"" AND LT  ""1000"""," EQ ""BV"" OR EQ ""LNB"" OR EQ ""BKA""","","","","","","","","","","","","","","","MISSING()")
--disp remeval(rem,"$get_febflow_cons_series_mul_filters(""EXSE.FEB.FLOW.RECO.C"",""01Apr2018 AND 30Apr2018"",""AUA"","" GT """"100"""" AND LT  """"1000"""""","" EQ """"BV"""" OR EQ """"LNB"""" OR EQ """"BKA"""""","""","""","""","""","""","""","""","""","""","""","""","""","""","""","""")")
--clear;cload "C:\Projects\map\fame\procs-fedm\customfame_fix_string_like_search_bop.pro";clear output, info;disp<case *> $get_febflow_cons_series_mul_filters("EXSE.FEB.FLOW.ALL.RECO.C","01Apr018 AND 30Apr2018","","","","","","","","","","","","","","","","","","")
FUNCTION $get_febflow_cons_series_mul_filters
ARGUMENTS %series_name, %sdate_and_date, %institue, %reid="", %acty="", %rena="", %nona="", %trco="", %isic="", %coco="", %cuco="",%fcde="",%afld="",%fccr="",%aflc="",%fec="",%lice="",%desc="",%cnc="",%cbar=""
OVER ON
FREQ DAILY
TRY
	IF MISSING(LOCATION(TRIM(UPPER(%series_name)), "ALL")) 
		IF NOT MISSING(location(TYPE(ID(%series_name)),"DATE"))
			LOCAL SERIES cons_series:DATE(DAILY) BY CASE
		ELSE
			EXECUTE "LOCAL SERIES cons_series:"+TYPE(ID(%series_name))+" BY CASE"
		END IF
	END IF
	LOCAL NEW temp_str = SUBSTRING(MIRROR(%series_name), LOCATION(MIRROR(%series_name), ".") +1, LENGTH(MIRROR(%series_name)))
	LOCAL NEW series_dim = MIRROR(SUBSTRING(LOCAL'temp_str, 0, LOCATION(LOCAL'temp_str, ".")-1))
	LOCAL SERIES <CASE 1 TO 6> !loperatos:STRING BY CASE = "GT", "GE", "LT", "LE", "EQ", "NE"
	LOCAL SERIES <CASE 1 TO 2> !boperatos:STRING BY CASE = "AND", "OR"
	LOCAL SCALAR !tempcs : NUMERIC = 1
	LOCAL SCALAR !queryStr : STRING
	LOCAL NEW temp_reid = %reid
	LOCAL NEW temp_trco = %trco
	LOCAL NEW temp_isic = %isic
	LOCAL NEW temp_fcde = %fcde
	LOCAL NEW temp_afld = %afld
	LOCAL NEW temp_fccr = %fccr
	LOCAL NEW temp_aflc = %aflc
	LOCAL NEW temp_fec	= %fec
	LOCAL NEW temp_acty = UPPER(%acty)
	LOCAL NEW temp_rena = UPPER(%rena)
	LOCAL NEW temp_nona = UPPER(%nona)
	LOCAL NEW temp_coco = UPPER(%coco)
	LOCAL NEW temp_cuco = UPPER(%cuco)
	LOCAL NEW temp_lice = UPPER(%lice)
	LOCAL NEW temp_desc = UPPER(%desc)
	LOCAL NEW temp_cnc 	= UPPER(%cnc)
	LOCAL NEW temp_cbar = UPPER(%cbar)
	
	--Get the start and end date
	LOCAL NEW tdate = %sdate_and_date
	BLOCK
		CASE FIRSTVALUE(loperatos) TO LASTVALUE(loperatos)
		LOOP FOR ex in CASE
			SET LOCAL'tdate = REPLACE(LOCAL'tdate, loperatos[ex], "")
		END LOOP
		CASE FIRSTVALUE(boperatos) TO LASTVALUE(boperatos)
		LOOP FOR bl in CASE
			SET LOCAL'tdate = REPLACE(LOCAL'tdate, boperatos[bl], "")
		END LOOP
		LOCAL NEW %sdate = TRIM(SUBSTRING(TRIM(LOCAL'tdate), 1, LOCATION(TRIM(LOCAL'tdate)," ")))
		LOCAL NEW %edate = TRIM(SUBSTRING(TRIM(LOCAL'tdate), LOCATION(TRIM(LOCAL'tdate)," ")+1, LENGTH(TRIM(LOCAL'tdate))))
		--DISP LOCAL'tdate, LOCAL'%sdate, LOCAL'%edate
	END BLOCK
	
	  --Get data for all institues
 	 IF NOT MISSING(LOCATION(TRIM(UPPER(%series_name)), "ALL"))
 		 	RETURN <case *>$get_all_institutes_data(%series_name, %sdate, %edate, series_dim)
 	 END IF
	
	--IF sdate and edate are present create series ignore CASE range
	IF (LENGTH(TRIM(LOCAL'%sdate)) NE 0) AND (LENGTH(TRIM(LOCAL'%edate)) NE 0)
		BLOCK
			DATE MAKE(DATE(DAILY),LOCAL'%sdate) TO MAKE(DATE(DAILY),LOCAL'%edate)
			FREQ MON
    	SET LOCAL'tempcs = 1
			LOOP FOR dt IN DATE
				--DISP dt
				IF MONTH(dt) LE 9
					LOCAL NEW monstr = "0"+STRING(MONTH(dt))
				ELSE
					LOCAL NEW monstr = STRING(MONTH(dt))
				END IF
				SET LOCAL'queryStr=""
				LOCAL NEW obj_name = "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C"
				LOCAL NEW pk_obj_name = "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".PRIMKEY.C"
				--Convert expressions to FAMe 4GL expressions
				--DISP "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".REID.C"
					LOCAL NEW reid = $get_4gl_expression(LOCAL'temp_reid, loperatos, "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".REID.C")
					LOCAL NEW trco = $get_4gl_expression(LOCAL'temp_trco, loperatos, "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".TRCO.C")
					LOCAL NEW isic = $get_4gl_expression(LOCAL'temp_isic, loperatos, "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".ISIC.C")
					LOCAL NEW fcde = $get_4gl_expression(LOCAL'temp_fcde, loperatos, "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".FCDE.C")
					LOCAL NEW afld = $get_4gl_expression(LOCAL'temp_afld, loperatos, "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".AFLD.C")
					LOCAL NEW fccr = $get_4gl_expression(LOCAL'temp_fccr, loperatos, "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".FCCR.C")
					LOCAL NEW aflc = $get_4gl_expression(LOCAL'temp_aflc, loperatos, "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".AFLC.C")
					LOCAL NEW fec  = $get_4gl_expression(LOCAL'temp_fec, loperatos, "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".FEC.C")
					LOCAL NEW acty = $get_4gl_expression(LOCAL'temp_acty, loperatos, "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".ACTY.C")
					LOCAL NEW rena = $get_4gl_expression(LOCAL'temp_rena, loperatos, "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".RENA.C")
					LOCAL NEW nona = $get_4gl_expression(LOCAL'temp_nona, loperatos, "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".NONA.C")
					LOCAL NEW coco = $get_4gl_expression(LOCAL'temp_coco, loperatos, "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".COCO.C")
					LOCAL NEW cuco = $get_4gl_expression(LOCAL'temp_cuco, loperatos, "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".CUCO.C")
					LOCAL NEW lice = $get_4gl_expression(LOCAL'temp_lice, loperatos, "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".LICE.C")
					LOCAL NEW desc = $get_4gl_expression(LOCAL'temp_desc, loperatos, "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".DESC.C")
					LOCAL NEW cnc  = $get_4gl_expression(LOCAL'temp_cnc, loperatos, "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".CNC.C")
					LOCAL NEW cbar = $get_4gl_expression(LOCAL'temp_cbar, loperatos, "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".CBAR.C")
					
					--DISP LOCAL'reid	,LOCAL'acty,LOCAL'rena, LOCAL'nona, LOCAL'trco, LOCAL'isic, LOCAL'coco, LOCAL'cuco, LOCAL'fcde, LOCAL'afld, LOCAL'fccr, LOCAL'aflc
					--DISP LOCAL'fec, LOCAL'lice, LOCAL'desc, LOCAL'cnc
					--DISP LOCAL'cbar
								
				IF EXISTS(ID(LOCAL'obj_name) AND NOT MISSING(FIRSTVALUE(ID(LOCAL'obj_name)))
					CASE *
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'reid))) AND LENGTH(TRIM(LOCAL'reid)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'reid), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'reid +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'trco))) AND LENGTH(TRIM(LOCAL'trco)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'trco), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'trco  +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'isic))) AND LENGTH(TRIM(LOCAL'isic)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'isic), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'isic +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'fcde))) AND LENGTH(TRIM(LOCAL'fcde)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'fcde), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'fcde +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'afld))) AND LENGTH(TRIM(LOCAL'afld)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'afld), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'afld +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'fccr))) AND LENGTH(TRIM(LOCAL'fccr)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'fccr), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'fccr +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'aflc))) AND LENGTH(TRIM(LOCAL'aflc)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'aflc), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'aflc +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'fec))) AND LENGTH(TRIM(LOCAL'fec)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'fec), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'fec +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'acty))) AND LENGTH(TRIM(LOCAL'acty)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'acty), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'acty +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'rena))) AND LENGTH(TRIM(LOCAL'rena)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'rena), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'rena +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'nona))) AND LENGTH(TRIM(LOCAL'nona)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'nona), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'nona  +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'coco))) AND LENGTH(TRIM(LOCAL'coco)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'coco), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'coco +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'cuco))) AND LENGTH(TRIM(LOCAL'cuco)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'cuco), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'cuco +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'lice))) AND LENGTH(TRIM(LOCAL'lice)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'lice), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'lice +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'desc))) AND LENGTH(TRIM(LOCAL'desc)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'desc), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'desc +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'cnc))) AND LENGTH(TRIM(LOCAL'cnc)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'cnc), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'cnc +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'cbar))) AND LENGTH(TRIM(LOCAL'cbar)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'cbar), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'cbar
					END IF
					--DISP LOCAL'queryStr
					IF NOT MISSING(LOCATION(SUBSTRING(MIRROR(TRIM(LOCAL'queryStr)),0,4), "DNA", 1))
						SET LOCAL'queryStr = TRIM(MIRROR(SUBSTRING(MIRROR(TRIM(LOCAL'queryStr)), LOCATION(MIRROR(TRIM(LOCAL'queryStr)), "DNA")+3, LENGTH(MIRROR(TRIM(LOCAL'queryStr)))))
					END IF
					--DISP LOCAL'queryStr
					--				      				
					
					IF NOT MISSING(local'queryStr) AND local'queryStr NE ""
						EXECUTE "WHICH "+local'queryStr
					ELSE
						CASE FIRSTVALUE(ID(LOCAL'pk_obj_name)) TO LASTVALUE(ID(LOCAL'pk_obj_name))
					END IF
					--DISPLAY LENGTHCASE
					
 					IF LENGTHCASE GT 0
 						LOOP FOR cs IN CASE
 							SET LOCAL'cons_series[LOCAL'tempcs] = ID(LOCAL'obj_name)[cs]
							SET LOCAL'tempcs = LOCAL'tempcs + 1
						END LOOP
 					END IF --LENGTHCASE GT 0
				ELSE
					SIGNAL CONTINUE : ERRORTEXT
				END IF --EXISTS(ID("EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C")) AND &&
			END LOOP
		END BLOCK
	ELSE
		SIGNAL ERROR : "$get_febflow_cons_series_mul_filters() : INVALID DATE RANGE"
	END IF	--(LENGTH(TRIM(LOCAL'%sdate)) NE 0) AND (LENGTH(TRIM(LOCAL'%edate)) NE 0)
OTHERWISE
	SIGNAL ERROR:ERRORTEXT
END TRY	
	RETURN <case *>cons_series
END FUNCTION

--Used by function $get_febflow_cons_series_mul_filters
FUNCTION $get_4gl_expression
	ARGUMENT %expr, %loperatos, %obj_name 
	IGNORE ADD ON, MULT ON, FUNC ON
	LOCAL SCALAR !fameexpression: STRING = ""
	LOCAL SERIES !tseries:STRING BY CASE
	LOCAL SCALAR !csindex:NUMERIC = 1
	--DISP <case *> %expr, %loperatos, %obj_name 
	IF NOT MISSING(%expr) AND LENGTH(TRIM(%expr)) NE 0
		IF INISCAN(%expr)
			LOOP
				SET LOCAL'tseries[LOCAL'csindex] = SCAN
				ESCAPE IF LOCAL'tseries[LOCAL'csindex] EQ ND
				SET LOCAL'csindex = LOCAL'csindex + 1
			END LOOP
		END IF
	ELSE
		RETURN "EXPRESSSION_MISSING"
	END IF
	--DISP <case *> LOCAL'tseries		
	BLOCK
		CASE FIRSTVALUE(%loperatos) TO LASTVALUE(%loperatos)
		LOOP FOR ex in CASE
			LOOP FOR tsr = FIRSTVALUE(LOCAL'tseries) TO LASTVALUE(LOCAL'tseries)
				IF TRIM(LOCAL'tseries[tsr]) EQ %loperatos[ex] AND (TYPE(ID(%obj_name)) EQ "STRING")
					--SET LOCAL'tseries[tsr] = TRIM(REPLACE(TRIM(LOCAL'tseries[tsr]), %loperatos[ex], "TRIM(UPPER("+%obj_name+")) "+%loperatos[ex]))
					SET LOCAL'tseries[tsr] = TRIM(REPLACE(TRIM(LOCAL'tseries[tsr]), %loperatos[ex], "NOT MISSING(LOCATION(TRIM(UPPER("+%obj_name+")), "))
					--TYPE tsr,LOCAL'tseries[tsr]
				ELSE IF TRIM(LOCAL'tseries[tsr]) EQ %loperatos[ex] AND (TYPE(ID(%obj_name)) NE "STRING")
					SET LOCAL'tseries[tsr] = TRIM(REPLACE(TRIM(LOCAL'tseries[tsr]), %loperatos[ex], %obj_name+" "+%loperatos[ex]))
					--TYPE "ELSE",tsr,LOCAL'tseries[tsr]
				END IF
			END LOOP
		END LOOP
	END BLOCK	
	--DISP <case *> LOCAL'tseries	
	BLOCK
		LOOP FOR tsr = FIRSTVALUE(LOCAL'tseries) TO LASTVALUE(LOCAL'tseries)
			IF NOT MISSING(LOCATION(UPPER(LOCAL'tseries[tsr]),"MISSING()")) AND TYPE(ID(%obj_name)) EQ "STRING"
					SET LOCAL'tseries[tsr] = TRIM(REPLACE(LOCAL'tseries[tsr], "MISSING()", "MISSING("+%obj_name+")"))
			ELSE IF NOT MISSING(LOCATION(UPPER(LOCAL'tseries[tsr]),"MISSING()")) AND TYPE(ID(%obj_name)) EQ "NUMERIC" OR TYPE(ID(%obj_name)) EQ "PRECISION"
					SET LOCAL'tseries[tsr] = TRIM(REPLACE(LOCAL'tseries[tsr], "MISSING()", "MISSING("+%obj_name+")"))
			ELSE IF TYPE(ID(%obj_name)) NE "NUMERIC" AND TYPE(ID(%obj_name)) NE "PRECISION" 
				IF MISSING(LOCATION(LOCAL'tseries[tsr],%obj_name)) AND (TRIM(UPPER(LOCAL'tseries[tsr])) NE "AND") AND (TRIM(UPPER(LOCAL'tseries[tsr])) NE "OR")
					--SET LOCAL'tseries[tsr] = TRIM(QUOTE+LOCAL'tseries[tsr]+QUOTE)
					SET LOCAL'tseries[tsr] = TRIM(QUOTE+LOCAL'tseries[tsr]+QUOTE+"))")
				END IF
			END IF
		END LOOP
	END BLOCK	
	--DISP <case *> LOCAL'tseries			
	
	--Merge the expression
	LOOP FOR tsr = FIRSTVALUE(LOCAL'tseries) TO LASTVALUE(LOCAL'tseries)
		SET LOCAL'fameexpression = LOCAL'fameexpression + " " + TRIM(LOCAL'tseries[tsr])
	END LOOP
	--DISP LOCAL'fameexpression
	IF LENGTH(TRIM(LOCAL'fameexpression)) LE 3
		RETURN "EXPRESSSION_MISSING"
	ELSE
		RETURN "("+TRIM(LOCAL'fameexpression)+")"
	END IF
END FUNCTION

--Used by $get_febflow_cons_series_mul_filters
FUNCTION $get_all_institutes_data
ARGUMENTS %series_name, %sdate, %edate, series_dim
TRY
	LOCAL NEW tempcs = 1
	IF NOT MISSING(location(TYPE(ID("EXSE.FEB.FLOW."+series_dim+".C")),"DATE"))  
		LOCAL SERIES cons_series:DATE(DAILY) BY CASE
	ELSE
		EXECUTE "LOCAL SERIES cons_series:"+TYPE(ID("EXSE.FEB.FLOW."+series_dim+".C"))+" BY CASE"
	END IF
	
	IF (LENGTH(TRIM(LOCAL'%sdate)) NE 0) AND (LENGTH(TRIM(LOCAL'%edate)) NE 0)
		BLOCK
			DATE MAKE(DATE(DAILY),LOCAL'%sdate) TO MAKE(DATE(DAILY),LOCAL'%edate)
			FREQ MON
			LOOP FOR dt IN DATE
				--DISP dt
				IF MONTH(dt) LE 9
					LOCAL NEW monstr = "0"+STRING(MONTH(dt))
				ELSE
					LOCAL NEW monstr = STRING(MONTH(dt))
				END IF
				
				LOOP FOR %institue IN {AUA, BDC, CMB, RBCARUBA, FCIBARUBA, CBA}
					LOCAL NEW obj_name = "EXSE.FEB.FLOW."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+series_dim+".C"
					LOCAL NEW pk_obj_name = "EXSE.FEB.FLOW."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".PRIMKEY.C"
					--DISP LOCAL'obj_name
					IF EXISTS(ID(LOCAL'pk_obj_name)) AND NOT MISSING(FIRSTVALUE(ID(LOCAL'pk_obj_name)))
						CASE FIRSTVALUE(ID(LOCAL'pk_obj_name)) TO LASTVALUE(ID(LOCAL'pk_obj_name))
						--DISP LOCAL'obj_name, @CASE
						LOOP FOR cs IN CASE
							SET LOCAL'cons_series[LOCAL'tempcs] = ID(LOCAL'obj_name)[cs]
							SET LOCAL'tempcs = LOCAL'tempcs + 1
						END LOOP
					END IF
				END LOOP
			END LOOP
		END BLOCK
		
	ELSE
		SIGNAL ERROR : "$get_febflow_cons_series_mul_filters() : INVALID DATE RANGE"
	END IF --(LENGTH(TRIM(LOCAL'%sdate)) NE 0) AND (LENGTH(TRIM(LOCAL'%edate)) NE 0)
OTHERWISE
	SIGNAL ERROR:ERRORTEXT
END TRY
RETURN <CASE *> LOCAL'cons_series
END FUNCTION


--FUNCTION TO RETURN CONSOLIDATED FROM FEBFLOW SPLIT SERIES FOR ALL INSTITUTES
--Developed for edit case series forms
--clear output, info; cload <channel warnings none, results none>"customfame"; disp $get_febflow_cons_series_mul_filters_all_ins("EXSE.FEB.FLOW.ALL.RECO.C","01Apr2018 AND 30Apr2018","ALL","","","","","","","","","","","","","","","","","")
FUNCTION $get_febflow_cons_series_mul_filters_all_ins
ARGUMENTS %series_name, %sdate_and_date, %institute, %reid="", %acty="", %rena="", %nona="", %trco="", %isic="", %coco="", %cuco="",%fcde="",%afld="",%fccr="",%aflc="",%fec="",%lice="",%desc="",%cnc="",%cbar=""
OVER ON
FREQ DAILY
TRY
	LOCAL NEW temp_str = SUBSTRING(MIRROR(%series_name), LOCATION(MIRROR(%series_name), ".") +1, LENGTH(MIRROR(%series_name)))
	LOCAL NEW series_dim = MIRROR(SUBSTRING(LOCAL'temp_str, 0, LOCATION(LOCAL'temp_str, ".")-1))
	LOCAL SERIES <CASE 1 TO 6> !loperatos:STRING BY CASE = "GT", "GE", "LT", "LE", "EQ", "NE"
	LOCAL SERIES <CASE 1 TO 2> !boperatos:STRING BY CASE = "AND", "OR"
	LOCAL SCALAR !tempcs : NUMERIC = 1
	LOCAL SCALAR !queryStr : STRING
	LOCAL NEW temp_reid = %reid
	LOCAL NEW temp_trco = %trco
	LOCAL NEW temp_isic = %isic
	LOCAL NEW temp_fcde = %fcde
	LOCAL NEW temp_afld = %afld
	LOCAL NEW temp_fccr = %fccr
	LOCAL NEW temp_aflc = %aflc
	LOCAL NEW temp_fec	= %fec
	LOCAL NEW temp_acty = UPPER(%acty)
	LOCAL NEW temp_rena = UPPER(%rena)
	LOCAL NEW temp_nona = UPPER(%nona)
	LOCAL NEW temp_coco = UPPER(%coco)
	LOCAL NEW temp_cuco = UPPER(%cuco)
	LOCAL NEW temp_lice = UPPER(%lice)
	LOCAL NEW temp_desc = UPPER(%desc)
	LOCAL NEW temp_cnc 	= UPPER(%cnc)
	LOCAL NEW temp_cbar = UPPER(%cbar)
	
	
	IF NOT MISSING(location(TYPE(ID("EXSE.FEB.FLOW."+series_dim+".C")),"DATE"))  
		LOCAL SERIES cons_series:DATE(DAILY) BY CASE
	ELSE
		EXECUTE "LOCAL SERIES cons_series:"+TYPE(ID("EXSE.FEB.FLOW."+series_dim+".C"))+" BY CASE"
	END IF
	
	--Get the start and end date
	LOCAL NEW tdate = %sdate_and_date
	BLOCK
		CASE FIRSTVALUE(loperatos) TO LASTVALUE(loperatos)
		LOOP FOR ex in CASE
			SET LOCAL'tdate = REPLACE(LOCAL'tdate, loperatos[ex], "")
		END LOOP
		CASE FIRSTVALUE(boperatos) TO LASTVALUE(boperatos)
		LOOP FOR bl in CASE
			SET LOCAL'tdate = REPLACE(LOCAL'tdate, boperatos[bl], "")
		END LOOP
		LOCAL NEW %sdate = TRIM(SUBSTRING(TRIM(LOCAL'tdate), 1, LOCATION(TRIM(LOCAL'tdate)," ")))
		LOCAL NEW %edate = TRIM(SUBSTRING(TRIM(LOCAL'tdate), LOCATION(TRIM(LOCAL'tdate)," ")+1, LENGTH(TRIM(LOCAL'tdate))))
		--DISP LOCAL'tdate, LOCAL'%sdate, LOCAL'%edate
	END BLOCK
	
	--IF sdate and edate are present create series ignore CASE range
	IF (LENGTH(TRIM(LOCAL'%sdate)) NE 0) AND (LENGTH(TRIM(LOCAL'%edate)) NE 0)
		BLOCK
			DATE MAKE(DATE(DAILY),LOCAL'%sdate) TO MAKE(DATE(DAILY),LOCAL'%edate)
			FREQ MON
    	SET LOCAL'tempcs = 1
			LOOP FOR dt IN DATE
				--DISP dt
				IF MONTH(dt) LE 9
					LOCAL NEW monstr = "0"+STRING(MONTH(dt))
				ELSE
					LOCAL NEW monstr = STRING(MONTH(dt))
				END IF
				LOOP FOR %institue IN {AUA, BDC, CMB, RBCARUBA, FCIBARUBA, CBA}
					SET LOCAL'queryStr=""
					LOCAL NEW obj_name = "EXSE.FEB.FLOW."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C"
					LOCAL NEW pk_obj_name = "EXSE.FEB.FLOW."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".PRIMKEY.C"
					IF EXISTS(ID(LOCAL'pk_obj_name)) AND NOT MISSING(FIRSTVALUE(ID(LOCAL'pk_obj_name)))
					  --Convert expressions to FAMe 4GL expressions
					  --DISP "EXSE.FEB.FLOW."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".REID.C"
					  LOCAL NEW reid = $get_4gl_expression(LOCAL'temp_reid, loperatos, "EXSE.FEB.FLOW."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".REID.C")
					  LOCAL NEW trco = $get_4gl_expression(LOCAL'temp_trco, loperatos, "EXSE.FEB.FLOW."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".TRCO.C")
					  LOCAL NEW isic = $get_4gl_expression(LOCAL'temp_isic, loperatos, "EXSE.FEB.FLOW."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".ISIC.C")
					  LOCAL NEW fcde = $get_4gl_expression(LOCAL'temp_fcde, loperatos, "EXSE.FEB.FLOW."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".FCDE.C")
					  LOCAL NEW afld = $get_4gl_expression(LOCAL'temp_afld, loperatos, "EXSE.FEB.FLOW."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".AFLD.C")
					  LOCAL NEW fccr = $get_4gl_expression(LOCAL'temp_fccr, loperatos, "EXSE.FEB.FLOW."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".FCCR.C")
					  LOCAL NEW aflc = $get_4gl_expression(LOCAL'temp_aflc, loperatos, "EXSE.FEB.FLOW."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".AFLC.C")
					  LOCAL NEW fec  = $get_4gl_expression(LOCAL'temp_fec, loperatos, "EXSE.FEB.FLOW." +NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".FEC.C")
					  LOCAL NEW acty = $get_4gl_expression(LOCAL'temp_acty, loperatos, "EXSE.FEB.FLOW."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".ACTY.C")
					  LOCAL NEW rena = $get_4gl_expression(LOCAL'temp_rena, loperatos, "EXSE.FEB.FLOW."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".RENA.C")
					  LOCAL NEW nona = $get_4gl_expression(LOCAL'temp_nona, loperatos, "EXSE.FEB.FLOW."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".NONA.C")
					  LOCAL NEW coco = $get_4gl_expression(LOCAL'temp_coco, loperatos, "EXSE.FEB.FLOW."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".COCO.C")
					  LOCAL NEW cuco = $get_4gl_expression(LOCAL'temp_cuco, loperatos, "EXSE.FEB.FLOW."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".CUCO.C")
					  LOCAL NEW lice = $get_4gl_expression(LOCAL'temp_lice, loperatos, "EXSE.FEB.FLOW."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".LICE.C")
					  LOCAL NEW desc = $get_4gl_expression(LOCAL'temp_desc, loperatos, "EXSE.FEB.FLOW."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".DESC.C")
					  LOCAL NEW cnc  = $get_4gl_expression(LOCAL'temp_cnc, loperatos, "EXSE.FEB.FLOW." +NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".CNC.C")
					  LOCAL NEW cbar = $get_4gl_expression(LOCAL'temp_cbar, loperatos, "EXSE.FEB.FLOW."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".CBAR.C")
					  
					  --DISP LOCAL'reid	,LOCAL'acty,LOCAL'rena, LOCAL'nona, LOCAL'trco, LOCAL'isic, LOCAL'coco, LOCAL'cuco, LOCAL'fcde, LOCAL'afld, LOCAL'fccr, LOCAL'aflc
					  --DISP LOCAL'fec, LOCAL'lice, LOCAL'desc, LOCAL'cnc
					  --DISP LOCAL'cbar
					  --DISP LOCAL'obj_name				
					  IF EXISTS(ID(LOCAL'obj_name)) --AND NOT MISSING(FIRSTVALUE(ID(LOCAL'obj_name)))
					  	CASE *
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'reid))) AND LENGTH(TRIM(LOCAL'reid)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'reid), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'reid +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'trco))) AND LENGTH(TRIM(LOCAL'trco)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'trco), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'trco  +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'isic))) AND LENGTH(TRIM(LOCAL'isic)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'isic), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'isic +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'fcde))) AND LENGTH(TRIM(LOCAL'fcde)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'fcde), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'fcde +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'afld))) AND LENGTH(TRIM(LOCAL'afld)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'afld), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'afld +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'fccr))) AND LENGTH(TRIM(LOCAL'fccr)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'fccr), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'fccr +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'aflc))) AND LENGTH(TRIM(LOCAL'aflc)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'aflc), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'aflc +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'fec))) AND LENGTH(TRIM(LOCAL'fec)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'fec), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'fec +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'acty))) AND LENGTH(TRIM(LOCAL'acty)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'acty), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'acty +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'rena))) AND LENGTH(TRIM(LOCAL'rena)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'rena), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'rena +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'nona))) AND LENGTH(TRIM(LOCAL'nona)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'nona), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'nona  +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'coco))) AND LENGTH(TRIM(LOCAL'coco)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'coco), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'coco +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'cuco))) AND LENGTH(TRIM(LOCAL'cuco)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'cuco), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'cuco +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'lice))) AND LENGTH(TRIM(LOCAL'lice)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'lice), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'lice +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'desc))) AND LENGTH(TRIM(LOCAL'desc)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'desc), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'desc +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'cnc))) AND LENGTH(TRIM(LOCAL'cnc)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'cnc), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'cnc +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'cbar))) AND LENGTH(TRIM(LOCAL'cbar)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'cbar), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'cbar
					  	END IF
					  	--DISP LOCAL'queryStr
					  	IF NOT MISSING(LOCATION(SUBSTRING(MIRROR(TRIM(LOCAL'queryStr)),0,4), "DNA", 1))
					  		SET LOCAL'queryStr = TRIM(MIRROR(SUBSTRING(MIRROR(TRIM(LOCAL'queryStr)), LOCATION(MIRROR(TRIM(LOCAL'queryStr)), "DNA")+3, LENGTH(MIRROR(TRIM(LOCAL'queryStr)))))
					  	END IF
					  	--DISP LOCAL'queryStr
					  					      				
					  	
					  	IF NOT MISSING(local'queryStr) AND local'queryStr NE ""
					  		EXECUTE "WHICH "+local'queryStr
					  	ELSE
					  		CASE FIRSTVALUE(ID(LOCAL'pk_obj_name)) TO LASTVALUE(ID(LOCAL'pk_obj_name))
					  	END IF
					  	--DISPLAY DT,LENGTHCASE
					  	
 					  	IF LENGTHCASE GT 0
 					  		LOOP FOR cs IN CASE
 					  			DECIMAL 15
 					  			--TYPE LOCAL'tempcs, cs, ID(LOCAL'obj_name)[cs]
 					  			SET LOCAL'cons_series[LOCAL'tempcs] = ID(LOCAL'obj_name)[cs]
					  			SET LOCAL'tempcs = LOCAL'tempcs + 1
					  		END LOOP
 					  	END IF --LENGTHCASE GT 0
 					  	--whats LOCAL'cons_series
					  ELSE
					  	SIGNAL CONTINUE : ERRORTEXT
					  END IF --IF EXISTS(ID(LOCAL'obj_name) AND NOT MISSING(FIRSTVALUE(ID(LOCAL'obj_name)))
					  END IF --IF EXISTS(pk_obj_name)
				END LOOP --FOR %institue IN {AUA, BDC, CMB, RBCARUBA, FCIBARUBA, CBA}
			END LOOP --LOOP FOR dt in DATE
		END BLOCK
	ELSE
		SIGNAL ERROR : "$get_febflow_cons_series_mul_filters_all_ins() : INVALID DATE RANGE"
	END IF	--(LENGTH(TRIM(LOCAL'%sdate)) NE 0) AND (LENGTH(TRIM(LOCAL'%edate)) NE 0)
OTHERWISE
	SIGNAL ERROR:ERRORTEXT
END TRY	
	--CASE *
	RETURN <case *>cons_series	
	--RETURN ID(obj_name)
END FUNCTION



FUNCTION $SORT_DATA_FEBFLOWSPLIT_NOCBA
ARGUMENTS %DATE_OBJS,%DATE_OBJE
TRY
  OVER ON
  Ignore ON,ADD ON,MUL ON
  CASE *
  FREQ DAILY
  EXECUTE "DATE "+%DATE_OBJS+" TO "+%DATE_OBJE
  FREQ MON
  LOCAL SCALAR objexpression:STRING = BLANK
  IMAGE DATE MON "<mz><year>"
  LOCAL NEW month = SUBSTRING(@DATE, 0, 2)
  LOCAL NEW year = SUBSTRING(@DATE, 3, 6)
  
  LOOP FOR INS IN {AUA, CMB, BDC, AAB, IBA, FCIBARUBA, FNB, RBCARUBA}
  	IF EXISTS(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C")) AND NOT MISSING(FIRSTVALUE(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C")))
  		IF NAME(INS) EQ "AUA"
			IF NOT MISSING(LASTVALUE(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C")))
				SET LOCAL'objexpression = LOCAL'objexpression + "SORTDATA(EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C)"
			ELSE 
				SET LOCAL'objexpression = LOCAL'objexpression + "EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C"
			END IF
 		ELSE
			IF NOT MISSING(LASTVALUE(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C")))
				SET LOCAL'objexpression = LOCAL'objexpression + ",SORTDATA(EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C)"
			ELSE 
				SET LOCAL'objexpression = LOCAL'objexpression + ",EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C"
			END IF
  		END IF
  	END IF
  END LOOP
  IF LENGTH(TRIM(LOCAL'objexpression)) EQ 0
  	RETURN ND
  END IF
  SET LOCAL'objexpression = "MERGE("+LOCAL'objexpression+")"
   
  EXECUTE "NEW LOCAL'rettrco = "+ LOCAL'objexpression
  RETURN <CASE *>  LOCAL'rettrco
OTHERWISE
	SIGNAL ERROR: ERRORTEXT
END TRY
END FUNCTION

FUNCTION $GET_MONTHLY_BOP_DATA_FEBFLOWSPLIT_NOCBA
ARGUMENTS %DATE_OBJS, %DATE_OBJE, %SUMMED_VALUE_OBJECT_DIM
TRY
	OVER ON
  Ignore ON,ADD ON,MUL ON
  LOCAL SCALAR objexpression:STRING = BLANK
  LOCAL NEW SORTED_DATA = $SORT_DATA_FEBFLOWSPLIT_NOCBA(%DATE_OBJS, %DATE_OBJE)
  FREQ DAILY
  EXECUTE "DATE "+%DATE_OBJS+" TO "+%DATE_OBJE
  FREQ MON
  LOCAL SCALAR objexpression:STRING = BLANK
  IMAGE DATE MON "<mz><year>"
  LOCAL NEW month = SUBSTRING(@DATE, 0, 2)
  LOCAL NEW year = SUBSTRING(@DATE, 3, 6)
  
  IF INDEX(SORTED_DATA) EQ "CASE" AND LASTVALUE(SORTED_DATA) GT 0
  	CASE FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
    LOCAL SERIES RESULT : PRECISION BY CASE
	  LOOP FOR CODE=FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
	  	LOOP FOR INS IN {AUA,  CMB, BDC, AAB, IBA, FCIBARUBA, FNB, RBCARUBA}
	  		IF EXISTS(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%SUMMED_VALUE_OBJECT_DIM+".C"))
    		--SET RESULT[CODE] = $FILTER_DATA(EXSE.FEB.FLOW.TRDA.C,%FILTER_STMT+" AND  EXSE.FEB.FLOW.TRCO.C EQ "+string(SORTED_DATA[CODE]),%SUMMED_VALUE_OBJECT)
    		CASE *
    		WHICH ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C") EQ SORTED_DATA[CODE]
    		SET RESULT[CODE] = RESULT[CODE] + SUM(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%SUMMED_VALUE_OBJECT_DIM+".C"))
    		END IF
    	END LOOP
    END LOOP
  ELSE
   	 LOCAL NEW RESULT = ND
  END IF
  RETURN<case *> RESULT
OTHERWISE
	SIGNAL ERROR: ERRORTEXT
END TRY
END FUNCTION

FUNCTION $SORT_DATA_FEBFLOWSPLIT_CBA
ARGUMENTS %DATE_OBJS,%DATE_OBJE
TRY
  OVER ON
  Ignore ON,ADD ON,MUL ON
  CASE *
  FREQ DAILY
  EXECUTE "DATE "+%DATE_OBJS+" TO "+%DATE_OBJE
  FREQ MON
  LOCAL SCALAR objexpression:STRING = BLANK
  IMAGE DATE MON "<mz><year>"
  LOCAL NEW month = SUBSTRING(@DATE, 0, 2)
  LOCAL NEW year = SUBSTRING(@DATE, 3, 6)
  
  LOOP FOR INS IN {AUA,  CBA,  CMB, BDC, AAB, IBA, FCIBARUBA, FNB, RBCARUBA}
  	IF EXISTS(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C")) AND NOT MISSING(FIRSTVALUE(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C")))
  		IF NAME(INS) EQ "AUA"
			IF NOT MISSING(LASTVALUE(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C")))
				SET LOCAL'objexpression = LOCAL'objexpression + "SORTDATA(EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C)"
			ELSE 
				SET LOCAL'objexpression = LOCAL'objexpression + "EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C"
			END IF
 		ELSE
			IF NOT MISSING(LASTVALUE(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C")))
				SET LOCAL'objexpression = LOCAL'objexpression + ",SORTDATA(EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C)"
			ELSE 
				SET LOCAL'objexpression = LOCAL'objexpression + ",EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C"
			END IF
  		END IF
  	END IF
  END LOOP
  IF LENGTH(TRIM(LOCAL'objexpression)) EQ 0
  	RETURN ND
  END IF
  SET LOCAL'objexpression = "MERGE("+LOCAL'objexpression+")"
   
  EXECUTE "NEW LOCAL'rettrco = "+ LOCAL'objexpression
  RETURN <CASE *>  LOCAL'rettrco
OTHERWISE
	SIGNAL ERROR: ERRORTEXT
END TRY
END FUNCTION

FUNCTION $GET_MONTHLY_BOP_DATA_FEBFLOWSPLIT_CBA
ARGUMENTS %DATE_OBJS, %DATE_OBJE, %SUMMED_VALUE_OBJECT_DIM
TRY
	OVER ON
  Ignore ON,ADD ON,MUL ON
  LOCAL SCALAR objexpression:STRING = BLANK
  LOCAL NEW SORTED_DATA = $SORT_DATA_FEBFLOWSPLIT_CBA(%DATE_OBJS, %DATE_OBJE)
  FREQ DAILY
  EXECUTE "DATE "+%DATE_OBJS+" TO "+%DATE_OBJE
  FREQ MON
  LOCAL SCALAR objexpression:STRING = BLANK
  IMAGE DATE MON "<mz><year>"
  LOCAL NEW month = SUBSTRING(@DATE, 0, 2)
  LOCAL NEW year = SUBSTRING(@DATE, 3, 6)
  
  IF INDEX(SORTED_DATA) EQ "CASE" AND LASTVALUE(SORTED_DATA) GT 0
  	CASE FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
    LOCAL SERIES RESULT : PRECISION BY CASE
	  LOOP FOR CODE=FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
	  	LOOP FOR INS IN {AUA,  CBA,  CMB, BDC, AAB, IBA, FCIBARUBA, FNB, RBCARUBA}
	  		IF EXISTS(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%SUMMED_VALUE_OBJECT_DIM+".C")) AND NOT MISSING(FIRSTVALUE(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%SUMMED_VALUE_OBJECT_DIM+".C")))
    		--SET RESULT[CODE] = $FILTER_DATA(EXSE.FEB.FLOW.TRDA.C,%FILTER_STMT+" AND  EXSE.FEB.FLOW.TRCO.C EQ "+string(SORTED_DATA[CODE]),%SUMMED_VALUE_OBJECT)
    		CASE *
    		WHICH ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C") EQ SORTED_DATA[CODE]
    		SET RESULT[CODE] = RESULT[CODE] + SUM(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%SUMMED_VALUE_OBJECT_DIM+".C"))
    		END IF
    	END LOOP
    END LOOP
  ELSE
   	 LOCAL NEW RESULT = ND
  END IF
  RETURN<case *> RESULT
OTHERWISE
	SIGNAL ERROR: ERRORTEXT
END TRY
END FUNCTION

--clear info;clear;disp $GET_MONTHLY_BOP_DATA_FEBFLOWSPLIT_NOCBA_FOREX("1jan2018", "31jan2018", "FCCR")
FUNCTION $GET_MONTHLY_BOP_DATA_FEBFLOWSPLIT_NOCBA_FOREX
ARGUMENTS %DATE_OBJS, %DATE_OBJE, %SUMMED_VALUE_OBJECT_DIM
TRY
	OVER ON
  Ignore ON,ADD ON,MUL ON
  LOCAL SCALAR !objexpression:STRING = BLANK
  LOCAL SERIES !prod_result:PRECISION BY CASE
  LOCAL NEW SORTED_DATA = $SORT_DATA_FEBFLOWSPLIT_NOCBA(%DATE_OBJS, %DATE_OBJE)
  FREQ DAILY
  EXECUTE "DATE "+%DATE_OBJS+" TO "+%DATE_OBJE
  FREQ MON
  LOCAL SCALAR objexpression:STRING = BLANK
  IMAGE DATE MON "<mz><year>"
  LOCAL NEW month = SUBSTRING(@DATE, 0, 2)
  LOCAL NEW year = SUBSTRING(@DATE, 3, 6)
  
  IF INDEX(SORTED_DATA) EQ "CASE" AND LASTVALUE(SORTED_DATA) GT 0
  	CASE FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
    LOCAL SERIES RESULT : PRECISION BY CASE
	  LOOP FOR CODE=FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
	  	--DISP SORTED_DATA[CODE]
	  	LOOP FOR INS IN {AUA,  CMB, BDC, AAB, IBA, FCIBARUBA, FNB, RBCARUBA}
	  		IF EXISTS(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%SUMMED_VALUE_OBJECT_DIM+".C")) AND NOT MISSING(FIRSTVALUE(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%SUMMED_VALUE_OBJECT_DIM+".C")))
    			CASE *
    			WHICH ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C") EQ SORTED_DATA[CODE]
    			EXECUTE "LOCAL NEW filtererdCUCO = EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".CUCO.C"
    			--DISP LENGTHCASE    			
    			--report<show v; len full> ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C"),ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%SUMMED_VALUE_OBJECT_DIM+".C"), &&
    			--ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".CUCO.C"), $GET_EXCH_RATE_FOREX(LOCAL'filtererdCUCO, %DATE_OBJS, %DATE_OBJE, "MONTHLY", @CASE) AS "EXCH_RATE", &&
    			--ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%SUMMED_VALUE_OBJECT_DIM+".C")*$GET_EXCH_RATE_FOREX(LOCAL'filtererdCUCO, %DATE_OBJS, %DATE_OBJE, "MONTHLY", @CASE) AS "final_val"
    			--GET EXCH RATES
    			BLOCK
					IF INDEX(LOCAL'filtererdCUCO) EQ "CASE" AND NOT MISSING(LASTVALUE(LOCAL'filtererdCUCO))
    					--CASE FIRSTVALUE(LOCAL'filtererdCUCO) TO LASTVALUE(LOCAL'filtererdCUCO)
							LOCAL SERIES EXCH_RATE : PRECISION BY CASE
   						FREQ DAILY
   						EXECUTE "DATE "+%DATE_OBJS+" TO "+ %DATE_OBJE
   						--DISP @DATE
   						FREQ MONTHLY
    					--DISP @DATE, @CASE, @FREQ, LOCAL'filtererdCUCO
  						LOOP FOR CURR IN CASE	
						IF LOCAL'filtererdCUCO[CURR] NE "ND"
							EXECUTE "LOCAL SCALAR RS = CONVERT(EXSE.FEB.EXRA."+LOCAL'filtererdCUCO[CURR]+".MIDD.D,MONTHLY,DISCRETE,AVERAGED)"
						ELSE
							LOCAL SCALAR RS = 1
						END IF
     						IF RS NE ND AND RS NE NC
      		    			IF UPPER(LOCAL'filtererdCUCO[CURR])  EQ "ITL"
      		           		SET RS = RS/1000
	    		          ELSE IF UPPER(LOCAL'filtererdCUCO[CURR])  EQ "JPY"
    									SET RS = RS/10000	                    
										ELSE IF UPPER(LOCAL'filtererdCUCO[CURR])  EQ "AWG" OR UPPER(LOCAL'filtererdCUCO[CURR])  EQ "ANG" OR UPPER(LOCAL'filtererdCUCO[CURR])  EQ "GBP" OR UPPER(LOCAL'filtererdCUCO[CURR])  EQ "CAD" OR UPPER(LOCAL'filtererdCUCO[CURR]) EQ "USD" OR LOCAL'filtererdCUCO[CURR] EQ "AFL"
      		    	   		SET RS = RS/1
      		         	ELSE
      		           	SET RS = RS/100
      		         END IF
      		      	SET EXCH_RATE[CURR] = RS
      					END IF
		  		    END LOOP
						END IF
    			END BLOCK
    				SET LOCAL'prod_result[CODE] = LOCAL'prod_result[CODE] + SUM(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%SUMMED_VALUE_OBJECT_DIM+".C")*EXCH_RATE
    			--DISP LOCAL'prod_result[CODE]
    		END IF
    	END LOOP
    END LOOP --FOR CODE=FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
  ELSE
   	 LOCAL NEW LOCAL'prod_result = ND
  END IF

  RETURN<case *> LOCAL'prod_result
OTHERWISE
	SIGNAL ERROR: ERRORTEXT
END TRY
END FUNCTION

FUNCTION $GET_EXCH_RATE_FOREX
ARGUMENTS %CURR_LIST,%DATES, %DATEE, %FREQ, %caserange
    	TRY
			OVER ON
			IGNORE ON,ADD ON,MUL ON,FUNC ON
			IF INDEX(%CURR_LIST) EQ "CASE" AND NOT MISSING(LASTVALUE(%CURR_LIST))
					EXECUTE "CASE "+%caserange
    			--CASE FIRSTVALUE(%CURR_LIST) TO LASTVALUE(%CURR_LIST)
					LOCAL SERIES EXCH_RATE : PRECISION BY CASE
   				FREQ DAILY
   				EXECUTE "DATE "+%DATES+" TO "+ %DATEE
   				--DISP @DATE
   				EXECUTE "FREQ "+%FREQ
    			--DISP @DATE, @CASE, @FREQ, %CURR_LIST
  				LOOP FOR CURR IN CASE	
       			IF %CURR_LIST[CURR] NE "ND"
					EXECUTE "LOCAL SCALAR RS = CONVERT(EXSE.FEB.EXRA."+%CURR_LIST[CURR]+".MIDD.D,"+STRING(%FREQ)+",DISCRETE,AVERAGED)"
				ELSE 
					LOCAL SCALAR RS = 1
				END IF 
     				IF RS NE ND AND RS NE NC
          			IF UPPER(%CURR_LIST[CURR])  EQ "ITL"
                 		SET RS = RS/1000
	              ELSE IF UPPER(%CURR_LIST[CURR])  EQ "JPY"
    							SET RS = RS/10000	                    
								ELSE IF UPPER(%CURR_LIST[CURR])  EQ "AWG" OR UPPER(%CURR_LIST[CURR])  EQ "ANG" OR UPPER(%CURR_LIST[CURR])  EQ "GBP" OR UPPER(%CURR_LIST[CURR])  EQ "CAD" OR UPPER(%CURR_LIST[CURR])  EQ "USD"  OR UPPER(%CURR_LIST[CURR])  EQ "AFL" 
          	   		SET RS = RS/1
               	ELSE
                 	SET RS = RS/100
               END IF
            	SET EXCH_RATE[CURR] = RS
      			END IF
		      END LOOP
					RETURN EXCH_RATE
				ELSE
					RETURN ND
				END IF
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION

--clear info;clear;disp $GET_MONTHLY_BOP_DATA_FEBFLOWSPLIT_CBA_FOREX("1jan2018", "31jan2018", "FCCR")
FUNCTION $GET_MONTHLY_BOP_DATA_FEBFLOWSPLIT_CBA_FOREX
ARGUMENTS %DATE_OBJS, %DATE_OBJE, %SUMMED_VALUE_OBJECT_DIM
TRY
  OUTPUT <acc overwrite> FILE("D:\map\logs\procslog3.txt");
  INFO <acc overwrite> FILE("D:\map\logs\Infoprocslog3.txt");
  OVER ON
  Ignore ON,ADD ON,MUL ON
  LOCAL SCALAR !objexpression:STRING = BLANK
  LOCAL SERIES !prod_result:PRECISION BY CASE
  LOCAL NEW SORTED_DATA = $SORT_DATA_FEBFLOWSPLIT_CBA(%DATE_OBJS, %DATE_OBJE)
  FREQ DAILY
  EXECUTE "DATE "+%DATE_OBJS+" TO "+%DATE_OBJE
  FREQ MON
  LOCAL SCALAR objexpression:STRING = BLANK
  IMAGE DATE MON "<mz><year>"
  LOCAL NEW month = SUBSTRING(@DATE, 0, 2)
  LOCAL NEW year = SUBSTRING(@DATE, 3, 6)
  
  IF INDEX(SORTED_DATA) EQ "CASE" AND LASTVALUE(SORTED_DATA) GT 0
  	CASE FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
    LOCAL SERIES RESULT : PRECISION BY CASE
	  LOOP FOR CODE=FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
	  	DISP CODE
	  	LOOP FOR INS IN {AUA, CBA, CMB , BDC, AAB, IBA, FCIBARUBA, FNB, RBCARUBA}
	  		IF EXISTS(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%SUMMED_VALUE_OBJECT_DIM+".C")) AND NOT MISSING(FIRSTVALUE(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%SUMMED_VALUE_OBJECT_DIM+".C")))
    			CASE *
    			WHICH ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C") EQ SORTED_DATA[CODE]
    			EXECUTE "LOCAL NEW filtererdCUCO = EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".CUCO.C"
				-- DISP "LOCAL NEW filtererdCUCO = EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".CUCO.C"
    			-- DISP LENGTHCASE    			
    			-- DISP ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C"),ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%SUMMED_VALUE_OBJECT_DIM+".C"), &&
    			-- ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".CUCO.C"), $GET_EXCH_RATE_FOREX(LOCAL'filtererdCUCO, %DATE_OBJS, %DATE_OBJE, "MONTHLY", @CASE) AS "EXCH_RATE", &&
    			-- ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%SUMMED_VALUE_OBJECT_DIM+".C")*$GET_EXCH_RATE_FOREX(LOCAL'filtererdCUCO, %DATE_OBJS, %DATE_OBJE, "MONTHLY", @CASE) AS "final_val"
    			BLOCK
					IF INDEX(LOCAL'filtererdCUCO) EQ "CASE" AND NOT MISSING(LASTVALUE(LOCAL'filtererdCUCO))
    					--CASE FIRSTVALUE(LOCAL'filtererdCUCO) TO LASTVALUE(LOCAL'filtererdCUCO)
							LOCAL SERIES EXCH_RATE : PRECISION BY CASE
   						FREQ DAILY
   						EXECUTE "DATE "+%DATE_OBJS+" TO "+ %DATE_OBJE
   						-- DISP @DATE
   						FREQ MONTHLY
    					-- DISP @DATE, @CASE, @FREQ, LOCAL'filtererdCUCO
  						LOOP FOR CURR IN CASE	
      		 			IF LOCAL'filtererdCUCO[CURR] NE "ND"
							EXECUTE "LOCAL SCALAR RS = CONVERT(EXSE.FEB.EXRA."+LOCAL'filtererdCUCO[CURR]+".MIDD.D,MONTHLY,DISCRETE,AVERAGED)"
						ELSE
							LOCAL SCALAR RS = 1
						END IF
						IF RS NE ND AND RS NE NC
							IF UPPER(LOCAL'filtererdCUCO[CURR])  EQ "ITL"
								SET RS = RS/1000
							ELSE IF UPPER(LOCAL'filtererdCUCO[CURR])  EQ "JPY"
									SET RS = RS/10000	                    
							ELSE IF UPPER(LOCAL'filtererdCUCO[CURR])  EQ "AWG" OR UPPER(LOCAL'filtererdCUCO[CURR])  EQ "ANG" OR UPPER(LOCAL'filtererdCUCO[CURR])  EQ "GBP" OR UPPER(LOCAL'filtererdCUCO[CURR])  EQ "CAD" OR UPPER(LOCAL'filtererdCUCO[CURR])  EQ "USD" OR UPPER(LOCAL'filtererdCUCO[CURR])  EQ "AFL" 
								SET RS = RS/1
							ELSE
								SET RS = RS/100
							END IF
						SET EXCH_RATE[CURR] = RS
      					END IF
		  		    END LOOP
						END IF
    			END BLOCK
    			
    				SET LOCAL'prod_result[CODE] = LOCAL'prod_result[CODE] + SUM(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%SUMMED_VALUE_OBJECT_DIM+".C")*EXCH_RATE
					DISP LOCAL'prod_result[CODE]
    		END IF
    	END LOOP
    END LOOP --FOR CODE=FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
  ELSE
   	 LOCAL NEW LOCAL'prod_result = ND
  END IF

  RETURN<case *> LOCAL'prod_result
OTHERWISE
	DISP ERRORTEXT
	SIGNAL ERROR: ERRORTEXT
END TRY
END FUNCTION

--clear;disp<case *> $GET_MONTHLY_BOP_DATA_FEBFLOWSPLIT_NOCBA_OLDOBJS_FOREX("1Jan2017", "31Jan2017", "FCDE")
FUNCTION $GET_MONTHLY_BOP_DATA_FEBFLOWSPLIT_NOCBA_OLDOBJS_FOREX
ARGUMENTS %DATE_OBJS, %DATE_OBJE, %SUMMED_VALUE_OBJECT_DIM
TRY
	OVER ON
  Ignore ON,ADD ON,MUL ON
  LOCAL SCALAR !whichexpr:STRING = BLANK
  LOCAL SERIES !avg_exch_rate:PRECISION BY CASE
  LOCAL SERIES !prod_result:PRECISION BY CASE
  FREQ DAILY
  EXECUTE "DATE "+%DATE_OBJS+" TO "+%DATE_OBJE
  
  --SET THE WHICH EXPRESSION FOR DMB REPORT WHICH EXCLUDES CBA
	LOOP FOR INS IN {AUA,BDC,CMB,FNB,IBA,RBT,FCI}
		IF NAME(INS) EQ "AUA"
			SET whichexpr = whichexpr + "UPPER(EXSE.FEB.FLOW.RECO.C) EQ "+QUOTE+NAME(INS)+QUOTE
		ELSE
			SET whichexpr = whichexpr + " OR UPPER(EXSE.FEB.FLOW.RECO.C) EQ "+QUOTE+NAME(INS)+QUOTE
		END IF
	END LOOP
 	--DISP whichexpr
  --Get the unique trancaction codes
  CASE *
  EXECUTE "WHICH EXSE.FEB.FLOW.DATEINDEX.C GE "+%DATE_OBJS+" AND EXSE.FEB.FLOW.DATEINDEX.C LE "+%DATE_OBJE
  --DISP LENGTHCASE
  EXECUTE "WHICH "+whichexpr
 	--DISP LENGTHCASE, @CASE
 	
 	BLOCK
	IF NOT MISSING(LENGTHCASE)
  		--CASE FIRSTVALUE(LOCAL'filtererdCUCO) TO LASTVALUE(LOCAL'filtererdCUCO)
		LOCAL SERIES avg_exch_rate : PRECISION BY CASE
  		FREQ DAILY
  		EXECUTE "DATE "+%DATE_OBJS+" TO "+ %DATE_OBJE
  		--DISP @DATE
  		FREQ MONTHLY
  		--DISP @DATE, @CASE, @FREQ, LOCAL'filtererdCUCO
  	LOOP FOR CURR IN CASE	
		IF EXSE.FEB.FLOW.CUCO.C[CURR] NE "ND"
  	 		EXECUTE "LOCAL SCALAR RS = CONVERT(EXSE.FEB.EXRA."+EXSE.FEB.FLOW.CUCO.C[CURR]+".MIDD.D,MONTHLY,DISCRETE,AVERAGED)"
		ELSE
			LOCAL SCALAR RS = 1
		END IF
  				IF RS NE ND AND RS NE NC
  	    			IF UPPER(EXSE.FEB.FLOW.CUCO.C[CURR]) EQ "ITL"
  	           		SET RS = RS/1000
		          ELSE IF UPPER(EXSE.FEB.FLOW.CUCO.C[CURR]) EQ "JPY"
  						SET RS = RS/10000	                    
					ELSE IF UPPER(EXSE.FEB.FLOW.CUCO.C[CURR]) EQ "AWG" OR UPPER(EXSE.FEB.FLOW.CUCO.C[CURR]) EQ "ANG" OR UPPER(EXSE.FEB.FLOW.CUCO.C[CURR]) EQ "GBP" OR UPPER(EXSE.FEB.FLOW.CUCO.C[CURR]) EQ "CAD" OR UPPER(EXSE.FEB.FLOW.CUCO.C[CURR]) EQ "USD" OR UPPER(EXSE.FEB.FLOW.CUCO.C[CURR]) EQ "AFL"
  	    	   		SET RS = RS/1
  	         	ELSE
  	           	SET RS = RS/100
  	         END IF
  	      	SET avg_exch_rate[CURR] = RS
  				END IF
	    END LOOP
	END IF
  END BLOCK

 	IF NOT MISSING(LENGTHCASE)
 		
 		LOCAL NEW filtTRCO = EXSE.FEB.FLOW.TRCO.C
 		LOCAL NEW filtmult = ID("EXSE.FEB.FLOW."+%SUMMED_VALUE_OBJECT_DIM+".C") * avg_exch_rate
 		--whats filtTRCO, filtmult
 		--REPO <show v; len full; width 2000> EXSE.FEB.FLOW.CUCO.C, EXSE.FEB.FLOW.TRCO.C, ID("EXSE.FEB.FLOW."+%SUMMED_VALUE_OBJECT_DIM+".C"), avg_exch_rate, ID("EXSE.FEB.FLOW."+%SUMMED_VALUE_OBJECT_DIM+".C") * avg_exch_rate AS "MULT"	, filtTRCO, filtmult
 		BLOCK
 			LOCAL NEW uniqueTRCO = MERGE(SORTDATA(filtTRCO))
 			--DISP <case *>LOCAL'uniqueTRCO;whats LOCAL'uniqueTRCO
 			CASE FIRSTVALUE(LOCAL'uniqueTRCO) TO LASTVALUE(LOCAL'uniqueTRCO)
 			LOOP FOR x IN CASE
 				BLOCK
 					CASE *
 					WHICH filtTRCO EQ LOCAL'uniqueTRCO[x]
 					--REPO <show v; len full; width 2000> EXSE.FEB.FLOW.CUCO.C, EXSE.FEB.FLOW.TRCO.C, ID("EXSE.FEB.FLOW."+%SUMMED_VALUE_OBJECT_DIM+".C"), avg_exch_rate, ID("EXSE.FEB.FLOW."+%SUMMED_VALUE_OBJECT_DIM+".C") * avg_exch_rate AS "MULT"	, filtTRCO, filtmult
 					SET LOCAL'prod_result[x] = SUM(filtmult)
 				END BLOCK	
 			END LOOP
 		END BLOCK
 	ELSE
 		LOCAL NEW prod_result = ND
 	END IF
 	
  RETURN<case *> prod_result
OTHERWISE
	SIGNAL ERROR: ERRORTEXT
END TRY
END FUNCTION

--clear;disp<case *> $GET_MONTHLY_BOP_DATA_FEBFLOWSPLIT_NOCBA_OLDOBJS_FOREX("1Jan2017", "31Jan2017", "FCDE")
FUNCTION $GET_MONTHLY_BOP_DATA_FEBFLOWSPLIT_CBA_OLDOBJS_FOREX
ARGUMENTS %DATE_OBJS, %DATE_OBJE, %SUMMED_VALUE_OBJECT_DIM
TRY
	OVER ON
  Ignore ON,ADD ON,MUL ON
  LOCAL SCALAR !whichexpr:STRING = BLANK
  LOCAL SERIES !avg_exch_rate:PRECISION BY CASE
  LOCAL SERIES !prod_result:PRECISION BY CASE
  FREQ DAILY
  EXECUTE "DATE "+%DATE_OBJS+" TO "+%DATE_OBJE
  
  --SET THE WHICH EXPRESSION FOR DMB+CBA REPORT WHICH INCLUDES CBA
	LOOP FOR INS IN {AUA,CBA,BDC,CMB,FNB,IBA,RBT,FCI}
		IF NAME(INS) EQ "AUA"
			SET whichexpr = whichexpr + "UPPER(EXSE.FEB.FLOW.RECO.C) EQ "+QUOTE+NAME(INS)+QUOTE
		ELSE
			SET whichexpr = whichexpr + " OR UPPER(EXSE.FEB.FLOW.RECO.C) EQ "+QUOTE+NAME(INS)+QUOTE
		END IF
	END LOOP
 	--DISP whichexpr
  --Get the unique trancaction codes
  CASE *
  EXECUTE "WHICH EXSE.FEB.FLOW.DATEINDEX.C GE "+%DATE_OBJS+" AND EXSE.FEB.FLOW.DATEINDEX.C LE "+%DATE_OBJE
  --DISP LENGTHCASE
  EXECUTE "WHICH "+whichexpr
 	--DISP LENGTHCASE, @CASE
 	
 	 	BLOCK
	IF NOT MISSING(LENGTHCASE)
  		--CASE FIRSTVALUE(LOCAL'filtererdCUCO) TO LASTVALUE(LOCAL'filtererdCUCO)
		LOCAL SERIES avg_exch_rate : PRECISION BY CASE
  		FREQ DAILY
  		EXECUTE "DATE "+%DATE_OBJS+" TO "+ %DATE_OBJE
  		--DISP @DATE
  		FREQ MONTHLY
  		--DISP @DATE, @CASE, @FREQ, LOCAL'filtererdCUCO
  	LOOP FOR CURR IN CASE	
  	 				IF EXSE.FEB.FLOW.CUCO.C[CURR] NE "ND"
  	 		EXECUTE "LOCAL SCALAR RS = CONVERT(EXSE.FEB.EXRA."+EXSE.FEB.FLOW.CUCO.C[CURR]+".MIDD.D,MONTHLY,DISCRETE,AVERAGED)"
		ELSE
			LOCAL SCALAR RS = 1
		END IF
  				IF RS NE ND AND RS NE NC
  	    			IF UPPER(EXSE.FEB.FLOW.CUCO.C[CURR]) EQ "ITL"
  	           		SET RS = RS/1000
		          ELSE IF UPPER(EXSE.FEB.FLOW.CUCO.C[CURR]) EQ "JPY"
  						SET RS = RS/10000	                    
					ELSE IF UPPER(EXSE.FEB.FLOW.CUCO.C[CURR]) EQ "AWG" OR UPPER(EXSE.FEB.FLOW.CUCO.C[CURR]) EQ "ANG" OR UPPER(EXSE.FEB.FLOW.CUCO.C[CURR]) EQ "GBP" OR UPPER(EXSE.FEB.FLOW.CUCO.C[CURR]) EQ "CAD" OR UPPER(EXSE.FEB.FLOW.CUCO.C[CURR]) EQ "USD" OR UPPER(EXSE.FEB.FLOW.CUCO.C[CURR]) EQ "AFL"
  	    	   		SET RS = RS/1
  	         	ELSE
  	           	SET RS = RS/100
  	         END IF
  	      	SET avg_exch_rate[CURR] = RS
  				END IF
	    END LOOP
	END IF
  END BLOCK
 	IF NOT MISSING(LENGTHCASE)
 		
 		LOCAL NEW filtTRCO = EXSE.FEB.FLOW.TRCO.C
 		LOCAL NEW filtmult = ID("EXSE.FEB.FLOW."+%SUMMED_VALUE_OBJECT_DIM+".C") * avg_exch_rate
 		--whats filtTRCO, filtmult
 		--REPO <show v; len full; width 2000> EXSE.FEB.FLOW.CUCO.C, EXSE.FEB.FLOW.TRCO.C, ID("EXSE.FEB.FLOW."+%SUMMED_VALUE_OBJECT_DIM+".C"), avg_exch_rate, ID("EXSE.FEB.FLOW."+%SUMMED_VALUE_OBJECT_DIM+".C") * avg_exch_rate AS "MULT"	, filtTRCO, filtmult
 		BLOCK
 			LOCAL NEW uniqueTRCO = MERGE(SORTDATA(filtTRCO))
 			--DISP <case *>LOCAL'uniqueTRCO;whats LOCAL'uniqueTRCO
 			CASE FIRSTVALUE(LOCAL'uniqueTRCO) TO LASTVALUE(LOCAL'uniqueTRCO)
 			LOOP FOR x IN CASE
 				BLOCK
 					CASE *
 					WHICH filtTRCO EQ LOCAL'uniqueTRCO[x]
 					--REPO <show v; len full; width 2000> EXSE.FEB.FLOW.CUCO.C, EXSE.FEB.FLOW.TRCO.C, ID("EXSE.FEB.FLOW."+%SUMMED_VALUE_OBJECT_DIM+".C"), avg_exch_rate, ID("EXSE.FEB.FLOW."+%SUMMED_VALUE_OBJECT_DIM+".C") * avg_exch_rate AS "MULT"	, filtTRCO, filtmult
 					SET LOCAL'prod_result[x] = SUM(filtmult)
 				END BLOCK	
 			END LOOP
 		END BLOCK
 	ELSE
 		LOCAL NEW prod_result = ND
 	END IF
 	
  RETURN<case *> prod_result
OTHERWISE
	SIGNAL ERROR: ERRORTEXT
END TRY
END FUNCTION




---------------------------ANIKET-------------------
FUNCTION $SORT_RECONCILE_OBJECTDATA
ARGUMENTS %DATE_OBJS,%OBJ_RET_DIM
 
 TRY
	FREQ M
	OVER on
	EXECUTE "DATE "+%DATE_OBJS
	IGNORE ADD ON
	CASE *
	LOCAL series mergeCode:STRING by case
	LOCAL SCALAR lastvalCs:NUMERIC
	set lastvalCs=1
	IMAGE DATE MON "<mz><year>"

    LOCAL NEW month=SUBSTRING(@DATE, 0, 2)
    LOCAL NEW year=SUBSTRING(@DATE, 3, 6)
	LOCAL SER TEMP1:STRING BY CASE
	LOCAL SER TEMP2:PRECISION BY CASE
	
		LOOP FOR INS IN {AUA,  CBA,  CMB, BDC, AAB, IBA, FCIBARUBA, FNB, RBCARUBA}
			IF EXISTS(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".DESC.C")) AND EXISTS(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C"))
				case *
				IF NOT MISSING(LASTVALUE(TEMP2)) 
					case firstvalue(TEMP2) to lastvalue(TEMP2)
						SET TEMP1=ND
						SET TEMP2=ND
					CASE *
				END IF
				
				WHICH ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C") GE 5000 AND ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C") LT 5555
				IF lengthcase GT 0
					SET TEMP1[CASEORDER]=ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".DESC.C")
					SET TEMP2[CASEORDER]=ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C")
					CASE *
								
     			LOCAL NEW COUNT = LASTVALUE(mergeCode)+1
					CASE firstVALUE(TEMP2) TO LASTVALUE(TEMP2)
    	            LOOP FOR CS in case
						set mergeCode[LOCAL'COUNT] = UPPER(SUBSTRING(TEMP1[CS],0,5))+"_"+PRECFMT(TEMP2[CS])
				      SET LOCAL'COUNT = LOCAL'COUNT + 1
				      end loop
     				CASE *    			
										
				END IF
			END IF
		END LOOP
	case * 
	IF NOT MISSING(LASTVALUE(mergeCode))
		LOCAL NEW mergeCode2=merge(sortdata(mergeCode))
  	
		RETURN mergeCode2
 
	ELSE 
		RETURN ND
	END IF
	
 OTHERWISE
	SIGNAL ERROR: ERRORTEXT
 END TRY
END FUNCTION


FUNCTION $GET_SORTED_OBJECTDATA
ARGUMENTS %DATE_OBJS,%OBJ_RET_DIM
 
 TRY
	freq m
	OVER on
	EXECUTE "DATE "+%DATE_OBJS
	IGNORE ADD ON
	CASE *
	LOCAL series mergeCoded:STRING by case
	LOCAL SCALAR lastvalCs2:NUMERIC
	set lastvalCs2=1
	
   
    LOCAL SCALAR month:STRING = BLANK
    LOCAL SCALAR year:STRING = BLANK
	
	
	LOOP FOR X IN DATE
		LOCAL NEW TEMP3=$SORT_RECONCILE_OBJECTDATA(string(X),%OBJ_RET_DIM)

		case *
		IF NOT MISSING(LASTVALUE(TEMP3))
			CASE LASTVALUE(mergeCoded)+1 TO LASTVALUE(mergeCoded)+LASTVALUE(TEMP3)
			SET mergeCoded=TEMP3[CASEORDER]
		END IF
	END LOOP
	
	IF NOT MISSING(LASTVALUE(mergeCoded)) OR LASTVALUE(mergeCoded) GT 1
		case *
  
		LOCAL NEW mergeCoded1=merge(sortdata(mergeCoded))
		LOCAL SERIES TEMPDESC:STRING by case
		LOCAL SERIES TEMPTRCO:PRECISION by case
 	
		
		LOOP FOR v=FIRSTVALUE(mergeCoded1) to LASTVALUE(mergeCoded1)
			SET TEMPDESC[v]=SUBSTRING(LOCAL'mergeCoded1[v], 0,LOCATION(LOCAL'mergeCoded1[v],"_")-1)
			SET TEMPTRCO[v]=NUMBER(SUBSTRING(LOCAL'mergeCoded1[v], LOCATION(LOCAL'mergeCoded1[v],"_")+1,LENGTH(LOCAL'mergeCoded1[v])))
		END LOOP
		
		IF %OBJ_RET_DIM EQ "DESC"
			RETURN TEMPDESC
		ELSE IF %OBJ_RET_DIM EQ "TRCO"
			RETURN TEMPTRCO
		ELSE 
		LOCAL SCALAR SUMVALUE:PRECISION 
		set SUMVALUE=0
		LOCAL SERIES SUMVALUE2:PRECISION BY CASE
		LOOP FOR v=FIRSTVALUE(mergeCoded1) to LASTVALUE(mergeCoded1)
       	EXECUTE "DATE "+%DATE_OBJS
			LOOP FOR X IN DATE
				freq m
				date x
    
				IMAGE DATE MON "<mz><year>"
				set month=SUBSTRING(@DATE, 0, 2)
				set year=SUBSTRING(@DATE, 3, 6)
				LOOP FOR INS IN {AUA,  CBA,  CMB, BDC, AAB, IBA, FCIBARUBA, FNB, RBCARUBA}
					IF EXISTS(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%OBJ_RET_DIM+".C"))
						case *
						WHICH ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C") EQ TEMPTRCO[v]
   				        EXECUTE "WHICH UPPER(SUBSTRING(EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".DESC.C,0,5)) EQ "+QUOTE+TEMPDESC[v]+QUOTE
							set SUMVALUE=SUMVALUE+SUM(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%OBJ_RET_DIM+".C"))
					END IF
				END LOOP
			END LOOP
			case *
			SET SUMVALUE2[v]=SUMVALUE
			SET SUMVALUE=0
		END LOOP
		RETURN SUMVALUE2
		END IF
	ELSE 
		RETURN ND
	END IF
	
 OTHERWISE
	SIGNAL ERROR: ERRORTEXT
 END TRY
END FUNCTION


--24 May

FUNCTION $GET_SORTED_OBJECTDATA_FOREX
ARGUMENTS %DATE_OBJS,%OBJ_RET_DIM
 
 TRY
	freq m
	OVER on
	EXECUTE "DATE "+%DATE_OBJS
	IGNORE ADD ON
	
	CASE *
	LOCAL series mergeCoded:STRING by case
	LOCAL SCALAR lastvalCs2:NUMERIC
	set lastvalCs2=1  
    LOCAL SCALAR month:STRING = BLANK
    LOCAL SCALAR year:STRING = BLANK
	
	LOCAL SERIES TEMPDESC:STRING by case
	LOCAL SERIES TEMPTRCO:PRECISION by case
	LOCAL SERIES SUMVALUE2:PRECISION BY CASE
	LOCAL SERIES !prod_result:PRECISION BY CASE

	
	LOOP FOR X IN DATE
		LOCAL NEW TEMP3=$SORT_RECONCILE_OBJECTDATA(string(X),%OBJ_RET_DIM)

		case *
		IF NOT MISSING(LASTVALUE(TEMP3))
			CASE LASTVALUE(mergeCoded)+1 TO LASTVALUE(mergeCoded)+LASTVALUE(TEMP3)
			SET mergeCoded=TEMP3[CASEORDER]
		END IF
	END LOOP
	case *
	
	LOCAL NEW mergeCoded1=merge(sortdata(mergeCoded))
	
	LOOP FOR v=FIRSTVALUE(mergeCoded1) to LASTVALUE(mergeCoded1)
			SET TEMPDESC[v]=SUBSTRING(LOCAL'mergeCoded1[v], 0,LOCATION(LOCAL'mergeCoded1[v],"_")-1)
			SET TEMPTRCO[v]=NUMBER(SUBSTRING(LOCAL'mergeCoded1[v], LOCATION(LOCAL'mergeCoded1[v],"_")+1,LENGTH(LOCAL'mergeCoded1[v])))
	END LOOP
	
	LOOP FOR v=FIRSTVALUE(mergeCoded1) to LASTVALUE(mergeCoded1)
     EXECUTE "DATE "+%DATE_OBJS
		LOOP FOR X IN DATE
			freq m
			date x
			IMAGE DATE MON "<mz><year>"
			set month=SUBSTRING(@DATE, 0, 2)
			set year=SUBSTRING(@DATE, 3, 6)

			LOOP FOR INS IN {AUA,  CBA,  CMB, BDC, AAB, IBA, FCIBARUBA, FNB, RBCARUBA}
				IF EXISTS(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%OBJ_RET_DIM+".C"))
					case *
     				LOCAL NEW SORTED_CURLIST = $SORT_DATA(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".DATEINDEX.C"),ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".CUCO.C"),"EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C EQ "+ STRING(TEMPTRCO[v]))
                        
                    SERIES EXCH_RATES : PRECISION BY CASE
		                   
                    IF LASTVALUE(SORTED_CURLIST) NE NC AND LASTVALUE(SORTED_CURLIST)GT 0                    			
                    	CASE FIRSTVALUE(SORTED_CURLIST) TO LASTVALUE(SORTED_CURLIST)
                    			SET EXCH_RATES[CASEORDER] = $GET_EXCH_RATE(SORTED_CURLIST,MAKE(DATE(MONTHLY),DATEFMT(X,"<Mtxt><year>")),"M")
                               CASE *
                    END IF
                
					WHICH ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C") EQ TEMPTRCO[v]
     				EXECUTE "WHICH UPPER(SUBSTRING(EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".DESC.C,0,5)) EQ "+QUOTE+TEMPDESC[v]+QUOTE
				
    						LOCAL SERIES RESULT_CURR : STRING BY CASE 
							SET LOCAL'RESULT_CURR = ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".CUCO.C") 
						
                   			IF LENGTHCASE GT 0
                   				 LOOP FOR CURR = FIRSTVALUE(SORTED_CURLIST) TO LASTVALUE(SORTED_CURLIST)
									SET RESULT_CURR = REPLACE(RESULT_CURR,SORTED_CURLIST[CURR],STRING(EXCH_RATES[CURR]), ALL)
								END LOOP
							END IF
				--	EXECUTE "LOCAL NEW filtererdCUCO = EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".CUCO.C"
				--	freq d
				--	IMAGE DATE DAILY "<dz><MTXT><YEAR>" 
				--	SET LOCAL'prod_result[v] = LOCAL'prod_result[v] + SUM(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%OBJ_RET_DIM+".C")*$GET_EXCH_RATE_FOREX(LOCAL'filtererdCUCO, datefmt(firstdate), datefmt(lastdate), "MONTHLY", @CASE))
					--freq m
     SET LOCAL'prod_result[v] = LOCAL'prod_result[v] + SUM(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%OBJ_RET_DIM+".C")*NUMBER(RESULT_CURR))
				END IF
			END LOOP
		END LOOP

		case *
		SET SUMVALUE2[v]=prod_result[v]

	END LOOP
	RETURN SUMVALUE2

 OTHERWISE
	SIGNAL ERROR: ERRORTEXT
 END TRY
END FUNCTION




--For Reconcillation Report
FUNCTION $GET_FILTERED_DATA
ARGUMENTS %FILTER_SERIES_FREQ,%FILTER_SERIES_TYPE,%OBJ_NAME,%FILTER
    	TRY
		
			OVER ON
			IGNORE ON,ADD ON,MUL ON,FUNC ON
   			EXECUTE "FREQ "+%FILTER_SERIES_FREQ
      
      
      		CASE *
            EXECUTE "LOCAL SERIES FILTERED_DATA : "+%FILTER_SERIES_TYPE+" BY CASE"
          
      		EXECUTE "WHICH "+%FILTER
        
            IF LENGTHCASE GT 0
	        	SET FILTERED_DATA[CASEORDER] =  %OBJ_NAME
            END IF
          
           RETURN<CASE *> FILTERED_DATA
           
   
OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION


FUNCTION $GET_CNC_FROM_BRF
ARGUMENTS %FILTER_SERIES_FREQ,%FILTER_SERIES_TYPE,%OBJ_NAME,%FILTER
    	TRY
		
			OVER ON
			IGNORE ON,ADD ON,MUL ON,FUNC ON
   			EXECUTE "FREQ "+%FILTER_SERIES_FREQ
      
            
      		LOCAL NEW FILTERED_DATA = MERGE(SORTDATA(REPLACE($GET_FILTERED_DATA(%FILTER_SERIES_FREQ,%FILTER_SERIES_TYPE,%OBJ_NAME,%FILTER),"x","X"))
          
           
      		CASE 1 TO LASTVALUE(FILTERED_DATA)
        
            LOCAL SERIES CNC_LIST : STRING BY CASE
            
            
           IF NOT MISSING(LASTVALUE(FILTERED_DATA))
           	LOOP FOR COUNT=FIRSTVALUE(FILTERED_DATA) TO LASTVALUE(FILTERED_DATA)	
                 CASE *
	            LOCAL SCALAR XCODE : STRING	
          
                       SET XCODE = STRING(NUMBER(REPLACE(SUBSTRING(FILTERED_DATA[COUNT],1,LOCATION(FILTERED_DATA[COUNT],"_")-1),"X","0")))+".0"
                      
         			
                       WHICH EXSE.BRF.COX.C EQ trim(XCODE)
                   
                       SET CNC_LIST[COUNT] =  FIRST(EXSE.BRF.KNOC.C)
                      
    	    END LOOP
        END IF
        CASE *
           RETURN<CASE *> CNC_LIST
                                   
OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION


FUNCTION $GET_BBR_RECON_DATA
ARGUMENTS %FILTER_SERIES_FREQ,%FILTER_SERIES_TYPE,%OBJ_NAME,%FILTER,%SUMMED_OBJ
    	TRY
		
			OVER ON
			IGNORE ON,ADD ON,MUL ON,FUNC ON
   			EXECUTE "FREQ "+%FILTER_SERIES_FREQ
      
            CASE *
      		LOCAL NEW FILTERED_DATA = MERGE(SORTDATA(REPLACE($GET_FILTERED_DATA(%FILTER_SERIES_FREQ,%FILTER_SERIES_TYPE,%OBJ_NAME,%FILTER),"x","X"))
          
           EXECUTE "WHICH "+%FILTER
           
           LOCAL SCALAR CASES : STRING = @CASE
           
      		CASE 1 TO LASTVALUE(FILTERED_DATA)
        
            LOCAL SERIES RESULT_LIST : PRECISION BY CASE
            
            
           IF NOT MISSING(LASTVALUE(FILTERED_DATA))
           	LOOP FOR COUNT=FIRSTVALUE(FILTERED_DATA) TO LASTVALUE(FILTERED_DATA)	
                 EXECUTE "CASE "+CASES
	            LOCAL SCALAR XCODE : STRING	
                LOCAL SCALAR TRCO : PRECISION 	
          
                       SET XCODE = SUBSTRING(FILTERED_DATA[COUNT],1,LOCATION(FILTERED_DATA[COUNT],"_")-1)
                       SET TRCO  = NUMBER(SUBSTRING(FILTERED_DATA[COUNT],LOCATION(FILTERED_DATA[COUNT],"_")+1,LENGTH(FILTERED_DATA[COUNT])))
         			
                        WHICH EXSE.FBFI.TRAN.TRCO.C EQ TRCO AND REPLACE(EXSE.FBFI.TRAN.RECO.C,"x","X") EQ XCODE
                   
                       SET RESULT_LIST[COUNT] =  SUM(%SUMMED_OBJ)
                      
    	    END LOOP
        END IF
        CASE *
           RETURN<CASE *> RESULT_LIST
                                   
OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION


FUNCTION $CAL_FOREX_FET_REPO
ARGUMENTS %DATE_OBJ,%FREQ,%MONTH_DATE,%CURRENCY_OBJ,%FILTER,%FCURR_OBJ
	TRY
		-- OUTPUT <acc append> FILE("D:\map\logs\procslog2.txt");
		OVER ON
		IGNORE ON,ADD ON,MUL OFF
		CASE *
		LOCAL NEW SORTED_CURLIST = $SORT_DATA(%DATE_OBJ,%CURRENCY_OBJ,%FILTER)
		
		SERIES EXCH_RATES : PRECISION BY CASE
		IF LASTVALUE(SORTED_CURLIST) NE NC AND LASTVALUE(SORTED_CURLIST)GT 0
			
			CASE FIRSTVALUE(SORTED_CURLIST) TO LASTVALUE(SORTED_CURLIST)
			SET EXCH_RATES[CASEORDER] = $GET_EXCH_RATE(SORTED_CURLIST,%MONTH_DATE,%FREQ)
			CASE *
			
		END IF
																													
		LOCAL NEW filt_obj_type_1 = TYPE(%DATE_OBJ)            
		LOCAL NEW filt_obj_freq = SUBSTRING(LOCAL'filt_obj_type_1, LOCATION(LOCAL'filt_obj_type_1,":")+1, LENGTH(LOCAL'filt_obj_type_1))
		EXECUTE "FREQUENCY "+LOCAL'filt_obj_freq
		EXECUTE "WHICH "+%FILTER
		DISP "WHICH "+%FILTER; 
		IF LENGTHCASE GT 0 
			LOCAL SERIES RESULT_CURR : STRING BY CASE 
			SET LOCAL'RESULT_CURR = %CURRENCY_OBJ 
			LOCAL SCALAR RESULT : PRECISION
			LOOP FOR CURR = FIRSTVALUE(SORTED_CURLIST) TO LASTVALUE(SORTED_CURLIST)
				SET RESULT_CURR = REPLACE(RESULT_CURR,SORTED_CURLIST[CURR],STRING(EXCH_RATES[CURR]), ALL)
			END LOOP
			DISP %FCURR_OBJ
			
			DISP RESULT_CURR
			DISP "SUM"
			DISP SUM(NUMBER(RESULT_CURR) * %FCURR_OBJ)
			RETURN <CASE *> SUM(NUMBER(RESULT_CURR) * %FCURR_OBJ)
		ELSE
			LOCAL NEW RESULT = ND
		END IF
		RETURN <CASE *>RESULT
	OTHERWISE
		SIGNAL ERROR: ERRORTEXT
	END TRY
END FUNCTION



FUNCTION $CAL_FOREX_NFA_REPO
ARGUMENTS %DATE_OBJ,%FREQ,%MONTH_DATE,%CURRENCY_OBJ,%FILTER,%FCURR_OBJ
TRY
                OVER ON
                IGNORE ON,ADD ON,MUL ON
                CASE *
                
                
				LOCAL NEW DIM1_FEB_FLOW 		= "EXSE.FEB.FLOW.AUA"
    			LOCAL NEW BANK_LIST 			= {"AUA","BDC","CMB","FCIBARUBA", "RBCARUBA", "CBA"}
       			LOCAL SCALAR MON : STRING 		= IF MONTH(%MONTH_DATE) GT 9 THEN STRING(MONTH(%MONTH_DATE)) ELSE "0"+STRING(MONTH(%MONTH_DATE))
          		LOCAL SCALAR RESULT : PRECISION 
          
				
				LOOP FOR BANK IN BANK_LIST
					
				 --REPLACE(DIM1_FEB_FLOW,NAME(BANK),ALL)+"."+STRING(YEAR(%MONTH_DATE))+"."+MON+".C"
    
    				SET RESULT = RESULT + $CAL_FOREX_FET_REPO(ID(REPLACE(NAME(%DATE_OBJ),"AUA",NAME(BANK),ALL)),%FREQ,%MONTH_DATE,ID(REPLACE(NAME(%CURRENCY_OBJ),"AUA",NAME(BANK),ALL)),REPLACE(%FILTER,"AUA",NAME(BANK),ALL),ID(REPLACE(NAME(%FCURR_OBJ),"AUA",NAME(BANK),ALL)))
     			
				END LOOP
				
	RETURN RESULT
 			
OTHERWISE
                SIGNAL ERROR: ERRORTEXT
  END TRY
END FUNCTION

FUNCTION $GET_SORTED_OBJECTDATA_DMBBBR_FOREX
ARGUMENTS %DATE_OBJS,%OBJ_RET_DIM
 
 TRY
	FREQ Q
	OVER on
	EXECUTE "DATE "+%DATE_OBJS
	IGNORE ADD ON,FUN OFF,MUL OFF,ON
	freq m
	CASE *
    LOCAL SCALAR month:STRING = BLANK
    LOCAL SCALAR year:STRING = BLANK
	LOCAL SERIES SUMVALUE2:PRECISION BY CASE
    LOCAL SERIES CASERANGE:NUMERIC BY CASE
	LOCAL SERIES SORTED_DATA1:PRECISION BY CASE
	LOCAL SERIES prod_result:PRECISION BY CASE
    LOCAL SERIES DATES:DATE(MONTHLY) BY CASE
	LOOP FOR X IN DATE
     	DATE X
		freq d
		IMAGE DATE DAILY "<dz><MTXT><YEAR>" 
		LOCAL NEW TEMPTRCO = $SORT_DATA_FEBFLOWSPLIT_CBA(datefmt(firstdate), datefmt(lastdate))
		IF NOT MISSING(lastvalue(TEMPTRCO))
			case lastvalue(SORTED_DATA1)+1 to lastvalue(SORTED_DATA1)+lastvalue(TEMPTRCO)
			set SORTED_DATA1=TEMPTRCO[caseorder]
			CASE *
		END IF
		freq m
	END LOOP

	LOCAL NEW SORTED_DATA=MERGE(SORTDATA(SORTED_DATA1))
   
	IF NOT MISSING(LASTVALUE(SORTED_DATA))
		LOOP FOR v=FIRSTVALUE(SORTED_DATA) to LASTVALUE(SORTED_DATA)
    -- IF SORTED_DATA[V] EQ 1020
			EXECUTE "DATE "+%DATE_OBJS
      			LOOP FOR X IN DATE
                
                date x
				IMAGE DATE MON "<mz><year>"
				set month=SUBSTRING(@DATE, 0, 2)
				set year=SUBSTRING(@DATE, 3, 6)
	       
				LOOP FOR INS IN {AUA,  CBA,  CMB, BDC, AAB, IBA, FCIBARUBA, FNB, RBCARUBA}
					IF EXISTS(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%OBJ_RET_DIM+".C"))
								case *
                             LOCAL NEW SORTED_CURLIST = $SORT_DATA(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".DATEINDEX.C"),ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".CUCO.C"),"EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C EQ "+ STRING(SORTED_DATA[v]))
                        
                        	 SERIES EXCH_RATES : PRECISION BY CASE
		                   
                          	 IF LASTVALUE(SORTED_CURLIST) NE NC AND LASTVALUE(SORTED_CURLIST)GT 0                    			
                    			CASE FIRSTVALUE(SORTED_CURLIST) TO LASTVALUE(SORTED_CURLIST)
                    			SET EXCH_RATES[CASEORDER] = $GET_EXCH_RATE(SORTED_CURLIST,MAKE(DATE(MONTHLY),DATEFMT(X,"<Mtxt><year>")),"M")
                                CASE *
                    		END IF
			
                        
				   	 WHICH ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C") EQ SORTED_DATA[v]
      
      						LOCAL SERIES RESULT_CURR : STRING BY CASE 
							SET LOCAL'RESULT_CURR = ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".CUCO.C") 
						
                   			IF LENGTHCASE GT 0
                   				 LOOP FOR CURR = FIRSTVALUE(SORTED_CURLIST) TO LASTVALUE(SORTED_CURLIST)
									SET RESULT_CURR = REPLACE(RESULT_CURR,SORTED_CURLIST[CURR],STRING(EXCH_RATES[CURR]), ALL)
								END LOOP
							END IF
                     	 
						SET LOCAL'prod_result[v] = LOCAL'prod_result[v] + SUM(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%OBJ_RET_DIM+".C")*NUMBER(RESULT_CURR))
      
					 --   TYPE  X,NAME(INS),SORTED_DATA[v],prod_result[v],SUM(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%OBJ_RET_DIM+".C")
					
					END IF
				END LOOP

				CASE *
			END LOOP
		
		case *
		SET SUMVALUE2[v]=prod_result[v]
--END IF
		END LOOP
	END IF
	RETURN SUMVALUE2

 OTHERWISE
	SIGNAL ERROR: ERRORTEXT
 END TRY
END FUNCTION

FUNCTION $GET_SORTED_OBJECTDATA_DMBBBR_LOCAL
ARGUMENTS %DATE_OBJS,%OBJ_RET_DIM
 
 TRY
	FREQ Q
	OVER on
	EXECUTE "DATE "+%DATE_OBJS
	IGNORE ADD ON,FUN OFF,MUL OFF,ON
	freq m
	CASE *
    LOCAL SCALAR month:STRING = BLANK
    LOCAL SCALAR year:STRING = BLANK
	LOCAL SERIES SUMVALUE2:PRECISION BY CASE
    LOCAL SERIES CASERANGE:NUMERIC BY CASE
	LOCAL SERIES SORTED_DATA1:PRECISION BY CASE
	LOCAL SERIES prod_result:PRECISION BY CASE
    LOCAL SERIES DATES:DATE(MONTHLY) BY CASE
	LOOP FOR X IN DATE
     DATE X
		freq d
		IMAGE DATE DAILY "<dz><MTXT><YEAR>" 
		LOCAL NEW TEMPTRCO = $SORT_DATA_FEBFLOWSPLIT_CBA(datefmt(firstdate), datefmt(lastdate))
		IF NOT MISSING(lastvalue(TEMPTRCO))
			case lastvalue(SORTED_DATA1)+1 to lastvalue(SORTED_DATA1)+lastvalue(TEMPTRCO)
			set SORTED_DATA1=TEMPTRCO[CASEORDER]
			CASE *
		END IF
		freq m
	END LOOP
	LOCAL NEW SORTED_DATA=MERGE(SORTDATA(SORTED_DATA1))
 
   
	IF NOT MISSING(LASTVALUE(SORTED_DATA))
		LOOP FOR v=FIRSTVALUE(SORTED_DATA) to LASTVALUE(SORTED_DATA)
   --  IF SORTED_DATA[V] EQ 1113
			EXECUTE "DATE "+%DATE_OBJS
      			LOOP FOR X IN DATE
                
                date x
				IMAGE DATE MON "<mz><year>"
				set month=SUBSTRING(@DATE, 0, 2)
				set year=SUBSTRING(@DATE, 3, 6)
	       
				LOOP FOR INS IN {AUA,  CBA,  CMB, BDC, AAB, IBA, FCIBARUBA, FNB, RBCARUBA}
					IF EXISTS(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%OBJ_RET_DIM+".C"))
								case *
                             
			
                        
				   	 WHICH ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C") EQ SORTED_DATA[v]
      
      						
                     	 
						SET LOCAL'prod_result[v] = LOCAL'prod_result[v] + SUM(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%OBJ_RET_DIM+".C"))
      
						
					
					END IF
				END LOOP

				CASE *
			END LOOP
		
		case *
		SET SUMVALUE2[v]=prod_result[v]
--END IF
		END LOOP
	END IF
	RETURN SUMVALUE2

 OTHERWISE
	SIGNAL ERROR: ERRORTEXT
 END TRY
END FUNCTION

FUNCTION $GET_SORTED_OBJECTDATA_DMB_NOCBA_FOREX
ARGUMENTS %DATE_OBJS,%OBJ_RET_DIM
 
 TRY
	FREQ Q
	OVER on
	EXECUTE "DATE "+%DATE_OBJS
	IGNORE ADD ON,FUN OFF,MUL OFF,ON
	freq m
	CASE *
    LOCAL SCALAR month:STRING = BLANK
    LOCAL SCALAR year:STRING = BLANK
	LOCAL SERIES SUMVALUE2:PRECISION BY CASE
    LOCAL SERIES CASERANGE:NUMERIC BY CASE
	LOCAL SERIES SORTED_DATA1:PRECISION BY CASE
	LOCAL SERIES prod_result:PRECISION BY CASE
    LOCAL SERIES DATES:DATE(MONTHLY) BY CASE
	LOOP FOR X IN DATE
     DATE X
		freq d
		IMAGE DATE DAILY "<dz><MTXT><YEAR>" 
		LOCAL NEW TEMPTRCO = $SORT_DATA_FEBFLOWSPLIT_NOCBA(datefmt(firstdate), datefmt(lastdate))
		IF NOT MISSING(lastvalue(TEMPTRCO))
			case lastvalue(SORTED_DATA1)+1 to lastvalue(SORTED_DATA1)+lastvalue(TEMPTRCO)
			set SORTED_DATA1=TEMPTRCO[CASEORDER]
			CASE *
		END IF
		freq m
	END LOOP
	LOCAL NEW SORTED_DATA=MERGE(SORTDATA(SORTED_DATA1))
 
   
	IF NOT MISSING(LASTVALUE(SORTED_DATA))
		LOOP FOR v=FIRSTVALUE(SORTED_DATA) to LASTVALUE(SORTED_DATA)
    -- IF SORTED_DATA[V] EQ 1020
			EXECUTE "DATE "+%DATE_OBJS
      			LOOP FOR X IN DATE
                
                date x
				IMAGE DATE MON "<mz><year>"
				set month=SUBSTRING(@DATE, 0, 2)
				set year=SUBSTRING(@DATE, 3, 6)
	       
				LOOP FOR INS IN {AUA, CMB, BDC, AAB, IBA, FCIBARUBA, FNB, RBCARUBA}
					IF EXISTS(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%OBJ_RET_DIM+".C"))
								case *
                             LOCAL NEW SORTED_CURLIST = $SORT_DATA(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".DATEINDEX.C"),ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".CUCO.C"),"EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C EQ "+ STRING(SORTED_DATA[v]))
                        
                        	 SERIES EXCH_RATES : PRECISION BY CASE
		                   
                          	 IF LASTVALUE(SORTED_CURLIST) NE NC AND LASTVALUE(SORTED_CURLIST)GT 0                    			
                    			CASE FIRSTVALUE(SORTED_CURLIST) TO LASTVALUE(SORTED_CURLIST)
                    			SET EXCH_RATES[CASEORDER] = $GET_EXCH_RATE(SORTED_CURLIST,MAKE(DATE(MONTHLY),DATEFMT(X,"<Mtxt><year>")),"M")
                                CASE *
                    		END IF
			
                        
				   	 WHICH ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C") EQ SORTED_DATA[v]
      
      						LOCAL SERIES RESULT_CURR : STRING BY CASE 
							SET LOCAL'RESULT_CURR = ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".CUCO.C") 
						
                   			IF LENGTHCASE GT 0
                   				 LOOP FOR CURR = FIRSTVALUE(SORTED_CURLIST) TO LASTVALUE(SORTED_CURLIST)
									SET RESULT_CURR = REPLACE(RESULT_CURR,SORTED_CURLIST[CURR],STRING(EXCH_RATES[CURR]), ALL)
								END LOOP
							END IF
                     	 
						SET LOCAL'prod_result[v] = LOCAL'prod_result[v] + SUM(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%OBJ_RET_DIM+".C")*NUMBER(RESULT_CURR))
      
					 --   TYPE  X,NAME(INS),SORTED_DATA[v],prod_result[v],SUM(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%OBJ_RET_DIM+".C")
					
					END IF
				END LOOP

				CASE *
			END LOOP
		
		case *
		SET SUMVALUE2[v]=prod_result[v]
--END IF
		END LOOP
	END IF
	RETURN SUMVALUE2

 OTHERWISE
	SIGNAL ERROR: ERRORTEXT
 END TRY
END FUNCTION

FUNCTION $GET_SORTED_OBJECTDATA_DMB_NOCBA_LOCAL
ARGUMENTS %DATE_OBJS,%OBJ_RET_DIM
 
 TRY
	FREQ Q
	OVER on
	EXECUTE "DATE "+%DATE_OBJS
	IGNORE ADD ON,FUN OFF,MUL OFF,ON
	freq m
	CASE *
    LOCAL SCALAR month:STRING = BLANK
    LOCAL SCALAR year:STRING = BLANK
	LOCAL SERIES SUMVALUE2:PRECISION BY CASE
    LOCAL SERIES CASERANGE:NUMERIC BY CASE
	LOCAL SERIES SORTED_DATA1:PRECISION BY CASE
	LOCAL SERIES prod_result:PRECISION BY CASE
    LOCAL SERIES DATES:DATE(MONTHLY) BY CASE
	LOOP FOR X IN DATE
     DATE X
		freq d
		IMAGE DATE DAILY "<dz><MTXT><YEAR>" 
		LOCAL NEW TEMPTRCO = $SORT_DATA_FEBFLOWSPLIT_NOCBA(datefmt(firstdate), datefmt(lastdate))
		IF NOT MISSING(lastvalue(TEMPTRCO))
			case lastvalue(SORTED_DATA1)+1 to lastvalue(SORTED_DATA1)+lastvalue(TEMPTRCO)
			set SORTED_DATA1=TEMPTRCO[CASEORDER]
			CASE *
		END IF
		freq m
	END LOOP
	LOCAL NEW SORTED_DATA=MERGE(SORTDATA(SORTED_DATA1))
 
   
	IF NOT MISSING(LASTVALUE(SORTED_DATA))
		LOOP FOR v=FIRSTVALUE(SORTED_DATA) to LASTVALUE(SORTED_DATA)
   --  IF SORTED_DATA[V] EQ 1113
			EXECUTE "DATE "+%DATE_OBJS
      			LOOP FOR X IN DATE
                
                date x
				IMAGE DATE MON "<mz><year>"
				set month=SUBSTRING(@DATE, 0, 2)
				set year=SUBSTRING(@DATE, 3, 6)
	       
				LOOP FOR INS IN {AUA, CMB, BDC, AAB, IBA, FCIBARUBA, FNB, RBCARUBA}
					IF EXISTS(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%OBJ_RET_DIM+".C"))
								case *
                             
			
                        
				   	 WHICH ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+".TRCO.C") EQ SORTED_DATA[v]
      
      						
                     	 
						SET LOCAL'prod_result[v] = LOCAL'prod_result[v] + SUM(ID("EXSE.FEB.FLOW."+NAME(INS)+"."+LOCAL'year+"."+LOCAL'month+"."+%OBJ_RET_DIM+".C"))
      
						
					
					END IF
				END LOOP

				CASE *
			END LOOP
		
		case *
		SET SUMVALUE2[v]=prod_result[v]
--END IF
		END LOOP
	END IF
	RETURN SUMVALUE2

 OTHERWISE
	SIGNAL ERROR: ERRORTEXT
 END TRY
END FUNCTION


--This function is used for calculating Saldi NFA forex values based given object,date and filter statement 
FUNCTION $Get_Saldi_NFA_Forex
ARGUMENTS %DATE_OBJ,%FREQ,%DATE,%CURRENCY_OBJ,%FILTER_STMT,%SUMMED_VALUE_OBJECT
    	TRY

           OVER ON
           IGNORE ADD ON
           LOCAL SCALAR RESULT:PRECISION = 0
           LOCAL NEW SORTED_CUCO  = $SORT_DATA(%DATE_OBJ,%CURRENCY_OBJ,%FILTER_STMT)
           FREQ D
		   LOCAL SERIES EXCH_RATES : PRECISION BY CASE
		   
		   IF NOT MISSING(SORTED_CUCO)
          		 CASE FIRSTVALUE(SORTED_CUCO) TO LASTVALUE(SORTED_CUCO)
		    		SET EXCH_RATES[CASEORDER] = $GET_EXCH_RATE_sALDI(SORTED_CUCO,%DATE,%FREQ)
     	   		 CASE *
		   END IF

		   EXECUTE "WHICH "+%FILTER_STMT
    
    	   LOCAL SERIES RESULT_CURR : STRING BY CASE 
		   SET LOCAL'RESULT_CURR = %CURRENCY_OBJ
						
           IF LENGTHCASE GT 0
                    
				LOOP FOR CURR = FIRSTVALUE(SORTED_CUCO) TO LASTVALUE(SORTED_CUCO)
						SET RESULT_CURR = REPLACE(RESULT_CURR,SORTED_CUCO[CURR],STRING(EXCH_RATES[CURR]), ALL)
				END LOOP

				SET RESULT = SUM(%SUMMED_VALUE_OBJECT * NUMBER(RESULT_CURR))
    
				RETURN RESULT
		   ELSE 
				RETURN ND
       	   END IF
			
       
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION


FUNCTION $GET_EXCH_RATE_sALDI
ARGUMENTS %CURR_LIST,%DATE,%FREQ
    	TRY
		
			OVER ON
			IGNORE ON,ADD ON,MUL ON,FUNC ON
		
				IF INDEX(%CURR_LIST) EQ "CASE" AND LASTVALUE(%CURR_LIST) GT 0
          
					CASE FIRSTVALUE(%CURR_LIST) TO LASTVALUE(%CURR_LIST)
					LOCAL SERIES EXCH_RATE : PRECISION BY CASE
   				
					EXECUTE "FREQ "+%FREQ
     
					DATE %DATE
  
					
					LOOP FOR CURR=FIRSTVALUE(%CURR_LIST) TO LASTVALUE(%CURR_LIST)	
         	
					IF UPPER(%CURR_LIST[CURR]) EQ "AFL"
                       LOCAL SCALAR RS = 1
                    ELSE
         				IF %CURR_LIST[CURR] NE "ND"
							EXECUTE "LOCAL SCALAR RS = CONVERT(EXSE.FEB.EXRA."+%CURR_LIST[CURR]+".MIDD.D,"+STRING(%FREQ)+",CONSTANT,END)"
						ELSE 
							LOCAL SCALAR RS = 1
						END IF
            		END IF 
    	        
       				IF RS NE ND AND RS NE NC
               			
               			IF UPPER(%CURR_LIST[CURR])  EQ "ITL"
                      		SET RS = RS/1000
	                    ELSE IF UPPER(%CURR_LIST[CURR])  EQ "JPY"
    						SET RS = RS/10000	                    
						ELSE IF UPPER(%CURR_LIST[CURR])  EQ "AWG" OR UPPER(%CURR_LIST[CURR])  EQ "ANG" OR UPPER(%CURR_LIST[CURR])  EQ "GBP" OR UPPER(%CURR_LIST[CURR])  EQ "CAD" OR UPPER(%CURR_LIST[CURR])  EQ "USD" OR UPPER(%CURR_LIST[CURR])  EQ "AFL"
                      		SET RS = RS/1
                    	ELSE
                        	SET RS = RS/100

                        END IF
               
						SET EXCH_RATE[CURR] = RS
      				END IF
		            
		            END LOOP

              
					RETURN EXCH_RATE
				ELSE
					RETURN ND
				END IF
       
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION
                       
FUNCTION $SORT_DATA_FEBFLOWSPLIT_CBA_Q
ARGUMENTS %DT1,%DT2
 
 TRY
	FREQ D
	OVER on
	EXECUTE "DATE "+%DT1+" TO "+%DT2
	IGNORE ADD ON,FUN OFF,MUL OFF,ON
	freq m
	CASE *
	LOCAL SERIES SORTED_DATA1:PRECISION BY CASE
	LOOP FOR X IN DATE
     Date x
		freq d
		IMAGE DATE DAILY "<dz><MTXT><YEAR>" 
		LOCAL NEW TEMPTRCO = $SORT_DATA_FEBFLOWSPLIT_CBA(datefmt(firstdate), datefmt(lastdate))
		IF NOT MISSING(lastvalue(TEMPTRCO))
			case lastvalue(SORTED_DATA1)+1 to lastvalue(SORTED_DATA1)+lastvalue(TEMPTRCO)
			set SORTED_DATA1=TEMPTRCO[caseorder]
			CASE *
		END IF
		freq m
	END LOOP
	LOCAL NEW SORTED_DATA=MERGE(SORTDATA(SORTED_DATA1))
 
    RETURN SORTED_DATA
    
 OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
 END TRY

End  FUNCTION  

FUNCTION $SORT_DATA_FEBFLOWSPLIT_NOCBA_Q
ARGUMENTS %DT1,%DT2
 
 TRY
	FREQ D
	OVER on
	EXECUTE "DATE "+%DT1+" TO "+%DT2
	IGNORE ADD ON,FUN OFF,MUL OFF,ON
	freq m
	CASE *
	LOCAL SERIES SORTED_DATA1:PRECISION BY CASE
	LOOP FOR X IN DATE
     Date x
		freq d
		IMAGE DATE DAILY "<dz><MTXT><YEAR>" 
		LOCAL NEW TEMPTRCO = $SORT_DATA_FEBFLOWSPLIT_NOCBA(datefmt(firstdate), datefmt(lastdate))
		IF NOT MISSING(lastvalue(TEMPTRCO))
			case lastvalue(SORTED_DATA1)+1 to lastvalue(SORTED_DATA1)+lastvalue(TEMPTRCO)
			set SORTED_DATA1=TEMPTRCO[caseorder]
			CASE *
		END IF
		freq m
	END LOOP
	LOCAL NEW SORTED_DATA=MERGE(SORTDATA(SORTED_DATA1))
 
    RETURN SORTED_DATA
    
 OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
 END TRY

End  FUNCTION					   
                

--This function is used to get Unique code from MOTR
FUNCTION $GET_MOTR_SORTEDCODE
ARGUMENTS %FILTER_STMT1,%FILTER_STMT2,%CODE_OBJECT1,%CODE_OBJECT2
		CASE *
		FREQUENCY QUARTERLY
		 LOCAL SERIES SORTED_DATA:STRING BY CASE
		   EXECUTE "WHICH "+%FILTER_STMT1 
    	   IF LENGTHCASE GT 0
	           LOCAL NEW SORTED_DATA1 = MERGE(SORTDATA(STRING(%CODE_OBJECT1)))
		   ELSE
				LOCAL NEW SORTED_DATA1 = ND
		   END IF
		   CASE *
		   EXECUTE "WHICH "+%FILTER_STMT2
    	   IF LENGTHCASE GT 0
	           LOCAL NEW SORTED_DATA2 = MERGE(SORTDATA(STRING(%CODE_OBJECT2)))
		   ELSE
				LOCAL NEW SORTED_DATA2 = ND
		   END IF
		   CASE *
		  IF NOT MISSING(FIRSTVALUE(SORTED_DATA1))
			IF NOT MISSING(FIRSTVALUE(SORTED_DATA2))
				CASE *;CASE 1 TO LASTVALUE(SORTED_DATA1)+LASTVALUE(SORTED_DATA2)
				SET SORTED_DATA=MERGE(SORTED_DATA1,SORTED_DATA2)
			ELSE
				CASE *;CASE 1 TO LASTVALUE(SORTED_DATA1)
				SET SORTED_DATA=SORTED_DATA1
			END IF
		  ELSE IF NOT MISSING(FIRSTVALUE(SORTED_DATA2))
				CASE *;CASE 1 TO LASTVALUE(SORTED_DATA2)
				SET SORTED_DATA=SORTED_DATA2
		  END IF
	RETURN<CASE *>SORTED_DATA

END FUNCTION



--This Function is used to fetched MOTR data.
FUNCTION $GET_MOTR_DATA
ARGUMENTS %FILTER_STMT1,%FILTER_STMT2,%CODE_OBJECT1,%CODE_OBJECT2,%SUMMED_VALUE_OBJECT
    	TRY
           CASE *
           OVER ON
           Ignore ON,ADD ON,MUL ON
           FREQUENCY QUARTERLY
         
           LOCAL NEW SORTED_DATA = $GET_MOTR_SORTEDCODE(%FILTER_STMT1,%FILTER_STMT2,%CODE_OBJECT1,%CODE_OBJECT2)
           
          IF INDEX(SORTED_DATA) EQ "CASE" AND NOT MISSING(FIRSTVALUE(SORTED_DATA))
          
           LOCAL SERIES RESULT : PRECISION BY CASE
           LOCAL SCALAR FILTER : STRING 
           
           LOOP FOR CODE=FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
			  CASE *
			  
              EXECUTE "WHICH "+%FILTER_STMT1+" AND "+NAME(%CODE_OBJECT1)+" EQ "+string(SORTED_DATA[CODE])
			  
              SET RESULT[CODE] = SUM(%SUMMED_VALUE_OBJECT)
             	
	       END LOOP
          ELSE
          	 LOCAL NEW RESULT = ND
          END IF
     
     	RETURN<CASE *> RESULT
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION

FUNCTION $Operations_Get_Data
	ARGUMENTS %Object,%DATE_OBJS,%DATE_OBJE,%institue, %type
TRY
  OVER ON
  Ignore ON,ADD ON,MUL ON
  CASE *
  FREQ DAILY

  
  IF NOT MISSING(location(TYPE(ID(%Object)),"DATE"))  
		LOCAL SERIES cons_series:DATE(DAILY) BY CASE
	ELSE
		EXECUTE "LOCAL SERIES cons_series:"+TYPE(ID(%Object))+" BY CASE"
	END IF
	LOCAL NEW temp_str = SUBSTRING(MIRROR(%Object), LOCATION(MIRROR(%Object), ".") +1, LENGTH(MIRROR(%Object)))
	LOCAL NEW series_dim = MIRROR(SUBSTRING(LOCAL'temp_str, 0, LOCATION(LOCAL'temp_str, ".")-1))
	LOCAL SCALAR !tempcs : NUMERIC = 1

	DATE %DATE_OBJS TO %DATE_OBJE
	FREQ M

	IF LENGTHDATE GE 1
		BLOCK
			FREQ MON
    
			SET LOCAL'tempcs = 1
			LOOP FOR dt IN DATE

				IF MONTH(dt) LE 9
					LOCAL NEW monstr = "0"+STRING(MONTH(dt))
				ELSE
					LOCAL NEW monstr = STRING(MONTH(dt))
				END IF
				--TYPE "OPER.TFMABO."+%type+"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C"
								
				IF EXISTS(ID("OPER.TFMABO."+%type+"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C")) AND &&
					NOT MISSING(FIRSTVALUE(ID("OPER.TFMABO."+%type+"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C")))
					
					CASE *
					FREQ DAILY
					WHICH ID("OPER.TFMABO."+%type+"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".TRDA.C") GE %DATE_OBJS AND &&
					ID("OPER.TFMABO."+%type+"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".TRDA.C") LE %DATE_OBJE
						IF LENGTHCASE GT 0
							LOOP FOR cs IN CASE
								SET LOCAL'cons_series[LOCAL'tempcs] = ID("OPER.TFMABO."+%type+"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C")[cs]
								SET LOCAL'tempcs = LOCAL'tempcs + 1
							END LOOP
						END IF
				 	
				ELSE
					SIGNAL CONTINUE : ERRORTEXT
				END IF --EXISTS(ID("OPER.TFMABO."+%type+"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C")) AND &&
				
			END LOOP
				
		END BLOCK
		
	ELSE
		SIGNAL ERROR : "$Operations_Get_Data : INVALID DATE RANGE"
	END IF	--LENGTHDATE GE 1
  
  
OTHERWISE
	SIGNAL ERROR: ERRORTEXT
END TRY
	RETURN <case *>cons_series
END FUNCTION
---------------------Added by santosh on 7thFeb2020 for BOP Internal reports by total by reporter----------------

FUNCTION $SORT_DATA_WITH_CLOSING_ENTRY_WITH_RECO
ARGUMENTS %DATE_OBJ,%START_DT,%END_DT,%OBJ_NAME,%RECO,%FILTER
    	TRY
           
           OVER ON
           Ignore ON,ADD ON,MUL ON
     
           CASE *
           LOCAL NEW filt_obj_type_1 = TYPE(%DATE_OBJ)	
           
           LOCAL NEW filt_obj_freq = SUBSTRING(LOCAL'filt_obj_type_1, LOCATION(LOCAL'filt_obj_type_1,":")+1, LENGTH(LOCAL'filt_obj_type_1))
           EXECUTE "FREQUENCY "+LOCAL'filt_obj_freq
    
           EXECUTE "WHICH "+%FILTER 
           
    	   IF LENGTHCASE GT 0
	           LOCAL NEW SORTED_DATA = MERGE(SORTDATA(STRING(%OBJ_NAME)))
		   ELSE
				SERIES WORK'SORTED_DATA:STRING BY CASE
		   END IF	 

			CASE *
   			
       
            EXECUTE "WHICH EXSE.FBFI.BALA.DATEINDEX.C GE "+ %START_DT +" AND EXSE.FBFI.BALA.DATEINDEX.C LE "+%END_DT+" AND UPPER(TRIM(EXSE.FBFI.BALA.RECO.C)) EQ """+UPPER(TRIM(%RECO))+""" "
            
            IF LENGTHCASE GT 0
	           LOCAL NEW SORTED_DATA1 = MERGE(SORTDATA(STRING(PRECISION(NUMBER(EXSE.FBFI.BALA.FATY.C)))))
		   ELSE
				SERIES WORK'SORTED_DATA1:STRING BY CASE
		   END IF

			case *
   
   		  LOCAL NEW SORTED_DATA2 = MERGE(SORTED_DATA,SORTED_DATA1)
  	
         	RETURN <CASE *>LOCAL'SORTED_DATA2
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION

FUNCTION GET_MONTHLY_BOP_DATA_WITH_CLOSING_ENTRY_WITH_RECO
ARGUMENTS %DATE_OBJ,%CODE_OBJECT,%FILTER_STMT,%SUMMED_VALUE_OBJECT,%START_DT,%END_DT,%TRAN_TYPE,%RECO
    	TRY
           
           OVER ON
           Ignore ON,ADD ON,MUL ON
           
		   FREQ M
         
           LOCAL NEW SORTED_DATA =  $SORT_DATA_WITH_CLOSING_ENTRY_WITH_RECO(%DATE_OBJ,%START_DT,%END_DT,%CODE_OBJECT,%RECO,%FILTER_STMT)
		   
		   IF LASTVALUE(SORTED_DATA) EQ NC
		     RETURN ND
			END  IF
           
           IF INDEX(SORTED_DATA) EQ "CASE" AND LASTVALUE(SORTED_DATA) NE NC AND LASTVALUE(SORTED_DATA) GT 0
          
           CASE FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
           LOCAL SERIES RESULT : PRECISION BY CASE
           
           
           LOOP FOR CODE=FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
              
              SET RESULT[CODE] = $FILTER_DATA(%DATE_OBJ,%FILTER_STMT+" AND  "+NAME(%CODE_OBJECT)+" EQ "+string(SORTED_DATA[CODE]),%SUMMED_VALUE_OBJECT) + $GET_CLOSING_ENTRY_BALA_WITH_RECO(%START_DT,%END_DT,SORTED_DATA[CODE],%TRAN_TYPE,%RECO)
             	
	       END LOOP
          ELSE
          	 LOCAL NEW RESULT = ND
          END IF
     
     	RETURN RESULT
       
         	RETURN<case *> SORTED_DATA
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION

FUNCTION $GET_CLOSING_ENTRY_BALA_WITH_RECO
ARGUMENTS %START_DT,%END_DT,%CODE,%TRAN_TYPE,%RECO
    	TRY
           
           OVER ON
           Ignore ON,ADD ON,MUL ON
		   FREQ M
	   CASE *
           DATE *

           LOCAL SCALAR RESULT  : PRECISION 
         
           EXEC "WHICH EXSE.FBFI.BALA.DATEINDEX.C GE "+ %START_DT +" AND EXSE.FBFI.BALA.DATEINDEX.C LE "+%END_DT+" AND NUMBER(EXSE.FBFI.BALA.FATY.C) EQ "+%CODE+" AND UPPER(TRIM(EXSE.FBFI.BALA.RECO.C)) EQ """+UPPER(TRIM(%RECO))+""" "
           
           IF LENGTHCASE GT 0
               
               SET RESULT = SUM(EXSE.FBFI.BALA.TODE.C) - SUM(EXSE.FBFI.BALA.TOCR.C)
               
               IF NOT MISSING(RESULT) AND %TRAN_TYPE EQ "CR" AND RESULT GT 0
                      RETURN RESULT
               ELSE IF NOT MISSING(RESULT) AND %TRAN_TYPE EQ "DR" AND RESULT LT 0
					  RETURN (RESULT * -1)
               ELSE  
               		RETURN 0
               END IF
               
	       ELSE

           	RETURN 0
           
           END IF

    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION

----------------------------------------
-----Added by Santosh for Internal bop table for Balances
FUNCTION GET_MONTHLY_BOP_DATA_BALA
ARGUMENTS %DATE_OBJ,%CODE_OBJECT,%FILTER_STMT,%SUMMED_VALUE_OBJECT
    	TRY
           
           OVER ON
           Ignore ON,ADD ON,MUL ON
           
         
           LOCAL NEW SORTED_DATA = $SORT_DATA(%DATE_OBJ,%CODE_OBJECT,%FILTER_STMT)
           
          IF INDEX(SORTED_DATA) EQ "CASE" AND LASTVALUE(SORTED_DATA) GT 0
          
           CASE FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
           LOCAL SERIES RESULT : PRECISION BY CASE
           
           
           LOOP FOR CODE=FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
              
              SET RESULT[CODE] = $FILTER_DATA(%DATE_OBJ,%FILTER_STMT+" AND  NUMBER("+NAME(%CODE_OBJECT)+") EQ "+SORTED_DATA[CODE],%SUMMED_VALUE_OBJECT)
             	
	       END LOOP
          ELSE
          	 LOCAL NEW RESULT = ND
          END IF
     
     	RETURN RESULT
       
         	RETURN<case *> SORTED_DATA
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION

-----Added by Santosh for Internal bop table for Balances BY Affiliate name and account number
FUNCTION getBalaDataByFatyAccNo
ARGUMENTS %DATE_OBJ,%CODE_OBJECT,%AFFLIATE_NAME,%ACC_NO,%FILTER_STMT,%SUMMED_VALUE_OBJECT
    	TRY
           
           OVER ON
           Ignore ON,ADD ON,MUL ON
           
         
           LOCAL NEW SORTED_DATA = $SORT_DATA(%DATE_OBJ,%CODE_OBJECT+"%"+%AFFLIATE_NAME+"%"+%ACC_NO,%FILTER_STMT)		   
           
          IF INDEX(SORTED_DATA) EQ "CASE" AND LASTVALUE(SORTED_DATA) GT 0
          
           CASE FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
		   
				LOCAL SERIES RESULT : PRECISION BY CASE

				LOOP FOR COUNTER=FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)

				LOCAL NEW CURRENT_OBS = SORTED_DATA[COUNTER]
				LOCAL NEW SPLITTER1 = LOCATION(CURRENT_OBS,"%")
				LOCAL NEW SPLITTER2 = LOCATION(CURRENT_OBS,"%",SPLITTER1+1)

				LOCAL NEW CODE = SUBSTRING(SORTED_DATA[COUNTER], 0, SPLITTER1-1)
				LOCAL NEW AFFL_NAME = SUBSTRING(SORTED_DATA[COUNTER], SPLITTER1+1, SPLITTER2-1)
				LOCAL NEW ACC_NO = SUBSTRING(SORTED_DATA[COUNTER], SPLITTER2+1,LENGTH(SORTED_DATA[COUNTER]))
				LOCAL NEW AFFL_NAME_CHECK = " AND "+NAME(%AFFLIATE_NAME)+" EQ ND"
				LOCAL NEW ACC_NO_CHECK = " AND "+NAME(%ACC_NO)+" EQ ND"

				IF TRIM(AFFL_NAME) NE ""
					SET AFFL_NAME_CHECK = " AND "+NAME(%AFFLIATE_NAME)+" EQ "+QUOTE+AFFL_NAME+QUOTE
				END IF
				
				IF TRIM(ACC_NO) NE ""
					SET ACC_NO_CHECK = " AND "+NAME(%ACC_NO)+" EQ "+QUOTE+ACC_NO+QUOTE
				END IF
				
				--TYPE %FILTER_STMT+" AND  NUMBER("+NAME(%CODE_OBJECT)+") EQ "+CODE+AFFL_NAME_CHECK+ACC_NO_CHECK
				SET RESULT[COUNTER] = $FILTER_DATA(%DATE_OBJ,%FILTER_STMT+" AND  NUMBER("+NAME(%CODE_OBJECT)+") EQ "+CODE+AFFL_NAME_CHECK+ACC_NO_CHECK,%SUMMED_VALUE_OBJECT)   	
	       END LOOP
		   
          ELSE
          	 LOCAL NEW RESULT = ND
          END IF
     
     	RETURN RESULT
       
         	RETURN<case *> SORTED_DATA
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION



--Added By Santosh on 07July2020 For NFA Report

FUNCTION $NFAValuesForDateRange
ARGUMENT startDate, endDate, allInstitutions,codeObjectPre, codeObjectPost, sumObjectPre, sumObjectPost, exchObjectPre, exchObjectPost, currencyObjectPre, currencyObjectPost , filter
BLOCK
	OVERWRITE ON
	CONVERT ON
	IGNORE ADDITION ON
	IGNORE ON
 	DECIMAL AUTO
 	FORCE NUMERIC OFF

	FREQ MONTHLY
	DATE startDate to endDate
 
	LOCAL RESULT = 0
	LOOP FOR dd IN DATE
		LOCAL currMonth = PAD( STRING( MONTH( dd ) ), 2, RIGHT, "0" )
		LOCAL currYear = YEAR( dd )
		LOCAL iTotal = 0
   		LOCAL strCurrYearMonth = STRING( currYear ) + "." + STRING( currMonth )
		LOOP FOR ii IN allInstitutions
		
		SERIES LOCAL'seriesAllTransCode: PRECISION INDEXED BY CASE
		SERIES LOCAL'seriesAllCurrCode: STRING INDEXED BY CASE
		SERIES LOCAL'seriesAllInstCode: STRING INDEXED BY CASE
		SERIES LOCAL'seriesAllFXRate: PRECISION INDEXED BY CASE
		SERIES LOCAL'seriesAllValue: PRECISION INDEXED BY CASE
		SERIES LOCAL'seriesAllValueFX: PRECISION INDEXED BY CASE
					 
			LOCAL currCodeObject = codeObjectPre + "." + NAME( ii ) + "." + strCurrYearMonth + "." + codeObjectPost
			LOCAL currSumObject = sumObjectPre + "." + NAME( ii ) + "." + strCurrYearMonth + "." + sumObjectPost
			LOCAL currencyObject = currencyObjectPre + "." + NAME( ii ) + "." + strCurrYearMonth + "." + currencyObjectPost
			LOCAL dateObject = codeObjectPre + "." + NAME( ii ) + "." + strCurrYearMonth + ".DATEINDEX.C"
				
			IF EXISTS( ID( currCodeObject ) )
			BLOCK
			
       			DATE dd
				CASE * 
				LOCAL nlExchObjPre = MAKE( NAMELIST, exchObjectPre )
				LOCAL nlExchObjPost = MAKE( NAMELIST, exchObjectPost )
				
				LOCAL actualFilter = REPLACE(REPLACE(filter,"DATE",dateObject,ALL),"TRCO",currCodeObject,ALL)
				--DISP actualFilter
				FREQ DAILY;
				EXECUTE "WHICH "+actualFilter
				
				WHICH NOT MISSING( ID( currencyObject ) )
				NEXT IF LENGTHCASE LE 0 --CHANGE
				LOCAL allCurrRates = NL( ID( currencyObject ) + "" )
				FREQ M
				NEW <glue dot> LOCAL'myExchRates = SLICE( CROSSLIST( nlExchObjPre, allCurrRates, nlExchObjPost ),ALL)
				LOCAL exchRates = myExchRates[caseorder]
				
				FREQ DAILY;
				EXECUTE "WHICH "+actualFilter
				
				WHICH NOT MISSING( ID( currencyObject ) )
				NEXT IF LENGTHCASE LE 0 --CHANGE
			  
					SET LOCAL'seriesAllTransCode = ID( currCodeObject )
					SET LOCAL'seriesAllInstCode = NAME( ii )
					SET LOCAL'seriesAllCurrCode = UPPER(TRIM(ID( currencyObject )))  --CHANGE
					SET LOCAL'seriesAllValue = ID( currSumObject )
					SET LOCAL'seriesAllFXRate = exchRates
					
					CASE *
					WHICH NOT MISSING( LOCAL'seriesAllCurrCode )
					SET LOCAL'seriesAllValueFX = LOCAL'seriesAllValue * LOCAL'seriesAllFXRate / 100

					WHICH LOCAL'seriesAllCurrCode eq "AWG" OR LOCAL'seriesAllCurrCode eq "ANG" OR  LOCAL'seriesAllCurrCode eq "GBP" OR LOCAL'seriesAllCurrCode eq "CAD" or LOCAL'seriesAllCurrCode eq "USD" --CHANGE
					SET LOCAL'seriesAllValueFX = LOCAL'seriesAllValue * LOCAL'seriesAllFXRate
					CASE *
					
					--CHANGE
					WHICH LOCAL'seriesAllCurrCode eq "AFL"
					SET LOCAL'seriesAllValueFX = LOCAL'seriesAllValue
					CASE *

					WHICH LOCAL'seriesAllCurrCode eq "ITL"
					SET LOCAL'seriesAllValueFX = LOCAL'seriesAllValue * LOCAL'seriesAllFXRate / 1000
					CASE *

					WHICH LOCAL'seriesAllCurrCode eq "JPY"
					SET LOCAL'seriesAllValueFX = LOCAL'seriesAllValue * LOCAL'seriesAllFXRate / 10000
					CASE *
					
					SET RESULT = RESULT + SUM(seriesAllValueFX)
					
			END BLOCK
			END IF
    
		END LOOP
	END LOOP
	
	RETURN RESULT

END BLOCK
END FUNCTION

FUNCTION $getMtcrCodes
ARGUMENT startDate, endDate, allInstitutions,codeObjectPre, codeObjectPost
BLOCK
	OVERWRITE ON
	CONVERT ON
	IGNORE ADDITION ON
	IGNORE ON
 	DECIMAL AUTO
 	FORCE NUMERIC OFF

	FREQ MONTHLY
	
	DATE startDate to endDate

	SERIES WORK'MOTRAllTransCode: PRECISION INDEXED BY CASE
 
	SERIES LOCAL'MOTRAllTransCode: PRECISION INDEXED BY CASE
	
	CASE * 
	LOOP FOR objPost IN codeObjectPost
		LOOP FOR dd IN DATE
			LOCAL currMonth = PAD( STRING( MONTH( dd ) ), 2, RIGHT, "0" )
			LOCAL currYear = YEAR( dd )
			LOCAL strCurrYearMonth = STRING( currYear ) + "." + STRING( currMonth )
			LOOP FOR ii IN allInstitutions
				LOCAL codeObject = codeObjectPre + "." + NAME( ii ) + "." + strCurrYearMonth + "." + NAME(objPost)
				IF EXISTS( ID( codeObject ) )
				BLOCK
		   
					CASE *
					
					WHICH NOT MISSING( LOCAL'MOTRAllTransCode )
					IF LENGTHCASE EQ 0
						CASE *
						WHICH NOT MISSING( ID( codeObject ) )
						SET LOCAL'MOTRAllTransCode = ID( codeObject )
					ELSE
						LOCAL maxnum = MAXCASE
						CASE *
						WHICH NOT MISSING( ID( codeObject ) )
						SET LOCAL'MOTRAllTransCode[n+maxnum] = ID( codeObject )[n]
					END IF

				END BLOCK
				END IF
		
			END LOOP
		END LOOP
	END LOOP
	
	IF(LASTVALUE(LOCAL'MOTRAllTransCode) NE NC AND LASTVALUE(LOCAL'MOTRAllTransCode) NE ND)
		NEW <CASE *> WORK'MOTRAllTransCode = MERGE(SORTDATA(LOCAL'MOTRAllTransCode))
	ELSE
		NEW <CASE *> WORK'MOTRAllTransCode = ND
	END IF

RETURN WORK'MOTRAllTransCode
END BLOCK
END FUNCTION

FUNCTION $getMtcrData
ARGUMENT startDate, endDate, allInstitutions,codeListObject,codeObjectPre,codeObjectPost,valueObjectPre,valueObjectPost
BLOCK
	OVERWRITE ON
	CONVERT ON
	IGNORE ADDITION ON
	IGNORE ON
 	DECIMAL AUTO
 	FORCE NUMERIC OFF

	FREQ MONTHLY

	DATE startDate to endDate
	
	SERIES WORK'MOTRAllValue: PRECISION INDEXED BY CASE
 
	SERIES LOCAL'MOTRAllValue: PRECISION INDEXED BY CASE
 	LOCAL SCALAR counter : PRECISION = 0
	
	IF FIRSTVALUE(codeListObject) EQ ND OR FIRSTVALUE(codeListObject) EQ NC
		RETURN ND
	END IF
	
	CASE * 
	LOOP FOR code IN NL(STRING(codeListObject))
		LOCAL SCALAR totalValue : PRECISION = 0
		LOOP FOR dd IN DATE
			LOCAL currMonth = PAD( STRING( MONTH( dd ) ), 2, RIGHT, "0" )
			LOCAL currYear = YEAR( dd )
			LOCAL strCurrYearMonth = STRING( currYear ) + "." + STRING( currMonth )
			LOOP FOR ii IN allInstitutions
				 
				LOCAL codeObject = codeObjectPre + "." + NAME( ii ) + "." + strCurrYearMonth + "." + codeObjectPost
				LOCAL valueObject = valueObjectPre + "." + NAME( ii ) + "." + strCurrYearMonth + "." + valueObjectPost
				
				IF EXISTS( ID( codeObject ) ) AND EXISTS( ID( valueObject ) )
				BLOCK
						CASE *
						WHICH ID( codeObject ) EQ NUMBER(NAME(code))
							NEXT IF LENGTHCASE LE 0
						SET LOCAL'totalValue = totalValue + SUM(ID(valueObject))
						
				END BLOCK
				END IF
			END LOOP
		END LOOP
		
		SET counter = counter + 1
		SET LOCAL'MOTRAllValue[counter] = LOCAL'totalValue
		
	END LOOP
	
	NEW <CASE *> WORK'MOTRAllValue = LOCAL'MOTRAllValue

RETURN MOTRAllValue

END BLOCK
END FUNCTION

FUNCTION $getMtcrCodesByReporter
ARGUMENT startDate, endDate, allInstitutions,codeObjectPre, codeObjectPost , recoObjectPre, recoObjectPost , reco
BLOCK
	OVERWRITE ON
	CONVERT ON
	IGNORE ADDITION ON
	IGNORE ON
 	DECIMAL AUTO
 	FORCE NUMERIC OFF

	FREQ MONTHLY
	
	DATE startDate to startDate

	SERIES WORK'MOTRAllTransCode: PRECISION INDEXED BY CASE
 
	SERIES LOCAL'MOTRAllTransCode: PRECISION INDEXED BY CASE
	
	CASE * 
	LOOP FOR objPost IN codeObjectPost
		LOOP FOR dd IN DATE
			LOCAL currMonth = PAD( STRING( MONTH( dd ) ), 2, RIGHT, "0" )
			LOCAL currYear = YEAR( dd )
			LOCAL strCurrYearMonth = STRING( currYear ) + "." + STRING( currMonth )
			LOOP FOR ii IN allInstitutions
				 
				LOCAL codeObject = codeObjectPre + "." + NAME( ii ) + "." + strCurrYearMonth + "." + NAME(objPost)
				--LOCAL recoObject = recoObjectPre + "." + NAME( ii ) + "." + strCurrYearMonth + "." + NAME(recoObjectPost)

				IF EXISTS( ID( codeObject ) )
				BLOCK
		   
					CASE *
					
					WHICH NOT MISSING( LOCAL'MOTRAllTransCode ) AND (UPPER(TRIM(ID(recoObject))) eq TRIM(UPPER(reco)))
					IF LENGTHCASE EQ 0
						CASE *
						WHICH NOT MISSING( ID( codeObject ) )
						SET LOCAL'MOTRAllTransCode = ID( codeObject )
					ELSE
						LOCAL maxnum = MAXCASE
						CASE *
						WHICH NOT MISSING( ID( codeObject ) )
						SET LOCAL'MOTRAllTransCode[n+maxnum] = ID( codeObject )[n]
					END IF

				END BLOCK
				END IF
		
			END LOOP
		END LOOP
	END LOOP
	
	IF(LASTVALUE(LOCAL'MOTRAllTransCode) NE NC AND LASTVALUE(LOCAL'MOTRAllTransCode) NE ND)
		NEW <CASE *> WORK'MOTRAllTransCode = MERGE(SORTDATA(LOCAL'MOTRAllTransCode))
	ELSE
		NEW <CASE *> WORK'MOTRAllTransCode = ND
	END IF

RETURN WORK'MOTRAllTransCode
END BLOCK
END FUNCTION


FUNCTION $getMtcrDataByReporter
ARGUMENT startDate, endDate, allInstitutions,codeListObject,codeObjectPre,codeObjectPost,valueObjectPre,valueObjectPost ,recoObjectPre, recoObjectPost , reco
BLOCK
	OVERWRITE ON
	CONVERT ON
	IGNORE ADDITION ON
	IGNORE ON
 	DECIMAL AUTO
 	FORCE NUMERIC OFF

	FREQ MONTHLY

	DATE startDate to startDate
	
	SERIES WORK'MOTRAllValue: PRECISION INDEXED BY CASE
 
	SERIES LOCAL'MOTRAllValue: PRECISION INDEXED BY CASE
 	LOCAL SCALAR counter : PRECISION = 0
	CASE * 
	LOOP FOR code IN NL(STRING(codeListObject))
		LOCAL SCALAR totalValue : PRECISION = 0
		LOOP FOR dd IN DATE
			LOCAL currMonth = PAD( STRING( MONTH( dd ) ), 2, RIGHT, "0" )
			LOCAL currYear = YEAR( dd )
			LOCAL strCurrYearMonth = STRING( currYear ) + "." + STRING( currMonth )
			LOOP FOR ii IN allInstitutions
				 
				LOCAL codeObject = codeObjectPre + "." + NAME( ii ) + "." + strCurrYearMonth + "." + codeObjectPost
				LOCAL valueObject = valueObjectPre + "." + NAME( ii ) + "." + strCurrYearMonth + "." + valueObjectPost
				LOCAL recoObject = recoObjectPre + "." + NAME( ii ) + "." + strCurrYearMonth + "." + recoObjectPost

				IF EXISTS( ID( codeObject ) ) AND EXISTS( ID( valueObject ) )
				BLOCK
						CASE *
						WHICH ID( codeObject ) EQ NUMBER(NAME(code)) AND (UPPER(TRIM(ID(recoObject))) eq TRIM(UPPER(reco)))
							NEXT IF LENGTHCASE LE 0
						SET LOCAL'totalValue = totalValue + SUM(ID(valueObject))
						
				END BLOCK
				END IF
			END LOOP
		END LOOP
		
		SET counter = counter + 1
		SET LOCAL'MOTRAllValue[counter] = LOCAL'totalValue
		
	END LOOP
	
	NEW <CASE *> WORK'MOTRAllValue = LOCAL'MOTRAllValue

RETURN MOTRAllValue

END BLOCK
END FUNCTION

---------------------Added by Santosh on 10Sept 2020 for Monthly FALA change ------------
FUNCTION $SORT_DATA_MONTHLY_WITH_CLOSING_ENTRY
ARGUMENTS %DATE_OBJ,%START_DT,%END_DT,%OBJ_NAME,%FILTER
    	TRY
           
           OVER ON
           Ignore ON,ADD ON,MUL ON
     
           CASE *
           FREQ M
    
           EXECUTE "WHICH "+%FILTER 
           
    	   IF LENGTHCASE GT 0
	           LOCAL NEW SORTED_DATA = MERGE(SORTDATA(STRING(%OBJ_NAME)))
		   ELSE
				SERIES WORK'SORTED_DATA:STRING BY CASE
		   END IF	 

		   CASE *
   			
           EXECUTE "WHICH EXSE.FBFI.BALA.DATEINDEX.C GE "+ %START_DT +" AND EXSE.FBFI.BALA.DATEINDEX.C LE "+%END_DT
            
           IF LENGTHCASE GT 0
	           LOCAL NEW SORTED_DATA1 = MERGE(SORTDATA(STRING(PRECISION(NUMBER(EXSE.FBFI.BALA.FATY.C)))))
		   ELSE
				SERIES WORK'SORTED_DATA1:STRING BY CASE
		   END IF

		   CASE *
   
		   LOCAL NEW SORTED_DATA2 = MERGE(SORTED_DATA,SORTED_DATA1)
  	
         	RETURN <CASE *>LOCAL'SORTED_DATA2
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION

FUNCTION GET_MONTHLY_BBR_DATA_WITH_CLOSING_ENTRY
ARGUMENTS %DATE_OBJ,%CODE_OBJECT,%FILTER_STMT,%SUMMED_VALUE_OBJECT,%START_DT,%END_DT,%TRAN_TYPE
    	TRY
           
           OVER ON
           Ignore ON,ADD ON,MUL ON
           
		   FREQ M
           LOCAL NEW SORTED_DATA =  $SORT_DATA_MONTHLY_WITH_CLOSING_ENTRY(%DATE_OBJ,%START_DT,%END_DT,%CODE_OBJECT,%FILTER_STMT)
           
           IF INDEX(SORTED_DATA) EQ "CASE" AND LASTVALUE(SORTED_DATA) NE NC AND LASTVALUE(SORTED_DATA) GT 0
          
           CASE FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
           LOCAL SERIES RESULT : PRECISION BY CASE
           
           
           LOOP FOR CODE=FIRSTVALUE(SORTED_DATA) TO LASTVALUE(SORTED_DATA)
              
              SET RESULT[CODE] = $FILTER_DATA(%DATE_OBJ,%FILTER_STMT+" AND  "+NAME(%CODE_OBJECT)+" EQ "+string(SORTED_DATA[CODE]),%SUMMED_VALUE_OBJECT) + $GET_CLOSING_ENTRY_BALA_DATE_RANGE(%START_DT,%END_DT,SORTED_DATA[CODE],%TRAN_TYPE)
             	
	       END LOOP
          ELSE
          	 LOCAL NEW RESULT = ND
          END IF
     
     	RETURN RESULT
       
         	RETURN<case *> SORTED_DATA
    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION

FUNCTION $GET_CLOSING_ENTRY_BALA_DATE_RANGE
ARGUMENTS %START_DT,%END_DT,%CODE,%TRAN_TYPE
    	TRY
           
           OVER ON
           Ignore ON,ADD ON,MUL ON
		   
		   FREQ M
		   
		   CASE *
           DATE *

           LOCAL SCALAR RESULT  : PRECISION 
         
           EXEC "WHICH EXSE.FBFI.BALA.DATEINDEX.C GE "+%START_DT+" AND EXSE.FBFI.BALA.DATEINDEX.C LE "+%END_DT+" AND NUMBER(EXSE.FBFI.BALA.FATY.C) EQ "+%CODE
           
           IF LENGTHCASE GT 0
               
               SET RESULT = SUM(EXSE.FBFI.BALA.TODE.C) - SUM(EXSE.FBFI.BALA.TOCR.C)
               
               IF NOT MISSING(RESULT) AND %TRAN_TYPE EQ "CR" AND RESULT GT 0
                      RETURN RESULT
               ELSE IF NOT MISSING(RESULT) AND %TRAN_TYPE EQ "DR" AND RESULT LT 0
					  RETURN (RESULT * -1)
               ELSE  
               		RETURN 0
               END IF
               
	       ELSE

           	RETURN 0
           
           END IF

    	OTHERWISE
    		SIGNAL ERROR: ERRORTEXT
    	END TRY
END FUNCTION


--FUNCTION TO RETURN CONSOLIDATED FROM FEBFLOW SPLIT SERIES FOR ALL INSTITUTES
--Developed for edit case series forms
--clear output, info; cload <channel warnings none, results none>"customfame"; disp $get_mtcr_cons_series_mul_filters_all_ins("EXSE.FEB.FLOW.ALL.RECO.C","01Apr2018 AND 30Apr2018","ALL","","","","","","","","","","","","","","","","","")
FUNCTION $get_mtcr_cons_series_mul_filters_all_ins
ARGUMENTS %series_name, %sdate_and_date, %inna, %freqtype, %reco="", %rena="", %sena="", %srn="", %tram="", %trco="",%trde=""
OVER ON
EXECUTE "FREQ "+%freqtype
TRY
	LOCAL NEW temp_str = SUBSTRING(MIRROR(%series_name), LOCATION(MIRROR(%series_name), ".") +1, LENGTH(MIRROR(%series_name)))
	LOCAL NEW series_dim = MIRROR(SUBSTRING(LOCAL'temp_str, 0, LOCATION(LOCAL'temp_str, ".")-1))
	LOCAL NEW dotstr = SUBSTRING(temp_str,LOCATION(MIRROR(MIRROR(temp_str)), ".")+1,LENGTH(temp_str))
	LOCAL NEW sheet_name = MIRROR(SUBSTRING(dotstr,0,LOCATION(dotstr, ".")-1))
	LOCAL NEW inout = SUBSTRING(SUBSTRING(%series_name, 11 ,LENGTH(%series_name)),0,LOCATION(SUBSTRING(%series_name, 11 ,LENGTH(%series_name)), ".")-1)
	type inout
	
	LOCAL SERIES <CASE 1 TO 6> !loperatos:STRING BY CASE = "GT", "GE", "LT", "LE", "EQ", "NE"
	LOCAL SERIES <CASE 1 TO 2> !boperatos:STRING BY CASE = "AND", "OR"
	LOCAL SCALAR !tempcs : NUMERIC = 1
	LOCAL SCALAR !queryStr : STRING
	LOCAL NEW temp_trco = %trco
	LOCAL NEW temp_tram = %tram
	LOCAL NEW temp_reco = UPPER(%reco)
	LOCAL NEW temp_sena = UPPER(%sena)
	LOCAL NEW temp_rena = UPPER(%rena)
	LOCAL NEW temp_srn =  UPPER(%srn)
	LOCAL NEW temp_trde = UPPER(%trde)

	IF MISSING(LOCATION(TRIM(UPPER(%series_name)), "ALL")) 
		IF NOT MISSING(location(TYPE(ID(%series_name)),"DATE"))
			EXECUTE "LOCAL SERIES cons_series:DATE("+%freqtype+") BY CASE"
		ELSE
			EXECUTE "LOCAL SERIES cons_series:"+TYPE(ID(%series_name))+" BY CASE"
		END IF
	END IF
	
	--Get the start and end date
	LOCAL NEW tdate = %sdate_and_date
	BLOCK
		CASE FIRSTVALUE(loperatos) TO LASTVALUE(loperatos)
		LOOP FOR ex in CASE
			SET LOCAL'tdate = REPLACE(LOCAL'tdate, loperatos[ex], "")
		END LOOP
		CASE FIRSTVALUE(boperatos) TO LASTVALUE(boperatos)
		LOOP FOR bl in CASE
			SET LOCAL'tdate = REPLACE(LOCAL'tdate, boperatos[bl], "")
		END LOOP
		LOCAL NEW %sdate = TRIM(SUBSTRING(TRIM(LOCAL'tdate), 1, LOCATION(TRIM(LOCAL'tdate)," ")))
		LOCAL NEW %edate = TRIM(SUBSTRING(TRIM(LOCAL'tdate), LOCATION(TRIM(LOCAL'tdate)," ")+1, LENGTH(TRIM(LOCAL'tdate))))
		--DISP LOCAL'tdate, LOCAL'%sdate, LOCAL'%edate
	END BLOCK
	
	--IF sdate and edate are present create series ignore CASE range
	IF (LENGTH(TRIM(LOCAL'%sdate)) NE 0) AND (LENGTH(TRIM(LOCAL'%edate)) NE 0)
		BLOCK
			DATE MAKE(DATE(DAILY),LOCAL'%sdate) TO MAKE(DATE(DAILY),LOCAL'%edate)
			FREQ MON
    	SET LOCAL'tempcs = 1
			LOOP FOR dt IN DATE
				--DISP dt
				IF MONTH(dt) LE 9
					LOCAL NEW monstr = "0"+STRING(MONTH(dt))
				ELSE
					LOCAL NEW monstr = STRING(MONTH(dt))
				END IF
				LOOP FOR %institue IN {UNIONCARIBE, MAEXS}
					SET LOCAL'queryStr=""
					
				IF inout eq "NOTDITS" AND inout NE ""
					LOCAL NEW obj_name = "FISE.MOTR."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+inout+"."+sheet_name+"."+LOCAL'series_dim+".C"
					LOCAL NEW pk_obj_name = "FISE.MOTR."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+inout+"."+sheet_name+".PRIMKEY.C"
				ELSE
					LOCAL NEW obj_name = "FISE.MOTR."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+sheet_name+"."+LOCAL'series_dim+".C"
					LOCAL NEW pk_obj_name = "FISE.MOTR."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+sheet_name+".PRIMKEY.C"
				END IF
					IF EXISTS(ID(LOCAL'pk_obj_name)) AND NOT MISSING(FIRSTVALUE(ID(LOCAL'pk_obj_name)))
					  --Convert expressions to FAMe 4GL expressions
					  
					  --DISP "FISE.MOTR."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".OUTR.INNA.C"
					  LOCAL NEW trco = $get_4gl_expression(LOCAL'temp_trco, loperatos, "FISE.MOTR."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+sheet_name+".TRCO.C")
					  LOCAL NEW reco = $get_4gl_expression(LOCAL'temp_reco, loperatos, "FISE.MOTR."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+sheet_name+".RECO.C")
					  LOCAL NEW rena = $get_4gl_expression(LOCAL'temp_rena, loperatos, "FISE.MOTR."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+sheet_name+".RENA.C")
					  LOCAL NEW sena = $get_4gl_expression(LOCAL'temp_sena, loperatos, "FISE.MOTR."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+sheet_name+".SENA.C")
					  LOCAL NEW srn  = $get_4gl_expression(LOCAL'temp_srn,  loperatos, "FISE.MOTR."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+sheet_name+".SRN.C")
					  LOCAL NEW tram = $get_4gl_expression(LOCAL'temp_tram, loperatos, "FISE.MOTR."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+sheet_name+".TRAM.C")
					  LOCAL NEW trde = $get_4gl_expression(LOCAL'temp_trde, loperatos, "FISE.MOTR."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+sheet_name+".TRDE.C")
					  --DISP LOCAL'trco ,LOCAL'reco, LOCAL'rena, LOCAL'nona, LOCAL'sena, LOCAL'srn, LOCAL'tram, LOCAL'trde
					  --DISP LOCAL'obj_name				
					  IF EXISTS(ID(LOCAL'obj_name)) --AND NOT MISSING(FIRSTVALUE(ID(LOCAL'obj_name)))
					  	CASE *
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'reco))) AND LENGTH(TRIM(LOCAL'reco)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'reco), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'reco +" AND "
					  	END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'rena))) AND LENGTH(TRIM(LOCAL'rena)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'rena), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'rena +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'sena))) AND LENGTH(TRIM(LOCAL'sena)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'sena), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'sena  +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'srn))) AND LENGTH(TRIM(LOCAL'srn)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'srn), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'srn +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'tram))) AND LENGTH(TRIM(LOCAL'tram)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'tram), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'tram +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'trco))) AND LENGTH(TRIM(LOCAL'trco)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'trco), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'trco +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'trde))) AND LENGTH(TRIM(LOCAL'trde)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'trde), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'trde +" AND "
					  	END IF					  	
					  	--DISP LOCAL'queryStr
					  	IF NOT MISSING(LOCATION(SUBSTRING(MIRROR(TRIM(LOCAL'queryStr)),0,4), "DNA", 1))
					  		SET LOCAL'queryStr = TRIM(MIRROR(SUBSTRING(MIRROR(TRIM(LOCAL'queryStr)), LOCATION(MIRROR(TRIM(LOCAL'queryStr)), "DNA")+3, LENGTH(MIRROR(TRIM(LOCAL'queryStr)))))
					  	END IF
					  	--DISP LOCAL'queryStr
					  					      				
					  	
					  	IF NOT MISSING(local'queryStr) AND local'queryStr NE ""
					  		EXECUTE "WHICH "+local'queryStr
					  	ELSE
					  		CASE FIRSTVALUE(ID(LOCAL'pk_obj_name)) TO LASTVALUE(ID(LOCAL'pk_obj_name))
					  	END IF
					  	--DISPLAY DT,LENGTHCASE
					  	
 					  	IF LENGTHCASE GT 0
 					  		LOOP FOR cs IN CASE
 					  			DECIMAL 15
 					  			--TYPE LOCAL'tempcs, cs, ID(LOCAL'obj_name)[cs]
 					  			SET LOCAL'cons_series[LOCAL'tempcs] = ID(LOCAL'obj_name)[cs]
					  			SET LOCAL'tempcs = LOCAL'tempcs + 1
					  		END LOOP
 					  	END IF --LENGTHCASE GT 0
 					  	--whats LOCAL'cons_series
					  ELSE
					  	SIGNAL CONTINUE : ERRORTEXT
					  END IF --IF EXISTS(ID(LOCAL'obj_name) AND NOT MISSING(FIRSTVALUE(ID(LOCAL'obj_name)))
					  END IF --IF EXISTS(pk_obj_name)
				END LOOP --FOR %institue IN {AUA, BDC, CMB, RBCARUBA, FCIBARUBA, CBA}
			END LOOP --LOOP FOR dt in DATE
		END BLOCK
	ELSE
		SIGNAL ERROR : "$get_mtcr_cons_series_mul_filters_all_ins() : INVALID DATE RANGE"
	END IF	--(LENGTH(TRIM(LOCAL'%sdate)) NE 0) AND (LENGTH(TRIM(LOCAL'%edate)) NE 0)
OTHERWISE
	SIGNAL ERROR:ERRORTEXT
END TRY	
	--CASE *
	RETURN <case *>cons_series	
	--RETURN ID(obj_name)
END FUNCTION






---------------------------ANIKET-------------------
----------
--FUNCTION TO RETURN CONSOLIDATED FROM FEBFLOW SPLIT SERIES
--Developed for edit case series forms
--disp $get_febflow_cons_series_mul_filters("EXSE.FEB.FLOW.RECO.C","01Apr2018 AND 30Apr2018","AUA"," GT ""100"" AND LT  ""1000"""," EQ ""BV"" OR EQ ""LNB"" OR EQ ""BKA""","EQ ""DIVERSE INGEZETENEN"" OR EQ ""BANCO DI CARIBE ARUBA NV"" OR EQ ""BANCO DI CARIBE ARUBA NV""","EQ ""VASA RECEIVABLE CC PAYMENT ACCOUNT"" OR EQ ""BANCO DI CARIBE ARUBA NV""","LT 4000 AND GT 3000","EQ 5510 OR MISSING()","EQ ""AN"" OR EQ ""US""","EQ ""ANG"" OR EQ ""USD""","GT 1000 AND LT 50000","GT 1000 AND LT 50000","GT 1000 AND LT 50000","GT 1000 AND LT 50000","EQ 0 AND LT 2","EQ ""79"" OR MISSING()","EQ ""DIFFERENT CONSTRUCTION INVOICES"" OR EQ ""$$/MARCH AND APRIL INTEREST CORRESPOND""","EQ ""39120""","MISSING()")
--disp $get_febflow_cons_series_mul_filters("EXSE.FEB.FLOW.RECO.C","01Apr2018 AND 30Apr2018","AUA"," GT ""100"" AND LT  ""1000"""," EQ ""BV"" OR EQ ""LNB"" OR EQ ""BKA""","","","","","","","","","","","","","","","MISSING()")
--disp remeval(rem,"$get_febflow_cons_series_mul_filters(""EXSE.FEB.FLOW.RECO.C"",""01Apr2018 AND 30Apr2018"",""AUA"","" GT """"100"""" AND LT  """"1000"""""","" EQ """"BV"""" OR EQ """"LNB"""" OR EQ """"BKA"""""","""","""","""","""","""","""","""","""","""","""","""","""","""","""","""")")
--clear;cload "C:\Projects\map\fame\procs-fedm\customfame_fix_string_like_search_bop.pro";clear output, info;disp<case *> $get_febflow_cons_series_mul_filters("EXSE.FEB.FLOW.ALL.RECO.C","01Apr018 AND 30Apr2018","","","","","","","","","","","","","","","","","","")
FUNCTION $get_mtcr_cons_series_mul_filters
ARGUMENTS %series_name, %sdate_and_date, %institue, %freqtype, %reco="", %rena="", %sena="", %srn="", %tram="", %trco="",%trde=""
OVER ON
EXECUTE "FREQ "+%freqtype
TRY
	IF MISSING(LOCATION(TRIM(UPPER(%series_name)), "ALL")) 
		IF NOT MISSING(location(TYPE(ID(%series_name)),"DATE"))
			EXECUTE "LOCAL SERIES cons_series:DATE("+%freqtype+") BY CASE"
		ELSE
			EXECUTE "LOCAL SERIES cons_series:"+TYPE(ID(%series_name))+" BY CASE"
		END IF
	END IF
	LOCAL NEW temp_str = SUBSTRING(MIRROR(%series_name), LOCATION(MIRROR(%series_name), ".") +1, LENGTH(MIRROR(%series_name)))
	LOCAL NEW inout = SUBSTRING(SUBSTRING(%series_name, 11 ,LENGTH(%series_name)),0,LOCATION(SUBSTRING(%series_name, 11 ,LENGTH(%series_name)), ".")-1)
	LOCAL NEW series_dim = MIRROR(SUBSTRING(LOCAL'temp_str, 0, LOCATION(LOCAL'temp_str, ".")-1))
	LOCAL NEW dotstr = SUBSTRING(temp_str,LOCATION(MIRROR(MIRROR(temp_str)), ".")+1,LENGTH(temp_str))
	LOCAL NEW sheet_name = MIRROR(SUBSTRING(dotstr,0,LOCATION(dotstr, ".")-1))

	LOCAL SERIES <CASE 1 TO 6> !loperatos:STRING BY CASE = "GT", "GE", "LT", "LE", "EQ", "NE"
	LOCAL SERIES <CASE 1 TO 2> !boperatos:STRING BY CASE = "AND", "OR"
	LOCAL SCALAR !tempcs : NUMERIC = 1
	LOCAL SCALAR !queryStr : STRING
	LOCAL NEW temp_trco = %trco
	LOCAL NEW temp_tram = %tram
	LOCAL NEW temp_reco = UPPER(%reco)
	LOCAL NEW temp_sena = UPPER(%sena)
	LOCAL NEW temp_rena = UPPER(%rena)
	LOCAL NEW temp_srn =  UPPER(%srn)
	LOCAL NEW temp_trde = UPPER(%trde)
	
	--Get the start and end date
	LOCAL NEW tdate = %sdate_and_date
	BLOCK
		CASE FIRSTVALUE(loperatos) TO LASTVALUE(loperatos)
		LOOP FOR ex in CASE
			SET LOCAL'tdate = REPLACE(LOCAL'tdate, loperatos[ex], "")
		END LOOP
		CASE FIRSTVALUE(boperatos) TO LASTVALUE(boperatos)
		LOOP FOR bl in CASE
			SET LOCAL'tdate = REPLACE(LOCAL'tdate, boperatos[bl], "")
		END LOOP
		LOCAL NEW %sdate = TRIM(SUBSTRING(TRIM(LOCAL'tdate), 1, LOCATION(TRIM(LOCAL'tdate)," ")))
		LOCAL NEW %edate = TRIM(SUBSTRING(TRIM(LOCAL'tdate), LOCATION(TRIM(LOCAL'tdate)," ")+1, LENGTH(TRIM(LOCAL'tdate))))
		--DISP LOCAL'tdate, LOCAL'%sdate, LOCAL'%edate
	END BLOCK
	
	  --Get data for all institues
 	 IF NOT MISSING(LOCATION(TRIM(UPPER(%series_name)), "ALL"))
 		 	RETURN <case *>$get_all_institutes_data(%series_name, %sdate, %edate, series_dim)
 	 END IF
	 
	 
	
	--IF sdate and edate are present create series ignore CASE range
	IF (LENGTH(TRIM(LOCAL'%sdate)) NE 0) AND (LENGTH(TRIM(LOCAL'%edate)) NE 0)
		BLOCK
			DATE MAKE(DATE(DAILY),LOCAL'%sdate) TO MAKE(DATE(DAILY),LOCAL'%edate)
			FREQ MON
    	SET LOCAL'tempcs = 1
			LOOP FOR dt IN DATE
				--DISP dt
				IF MONTH(dt) LE 9
					LOCAL NEW monstr = "0"+STRING(MONTH(dt))
				ELSE
					LOCAL NEW monstr = STRING(MONTH(dt))
				END IF
				SET LOCAL'queryStr=""
				
				IF inout eq "NOTDITS" AND inout NE ""
					LOCAL NEW obj_name = "FISE.MOTR."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+inout+"."+sheet_name+"."+LOCAL'series_dim+".C"
					LOCAL NEW pk_obj_name = "FISE.MOTR."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+inout+"."+sheet_name+".PRIMKEY.C"
				ELSE
					LOCAL NEW obj_name = "FISE.MOTR."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+sheet_name+"."+LOCAL'series_dim+".C"
					LOCAL NEW pk_obj_name = "FISE.MOTR."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+sheet_name+".PRIMKEY.C"
				END IF
				--Convert expressions to FAMe 4GL expressions
				--DISP "FISE.MOTR."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".REID.C"
					LOCAL NEW trco = $get_4gl_expression(LOCAL'temp_trco, loperatos, "FISE.MOTR."+  %institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+sheet_name+".TRCO.C")
					  LOCAL NEW reco = $get_4gl_expression(LOCAL'temp_reco, loperatos, "FISE.MOTR."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+sheet_name+".RECO.C")
					  LOCAL NEW rena = $get_4gl_expression(LOCAL'temp_rena, loperatos, "FISE.MOTR."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+sheet_name+".RENA.C")
					  LOCAL NEW sena = $get_4gl_expression(LOCAL'temp_sena, loperatos, "FISE.MOTR."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+sheet_name+".SENA.C")
					  LOCAL NEW srn  = $get_4gl_expression(LOCAL'temp_srn, loperatos, "FISE.MOTR."+ %institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+sheet_name+".SRN.C")
					  LOCAL NEW tram = $get_4gl_expression(LOCAL'temp_tram, loperatos, "FISE.MOTR."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+sheet_name+".TRAM.C")
					  LOCAL NEW trde = $get_4gl_expression(LOCAL'temp_trde, loperatos, "FISE.MOTR."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+sheet_name+".TRDE.C")
					  
					--DISP LOCAL'reid,LOCAL'acty,LOCAL'rena, LOCAL'nona, LOCAL'trco, LOCAL'isic, LOCAL'coco, LOCAL'cuco, LOCAL'fcde, LOCAL'afld, LOCAL'fccr, LOCAL'aflc
					--DISP LOCAL'fec, LOCAL'lice, LOCAL'desc, LOCAL'cnc
					--DISP LOCAL'cbar
								
				IF EXISTS(ID(LOCAL'obj_name) AND NOT MISSING(FIRSTVALUE(ID(LOCAL'obj_name)))
					CASE *
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'reco))) AND LENGTH(TRIM(LOCAL'reco)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'reco), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'reco +" AND "
					  	END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'rena))) AND LENGTH(TRIM(LOCAL'rena)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'rena), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'rena +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'sena))) AND LENGTH(TRIM(LOCAL'sena)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'sena), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'sena  +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'srn))) AND LENGTH(TRIM(LOCAL'srn)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'srn), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'srn +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'tram))) AND LENGTH(TRIM(LOCAL'tram)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'tram), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'tram +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'trco))) AND LENGTH(TRIM(LOCAL'trco)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'trco), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'trco +" AND "
					  	END IF
					  	IF (NOT MISSING(LENGTH(TRIM(LOCAL'trde))) AND LENGTH(TRIM(LOCAL'trde)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'trde), "EXPRESSSION_MISSING"))
					  		SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'trde +" AND "
					  	END IF					  	
					--DISP LOCAL'queryStr
					IF NOT MISSING(LOCATION(SUBSTRING(MIRROR(TRIM(LOCAL'queryStr)),0,4), "DNA", 1))
						SET LOCAL'queryStr = TRIM(MIRROR(SUBSTRING(MIRROR(TRIM(LOCAL'queryStr)), LOCATION(MIRROR(TRIM(LOCAL'queryStr)), "DNA")+3, LENGTH(MIRROR(TRIM(LOCAL'queryStr)))))
					END IF
					--DISP LOCAL'queryStr
					--				      				
					
					IF NOT MISSING(local'queryStr) AND local'queryStr NE ""
						EXECUTE "WHICH "+local'queryStr
					ELSE
						CASE FIRSTVALUE(ID(LOCAL'pk_obj_name)) TO LASTVALUE(ID(LOCAL'pk_obj_name))
					END IF
					--DISPLAY LENGTHCASE
					
 					IF LENGTHCASE GT 0
 						LOOP FOR cs IN CASE
 							SET LOCAL'cons_series[LOCAL'tempcs] = ID(LOCAL'obj_name)[cs]
							SET LOCAL'tempcs = LOCAL'tempcs + 1
						END LOOP
 					END IF --LENGTHCASE GT 0
				ELSE
					SIGNAL CONTINUE : ERRORTEXT
				END IF --EXISTS(ID("FISE.MOTR."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C")) AND &&
			END LOOP
		END BLOCK
	ELSE
		SIGNAL ERROR : "$get_febflow_cons_series_mul_filters() : INVALID DATE RANGE"
	END IF	--(LENGTH(TRIM(LOCAL'%sdate)) NE 0) AND (LENGTH(TRIM(LOCAL'%edate)) NE 0)
OTHERWISE
	SIGNAL ERROR:ERRORTEXT
END TRY	
	RETURN <case *>cons_series
END FUNCTION

--Added By Santosh on 28-12-2020 For Prud report money trasnfer companies - summary ratios
--It calculates sum of splitted case series based on month name and institute name without any filter for given date range
--disp getInstitutewiseSplittedMonthlyData(2020M8,2020m10,{"MAEXS","UNIONCARIBE"},"FISE.MOTR","NOTDITS.OUTG.NOTB0A2.C")
FUNCTION getInstitutewiseSplittedMonthlyData
ARGUMENT startDate, endDate, allInstitutions,sumObjectPre, sumObjectPost
BLOCK
	CONVERT ON
	IGNORE ON
	IGNORE ADDITION ON
	OVERWRITE ON
 	FORCE NUMERIC OFF
	FREQ MONTHLY
	DATE *
	DATE startDate to endDate
	LOCAL SERIES seriesOut: PRECISION BY DATE
    
	LOOP FOR dd IN DATE
		 LOCAL currMonth = PAD( STRING( MONTH( dd ) ), 2, RIGHT, "0" )
		 LOCAL currYear = YEAR( dd )
		 LOCAL iTotal = 0
   		 LOCAL strCurrYearMonth = STRING( currYear ) + "." + STRING( currMonth )
		 
		 LOOP FOR ii IN allInstitutions
		 
			LOCAL sumObject = sumObjectPre + "." + NAME( ii ) + "." + strCurrYearMonth + "." + sumObjectPost
   			LOCAL currTotal = 0
			
   			IF EXISTS( ID( sumObject ) ) AND LASTVALUE( ID( sumObject ) ) NE NC
				SET currTotal = SUM(ID(sumObject))
			END IF

           	SET iTotal = iTotal + currTotal	   
   
		 END LOOP
		 SET seriesOut[dd] = iTotal
   		 SET iTotal = 0
	END LOOP
	
	RETURN seriesOut

END BLOCK
END FUNCTION

--eg : getLatestReportersList(1) 
FUNCTION getLatestReportersList
ARGUMENT whichDate
	CASE *
	FREQ D
	OVER ON
	SERIES WORK'dates: DATE INDEXED BY CASE
	SERIES WORK'accn: STRING INDEXED BY CASE
	SERIES WORK'recos: STRING INDEXED BY CASE
	SERIES WORK'resi: STRING INDEXED BY CASE
	SERIES WORK'nore: STRING INDEXED BY CASE
	SERIES WORK'tyfa: STRING INDEXED BY CASE
	
	NEW LOCAL'xCodes = MERGE(SORTDATA(TRIM(UPPER(EXSE.NOFO.FAFI.RECO.C))))
	
    LOOP FOR ii=FIRSTVALUE(xCodes) TO LASTVALUE(xCodes)
		CASE *
		WHICH TRIM(UPPER(EXSE.NOFO.FAFI.RECO.C)) EQ xCodes[ii] AND EXSE.NOFO.FAFI.DATEINDEX.C LE whichDate AND EXSE.NOFO.FAFI.DATEINDEX.C NE ND 
		IF LENGTHCASE GT 0
		WHICH EXSE.NOFO.FAFI.DATEINDEX.C EQ MAX(EXSE.NOFO.FAFI.DATEINDEX.C)
			IF LENGTHCASE GT 0
				IF LASTVALUE(WORK'dates) EQ NC OR LASTVALUE(WORK'dates) EQ ND
					SET dates[CASEORDER] = EXSE.NOFO.FAFI.DATEINDEX.C
					SET accn[CASEORDER] = EXSE.NOFO.FAFI.ACCN.C
					SET recos[CASEORDER] = xCodes[ii]
					SET resi[CASEORDER] = EXSE.NOFO.FAFI.LEGNA.C
					SET nore[CASEORDER] = OVERLAY(EXSE.NOFO.FAFI.BNAN.C,EXSE.NOFO.FAFI.FOIN.C)
					SET tyfa[CASEORDER] = if location("FBA",OVERLAY(EXSE.NOFO.FAFI.TYFA.C,EXSE.NOFO.FAFI.ACCO.C)) gt 0 OR location("FIA",OVERLAY(EXSE.NOFO.FAFI.TYFA.C,EXSE.NOFO.FAFI.ACCO.C)) gt 0 then LEFT(OVERLAY(EXSE.NOFO.FAFI.TYFA.C,EXSE.NOFO.FAFI.ACCO.C),3) else ""
		            disp tyfa

				ELSE
					NEW size = LASTVALUE(WORK'dates)
					SET dates[CASEORDER + size] = EXSE.NOFO.FAFI.DATEINDEX.C
					SET accn[CASEORDER + size] = EXSE.NOFO.FAFI.ACCN.C
					SET recos[CASEORDER + size] = xCodes[ii]
					SET resi[CASEORDER + size] = EXSE.NOFO.FAFI.LEGNA.C
					SET nore[CASEORDER + size] = OVERLAY(EXSE.NOFO.FAFI.BNAN.C,EXSE.NOFO.FAFI.FOIN.C)
					SET tyfa[CASEORDER + size] = if location("FBA",OVERLAY(EXSE.NOFO.FAFI.TYFA.C,EXSE.NOFO.FAFI.ACCO.C)) gt 0 OR location("FIA",OVERLAY(EXSE.NOFO.FAFI.TYFA.C,EXSE.NOFO.FAFI.ACCO.C)) gt 0 then LEFT(OVERLAY(EXSE.NOFO.FAFI.TYFA.C,EXSE.NOFO.FAFI.ACCO.C),3) else ""
				END IF
			END IF
		END IF
	END LOOP
	CASE *
	--SHOW V
	--LENGTH FULL
	--REPO WORK'dates,WORK'accn,WORK'recos,WORK'names
RETURN 1
END FUNCTION

FUNCTION $EvaluateStringExpression
  ARGUMENT %expression
               EXECUTE %expression
               RETURN TRUE
END FUNCTION

				 
FUNCTION $existsValues
ARGUMENT objName
	BLOCK
		If EXISTS( objName )
		 IF INDEX( objName ) EQ "CASE"
		  CASE *
		  WHICH NOT MISSING( objName )
		  IF LENGTHCASE GT 0
		      RETURN TRUE
		  END IF
		 ELSE IF INDEX( objName ) EQ "DATE"
		   DATE *
		   WHICH NOT MISSING( objName )
		   IF LENGTHDATE GT 0
		       RETURN TRUE
		   END IF
		 END IF
		END IF
		RETURN FALSE
	END BLOCK
END FUNCTION


--FUNCTION TO RETURN CONSOLIDATED FROM Resi SERIES
--Developed for edit case series forms
--disp $get_resi_cons_series_mul_filters("OPER.TFMABO.RESI.EX.ACTY.C","01Dec2021 AND 31Dec2021","","BDC","YES","","","","","","","","","","","","")
FUNCTION $get_resi_cons_series_mul_filters
ARGUMENTS %series_name, %sdate_and_date, %acty="", %institue, %eyon="", %oot="", %bot="", %curr="", %fcde="", %afde="",%fccr="",%afcr="", %desc="",%rfe="",%otini="",%otinii=""
OVER ON
FREQ DAILY
TRY
		
	LOCAL NEW temp_str = SUBSTRING(MIRROR(%series_name), LOCATION(MIRROR(%series_name), ".") +1, LENGTH(MIRROR(%series_name)))
	LOCAL NEW series_dim = MIRROR(SUBSTRING(LOCAL'temp_str, 0, LOCATION(LOCAL'temp_str, ".")-1))
	LOCAL SERIES <CASE 1 TO 6> !loperatos:STRING BY CASE = "GT", "GE", "LT", "LE", "EQ", "NE"
	LOCAL SERIES <CASE 1 TO 2> !boperatos:STRING BY CASE = "AND", "OR"
	LOCAL SCALAR !tempcs : NUMERIC = 1
	LOCAL SCALAR !queryStr : STRING	
	LOCAL SCALAR !objDim : STRING	
	LOCAL NEW temp_acty   = UPPER(%acty)
	LOCAL NEW temp_oot    = UPPER(%oot)
	LOCAL NEW temp_bot    = UPPER(%bot)
	LOCAL NEW temp_curr   = UPPER(%curr)
	LOCAL NEW temp_fcde   = UPPER(%fcde)
	LOCAL NEW temp_afde   = UPPER(%afde)
	LOCAL NEW temp_fccr   = UPPER(%fccr)
	LOCAL NEW temp_afcr   = UPPER(%afcr)
	LOCAL NEW temp_desc   = UPPER(%desc)
	LOCAL NEW temp_eyon   = UPPER(%eyon)
	LOCAL NEW temp_rfe    = UPPER(%rfe)
	LOCAL NEW temp_otini  = UPPER(%otini)
	LOCAL NEW temp_otinii = UPPER(%otinii)
	--Get the start and end date
	
	
	LOCAL NEW tdate = %sdate_and_date
	BLOCK
		CASE FIRSTVALUE(loperatos) TO LASTVALUE(loperatos)
		LOOP FOR ex in CASE
			SET LOCAL'tdate = REPLACE(LOCAL'tdate, loperatos[ex], "")
		END LOOP
		CASE FIRSTVALUE(boperatos) TO LASTVALUE(boperatos)
		LOOP FOR bl in CASE
			SET LOCAL'tdate = REPLACE(LOCAL'tdate, boperatos[bl], "")
		END LOOP
		LOCAL NEW %sdate = TRIM(SUBSTRING(TRIM(LOCAL'tdate), 1, LOCATION(TRIM(LOCAL'tdate)," ")))
		LOCAL NEW %edate = TRIM(SUBSTRING(TRIM(LOCAL'tdate), LOCATION(TRIM(LOCAL'tdate)," ")+1, LENGTH(TRIM(LOCAL'tdate))))
		
	END BLOCK
		
	--IF sdate and edate are present create series ignore CASE range
	IF (LENGTH(TRIM(LOCAL'%sdate)) NE 0) AND (LENGTH(TRIM(LOCAL'%edate)) NE 0)
		BLOCK
			DATE MAKE(DATE(DAILY),LOCAL'%sdate) TO MAKE(DATE(DAILY),LOCAL'%edate)
			FREQ MON
			SET LOCAL'tempcs = 1
			LOOP FOR dt IN DATE
				IF MONTH(dt) LE 9
					LOCAL NEW monstr = "0"+STRING(MONTH(dt))
				ELSE
					LOCAL NEW monstr = STRING(MONTH(dt))
				END IF
				SET LOCAL'queryStr=""
				
				--Check for exempt/non-exempt
				IF LOCAL'temp_eyon eq "YES" 
					SET LOCAL'objDim = "EX"
					ELSE
						SET LOCAL'objDim = "NOEX"
				END IF
				
				
				LOCAL NEW obj_name = "OPER.TFMABO.RESI."+LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C"
				LOCAL NEW pk_obj_name = "OPER.TFMABO.RESI."+LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".PRIMKEY.C"
				
				
				LOCAL series_name = "OPER.TFMABO.RESI."+LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C"
				
				IF NOT MISSING(location(TYPE(ID(LOCAL'series_name)),"DATE"))  
						LOCAL SERIES cons_series:DATE(DAILY) BY CASE
					ELSE
						EXECUTE "LOCAL SERIES cons_series:"+TYPE(ID(LOCAL'series_name))+" BY CASE"
				END IF
				--Convert expressions to FAMe 4GL expressions
				
				LOCAL NEW acty   =  $get_4gl_expression(LOCAL'temp_acty  ,loperatos, "OPER.TFMABO.RESI." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".ACTY.C")
				LOCAL NEW oot    =  $get_4gl_expression(LOCAL'temp_oot   ,loperatos, "OPER.TFMABO.RESI." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".OOT.C")
				LOCAL NEW bot    =  $get_4gl_expression(LOCAL'temp_bot   ,loperatos, "OPER.TFMABO.RESI." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".BOT.C")
				LOCAL NEW curr   =  $get_4gl_expression(LOCAL'temp_curr  ,loperatos, "OPER.TFMABO.RESI." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".CURR.C")
				LOCAL NEW fcde   =  $get_4gl_expression(LOCAL'temp_fcde  ,loperatos, "OPER.TFMABO.RESI." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".FCDE.C")
				LOCAL NEW afde   =  $get_4gl_expression(LOCAL'temp_afde  ,loperatos, "OPER.TFMABO.RESI." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".AFDE.C")
				LOCAL NEW fccr   =  $get_4gl_expression(LOCAL'temp_fccr  ,loperatos, "OPER.TFMABO.RESI." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".FCCR.C")
				LOCAL NEW afcr   =  $get_4gl_expression(LOCAL'temp_afcr  ,loperatos, "OPER.TFMABO.RESI." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".AFCR.C")
				LOCAL NEW desc   =  $get_4gl_expression(LOCAL'temp_desc  ,loperatos, "OPER.TFMABO.RESI." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".DESC.C")
				LOCAL NEW eyon   =  $get_4gl_expression(LOCAL'temp_eyon  ,loperatos, "OPER.TFMABO.RESI." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".EYON.C")
				LOCAL NEW rfe    =  $get_4gl_expression(LOCAL'temp_rfe   ,loperatos, "OPER.TFMABO.RESI." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".RFE.C")
				LOCAL NEW otini  =  $get_4gl_expression(LOCAL'temp_otini ,loperatos, "OPER.TFMABO.RESI." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".OTINI.C")
				LOCAL NEW otinii =  $get_4gl_expression(LOCAL'temp_otinii,loperatos, "OPER.TFMABO.RESI." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".OTINII.C")
				
				IF EXISTS(ID(LOCAL'obj_name) AND NOT MISSING(FIRSTVALUE(ID(LOCAL'obj_name)))
					CASE *
					
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'acty))) AND LENGTH(TRIM(LOCAL'acty)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'acty), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'acty +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'oot))) AND LENGTH(TRIM(LOCAL'oot)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'oot), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'oot +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'bot))) AND LENGTH(TRIM(LOCAL'bot)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'bot), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'bot +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'curr))) AND LENGTH(TRIM(LOCAL'curr)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'curr), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'curr +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'fcde))) AND LENGTH(TRIM(LOCAL'fcde)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'fcde), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'fcde +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'afde))) AND LENGTH(TRIM(LOCAL'afde)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'afde), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'afde +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'fccr))) AND LENGTH(TRIM(LOCAL'fccr)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'fccr), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'fccr +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'afcr))) AND LENGTH(TRIM(LOCAL'afcr)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'afcr), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'afcr +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'desc))) AND LENGTH(TRIM(LOCAL'desc)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'desc), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'desc +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'rfe))) AND LENGTH(TRIM(LOCAL'rfe)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'rfe), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'rfe +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'otini))) AND LENGTH(TRIM(LOCAL'otini)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'otini), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'otini +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'otinii))) AND LENGTH(TRIM(LOCAL'otinii)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'otinii), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'otinii 
					END IF
										
					IF NOT MISSING(LOCATION(SUBSTRING(MIRROR(TRIM(LOCAL'queryStr)),0,4), "DNA", 1))
						SET LOCAL'queryStr = TRIM(MIRROR(SUBSTRING(MIRROR(TRIM(LOCAL'queryStr)), LOCATION(MIRROR(TRIM(LOCAL'queryStr)), "DNA")+3, LENGTH(MIRROR(TRIM(LOCAL'queryStr)))))
					END IF
					
					IF NOT MISSING(local'queryStr) AND local'queryStr NE ""
						EXECUTE "WHICH "+local'queryStr
					ELSE
						CASE FIRSTVALUE(ID(LOCAL'pk_obj_name)) TO LASTVALUE(ID(LOCAL'pk_obj_name))
					END IF
					
					IF LENGTHCASE GT 0
 						LOOP FOR cs IN CASE
 							SET LOCAL'cons_series[LOCAL'tempcs] = ID(LOCAL'obj_name)[cs]
							SET LOCAL'tempcs = LOCAL'tempcs + 1
						END LOOP
 					END IF --LENGTHCASE GT 0
				ELSE
					SIGNAL CONTINUE : ERRORTEXT
				END IF
			END LOOP
		END BLOCK
	ELSE
		SIGNAL ERROR : "$get_resi_cons_series_mul_filters() : INVALID DATE RANGE"
	END IF	--(LENGTH(TRIM(LOCAL'%sdate)) NE 0) AND (LENGTH(TRIM(LOCAL'%edate)) NE 0)
OTHERWISE
	SIGNAL ERROR:ERRORTEXT		
END TRY				
	RETURN <case *>cons_series
END FUNCTION


--FUNCTION TO RETURN CONSOLIDATED FROM Resi SERIES FOR ALL INSTITUTES
--Developed for edit case series forms
--disp $get_resi_cons_series_mul_filters_all_ins("OPER.TFMABO.RESI.EX.ACTY.C","01Dec2021 AND 31Dec2021","","ALL","YES","","","","","","","","","","","","")
FUNCTION $get_resi_cons_series_mul_filters_all_ins
ARGUMENTS %series_name, %sdate_and_date, %acty="", %institute, %eyon="", %oot="", %bot="", %curr="", %fcde="", %afde="",%fccr="",%afcr="", %desc="",%rfe="",%otini="",%otinii=""
OVER ON
FREQ DAILY
TRY
	LOCAL NEW temp_str = SUBSTRING(MIRROR(%series_name), LOCATION(MIRROR(%series_name), ".") +1, LENGTH(MIRROR(%series_name)))
	LOCAL NEW series_dim = MIRROR(SUBSTRING(LOCAL'temp_str, 0, LOCATION(LOCAL'temp_str, ".")-1))
	LOCAL SERIES <CASE 1 TO 6> !loperatos:STRING BY CASE = "GT", "GE", "LT", "LE", "EQ", "NE"
	LOCAL SERIES <CASE 1 TO 2> !boperatos:STRING BY CASE = "AND", "OR"
	LOCAL SCALAR !tempcs : NUMERIC = 1
	LOCAL SCALAR !queryStr : STRING
	LOCAL SCALAR !objDim : STRING	
	 
	LOCAL NEW temp_acty   = UPPER(%acty)
	LOCAL NEW temp_oot    = UPPER(%oot)
	LOCAL NEW temp_bot    = UPPER(%bot)
	LOCAL NEW temp_curr   = UPPER(%curr)
	LOCAL NEW temp_fcde   = UPPER(%fcde)
	LOCAL NEW temp_afde   = UPPER(%afde)
	LOCAL NEW temp_fccr   = UPPER(%fccr)
	LOCAL NEW temp_afcr   = UPPER(%afcr)
	LOCAL NEW temp_desc   = UPPER(%desc)
	LOCAL NEW temp_eyon   = UPPER(%eyon)
	LOCAL NEW temp_rfe    = UPPER(%rfe)
	LOCAL NEW temp_otini  = UPPER(%otini)
	LOCAL NEW temp_otinii = UPPER(%otinii)
	
		
	--Get the start and end date
	LOCAL NEW tdate = %sdate_and_date
	BLOCK
		CASE FIRSTVALUE(loperatos) TO LASTVALUE(loperatos)
		LOOP FOR ex in CASE
			SET LOCAL'tdate = REPLACE(LOCAL'tdate, loperatos[ex], "")
		END LOOP
		CASE FIRSTVALUE(boperatos) TO LASTVALUE(boperatos)
		LOOP FOR bl in CASE
			SET LOCAL'tdate = REPLACE(LOCAL'tdate, boperatos[bl], "")
		END LOOP
		LOCAL NEW %sdate = TRIM(SUBSTRING(TRIM(LOCAL'tdate), 1, LOCATION(TRIM(LOCAL'tdate)," ")))
		LOCAL NEW %edate = TRIM(SUBSTRING(TRIM(LOCAL'tdate), LOCATION(TRIM(LOCAL'tdate)," ")+1, LENGTH(TRIM(LOCAL'tdate))))
		
	END BLOCK
	
	--IF sdate and edate are present create series ignore CASE range
	IF (LENGTH(TRIM(LOCAL'%sdate)) NE 0) AND (LENGTH(TRIM(LOCAL'%edate)) NE 0)
		BLOCK
			DATE MAKE(DATE(DAILY),LOCAL'%sdate) TO MAKE(DATE(DAILY),LOCAL'%edate)
			FREQ MON
			SET LOCAL'tempcs = 1
			LOOP FOR dt IN DATE
				IF MONTH(dt) LE 9
					LOCAL NEW monstr = "0"+STRING(MONTH(dt))
				ELSE
					LOCAL NEW monstr = STRING(MONTH(dt))
				END IF
				
				--Check for exempt/non-exempt
				IF LOCAL'temp_eyon eq "YES" 
					SET LOCAL'objDim = "EX"
					ELSE
						SET LOCAL'objDim = "NOEX"
				END IF
				
				
				LOOP FOR %institue IN {AUA, BDC, CMB, RBCARUBA, FCIBARUBA, CBA}
				
					LOCAL series_name = "OPER.TFMABO.RESI."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C"
					
					NEXT IF NOT EXISTS (ID(series_name))
					 
					IF NOT MISSING(location(TYPE(ID(LOCAL'series_name)),"DATE"))  
						LOCAL SERIES cons_series:DATE(DAILY) BY CASE
					ELSE
						EXECUTE "LOCAL SERIES cons_series:"+TYPE(ID(LOCAL'series_name))+" BY CASE"
					END IF
					SET LOCAL'queryStr=""
					LOCAL NEW obj_name = "OPER.TFMABO.RESI."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C"
					LOCAL NEW pk_obj_name = "OPER.TFMABO.RESI."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".PRIMKEY.C"
					IF EXISTS(ID(LOCAL'pk_obj_name)) AND NOT MISSING(FIRSTVALUE(ID(LOCAL'pk_obj_name)))
					  --Convert expressions to FAMe 4GL expressions
					  
					  LOCAL NEW acty   = $get_4gl_expression(LOCAL'temp_acty, loperatos, "OPER.TFMABO.RESI."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".ACTY.C")
					  LOCAL NEW oot    = $get_4gl_expression(LOCAL'temp_oot, loperatos, "OPER.TFMABO.RESI."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".OOT.C")
					  LOCAL NEW bot    = $get_4gl_expression(LOCAL'temp_bot, loperatos, "OPER.TFMABO.RESI."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".BOT.C")
					  LOCAL NEW curr   = $get_4gl_expression(LOCAL'temp_curr, loperatos, "OPER.TFMABO.RESI."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".CURR.C")
					  LOCAL NEW fcde   = $get_4gl_expression(LOCAL'temp_fcde, loperatos, "OPER.TFMABO.RESI."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".FCDE.C")
					  LOCAL NEW afde   = $get_4gl_expression(LOCAL'temp_afde, loperatos,  "OPER.TFMABO.RESI."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".AFDE.C")
					  LOCAL NEW fccr   = $get_4gl_expression(LOCAL'temp_fccr, loperatos, "OPER.TFMABO.RESI."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".FCCR.C")
					  LOCAL NEW afcr   = $get_4gl_expression(LOCAL'temp_afcr, loperatos, "OPER.TFMABO.RESI."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".AFCR.C")
					  LOCAL NEW desc   = $get_4gl_expression(LOCAL'temp_desc, loperatos, "OPER.TFMABO.RESI."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".DESC.C")
					  LOCAL NEW eyon   = $get_4gl_expression(LOCAL'temp_eyon, loperatos, "OPER.TFMABO.RESI."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".EYON.C")
					  LOCAL NEW rfe    = $get_4gl_expression(LOCAL'temp_rfe, loperatos, "OPER.TFMABO.RESI."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".RFE.C")
					  LOCAL NEW otini  = $get_4gl_expression(LOCAL'temp_otini, loperatos, "OPER.TFMABO.RESI."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".OTINI.C")
					  LOCAL NEW otinii = $get_4gl_expression(LOCAL'temp_otinii, loperatos, "OPER.TFMABO.RESI."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".OTINII.C")
					  
						IF EXISTS(ID(LOCAL'obj_name) AND NOT MISSING(FIRSTVALUE(ID(LOCAL'obj_name)))
						CASE *
						
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'acty))) AND LENGTH(TRIM(LOCAL'acty)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'acty), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'acty +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'oot))) AND LENGTH(TRIM(LOCAL'oot)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'oot), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'oot +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'bot))) AND LENGTH(TRIM(LOCAL'bot)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'bot), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'bot +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'curr))) AND LENGTH(TRIM(LOCAL'curr)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'curr), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'curr +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'fcde))) AND LENGTH(TRIM(LOCAL'fcde)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'fcde), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'fcde +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'afde))) AND LENGTH(TRIM(LOCAL'afde)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'afde), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'afde +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'fccr))) AND LENGTH(TRIM(LOCAL'fccr)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'fccr), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'fccr +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'afcr))) AND LENGTH(TRIM(LOCAL'afcr)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'afcr), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'afcr +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'desc))) AND LENGTH(TRIM(LOCAL'desc)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'desc), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'desc +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'rfe))) AND LENGTH(TRIM(LOCAL'rfe)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'rfe), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'rfe +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'otini))) AND LENGTH(TRIM(LOCAL'otini)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'otini), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'otini +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'otinii))) AND LENGTH(TRIM(LOCAL'otinii)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'otinii), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'otinii 
						END IF
						
						IF NOT MISSING(LOCATION(SUBSTRING(MIRROR(TRIM(LOCAL'queryStr)),0,4), "DNA", 1))
							SET LOCAL'queryStr = TRIM(MIRROR(SUBSTRING(MIRROR(TRIM(LOCAL'queryStr)), LOCATION(MIRROR(TRIM(LOCAL'queryStr)), "DNA")+3, LENGTH(MIRROR(TRIM(LOCAL'queryStr)))))
						END IF
										      				
					  	
					  	IF NOT MISSING(local'queryStr) AND local'queryStr NE ""
					  		EXECUTE "WHICH "+local'queryStr
					  	ELSE
					  		CASE FIRSTVALUE(ID(LOCAL'pk_obj_name)) TO LASTVALUE(ID(LOCAL'pk_obj_name))
					  	END IF
					  
 					  	IF LENGTHCASE GT 0
 					  		LOOP FOR cs IN CASE
 					  			DECIMAL 15
 					  			SET LOCAL'cons_series[LOCAL'tempcs] = ID(LOCAL'obj_name)[cs]
					  			SET LOCAL'tempcs = LOCAL'tempcs + 1
					  		END LOOP
 					  	END IF --LENGTHCASE GT 0
 					  	--whats LOCAL'cons_series
					  ELSE
					  	SIGNAL CONTINUE : ERRORTEXT
					  END IF --IF EXISTS(ID(LOCAL'obj_name) AND NOT MISSING(FIRSTVALUE(ID(LOCAL'obj_name)))
					  END IF --IF EXISTS(pk_obj_name)
				END LOOP --FOR %institue IN {AUA, BDC, CMB, RBCARUBA, FCIBARUBA, CBA}
			END LOOP --LOOP FOR dt in DATE
		END BLOCK
	ELSE
		SIGNAL ERROR : "$get_resi_cons_series_mul_filters_all_ins() : INVALID DATE RANGE"
	END IF	--(LENGTH(TRIM(LOCAL'%sdate)) NE 0) AND (LENGTH(TRIM(LOCAL'%edate)) NE 0)
OTHERWISE
	SIGNAL ERROR:ERRORTEXT
END TRY	
	--CASE *
	RETURN <case *>cons_series	
	--RETURN ID(obj_name)
END FUNCTION


--FUNCTION TO RETURN CONSOLIDATED FROM NON Resi SERIES
--Developed for edit case series forms
--disp $get_non_resi_cons_series_mul_filters("OPER.TFMABO.NORE.EX.ACTY.C","01Dec2021 AND 31Dec2021","","BDC","YES","","","","","","","","","","","","")
FUNCTION $get_non_resi_cons_series_mul_filters
ARGUMENTS %series_name, %sdate_and_date, %acty="", %institue, %eyon="", %oot="", %bot="",%feco="", %curr="", %fcde="", %afde="",%fccr="",%afcr="",%fec="", %desc="",%rfe="",%otini="",%otinii=""
OVER ON
FREQ DAILY
TRY
		
	LOCAL NEW temp_str = SUBSTRING(MIRROR(%series_name), LOCATION(MIRROR(%series_name), ".") +1, LENGTH(MIRROR(%series_name)))
	LOCAL NEW series_dim = MIRROR(SUBSTRING(LOCAL'temp_str, 0, LOCATION(LOCAL'temp_str, ".")-1))
	LOCAL SERIES <CASE 1 TO 6> !loperatos:STRING BY CASE = "GT", "GE", "LT", "LE", "EQ", "NE"
	LOCAL SERIES <CASE 1 TO 2> !boperatos:STRING BY CASE = "AND", "OR"
	LOCAL SCALAR !tempcs : NUMERIC = 1
	LOCAL SCALAR !queryStr : STRING	
	LOCAL SCALAR !objDim : STRING	
	LOCAL NEW temp_acty   = UPPER(%acty)
	LOCAL NEW temp_oot    = UPPER(%oot)
	LOCAL NEW temp_bot    = UPPER(%bot)
	LOCAL NEW temp_feco    = UPPER(%feco)
	LOCAL NEW temp_curr   = UPPER(%curr)
	LOCAL NEW temp_fcde   = UPPER(%fcde)
	LOCAL NEW temp_afde   = UPPER(%afde)
	LOCAL NEW temp_fccr   = UPPER(%fccr)
	LOCAL NEW temp_afcr   = UPPER(%afcr)
	LOCAL NEW temp_fec   = UPPER(%fec)
	LOCAL NEW temp_desc   = UPPER(%desc)
	LOCAL NEW temp_eyon   = UPPER(%eyon)
	LOCAL NEW temp_rfe    = UPPER(%rfe)
	LOCAL NEW temp_otini  = UPPER(%otini)
	LOCAL NEW temp_otinii = UPPER(%otinii)
	--Get the start and end date
	
	
	LOCAL NEW tdate = %sdate_and_date
	BLOCK
		CASE FIRSTVALUE(loperatos) TO LASTVALUE(loperatos)
		LOOP FOR ex in CASE
			SET LOCAL'tdate = REPLACE(LOCAL'tdate, loperatos[ex], "")
		END LOOP
		CASE FIRSTVALUE(boperatos) TO LASTVALUE(boperatos)
		LOOP FOR bl in CASE
			SET LOCAL'tdate = REPLACE(LOCAL'tdate, boperatos[bl], "")
		END LOOP
		LOCAL NEW %sdate = TRIM(SUBSTRING(TRIM(LOCAL'tdate), 1, LOCATION(TRIM(LOCAL'tdate)," ")))
		LOCAL NEW %edate = TRIM(SUBSTRING(TRIM(LOCAL'tdate), LOCATION(TRIM(LOCAL'tdate)," ")+1, LENGTH(TRIM(LOCAL'tdate))))
		
	END BLOCK
		
	--IF sdate and edate are present create series ignore CASE range
	IF (LENGTH(TRIM(LOCAL'%sdate)) NE 0) AND (LENGTH(TRIM(LOCAL'%edate)) NE 0)
		BLOCK
			DATE MAKE(DATE(DAILY),LOCAL'%sdate) TO MAKE(DATE(DAILY),LOCAL'%edate)
			FREQ MON
			SET LOCAL'tempcs = 1
			LOOP FOR dt IN DATE
				IF MONTH(dt) LE 9
					LOCAL NEW monstr = "0"+STRING(MONTH(dt))
				ELSE
					LOCAL NEW monstr = STRING(MONTH(dt))
				END IF
				SET LOCAL'queryStr=""
				
				--Check for exempt/non-exempt
				IF LOCAL'temp_eyon eq "YES" 
					SET LOCAL'objDim = "EX"
					ELSE
						SET LOCAL'objDim = "NOEX"
				END IF
				
				
				LOCAL NEW obj_name = "OPER.TFMABO.NORE."+LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C"
				LOCAL NEW pk_obj_name = "OPER.TFMABO.NORE."+LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".PRIMKEY.C"
				
				
				LOCAL series_name = "OPER.TFMABO.NORE."+LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C"
				
				IF NOT MISSING(location(TYPE(ID(LOCAL'series_name)),"DATE"))  
						LOCAL SERIES cons_series:DATE(DAILY) BY CASE
					ELSE
						EXECUTE "LOCAL SERIES cons_series:"+TYPE(ID(LOCAL'series_name))+" BY CASE"
				END IF
				--Convert expressions to FAMe 4GL expressions
				
				LOCAL NEW acty   =  $get_4gl_expression(LOCAL'temp_acty  ,loperatos, "OPER.TFMABO.NORE." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".ACTY.C")
				LOCAL NEW oot    =  $get_4gl_expression(LOCAL'temp_oot   ,loperatos, "OPER.TFMABO.NORE." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".OOT.C")
				LOCAL NEW bot    =  $get_4gl_expression(LOCAL'temp_bot   ,loperatos, "OPER.TFMABO.NORE." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".BOT.C")
				LOCAL NEW feco    =  $get_4gl_expression(LOCAL'temp_feco   ,loperatos, "OPER.TFMABO.NORE." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".FECO.C")
				LOCAL NEW curr   =  $get_4gl_expression(LOCAL'temp_curr  ,loperatos, "OPER.TFMABO.NORE." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".CURR.C")
				LOCAL NEW fcde   =  $get_4gl_expression(LOCAL'temp_fcde  ,loperatos, "OPER.TFMABO.NORE." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".FCDE.C")
				LOCAL NEW afde   =  $get_4gl_expression(LOCAL'temp_afde  ,loperatos, "OPER.TFMABO.NORE." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".AFDE.C")
				LOCAL NEW fccr   =  $get_4gl_expression(LOCAL'temp_fccr  ,loperatos, "OPER.TFMABO.NORE." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".FCCR.C")
				LOCAL NEW afcr   =  $get_4gl_expression(LOCAL'temp_afcr  ,loperatos, "OPER.TFMABO.NORE." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".AFCR.C")
				LOCAL NEW fec   =  $get_4gl_expression(LOCAL'temp_fec  ,loperatos, "OPER.TFMABO.NORE." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".FEC.C")
				LOCAL NEW desc   =  $get_4gl_expression(LOCAL'temp_desc  ,loperatos, "OPER.TFMABO.NORE." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".DESC.C")
				LOCAL NEW eyon   =  $get_4gl_expression(LOCAL'temp_eyon  ,loperatos, "OPER.TFMABO.NORE." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".EYON.C")
				LOCAL NEW rfe    =  $get_4gl_expression(LOCAL'temp_rfe   ,loperatos, "OPER.TFMABO.NORE." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".RFE.C")
				LOCAL NEW otini  =  $get_4gl_expression(LOCAL'temp_otini ,loperatos, "OPER.TFMABO.NORE." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".OTINI.C")
				LOCAL NEW otinii =  $get_4gl_expression(LOCAL'temp_otinii,loperatos, "OPER.TFMABO.NORE." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".OTINII.C")
				
				IF EXISTS(ID(LOCAL'obj_name) AND NOT MISSING(FIRSTVALUE(ID(LOCAL'obj_name)))
					CASE *
					
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'acty))) AND LENGTH(TRIM(LOCAL'acty)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'acty), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'acty +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'oot))) AND LENGTH(TRIM(LOCAL'oot)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'oot), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'oot +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'bot))) AND LENGTH(TRIM(LOCAL'bot)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'bot), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'bot +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'feco))) AND LENGTH(TRIM(LOCAL'feco)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'feco), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'feco +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'curr))) AND LENGTH(TRIM(LOCAL'curr)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'curr), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'curr +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'fcde))) AND LENGTH(TRIM(LOCAL'fcde)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'fcde), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'fcde +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'afde))) AND LENGTH(TRIM(LOCAL'afde)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'afde), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'afde +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'fccr))) AND LENGTH(TRIM(LOCAL'fccr)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'fccr), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'fccr +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'afcr))) AND LENGTH(TRIM(LOCAL'afcr)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'afcr), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'afcr +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'fec))) AND LENGTH(TRIM(LOCAL'fec)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'fec), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'fec +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'desc))) AND LENGTH(TRIM(LOCAL'desc)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'desc), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'desc +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'rfe))) AND LENGTH(TRIM(LOCAL'rfe)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'rfe), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'rfe +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'otini))) AND LENGTH(TRIM(LOCAL'otini)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'otini), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'otini +" AND "
					END IF
					IF (NOT MISSING(LENGTH(TRIM(LOCAL'otinii))) AND LENGTH(TRIM(LOCAL'otinii)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'otinii), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'otinii 
					END IF
										
					IF NOT MISSING(LOCATION(SUBSTRING(MIRROR(TRIM(LOCAL'queryStr)),0,4), "DNA", 1))
						SET LOCAL'queryStr = TRIM(MIRROR(SUBSTRING(MIRROR(TRIM(LOCAL'queryStr)), LOCATION(MIRROR(TRIM(LOCAL'queryStr)), "DNA")+3, LENGTH(MIRROR(TRIM(LOCAL'queryStr)))))
					END IF
					
					IF NOT MISSING(local'queryStr) AND local'queryStr NE ""
						EXECUTE "WHICH "+local'queryStr
					ELSE
						CASE FIRSTVALUE(ID(LOCAL'pk_obj_name)) TO LASTVALUE(ID(LOCAL'pk_obj_name))
					END IF
					
					IF LENGTHCASE GT 0
 						LOOP FOR cs IN CASE
 							SET LOCAL'cons_series[LOCAL'tempcs] = ID(LOCAL'obj_name)[cs]
							SET LOCAL'tempcs = LOCAL'tempcs + 1
						END LOOP
 					END IF --LENGTHCASE GT 0
				ELSE
					SIGNAL CONTINUE : ERRORTEXT
				END IF
			END LOOP
		END BLOCK
	ELSE
		SIGNAL ERROR : "$get_non_resi_cons_series_mul_filters() : INVALID DATE RANGE"
	END IF	--(LENGTH(TRIM(LOCAL'%sdate)) NE 0) AND (LENGTH(TRIM(LOCAL'%edate)) NE 0)
OTHERWISE
	SIGNAL ERROR:ERRORTEXT		
END TRY				
	RETURN <case *>cons_series
END FUNCTION


--FUNCTION TO RETURN CONSOLIDATED FROM Non Resi SERIES FOR ALL INSTITUTES
--Developed for edit case series forms
--disp $get_non_resi_cons_series_mul_filters_all_ins("OPER.TFMABO.NORE.NOEX.TRDA.C","01Dec021 AND 31Dec2021","","RBCARUBA","YES","","","","","","","","","","","","")
FUNCTION $get_non_resi_cons_series_mul_filters_all_ins
ARGUMENTS %series_name, %sdate_and_date, %acty="", %institute, %eyon="", %oot="", %bot="", %feco="",%curr="", %srn="", %fcde="", %afde="",%fccr="",%afcr="",%fec="", %desc="",%rfe="",%otini="",%otinii=""
OVER ON
FREQ DAILY
TRY
	LOCAL NEW temp_str = SUBSTRING(MIRROR(%series_name), LOCATION(MIRROR(%series_name), ".") +1, LENGTH(MIRROR(%series_name)))
	LOCAL NEW series_dim = MIRROR(SUBSTRING(LOCAL'temp_str, 0, LOCATION(LOCAL'temp_str, ".")-1))
	LOCAL SERIES <CASE 1 TO 6> !loperatos:STRING BY CASE = "GT", "GE", "LT", "LE", "EQ", "NE"
	LOCAL SERIES <CASE 1 TO 2> !boperatos:STRING BY CASE = "AND", "OR"
	LOCAL SCALAR !tempcs : NUMERIC = 1
	LOCAL SCALAR !queryStr : STRING
	LOCAL SCALAR !objDim : STRING	
	 
	LOCAL NEW temp_acty   = UPPER(%acty)
	LOCAL NEW temp_oot    = UPPER(%oot)
	LOCAL NEW temp_bot    = UPPER(%bot)
	LOCAL NEW temp_feco    = UPPER(%feco)
	LOCAL NEW temp_curr   = UPPER(%curr)
	LOCAL NEW temp_fcde   = UPPER(%fcde)
	LOCAL NEW temp_afde   = UPPER(%afde)
	LOCAL NEW temp_fccr   = UPPER(%fccr)
	LOCAL NEW temp_afcr   = UPPER(%afcr)
	LOCAL NEW temp_fec   = UPPER(%fec)
	LOCAL NEW temp_desc   = UPPER(%desc)
	LOCAL NEW temp_eyon   = UPPER(%eyon)
	LOCAL NEW temp_rfe    = UPPER(%rfe)
	LOCAL NEW temp_otini  = UPPER(%otini)
	LOCAL NEW temp_otinii = UPPER(%otinii)
	
		
	--Get the start and end date
	LOCAL NEW tdate = %sdate_and_date
	BLOCK
		CASE FIRSTVALUE(loperatos) TO LASTVALUE(loperatos)
		LOOP FOR ex in CASE
			SET LOCAL'tdate = REPLACE(LOCAL'tdate, loperatos[ex], "")
		END LOOP
		CASE FIRSTVALUE(boperatos) TO LASTVALUE(boperatos)
		LOOP FOR bl in CASE
			SET LOCAL'tdate = REPLACE(LOCAL'tdate, boperatos[bl], "")
		END LOOP
		LOCAL NEW %sdate = TRIM(SUBSTRING(TRIM(LOCAL'tdate), 1, LOCATION(TRIM(LOCAL'tdate)," ")))
		LOCAL NEW %edate = TRIM(SUBSTRING(TRIM(LOCAL'tdate), LOCATION(TRIM(LOCAL'tdate)," ")+1, LENGTH(TRIM(LOCAL'tdate))))
		
	END BLOCK
	
	--IF sdate and edate are present create series ignore CASE range
	IF (LENGTH(TRIM(LOCAL'%sdate)) NE 0) AND (LENGTH(TRIM(LOCAL'%edate)) NE 0)
		BLOCK
			DATE MAKE(DATE(DAILY),LOCAL'%sdate) TO MAKE(DATE(DAILY),LOCAL'%edate)
			FREQ MON
			SET LOCAL'tempcs = 1
			LOOP FOR dt IN DATE
				IF MONTH(dt) LE 9
					LOCAL NEW monstr = "0"+STRING(MONTH(dt))
				ELSE
					LOCAL NEW monstr = STRING(MONTH(dt))
				END IF
				
				--Check for exempt/non-exempt
				IF LOCAL'temp_eyon eq "YES" 
					SET LOCAL'objDim = "EX"
					ELSE
						SET LOCAL'objDim = "NOEX"
				END IF
				
				
				LOOP FOR %institue IN {AUA, BDC, CMB, RBCARUBA, FCIBARUBA, CBA}
				
					LOCAL series_name = "OPER.TFMABO.NORE."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C"
					
					NEXT IF NOT EXISTS (ID(series_name))
					 
					IF NOT MISSING(location(TYPE(ID(LOCAL'series_name)),"DATE"))  
						LOCAL SERIES cons_series:DATE(DAILY) BY CASE
					ELSE
						EXECUTE "LOCAL SERIES cons_series:"+TYPE(ID(LOCAL'series_name))+" BY CASE"
					END IF
					SET LOCAL'queryStr=""
					LOCAL NEW obj_name = "OPER.TFMABO.NORE."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+"."+LOCAL'series_dim+".C"
					LOCAL NEW pk_obj_name = "OPER.TFMABO.NORE."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".PRIMKEY.C"
					IF EXISTS(ID(LOCAL'pk_obj_name)) AND NOT MISSING(FIRSTVALUE(ID(LOCAL'pk_obj_name)))
					  --Convert expressions to FAMe 4GL expressions
					  
					  LOCAL NEW acty   = $get_4gl_expression(LOCAL'temp_acty, loperatos, "OPER.TFMABO.NORE."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".ACTY.C")
					  LOCAL NEW oot    = $get_4gl_expression(LOCAL'temp_oot, loperatos, "OPER.TFMABO.NORE."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".OOT.C")
					  LOCAL NEW bot    = $get_4gl_expression(LOCAL'temp_bot, loperatos, "OPER.TFMABO.NORE."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".BOT.C")
					  LOCAL NEW feco    =  $get_4gl_expression(LOCAL'temp_feco   ,loperatos, "OPER.TFMABO.NORE." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".FECO.C")
					  LOCAL NEW curr   = $get_4gl_expression(LOCAL'temp_curr, loperatos, "OPER.TFMABO.NORE."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".CURR.C")
					  LOCAL NEW fcde   = $get_4gl_expression(LOCAL'temp_fcde, loperatos, "OPER.TFMABO.NORE."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".FCDE.C")
					  LOCAL NEW afde   = $get_4gl_expression(LOCAL'temp_afde, loperatos,  "OPER.TFMABO.NORE."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".AFDE.C")
					  LOCAL NEW fccr   = $get_4gl_expression(LOCAL'temp_fccr, loperatos, "OPER.TFMABO.NORE."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".FCCR.C")
					  LOCAL NEW afcr   = $get_4gl_expression(LOCAL'temp_afcr, loperatos, "OPER.TFMABO.NORE."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".AFCR.C")
					  LOCAL NEW fec   =  $get_4gl_expression(LOCAL'temp_fec  ,loperatos, "OPER.TFMABO.NORE." + LOCAL'objDim +"."+%institue+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".FEC.C")
					  LOCAL NEW desc   = $get_4gl_expression(LOCAL'temp_desc, loperatos, "OPER.TFMABO.NORE."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".DESC.C")
					  LOCAL NEW eyon   = $get_4gl_expression(LOCAL'temp_eyon, loperatos, "OPER.TFMABO.NORE."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".EYON.C")
					  LOCAL NEW rfe    = $get_4gl_expression(LOCAL'temp_rfe, loperatos, "OPER.TFMABO.NORE."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".RFE.C")
					  LOCAL NEW otini  = $get_4gl_expression(LOCAL'temp_otini, loperatos, "OPER.TFMABO.NORE."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".OTINI.C")
					  LOCAL NEW otinii = $get_4gl_expression(LOCAL'temp_otinii, loperatos, "OPER.TFMABO.NORE."+LOCAL'objDim +"."+NAME(%institue)+"."+STRING(YEAR(dt))+"."+LOCAL'monstr+".OTINII.C")
					  
						IF EXISTS(ID(LOCAL'obj_name) AND NOT MISSING(FIRSTVALUE(ID(LOCAL'obj_name)))
						CASE *
						
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'acty))) AND LENGTH(TRIM(LOCAL'acty)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'acty), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'acty +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'oot))) AND LENGTH(TRIM(LOCAL'oot)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'oot), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'oot +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'bot))) AND LENGTH(TRIM(LOCAL'bot)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'bot), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'bot +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'feco))) AND LENGTH(TRIM(LOCAL'feco)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'feco), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'feco +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'curr))) AND LENGTH(TRIM(LOCAL'curr)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'curr), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'curr +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'fcde))) AND LENGTH(TRIM(LOCAL'fcde)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'fcde), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'fcde +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'afde))) AND LENGTH(TRIM(LOCAL'afde)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'afde), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'afde +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'fccr))) AND LENGTH(TRIM(LOCAL'fccr)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'fccr), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'fccr +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'afcr))) AND LENGTH(TRIM(LOCAL'afcr)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'afcr), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'afcr +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'fec))) AND LENGTH(TRIM(LOCAL'fec)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'fec), "EXPRESSSION_MISSING"))
						SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'fec +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'desc))) AND LENGTH(TRIM(LOCAL'desc)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'desc), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'desc +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'rfe))) AND LENGTH(TRIM(LOCAL'rfe)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'rfe), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'rfe +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'otini))) AND LENGTH(TRIM(LOCAL'otini)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'otini), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'otini +" AND "
						END IF
						IF (NOT MISSING(LENGTH(TRIM(LOCAL'otinii))) AND LENGTH(TRIM(LOCAL'otinii)) NE 0) AND MISSING(LOCATION(TRIM(LOCAL'otinii), "EXPRESSSION_MISSING"))
							SET LOCAL'queryStr = LOCAL'queryStr + LOCAL'otinii 
						END IF
						
						IF NOT MISSING(LOCATION(SUBSTRING(MIRROR(TRIM(LOCAL'queryStr)),0,4), "DNA", 1))
							SET LOCAL'queryStr = TRIM(MIRROR(SUBSTRING(MIRROR(TRIM(LOCAL'queryStr)), LOCATION(MIRROR(TRIM(LOCAL'queryStr)), "DNA")+3, LENGTH(MIRROR(TRIM(LOCAL'queryStr)))))
						END IF
										      				
					  	
					  	IF NOT MISSING(local'queryStr) AND local'queryStr NE ""
					  		EXECUTE "WHICH "+local'queryStr
					  	ELSE
					  		CASE FIRSTVALUE(ID(LOCAL'pk_obj_name)) TO LASTVALUE(ID(LOCAL'pk_obj_name))
					  	END IF
					  
 					  	IF LENGTHCASE GT 0
 					  		LOOP FOR cs IN CASE
 					  			DECIMAL 15
 					  			SET LOCAL'cons_series[LOCAL'tempcs] = ID(LOCAL'obj_name)[cs]
					  			SET LOCAL'tempcs = LOCAL'tempcs + 1
					  		END LOOP
 					  	END IF --LENGTHCASE GT 0
 					  	--whats LOCAL'cons_series
					  ELSE
					  	SIGNAL CONTINUE : ERRORTEXT
					  END IF --IF EXISTS(ID(LOCAL'obj_name) AND NOT MISSING(FIRSTVALUE(ID(LOCAL'obj_name)))
					  END IF --IF EXISTS(pk_obj_name)
				END LOOP --FOR %institue IN {AUA, BDC, CMB, RBCARUBA, FCIBARUBA, CBA}
			END LOOP --LOOP FOR dt in DATE
		END BLOCK
	ELSE
		SIGNAL ERROR : "$get_non_resi_cons_series_mul_filters_all_ins() : INVALID DATE RANGE"
	END IF	--(LENGTH(TRIM(LOCAL'%sdate)) NE 0) AND (LENGTH(TRIM(LOCAL'%edate)) NE 0)
OTHERWISE
	SIGNAL ERROR:ERRORTEXT
END TRY	
	--CASE *
	RETURN <case *>cons_series	
	--RETURN ID(obj_name)
END FUNCTION



-----------------------------------------------------------------------------------------
--$getgfsdata function to return NA in output if it stored as NA , Blank when ND else value
--FAME CALL :- $getgfsdata(OBJNAME)
--e.g       :- $getgfsdata(GFS.DIRFIN.T6.NET.MEM.DMI.GDDA.6M4.M)
-----------------------------------------------------------------------------------------
FUNCTION $getgfsdata
ARGUMENT %OBJECTNAME
FREQ M
LOCAL SERIES RESULT:STRING BY DATE

    TRY
	  
	  IF EXISTS(%OBJECTNAME) 
	     LOOP FOR J = FIRSTVALUE(T) TO LASTVALUE(T)
	        IF STRING(%OBJECTNAME[J]) EQ "NA"
		       SET RESULT[J]="NA"
	        ELSE IF STRING(%OBJECTNAME[J]) EQ "ND"
			   SET RESULT[J]=""
			ELSE
		       SET RESULT[J]=STRING(%OBJECTNAME)
	        END IF 
		 END LOOP
	  END IF
	  RETURN <DATE *>RESULT
	 
    OTHERWISE
        TYPE LASTERROR
        RETURN ND
    END TRY
END FUNCTION


-----------------------------------------------------------------------------------------
--             function will extract the case series records based on the Date range passed and charector pass A-Asset|L-Laibility
--FAME CALL :- $CBA_GET_CASE_DATA_FALA(DATEINDEX_OBJ,ASSET_LAIB_STATUS_OBJ,ASSET_LAIB_STATUS_VALUE,DATA_OBJ,SDATE,EDATE)
--e.g       :- $CBA_GET_CASE_DATA_FALA(EXSE.PIR.DATEINDEX.C,EXSE.PIR.TYPE.C,"A",EXSE.PIR.POB.C,Jan1999,Jan1999)
-----------------------------------------------------------------------------------------
FUNCTION $CBA_GET_CASE_DATA_FALA
ARGUMENTS %dtindex,%typeobjname,%typeobjvalue,%objname, %startdate, %enddate
TRY
	
	IF NOT MISSING( LOCATION(STRING(TYPE(%objname)), "DATE:") )
		EXECUTE "LOCAL SERIES resultantseries:"+REPLACE(TYPE(%objname),":","(")+")  BY CASE"
	ELSE
		EXECUTE "LOCAL SERIES resultantseries:"+TYPE(%objname)+"  BY CASE"
	END IF
		
	--Set the freq as per the %dtindex object
	LOCAL NEW freqtype = SUBSTRING(STRING(TYPE(%dtindex)), LOCATION(STRING(TYPE(%dtindex)),":")+1,  LENGTH(STRING(TYPE(%dtindex))) )
	EXECUTE "FREQUENCY "+LOCAL'freqtype
	CASE *
	
	WHICH %dtindex GE %startdate AND %dtindex LE %enddate AND %typeobjname EQ %typeobjvalue
	RESET ONLY FREQ
	SET LOCAL'resultantseries[caseorder] = %objname
	--disp <case *> resultantseries
	--whats resultantseries
OTHERWISE
	SIGNAL ERROR : ERRORTEXT
END TRY
RETURN <case *>resultantseries
END FUNCTION


