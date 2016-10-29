	'$INCLUDE: 'COMMON.BAS'

	DEF FNC$(A$)
		B$=LCASE$(A$):J%=1
		FOR I%=1 TO LEN(B$)
			IF J%=1 AND MID$(B$,I%,1)>="a" AND MID$(B$,I%,1)<="z" THEN B$=LEFT$(B$,I%-1)+UCASE$(MID$(B$,I%,1))+MID$(B$,I%+1):J%=0
			IF ASC(MID$(B$,I%,1))<65 THEN J%=1
		NEXT I%
		FNC$=B$
	END DEF

	IF FNW%(CR$+CR$+"--=:)) NEW USER REGISTRATION ((:=--"+CR$) THEN GOTO Hungup
	FOR USER%=1 TO 96
		GET #2,USER%
		IF ASC(USER.ACCESS$)=0 THEN EXIT FOR
	NEXT USER%
	IF ASC(USER.ACCESS$) THEN ZZ%=FNW%(CR$+BELL$+"User file full!"+CR$):GOTO Logoff
	GET #2,USER%
	LSET USER.REC$=STRING$(LEN(USER.REC$),0)
	LSET USER.ACCESS$=CHR$(1)
	FOR F%=1 TO 7:ON F% GOSUB Username,Sex,City,State,Phone,Handle,Password:NEXT F%
EditOne:
	IF FNW%(CR$+CR$+"Change which one <1-7 or RETURN=SAVE>? ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	IF VAL(I$) THEN ON VAL(I$) GOSUB Username,Sex,City,State,Phone,Handle,Password:GOTO EditOne

	IF FNW%(CR$+CR$+"Your user id is:"+STR$(USER%)+CR$+CR$) THEN GOTO Hungup
	LSET USER.LDATE$=RIGHT$(DATE$,4)+LEFT$(DATE$,2)+MID$(DATE$,4,2)
	LSET USER.LTIME$=LEFT$(TIME$,2)+MID$(TIME$,4,2)
	LSET USER.TCALL$=MKI$(1)
	PUT #2,USER%
	IF FNW%("Your registration has been saved for later validation."+CR$) THEN GOTO Hungup

	LAST$(3)=FNL$(LAST$(2))
	LAST$(2)=FNL$(LAST$(1))
	LAST$(1)=FNL$(USER.HANDLE$)
	LOGOFF%=60*VAL(LEFT$(TIME$,2))+VAL(MID$(TIME$,4,2))+MO%(1)
	CHAIN "MAIN"

Username:
	IF FNW%(CR$+"<1> Your REAL NAME [max 26]? ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	IF INSTR(I$," ")=0 OR LEN(I$)<4 OR LEN(I$)>LEN(USER.NAME$) THEN GOTO Username
	LSET USER.NAME$=FNC$(I$)+STRING$(LEN(USER.NAME$),0)
	RETURN
Sex:
	IF FNW%(CR$+"<2> Are you <M>ale or <F>emale? ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	I$=UCASE$(I$)
	IF I$<>"M" AND I$<>"F" THEN GOTO Sex
	LSET USER.SEX$=I$
	RETURN
City:
	IF FNW%(CR$+"<3> Your City [max 20]? ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	IF LEN(I$)<3 OR LEN(I$)>LEN(USER.CITY$) THEN GOTO City
	LSET USER.CITY$=FNC$(I$)+STRING$(LEN(USER.CITY$),0)
	RETURN
State:
	IF FNW%(CR$+"<4> Your State [max 2]? ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	IF LEN(I$)<>LEN(USER.STATE$) THEN GOTO State
	LSET USER.STATE$=UCASE$(I$)+STRING$(LEN(USER.STATE$),0)
	RETURN
Phone:
	IF FNW%(CR$+"<5> Your phone number (###-###-####)? ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	D#=INT(VAL(LEFT$(I$,3)+MID$(I$,5,3)+RIGHT$(I$,4)))
	IF D#<1011010000# OR LEN(I$)<>12 THEN GOTO Phone
	LSET USER.PHONE$=MKD$(D#)
	RETURN
Handle:
	IF FNW%(CR$+"<6> Your character name [max 26]? ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	IF LEN(I$)<2 OR LEN(I$)>LEN(USER.HANDLE$) THEN GOTO Handle
	USER.DISK$=USER.REC$+"":REC%=0
	ZZ%=FNW%(CR$+"Checking my files for duplicate handle"):I%=0
	WHILE REC%<96 AND ZZ%=0 AND I%=0
		REC%=REC%+1
		IF (REC% MOD 10)=0 THEN ZZ%=FNW%(".")
		GET #2,REC%
		IF UCASE$(I$)=UCASE$(FNL$(USER.HANDLE$)) THEN ZZ%=FNW%(CR$+BELL$+"Sorry -- that handle is already used."+CR$):I%=NOT 0
	WEND
	IF ZZ% THEN RETURN Hungup
	IF I% THEN GOTO Handle
	IF FNW%("Ok.") THEN RETURN Hungup
	LSET USER.REC$=USER.DISK$
	LSET USER.HANDLE$=FNC$(I$)+STRING$(LEN(USER.HANDLE$),0)
	RETURN
Password:
	IF FNW%(CR$+"<7> Your password [max 10]? ") THEN RETURN Hungup
	NOECHO%=1:IF FNR% THEN RETURN Hungup
	NOECHO%=0:I$=LTRIM$(UCASE$(I$)):I$=RTRIM$(I$)
	IF LEN(I$)<2 OR LEN(I$)>LEN(USER.PASSWORD$) THEN GOTO Password
	LSET USER.PASSWORD$=I$+STRING$(LEN(USER.PASSWORD$),0)
	IF FNW%(CR$+"Enter it again to verify: ") THEN RETURN Hungup
	NOECHO%=1:IF FNR% THEN RETURN Hungup
	NOECHO%=0:IF UCASE$(I$)<>FNL$(USER.PASSWORD$) THEN ZZ%=FNW%(BELL$+"-incorrect!"+CR$):GOTO Password
	RETURN
