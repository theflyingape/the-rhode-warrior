	'$INCLUDE: 'COMMON.BAS'

	DIM M$(50)
Top:
	IF FNT% THEN GOTO Expired
	IF FNW%(CR$+"Send mail to who? ") THEN GOTO Hungup
	REC%=FNUSER%
	ZZ%=FNW%(CR$)
	IF REC%=0 THEN CHAIN "MAIN"
	IF FNW%("Enter up to 50 lines to send.  Type / on a blank line for menu options."+CR$) THEN GOTO Hungup
	FOR MSG%=1 TO 50
		ZZ%=FNW%(RIGHT$(" "+MID$(STR$(MSG%),2)+"] ",4))
		IF FNR% THEN GOTO Hungup
		ZZ%=FNW%(CR$)
		IF LEFT$(I$,1)="/" THEN MSG%=MSG%-1:EXIT FOR
		M$(MSG%)=I$
	NEXT MSG%
	A$=UCASE$(MID$(I$,2))
	WHILE A$<>"S" AND A$<>"Q"
		ZZ%=FNW%(CR$+"<S>end or <Q>uit: ")
		IF FNR% THEN GOTO Hungup
		A$=UCASE$(LEFT$(I$,1))
	WEND
	IF A$="Q" THEN GOTO Top
	ZZ%=FNW%("ending...")
	GET #2,USER%
	OPEN "A",#4,"EMAIL\EMAIL"+MID$(STR$(REC%),2)+".MSG"
	PRINT #4,"Date: ";FND$(USER.LDATE$);"  ";FNT$(USER.LTIME$)
	PRINT #4,"From: ";FNL$(USER.HANDLE$)
	PRINT #4,""
	FOR I%=1 TO MSG%
		PRINT #4,M$(I%)
	NEXT I%
	PRINT #4,"":PRINT #4,"-=>*<=-":PRINT #4,""
	CLOSE #4
	ZZ%=FNW%("Ok."+CR$)
	GOTO Top
