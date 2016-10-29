	'$INCLUDE: 'COMMON.BAS'

	TIMER OFF
	CLS:VIEW PRINT 1 TO 25:LOCATE ,,0
	CLOSE #4
	BAUD$="2400"
	CLOSE #1:OPEN "COM2:"+BAUD$+",N,8,1" FOR RANDOM AS #1 LEN=128:WIDTH #1,80
	PRINT #1,""
	PRINT #1,"AT E1 H M0 S0=1 &C1 &D2"
	SLEEP 1
	PRINT "Auto-Answer mode enabled, phone is on hook."
	PRINT
	PRINT "Last callers: ";LAST$(1);", ";LAST$(2);", ";LAST$(3)
	PRINT
	PRINT DATE$;"  ";TIME$;"  Waiting for a caller...";
	IF LOC(1) THEN A$=INPUT$(LOC(1),1)
	LOCL%=0:USER%=0
	FIGHT%=0:JOUST%=0:BRAWL%=0:PARTY%=0:NAVAL%=0
	WHILE INKEY$="" AND FNCD%<>0
		IF LAST$(0)<>DATE$ THEN
			PRINT
			PRINT DATE$;" - Midnight crossed";
			FOR REC%=1 TO 96
				IF (REC% MOD 10)=0 THEN PRINT ".";
				GET #2,REC%
				LSET USER.CALLS$=CHR$(0)
				LSET USER.STATUS$=CHR$(0)
				PUT #2,REC%
			NEXT REC%
			PRINT "Ok."
			LAST$(0)=DATE$
		END IF
	WEND
	PRINT
	IF FNCD% THEN
		LOCL%=NOT 0
		PRINT "Auto-answer mode disabled";
		PRINT #1,"ATS0=0":SLEEP 1
		PRINT ", phone is off hook."
		PRINT #1,"ATH1":SLEEP 1
		INPUT "<L>ogon or <Q>uit";I$:I$=UCASE$(I$)
		IF LOC(1) THEN A$=INPUT$(LOC(1),1)
		IF I$="Q" THEN
			PRINT #1,"ATZ":SLEEP 2
			CLOSE:CLS
			OPEN "O",#1,"SYSINFO.DAT"
			PRINT #1,CALLERS
			FOR I%=0 TO 3:WRITE #1,LAST$(I%):NEXT I%
			WRITE #1,MAINMSG$
			CLOSE #1
			END
		END IF
		IF I$<>"L" THEN GOTO Start
	ELSE
		PRINT "<< Carrier detected >>"
		BAUD$=""
		WHILE BAUD$=""
			TIMER ON:INPUT #1,I$:PRINT "[";I$;"] "
			I$=RIGHT$(I$,4):TIMER OFF
			IF FNCD% THEN BAUD$="?"
			IF I$="NECT" THEN BAUD$="300"
			IF I$="1200" THEN BAUD$=I$
			IF I$="2400" THEN BAUD$=I$
			IF I$="9600" THEN BAUD$=I$
		WEND
		IF BAUD$="?" THEN GOTO Start
		CLOSE #1:OPEN "COM2:"+BAUD$+",N,8,1,LF,CD0,CS0,DS0,TB128" FOR RANDOM AS #1 LEN=128:WIDTH #1,80
		ZZ%=FNW%(CR$+BAUD$+" baud connection established."+CR$+CR$):SLEEP 2
	END IF

	RETRY%=3
	CLS 2:IF LOCL%=0 THEN PRINT #1,CHR$(27);"[H";CHR$(27);"[J";
	FILE$="LOGON.TXT":GOSUB PrintFile

Retry:
	IF FNW%(CR$+"Who dares to enter my dank domain <or NEW>? ") THEN GOTO Hungup
	USER%=FNUSER%
	IF USER%=0 THEN
		IF I$="NEW" THEN CHAIN "NEWUSER"
		RETRY%=RETRY%-1
		IF FNW%(CR$+BELL$+"Invalid response."+CR$) THEN GOTO Hungup
		IF RETRY%=0 THEN REASON$="Invalid response":GOTO Abort
		GOTO Retry
	END IF

	IF FNW%(CR$+FNL$(USER.HANDLE$)+", type your password: ") THEN GOTO Hungup
	NOECHO%=1:ZZ%=FNR%:NOECHO%=0:IF ZZ% THEN GOTO Hungup
	IF UCASE$(I$)<>FNL$(USER.PASSWORD$) THEN
		RETRY%=RETRY%-1:USER%=0
		IF RETRY%=2 THEN IF FNW%(CR$+"The guards eye you suspiciously."+CR$) THEN GOTO Hungup
		IF RETRY%=1 THEN IF FNW%(CR$+"The guards aim their crossbows at you."+CR$) THEN GOTO Hungup
		IF RETRY%=0 THEN REASON$="Password failure":GOTO Abort
		GOTO Retry
	END IF

GetDate:
	IF FNW%(CR$+CR$) THEN GOTO Hungup
	D$=RIGHT$(DATE$,4)+LEFT$(DATE$,2)+MID$(DATE$,4,2)
	T$=LEFT$(TIME$,2)+MID$(TIME$,4,2)
	T%=60*VAL(LEFT$(T$,2))+VAL(RIGHT$(T$,2))
	LT%=60*VAL(LEFT$(USER.LTIME$,2))+VAL(RIGHT$(USER.LTIME$,2))
	IF USER.LDATE$<>D$ THEN LSET USER.CALLS$=CHR$(0)

	IF MC%(ASC(USER.ACCESS$))<=ASC(USER.CALLS$) THEN
		ZZ%=FNW%(BELL$+"You have used all"+STR$(MC%(ASC(USER.ACCESS$)))+" of your calls for today."+CR$)
		GOTO Abort2
	END IF
	IF USER.LDATE$=D$ THEN
		IF T%-LT%<5 THEN
			ZZ%=FNW%(BELL$+"You were last on just"+STR$(T%-LT%)+" minutes ago."+CR$)
			ZZ%=FNW%("Please wait at least 5 minutes between calls."+CR$)
			GOTO Abort2
		END IF
	END IF
	CALLERS=CALLERS+1
	LSET USER.TCALL$=MKI$(CVI(USER.TCALL$)+1)
	LSET USER.CALLS$=CHR$(ASC(USER.CALLS$)+1)
	CLS
	IF LOCL%=0 THEN
		VIEW PRINT 1 TO 24:LOCATE 25,1:COLOR 0,7
		PRINT " ";FNL$(USER.NAME$);" ³ ";LEVEL$(ASC(USER.ACCESS$));" ³ ";FNL$(USER.HANDLE$);" ³ ";CLASS$(ASC(USER.CLASS$));TAB(80);
		COLOR 7,0:PRINT #1,CHR$(27);"[H";CHR$(27);"[J";
	END IF
	IF FNW%("--=:)) The Round Table ((:=--"+CR$+CR$) THEN GOTO Hungup
	IF FNW%(" Online: "+USER.HANDLE$+CR$) THEN GOTO Hungup
	IF FNW%(" Access: "+LEVEL$(ASC(USER.ACCESS$))+CR$) THEN GOTO Hungup
	IF FNW%("Last on: "+FND$(USER.LDATE$)+"  "+FNT$(USER.LTIME$)+CR$) THEN GOTO Hungup
	IF FNW%("Caller#:"+STR$(CALLERS)+CR$+CR$) THEN GOTO Hungup
	IF FNW%("You have"+STR$(MC%(ASC(USER.ACCESS$))-ASC(USER.CALLS$))+" calls remaining."+CR$+CR$) THEN GOTO Hungup
	IF FNW%("Last three callers were:"+CR$) THEN GOTO Hungup
	FOR I%=1 TO 3:ZZ%=FNW%(SPACE$(5)+FNL$(LAST$(I%))+CR$):NEXT I%
	LAST$(3)=FNL$(LAST$(2)):LAST$(2)=FNL$(LAST$(1)):LAST$(1)=FNL$(USER.HANDLE$)
	LSET USER.LDATE$=RIGHT$(DATE$,4)+LEFT$(DATE$,2)+MID$(DATE$,4,2)
	LSET USER.LTIME$=LEFT$(TIME$,2)+MID$(TIME$,4,2)
	LSET USER.STATUS$=CHR$(0)
	PUT #2,USER%
	LOGOFF%=60*VAL(LEFT$(TIME$,2))+VAL(MID$(TIME$,4,2))+MO%(ASC(USER.ACCESS$))
	I%=(ASC(USER.CURSED$)<>0)-(ASC(USER.BLESSED$)<>0)
	THP%=CVI(USER.HP$):TSP%=CVI(USER.SP$)
	TSTR%=ASC(USER.STR$)+10*I%:IF TSTR%>100 THEN TSTR%=100
	TAGL%=ASC(USER.AGL$)+10*I%:IF TAGL%>100 THEN TAGL%=100
	TINT%=ASC(USER.INT$)+10*I%:IF TINT%>100 THEN TINT%=100
	TCHR%=ASC(USER.CHR$)+10*I%:IF TCHR%>100 THEN TCHR%=100
	THIT%=10*I%*ASC(USER.WEAPON$)/100
	TSHIELD%=10*I%*ASC(USER.ARMOR$)/100
	THULL%=CVI(USER.HULL$)
	IF FNW%(CR$+"Press RETURN: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup

	ON ERROR GOTO ErrHandle1
	FILE$="NOTES\ARENA"+MID$(STR$(USER%),2)+".MSG":GOSUB PrintFile
	KILL FILE$
	IF FNW%(CR$+"Press RETURN: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
NoFile1:
	ON ERROR GOTO 0
	CLS 2:IF LOCL%=0 THEN PRINT #1,CHR$(27);"[H";CHR$(27);"[J";
	IF FNW%("       -------------"+CR$) THEN GOTO Hungup
	IF FNW%("--=:)) Announcements ((:=--"+CR$) THEN GOTO Hungup
	IF FNW%("       -------------"+CR$) THEN GOTO Hungup
	FILE$="WELCOME.TXT":GOSUB PrintFile
	ZZ%=FNW%(CR$)
	IF FNW%("       ------------"+CR$) THEN GOTO Hungup
	IF FNW%("--=:)) Auto-Message ((:=--"+CR$) THEN GOTO Hungup
	IF FNW%("       ------------"+CR$) THEN GOTO Hungup
	FILE$="AUTOMSG.TXT":GOSUB PrintFile
	IF FNW%(CR$+"Change Auto-Message (Y/N)? ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	IF UCASE$(I$)="Y" AND ASC(USER.ACCESS$)>4 THEN
		GOSUB Message
		IF MSG% THEN
			CLOSE #4:OPEN "O",#4,FILE$
			PRINT #4,"Date: ";FND$(USER.LDATE$)+"  "+FNT$(USER.LTIME$)
			PRINT #4,"From: ";FNL$(USER.HANDLE$)
			PRINT #4,""
			FOR I%=1 TO MSG%:PRINT #4,MSG$(I%):NEXT I%
			CLOSE #4
		END IF
	END IF
	ZZ%=FNW%(CR$+CR$)

	ON ERROR GOTO ErrHandle2
	IF FNW%("Checking for E-mail...") THEN GOTO Hungup
	FILE$="EMAIL\EMAIL"+MID$(STR$(USER%),2)+".MSG":GOSUB PrintFile
	KILL FILE$
	IF FNW%(CR$+"Press RETURN: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
NoFile2:
	ON ERROR GOTO 0
	D#=INT(2.7^((ASC(USER.LEVEL$)-1)/(100/MW%))*WC(1)*10000)
	IF D#<(CVD(USER.GOLD$)+CVD(USER.BANK$)) THEN
		TAX#=CVD(USER.GOLD$)+CVD(USER.BANK$)-D#:M!=TAX#
		LSET USER.BANK$=MKD$(CVD(USER.BANK$)-TAX#)
		IF CVD(USER.BANK$)<0 THEN
			LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+CVD(USER.BANK$))
			LSET USER.BANK$=MKD$(0)
			IF CVD(USER.GOLD$)<0 THEN LSET USER.GOLD$=MKD$(0)
		END IF
		PUT #2,USER%
		IF FNW%("Joe Mollicone, the banker, looks at your buldging sacks of gold and says,"+CR$) THEN GOTO Hungup
		IF FNW%("'Ah, let me invest your extra money!'  You are taken by his smile and his"+CR$) THEN GOTO Hungup
		IF FNW%("shoes, so you blindly hand over your reserves."+CR$+CR$) THEN GOTO Hungup
		IF FNW%("Your investment costs you"+STR$(M!)+" gold pieces."+CR$) THEN GOTO Hungup
	END IF
	CHAIN "MAIN"

ErrHandle1:
	RESUME NoFile1

ErrHandle2:
	IF ERR=53 THEN ZZ%=FNW%("No mail left for you."+CR$)
	RESUME NoFile2

Abort:
	ZZ%=FNW%(CR$+"The last thing you ever feel is several quarrels cutting deep into your chest."+CR$)
Abort2:
	PRINT "<< Hanging up >>"
	CLOSE #1:SLEEP 2
	GOTO Start
