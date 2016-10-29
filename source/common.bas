	'Common QuickBASIC code to all RT2 modules'
	DEFDBL A-Z
	COMMON LEVEL$(),MO%(),MC%(),CLASS$(),MA%,ARMOR$(),AC(),MW%,WEAPON$(),WC(),MR%,RE$(),RC(),MS%,SECURITY$(),SC(),SPELL$(),SP%(),GANG$(),LAST$(),CALLERS,LOCL%,MAINMSG$,ERRMSG$()
	COMMON USER%,LOGOFF%,THP%,TSP%,TSTR%,TAGL%,TINT%,TCHR%,THIT%,TSHIELD%,THULL%,FIGHT%,JOUST%,BRAWL%,PARTY%,NAVAL%

	DIM MSG$(5)
	BELL$=CHR$(7):BS$=CHR$(8):CR$=CHR$(13):DEL$=CHR$(127)

	CLOSE #2:OPEN "R",#2,"USER.DAT",192
	FIELD #2,192 AS USER.REC$
	FIELD #2,26 AS USER.HANDLE$,26 AS USER.NAME$,20 AS USER.CITY$,2 AS USER.STATE$,8 AS USER.PHONE$,10 AS USER.PASSWORD$,1 AS USER.SEX$,1 AS USER.ACCESS$,8 AS USER.LDATE$,4 AS USER.LTIME$,2 AS USER.TCALL$,1 AS USER.CALLS$
	FIELD #2,109 AS A$,1 AS USER.STATUS$,1 AS USER.CLASS$,1 AS USER.LEVEL$,8 AS USER.EXP$,2 AS USER.HP$,2 AS USER.SP$,1 AS USER.STR$,1 AS USER.AGL$,1 AS USER.INT$,1 AS USER.CHR$,2 AS USER.JW$,2 AS USER.JL$
	FIELD #2,132 AS A$,1 AS USER.WEAPON$,1 AS USER.ARMOR$,2 AS USER.SPELL$,2 AS USER.POISON$,1 AS USER.RE$,1 AS USER.SECURITY$,8 AS USER.GOLD$,8 AS USER.BANK$,8 AS USER.LOAN$
	FIELD #2,164 AS A$,1 AS USER.GANG$,2 AS USER.HULL$,1 AS USER.CANNON$,1 AS USER.RAM$
	FIELD #2,169 AS A$,1 AS USER.BLESSED$,1 AS USER.CURSED$,2 AS USER.IMMORTAL$

	ON TIMER(60) GOSUB TimeOut
	ON ERROR GOTO SystemError
	TIMER OFF
	GOTO Start

	DEF FNCD%=NOT (((INP(&H02FE) AND 128)=128) OR LOCL%)

	DEF FNW%(A$)
		PRINT A$;
		IF LOCL%=0 THEN PRINT #1,A$;
		FNW%=FNCD%
	END DEF

	DEF FNR%
		I$=""
		A$=""
		TIMEOUT%=2
		TIMER ON
		LOCATE ,,1,0,12
		WHILE A$<>CR$ AND FNCD%=0
			A$=INKEY$
			IF LOCL%=0 AND A$=CHR$(27) THEN
				TIMER OFF
				T%=60*VAL(LEFT$(TIME$,2))+VAL(MID$(TIME$,4,2))
				ZZ%=FNW%(CR$+">> Sysop is now online <<"+CR$+CR$)
				DO
					IF LOC(1) THEN A$=INPUT$(1,1):IF A$=BS$ OR A$=DEL$ THEN PRINT CHR$(29);" ";CHR$(29);:PRINT #1,BS$;" ";BS$; ELSE ZZ%=FNW%(A$)
					A$=INKEY$:IF A$=BS$ OR A$=DEL$ THEN PRINT CHR$(29);" ";CHR$(29);:PRINT #1,BS$;" ";BS$; ELSE ZZ%=FNW%(A$)
				LOOP UNTIL A$=CHR$(27)
				ZZ%=FNW%(CR$+"<< Sysop went offline >>"+CR$+CR$)
				ZZ%=FNW%("? "+I$)
				LOGOFF%=LOGOFF%+(60*VAL(LEFT$(TIME$,2))+VAL(MID$(TIME$,4,2))-T%+1)
				TIMER ON
			END IF
			IF LOCL%=0 THEN A$=""
			IF LOC(1) THEN A$=INPUT$(1,1):IF LOCL% THEN A$=""
			IF (A$=BS$ OR A$=DEL$) AND LEN(I$) THEN
				I$=LEFT$(I$,LEN(I$)-1)
				PRINT CHR$(29);" ";CHR$(29);
				IF LOCL%=0 THEN PRINT #1,BS$;" ";BS$;
			END IF
			IF A$=CHR$(20) THEN T%=60*VAL(LEFT$(TIME$,2))+VAL(MID$(TIME$,4,2)):ZZ%=FNW%(CR$+"Date: "+DATE$+"  Time: "+TIME$+"  Remaining:"+STR$(LOGOFF%-T%)+" min.  RAM:"+STR$(FRE(-1))):A$=CHR$(13)
			IF A$>=" " AND A$<DEL$ AND LEN(I$)<75 THEN
				I$=I$+A$
				IF NOECHO% THEN ZZ%=FNW%(".") ELSE ZZ%=FNW%(A$)
			END IF
		WEND
		TIMER OFF
		LOCATE ,,0
		IF UCASE$(I$)="OFF" THEN FNR%=-1 ELSE FNR%=FNCD%
	END DEF

	DEF FNT%
		GET #2,USER%
		T%=60*VAL(LEFT$(TIME$,2))+VAL(MID$(TIME$,4,2))
		IF DATE$<>LAST$(0) THEN
			ZZ%=FNW%("Midnight crossed")
			FIGHT%=0:JOUST%=0:BRAWL%=0:NAVAL%=0:PARTY%=0
			FOR REC%=1 TO 96
				IF (REC% MOD 10)=0 THEN ZZ%=FNW%(".")
				GET #2,REC%
				LSET USER.CALLS$=CHR$(0)
				LSET USER.STATUS$=CHR$(0)
				PUT #2,REC%
			NEXT REC%
			GET #2,USER%
			D$=RIGHT$(DATE$,4)+LEFT$(DATE$,2)+MID$(DATE$,4,2)
			LSET USER.LDATE$=D$
			LSET USER.TCALL$=MKI$(CVI(USER.TCALL$)+1)
			LSET USER.CALLS$=CHR$(1)
			PUT #2,USER%
			LOGOFF%=T%+MO%(ASC(USER.ACCESS$))
			ZZ%=FNW%("new call."+CR$+CR$)
			LAST$(0)=DATE$
		END IF
		FNT%=((LOGOFF%-T%)<0)
	END DEF

	DEF FND(X)=INT(X*RND(1)+1)

	DEF FNL$(A$)
		IF INSTR(A$,CHR$(0)) THEN A$=LEFT$(A$,INSTR(A$,CHR$(0))-1)
		FNL$=A$
	END DEF

	DEF FND$(A$)=MID$("???JanFebMarAprMayJunJulAugSepOctNovDec",3*VAL(MID$(A$,5,2))+1,3)+STR$(VAL(RIGHT$(A$,2)))+", "+LEFT$(A$,4)

	DEF FNT$(A$)
		IF VAL(A$)>=1200 THEN
			IF VAL(A$)>=1300 THEN A$=RIGHT$("0"+MID$(STR$(VAL(LEFT$(A$,2))-12),2),2)
			T$=LEFT$(A$,2)+":"+RIGHT$(A$,2)+" PM"
		ELSE
			IF VAL(LEFT$(A$,2))=0 THEN A$="12"+RIGHT$(A$,2)
			T$=LEFT$(A$,2)+":"+RIGHT$(A$,2)+" AM"
		END IF
		FNT$=T$
	END DEF

	DEF FNUSER%
		ZZ%=FNR%
		I$=UCASE$(LTRIM$(I$)):I$=RTRIM$(I$)
		REC%=0
		IF VAL(I$)>0 AND VAL(I$)<=96 THEN
			REC%=VAL(I$):GET #2,REC%
			IF ASC(USER.ACCESS$) THEN ZZ%=FNW%(" - "+USER.HANDLE$+CR$)
		END IF
		IF VAL(I$)=0 AND LEN(I$) AND UCASE$(I$)<>"NEW" AND I$<>"*" THEN
			REC%=0
			DO
				REC%=REC%+1
				GET #2,REC%
			LOOP WHILE I$<>UCASE$(FNL$(USER.HANDLE$)) AND REC%<96 AND ZZ%=0
			IF I$=UCASE$(FNL$(USER.HANDLE$)) THEN
				IF ASC(USER.ACCESS$) THEN ZZ%=FNW%(" - User"+STR$(REC%)+CR$)
			ELSE
				ZZ%=FNW%(BELL$+CR$)
				REC%=0
			END IF
		END IF
		IF ASC(USER.ACCESS$)=0 THEN REC%=0
		FNUSER%=REC%
	END DEF

Message:
	ZZ%=FNW%(CR$+"Enter up to five lines of text below.  Enter / on a blank line for options."+CR$)
	FOR MSG%=1 TO 5
		ZZ%=FNW%(STR$(MSG%)+"] ")
		IF FNR% THEN GOTO Hungup
		IF LEFT$(I$,1)="/" THEN EXIT FOR
		ZZ%=FNW%(CR$)
		MSG$(MSG%)=I$
	NEXT MSG%
	A$=UCASE$(MID$(I$,2)):MSG%=MSG%-1
	WHILE A$<>"S" AND A$<>"Q"
		ZZ%=FNW%(CR$+"<S>ave or <Q>uit: ")
		IF FNR% THEN GOTO Hungup
		A$=UCASE$(LEFT$(I$,1))
	WEND
	IF A$="Q" THEN MSG%=0
	ZZ%=FNW%("aving...")
	RETURN

Hungup:
	IF USER% THEN GOTO Logoff
	CHAIN "LOGON"

Caught:
	ZZ%=FNW%(BELL$+"A guard catches you and throws you into jail!"+CR$)
	ZZ%=FNW%("You might be released by your next call."+CR$)
	GOTO Logoff

Expired:
	GET #2,USER%
	ZZ%=FNW%(CR$+BELL$+"Your"+STR$(MO%(ASC(USER.ACCESS$)))+" minute time limit has expired!"+CR$)
	GOTO Logoff

Logoff:
	ZZ%=FNW%(CR$)
	FILE$="LOGOFF.TXT":GOSUB PrintFile
Hangup:
	IF USER% THEN
		GET #2,USER%
		LSET USER.LTIME$=LEFT$(TIME$,2)+MID$(TIME$,4,2)
		PUT #2,USER%
	END IF
	CLOSE #1:SLEEP 2
	CHAIN "LOGON"

PrintFile:
	ZZ%=FNW%(CR$):LP%=0:A$="":I$=""
	CLOSE #4:OPEN "I",#4,FILE$
	WHILE NOT EOF(4) AND ZZ%=0 AND A$<>CHR$(24) AND I$<>"N"
		LINE INPUT #4,A$
		ZZ%=FNW%(A$+CR$):LP%=LP%+1
		IF LP%>22 THEN
			ZZ%=FNW%("More (Y/N)? "):ZZ%=FNR%:I$=UCASE$(I$):ZZ%=FNW%(CR$)
			LP%=0
		ELSE
			A$=INKEY$
			IF LOC(1) THEN A$=INPUT$(1,1)
			IF A$=CHR$(24) THEN ZZ%=FNW%("<Abort>"+CR$)
		END IF
	WEND
	CLOSE #4
	RETURN

SystemError:
	CLOSE #4:SLEEP 2
	ZZ%=FNW%(BELL$+CR$+"System error #"+MID$(STR$(ERR),2)+" - "+ERRMSG$(ERR)+CR$)
	IF ERR=57 THEN RESUME Start
	IF ERR=53 OR ERR=61 THEN CHAIN "MAIN"
	ZZ%=FNW%("Bye!"+CR$)
	RESUME Hangup

TimeOut:
	IF TIMEOUT%=1 THEN
		TIMEOUT%=TIMEOUT%-1
		ZZ%=FNW%(BELL$+CR$)
		RETURN Logoff
	END IF
	IF TIMEOUT%=2 THEN
		TIMEOUT%=TIMEOUT%-1
		ZZ%=FNW%(BELL$)
	END IF
	RETURN

Start:
	ON ERROR GOTO SystemError
