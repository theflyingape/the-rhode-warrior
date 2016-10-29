	'$INCLUDE: 'COMMON.BAS'

	DEF FNC$(A$)
		B$=LCASE$(A$):J%=1
		FOR I%=1 TO LEN(B$)
			IF J%=1 AND MID$(B$,I%,1)>="a" AND MID$(B$,I%,1)<="z" THEN B$=LEFT$(B$,I%-1)+UCASE$(MID$(B$,I%,1))+MID$(B$,I%+1):J%=0
			IF ASC(MID$(B$,I%,1))<65 THEN J%=1
		NEXT I%
		FNC$=B$
	END DEF

	DIM GM%(24,4),G%(2),TL%(2),GI%(8),GJ%(8),GNAME$(2,4),GLVL%(2,4),GSTR%(2,4),GAGL%(2,4),GW%(2,4),GA%(2,4),GHP%(2,4)

	A%=0:B%=0:YOURGANG%=0:YOURMEMBER%=0
	OPEN "I",#4,"GANG.DAT"
	FOR I%=1 TO 24
		INPUT #4,GANG$(I%),GM%(I%,1),GM%(I%,2),GM%(I%,3),GM%(I%,4)
		IF LEN(GANG$(I%)) THEN
			F%=1
			FOR J%=1 TO 4
				IF GM%(I%,J%)<1 THEN F%=0
				IF GM%(I%,J%)=USER% THEN YOURGANG%=I%:YOURMEMBER%=J%
			NEXT J%
			A%=A%+F%:B%=B%+1
		END IF
	NEXT I%
	CLOSE #4
	IF FNW%(CR$+"There are"+STR$(A%)+" active out of"+STR$(B%)+" gangs."+CR$) THEN RETURN Hungup

Top:
	GET #2,USER%
	IF CVD(USER.EXP$)>(2^(ASC(USER.LEVEL$)-1)*(1100-ASC(USER.INT$)*2)) THEN ZZ%=FNW%("Hang on..."+CR$):CHAIN "MAIN"
	IF FNW%(CR$) THEN GOTO Hungup
	IF FNW%("<E> Edit your gang"+CR$) THEN GOTO Hungup
	IF FNW%("<F> Fight another gang"+CR$) THEN GOTO Hungup
	IF FNW%("<J> Join a gang"+CR$) THEN GOTO Hungup
	IF FNW%("<L> List current gangs"+CR$) THEN GOTO Hungup
	IF FNW%("<R> Resign your membership"+CR$) THEN GOTO Hungup
	IF FNW%("<S> Start a new gang"+CR$) THEN GOTO Hungup
	IF FNW%("<T> Transfer leadership"+CR$) THEN GOTO Hungup
	IF FNW%(CR$+"[Party] Option (Q=Quit): ") THEN GOTO Hungup
	IF FNT% THEN GOTO Expired
	IF FNR% THEN GOTO Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$)
	IF I$="Q" THEN CHAIN "MAIN"
	F%=-(I$="E")-2*(I$="F")-3*(I$="J")-4*(I$="L")-5*(I$="R")-6*(I$="S")-7*(I$="T")
	ON F% GOSUB EditGang,Fight,JoinGang,ListGang,Resign,StartNew,Transfer
	GOTO Top

EditGang:
	IF YOURMEMBER%<>1 THEN ZZ%=FNW%("Only your gang leader may do this."+CR$):RETURN
	IF FNW%(GANG$(YOURGANG%)+":"+CR$) THEN RETURN Hungup
	FOR J%=1 TO 4
		ZZ%=FNW%("<"+MID$(STR$(J%),2)+"> ")
		REC%=GM%(YOURGANG%,J%)
		IF REC%<>0 AND REC%<>-32767 THEN
			GET #2,ABS(REC%)
			IF ASC(USER.ACCESS$)>3 THEN
				ZZ%=FNW%(FNL$(USER.HANDLE$))
				IF J%=1 THEN ZZ%=FNW%(" (leader)")
				IF GM%(YOURGANG%,J%)<0 THEN ZZ%=FNW%(" (invited)")
			ELSE
				ZZ%=FNW%("User #"+MID$(STR$(ABS(REC%)),2)+" cannot party.")
			END IF
		ELSE
			IF REC%=-32767 THEN ZZ%=FNW%("(Any user is welcome!)") ELSE ZZ%=FNW%("(empty)")
		END IF
		ZZ%=FNW%(CR$)
	NEXT J%
	IF FNW%(CR$+"Replace who (2-4)? ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	X%=VAL(I$):IF X%<2 OR X%>4 THEN RETURN
	IF GM%(YOURGANG%,X%)<>-32767 AND GM%(YOURGANG%,X%)<>0 THEN
		GET #2,ABS(GM%(YOURGANG%,X%))
		ZZ%=FNW%(" - "+FNL$(USER.HANDLE$)+CR$)
	ELSE
		IF GM%(YOURGANG%,X%)=-32767 THEN ZZ%=FNW%(" - (Any user is welcome!)"+CR$) ELSE ZZ%=FNW%(" - (empty)"+CR$)
	END IF
	IF FNW%("With what user (*=Anybody)? ") THEN RETURN Hungup
	REC%=FNUSER%:IF I$="*" THEN REC%=-32767
	IF REC%=0 THEN RETURN
	FOR J%=1 TO 4
		IF ABS(GM%(YOURGANG%,J%))=REC% THEN ZZ%=FNW%("I don't think so!"+CR$):RETURN
	NEXT J%
	IF GM%(YOURGANG%,X%)>0 THEN
		GET #2,ABS(GM%(YOURGANG%,X%))
		LSET USER.GANG$=CHR$(0)
		PUT #2,GM%(YOURGANG%,X%)
	END IF
	IF REC%>0 THEN
		OPEN "A",#4,"NOTES\ARENA"+MID$(STR$(REC%),2)+".MSG"
		PRINT #4,"-=>*<=-":PRINT #4,""
		PRINT #4,"You have been invited to join the gang, ";GANG$(YOURGANG%);"."
		PRINT #4,""
		CLOSE #4
	END IF
	GM%(YOURGANG%,X%)=-ABS(REC%)
	GOSUB SaveGang
	RETURN

Fight:
	IF YOURGANG%=0 THEN ZZ%=FNW%("You are not a member of any gang!"+CR$+CR$):RETURN
	IF PARTY% THEN ZZ%=FNW%("You have already used your gang fight this call."+CR$+CR$):RETURN
	F%=0:TL%(1)=0
	FOR J%=1 TO 4
		IF GM%(YOURGANG%,J%)<1 THEN F%=1:EXIT FOR
		GET #2,GM%(YOURGANG%,J%)
		IF ASC(USER.ACCESS$)=0 THEN F%=1:EXIT FOR
		GNAME$(1,J%)=FNL$(USER.HANDLE$)
		GLVL%(1,J%)=ASC(USER.LEVEL$):GHP%(1,J%)=CVI(USER.HP$):GW%(1,J%)=ASC(USER.WEAPON$):GA%(1,J%)=ASC(USER.ARMOR$)
		GSTR%(1,J%)=ASC(USER.STR$):GINT%(1,J%)=ASC(USER.STR$):GAGL%(1,J%)=ASC(USER.STR$):GCHR%(1,J%)=ASC(USER.STR$)
		TL%(1)=TL%(1)+GLVL%(1,J%)
	NEXT J%
	IF F% THEN ZZ%=FNW%("Your gang is not active yet!"+CR$):RETURN
	FOR I%=1 TO 24
		IF I%<>YOURGANG% AND LEN(GANG$(I%))<>0 THEN ZZ%=FNW%(RIGHT$(" <"+MID$(STR$(I%),2)+"> ",5)+GANG$(I%)+CR$)
	NEXT I%
	IF FNW%(CR$+"Fight which gang: ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	ZZ%=FNW%(CR$+CR$)
	NMEGANG%=VAL(I$):IF NMEGANG%<1 OR NMEGANG%>24 THEN RETURN
	IF LEN(GANG$(NMEGANG%))=0 THEN GOTO Fight
	IF NMEGANG%=YOURGANG% THEN ZZ%=FNW%("They're just a bunch of wimps."+CR$):RETURN
	F%=0:TL%(2)=0
	FOR J%=1 TO 4
		IF GM%(NMEGANG%,J%)<1 THEN F%=1:EXIT FOR
		GET #2,GM%(NMEGANG%,J%)
		IF ASC(USER.ACCESS$)=0 THEN F%=1:EXIT FOR
		GNAME$(2,J%)=FNL$(USER.HANDLE$)
		GLVL%(2,J%)=ASC(USER.LEVEL$):GHP%(2,J%)=CVI(USER.HP$):GW%(2,J%)=ASC(USER.WEAPON$):GA%(2,J%)=ASC(USER.ARMOR$)
		GSTR%(2,J%)=ASC(USER.STR$):GINT%(2,J%)=ASC(USER.STR$):GAGL%(2,J%)=ASC(USER.STR$):GCHR%(2,J%)=ASC(USER.STR$)
		TL%(2)=TL%(2)+GLVL%(2,J%)
	NEXT J%
	IF F% THEN ZZ%=FNW%("Their gang is not active yet!"+CR$):RETURN
	IF FNW%(GANG$(NMEGANG%)+CR$+CR$) THEN RETURN Hungup
	IF FNW%("Members: "+GNAME$(2,1)+" (leader)"+CR$) THEN RETURN Hungup
	FOR I%=2 TO 4:ZZ%=FNW%(SPACE$(9)+GNAME$(2,I%)+CR$):NEXT I%
	IF FNW%(CR$+"Are you sure (Y/N)? ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	IF UCASE$(I$)<>"Y" THEN RETURN
	PARTY%=1
	ZZ%=FNW%(CR$+CR$+"The two parties advance cautiously towards each other."+CR$+CR$)
	SLEEP 1
	IF GW%(2,1) THEN ZZ%=FNW%(GNAME$(2,1)+" grins as he pulls out his "+WEAPON$(GW%(2,1))+"."+CR$) ELSE ZZ%=FNW%(GNAME$(2,1)+" quivers as he stares at his empty hands."+CR$)
	SLEEP 1
	G%(1)=4:G%(2)=4
FightLoop:
	ZZ%=FNW%(CR$)
	X%=0:FOR I%=1 TO 2:FOR J%=1 TO 4:X%=X%+1:GI%(X%)=I%:GJ%(X%)=J%:NEXT J%:NEXT I%
	FOR X%=1 TO 8:Y%=FND(8):SWAP GI%(X%),GI%(Y%):SWAP GJ%(X%),GJ%(Y%):NEXT X%
	FOR X%=1 TO 8
		IF GHP%(GI%(X%),GJ%(X%))<1 THEN GOTO SkipHere
		O%=-(GI%(X%)=2)-2*(GI%(X%)=1)
		DO:Y%=FND(4):LOOP WHILE GHP%(O%,Y%)<1
		IF FND(100)>GAGL%(GI%(X%),GJ%(X%)) THEN ZZ%=FNW%(GNAME$(GI%(X%),GJ%(X%))+" attacks "+GNAME$(O%,Y%)+", but misses."+CR$):GOTO SkipHere
		A%=4*GW%(GI%(X%),GJ%(X%))+GLVL%(GI%(X%),GJ%(X%))+GSTR%(GI%(X%),GJ%(X%))/10-GA%(O%,Y%)
		A%=A%/2+RND(1)*A%/2:IF A%<1 THEN A%=1
		GHP%(O%,Y%)=GHP%(O%,Y%)-A%
		ZZ%=FNW%(GNAME$(GI%(X%),GJ%(X%)))
		IF GHP%(O%,Y%)>0 THEN
			ZZ%=FNW%(" hits "+GNAME$(O%,Y%)+" for"+STR$(A%)+" hit points")
		ELSE
			I%=FND(5)
			IF I%=1 THEN ZZ%=FNW%("'s "+WEAPON$(GW%(GI%(X%),GJ%(X%)))+" makes a fatal blow to "+GNAME$(O%,Y%))
			IF I%=2 THEN ZZ%=FNW%(" blows "+GNAME$(O%,Y%)+" away")
			IF I%=3 THEN ZZ%=FNW%(" laughs, then kills "+GNAME$(O%,Y%))
			IF I%=4 THEN ZZ%=FNW%(" easily slays "+GNAME$(O%,Y%))
			IF I%=5 THEN ZZ%=FNW%("'s "+WEAPON$(GW%(GI%(X%),GJ%(X%)))+" makes minced-meat out of "+GNAME$(O%,Y%))
			G%(O%)=G%(O%)-1
		END IF
		ZZ%=FNW%("."+CR$)
		IF G%(O%)=0 THEN EXIT FOR
SkipHere:
	NEXT X%
WhatNext:
	ZZ%=FNW%(CR$)
	IF G%(1)>0 AND G%(2)>0 THEN
		DO
			IF FNW%("<C> Continue the slaughter, <R> Run like hell, <Y> Your gang's status: ") THEN RETURN Hungup
			IF FNR% THEN RETURN Hungup
			I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$)
		LOOP UNTIL I$="C" OR I$="R" OR I$="Y"
		IF I$="C" THEN GOTO FightLoop
		IF I$="R" THEN
			ZZ%=FNW%(GANG$(YOURGANG%)+" has become known as a bunch of cowards."+CR$)
			MSG$(1)=GANG$(YOURGANG%)+" ran like hell from "+GANG$(NMEGANG%)+"."
			MSG%=1:G%(1)=YOURGANG%:G%(2)=NMEGANG%
			FOR I%=1 TO 2:FOR J%=1 TO 4
				IF YOURGANG%<>G%(I%) OR YOURMEMBER%<>J% THEN
					OPEN "A",#4,"NOTES\ARENA"+MID$(STR$(GM%(G%(I%),J%)),2)+".MSG"
					PRINT #4,"-=>*<=-":PRINT #4,""
					FOR K%=1 TO MSG%:PRINT #4,MSG$(K%):NEXT K%
					PRINT #4,""
					CLOSE #4
				END IF
			NEXT J%:NEXT I%
			RETURN
		END IF
		FOR J%=1 TO 4
			IF GHP%(1,J%)>0 THEN ZZ%=FNW%(GNAME$(1,J%)+" has"+STR$(GHP%(1,J%))+" hit points."+CR$) ELSE ZZ%=FNW%(GNAME$(1,J%)+" is dead."+CR$)
		NEXT J%
		GOTO WhatNext
	END IF
	IF G%(1)=0 THEN
		ZZ%=FNW%(GANG$(YOURGANG%)+" have been annihilated!"+CR$)
		MSG$(1)=GANG$(YOURGANG%)+" was defeated by "+GANG$(NMEGANG%)+"!"
		L%=1:G%(L%)=YOURGANG%:W%=2:G%(W%)=NMEGANG%:GOSUB Spoils
		RETURN Logoff
	END IF
	IF G%(2)=0 THEN
		ZZ%=FNW%(GANG$(NMEGANG%)+" have been annihilated!"+CR$)
		MSG$(1)=GANG$(YOURGANG%)+" defeated "+GANG$(NMEGANG%)+"!"
		L%=2:G%(L%)=NMEGANG%:W%=1:G%(W%)=YOURGANG%:GOSUB Spoils
	END IF
	RETURN
Spoils:
	G#=0
	FOR J%=1 TO 4
		GET #2,GM%(G%(L%),J%)
		G#=G#+CVD(USER.GOLD$)
		LSET USER.GOLD$=MKD$(0)
		PUT #2,GM%(G%(L%),J%)
	NEXT J%
	P#=0:FOR J%=1 TO 4:P#=P#+2.5^(GLVL%(W%,J%)/3):NEXT J%
	X!=0:M!=0
	FOR J%=1 TO 4
		GET #2,GM%(G%(W%),J%)
		XP#=INT(2^(GLVL%(W%,J%)-2)*1000*TL%(L%)/TL%(W%)):XS!=XP#
		X!=X!+XS!
		LSET USER.EXP$=MKD$(CVD(USER.EXP$)+XP#)
		GOLD#=INT(G#*2.5^(GLVL%(W%,J%)/3)/P#):MS!=GOLD#
		M!=M!+MS!
		LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+GOLD#)
		PUT #2,GM%(G%(W%),J%)
		A$="You got"+STR$(XS!)+" experience and"+STR$(MS!)+" gold pieces."
		I%=W%:MSG$(2)=A$:MSG%=2:GOSUB AlertUser
		IF G%(W%)=YOURGANG% AND J%=YOURMEMBER% THEN ZZ%=FNW%(A$+CR$)
	NEXT J%
	FOR J%=1 TO 4
		A$=GANG$(G%(W%))+" got"+STR$(X!)+" experience and"+STR$(M!)+" gold pieces."
		I%=L%:MSG$(2)=A$:MSG%=2:GOSUB AlertUser
	NEXT J%
	RETURN
AlertUser:
	IF YOURGANG%<>G%(I%) OR YOURMEMBER%<>J% THEN
		OPEN "A",#4,"NOTES\ARENA"+MID$(STR$(GM%(G%(I%),J%)),2)+".MSG"
		PRINT #4,"-=>*<=-":PRINT #4,""
		FOR K%=1 TO MSG%:PRINT #4,MSG$(K%):NEXT K%
		PRINT #4,""
		CLOSE #4
	END IF
	RETURN

JoinGang:
	IF YOURGANG% THEN
		IF FNW%("You are already a member of "+GANG$(YOURGANG%)+"."+CR$) THEN RETURN Hungup
		RETURN
	END IF
	IF FNW%("You have been invited to join:"+CR$) THEN RETURN Hungup
	FOR I%=1 TO 24
		FOR J%=2 TO 4
			IF GM%(I%,J%)=-USER% OR GM%(I%,J%)=-32767 THEN ZZ%=FNW%(STR$(I%)+".  "+GANG$(I%)+CR$):EXIT FOR
		NEXT J%
	NEXT I%
	IF FNW%(CR$+"Join which gang? ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	X%=VAL(I$):IF X%<0 OR X%>24 THEN RETURN
	FOR J%=2 TO 4
		IF ABS(GM%(X%,J%))=USER% OR GM%(X%,J%)=-32767 THEN
			YOURGANG%=X%:YOURMEMBER%=J%
			GET #2,USER%
			LSET USER.GANG$=CHR$(YOURGANG%)
			PUT #2,USER%
			GM%(X%,J%)=USER%:GOSUB SaveGang
			EXIT FOR
		END IF
	NEXT J%
	IF YOURGANG%=0 THEN ZZ%=FNW%(" - they did not invite a wimp like you!"+CR$)
	RETURN

ListGang:
	X%=0
	FOR I%=1 TO 24
		IF LEN(GANG$(I%)) THEN
			IF FNW%("-=> "+GANG$(I%)+" <=-"+CR$) THEN RETURN Hungup
			FOR J%=1 TO 4
				IF GM%(I%,J%)<>0 AND GM%(I%,J%)<>-32767 THEN
					GET #2,ABS(GM%(I%,J%))
					IF ASC(USER.ACCESS$) THEN
						ZZ%=FNW%(SPACE$(4)+FNL$(USER.HANDLE$))
						IF J%=1 THEN ZZ%=FNW%(" (leader)")
						IF GM%(I%,J%)<0 THEN ZZ%=FNW%(" (invited)")
					ELSE
						ZZ%=FNW%("User #"+MID$(STR$(ABS(GM%(I%,J%))),2)+" is deleted.")
					END IF
				ELSE
					IF GM%(I%,J%)=-32767 THEN ZZ%=FNW%("    (Any user is welcome!)") ELSE ZZ%=FNW%("    (empty)")
				END IF
				ZZ%=FNW%(CR$)
			NEXT J%
			ZZ%=FNW%(CR$):X%=X%+1
			IF (X% MOD 3)=0 THEN 
				ZZ%=FNW%("Press RETURN: ")
				IF FNR% THEN RETURN Hungup
				ZZ%=FNW%(CR$+CR$)
			END IF
		END IF
	NEXT I%
	IF X% THEN 
		ZZ%=FNW%("Press RETURN: ")
		IF FNR% THEN RETURN Hungup
		ZZ%=FNW%(CR$+CR$)
	END IF
	RETURN

Resign:
	IF YOURGANG%=0 THEN ZZ%=FNW%("You are not a member of ANY gang!"+CR$):RETURN
	IF FNW%("Really resign from "+GANG$(YOURGANG%)+" (Y/N)? ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	IF UCASE$(I$)<>"Y" THEN RETURN
	ZZ%=FNW%(CR$+CR$)
	IF YOURMEMBER%=1 THEN
		ZZ%=FNW%("Since you were their leader, the gang will dissolve."+CR$)
		FOR J%=1 TO 4
			IF GM%(YOURGANG%,J%)>0 THEN
				GET #2,GM%(YOURGANG%,J%)
				ZZ%=FNW%(FNL$(USER.HANDLE$)+"...")
				LSET USER.GANG$=CHR$(0)
				PUT #2,GM%(YOURGANG%,J%)
			END IF
			GM%(YOURGANG%,J%)=0
		NEXT J%
		GANG$(YOURGANG%)=""
		ZZ%=FNW%(CR$+CR$)
	ELSE
		GET #2,USER%
		LSET USER.GANG$=CHR$(0)
		PUT #2,USER%
		GM%(YOURGANG%,YOURMEMBER%)=-32767
	END IF
	YOURGANG%=0:YOURMEMBER%=0
	GOSUB SaveGang
	RETURN

StartNew:
	IF YOURGANG% THEN
		IF FNW%("You are already a member of "+GANG$(YOURGANG%)+"!"+CR$) THEN RETURN Hungup
		RETURN
	END IF
	IF ASC(USER.ACCESS$)<6 THEN
		IF FNW%("You must reach Knight or status to be leader."+CR$) THEN RETURN Hungup
		RETURN
	END IF
	F%=0
	FOR I%=1 TO 24
		IF LEN(GANG$(I%))=0 THEN F%=I%:EXIT FOR
	NEXT I%
	IF F%=0 THEN ZZ%=FNW%(BELL$+"No more gangs can be created!"+CR$):RETURN
	IF FNW%("What is the name of this new gang [max.20]? ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	IF LEN(I$)<1 OR LEN(I$)>20 THEN RETURN
	I$=FNC$(I$)
	J%=0
	FOR I%=1 TO 24
		IF UCASE$(GANG$(I%))=UCASE$(I$) THEN J%=I%:EXIT FOR
	NEXT I%
	IF J% THEN ZZ%=FNW%(BELL$+" - gang already exists!"+CR$):RETURN
	GANG$(F%)=I$:GM%(F%,1)=USER%:FOR I%=2 TO 4:GM%(F%,I%)=-32767:NEXT I%
	LSET USER.GANG$=CHR$(F%)
	PUT #2,USER%
	GOSUB SaveGang
	YOURGANG%=F%:YOURMEMBER%=1
	RETURN

Transfer:
	IF YOURMEMBER%<>1 THEN ZZ%=FNW%("Only your gang leader may do this."+CR$):RETURN
	IF FNW%(GANG$(YOURGANG%)+":"+CR$) THEN RETURN Hungup
	FOR J%=1 TO 4
		IF GM%(YOURGANG%,J%) THEN
			GET #2,ABS(GM%(YOURGANG%,J%))
			IF ASC(USER.ACCESS$) THEN
				ZZ%=FNW%(FNL$(USER.HANDLE$))
				IF J%=1 THEN ZZ%=FNW%(" (leader)")
				IF GM%(YOURGANG%,J%)<0 THEN ZZ%=FNW%(" (invited)")
			ELSE
				ZZ%=FNW%("User #"+MID$(STR$(ABS(GM%(YOURGANG%,J%))),2)+" is deleted.")
			END IF
		ELSE
			ZZ%=FNW%("(missing)")
		END IF
		ZZ%=FNW%(CR$)
	NEXT J%
	IF FNW%(CR$+"Transfer to which (2-4)? ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	X%=VAL(I$):IF X%<2 OR X%>4 THEN RETURN
	IF GM%(YOURGANG%,X%)<1 THEN RETURN
	SWAP GM%(YOURGANG%,YOURMEMBER%),GM%(YOURGANG%,X%)
	YOURMEMBER%=X%
	GOSUB SaveGang
	RETURN

SaveGang:
	IF FNW%(CR$+"Saving...") THEN RETURN Hungup
	OPEN "O",#4,"GANG.DAT"
	FOR I%=1 TO 24
		WRITE #4,GANG$(I%),GM%(I%,1),GM%(I%,2),GM%(I%,3),GM%(I%,4)
	NEXT I%
	CLOSE #4
	IF FNW%("Ok."+CR$) THEN RETURN Hungup
	RETURN
