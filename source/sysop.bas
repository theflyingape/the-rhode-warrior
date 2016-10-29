	'$INCLUDE: 'COMMON.BAS'

	DIM GM%(24,4)

Top:
	CLS 2:IF LOCL%=0 THEN PRINT #1,CHR$(27);"[H";CHR$(27);"[J";
	IF FNW%(SPACE$(20)+"--=:)) Sysop Menu ((:=--"+CR$+CR$) THEN GOTO Hungup
	IF FNW%("Edit user number (1-96), <N>ew user report, <R>eroll, <S>wap: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$)
	IF I$="N" THEN GOSUB Newuser:GOTO Top
	IF I$="R" AND USER%=1 THEN GOSUB ReRoll:CHAIN "MAIN"
	IF I$="S" THEN
		ZZ%=FNW%("OLD user: ")
		OLD%=FNUSER%:IF OLD%=0 THEN GOTO Top
		GET #2,OLD%:OLD$=USER.REC$+""
		ZZ%=FNW%("NEW user: ")
		NEW%=FNUSER%:IF NEW%=0 THEN GOTO Top
		GET #2,NEW%:NEW$=USER.REC$+""
		LSET USER.REC$=NEW$:PUT #2,OLD%
		LSET USER.REC$=OLD$:PUT #2,NEW%
		OPEN "A",#4,"NOTES\ARENA"+MID$(STR$(REC%),2)+".MSG":CLOSE #4
		KILL "ARENA"+MID$(STR$(REC%),2)+".MSG"
		OPEN "A",#4,"EMAIL\EMAIL"+MID$(STR$(REC%),2)+".MSG":CLOSE #4
		KILL "EMAIL\EMAIL"+MID$(STR$(REC%),2)+".MSG"
		GOSUB ReadGang
		A%=OLD%:B%=NEW
		FOR I%=1 TO 24:FOR J%=1 TO 4
			IF GM%(I%,J%)=A% THEN GM%(I%,J%)=NEW%:A%=32767
			IF GM%(I%,J%)=B% THEN GM%(I%,J%)=OLD%:B%=32767
		NEXT J%:NEXT I%
		GOSUB SaveGang
		GOTO Top
	END IF
	REC%=VAL(I$)
	IF REC%<1 OR REC%>96 THEN CHAIN "MAIN"
	GET #2,REC%
	IF ASC(USER.ACCESS$)=0 THEN GOTO Top
Edit1:
	IF FNW%(CR$+CR$) THEN GOTO Hungup
	IF FNW%("<1> Character's name: "+USER.HANDLE$+CR$) THEN GOTO Hungup
	IF FNW%("<2> User's real name: "+USER.NAME$+CR$) THEN GOTO Hungup
	IF FNW%("<3> City name.......: "+USER.CITY$+CR$) THEN GOTO Hungup
	IF FNW%("<4> State name......: "+USER.STATE$+CR$) THEN GOTO Hungup
	I$=MID$(STR$(CVD(USER.PHONE$)),2)
	IF FNW%("<5> Phone number....: "+LEFT$(I$,3)+"-"+MID$(I$,4,3)+"-"+RIGHT$(I$,4)+CR$) THEN GOTO Hungup
	IF FNW%("<6> Access level....: "+MID$(STR$(ASC(USER.ACCESS$)),2)+" - "+LEVEL$(ASC(USER.ACCESS$))+CR$+CR$) THEN GOTO Hungup	
	IF FNW%("Total calls made....:"+STR$(CVI(USER.TCALL$))+"     "+FND$(USER.LDATE$)+"  "+FNT$(USER.LTIME$)+CR$+CR$) THEN GOTO Hungup
	I$="Character class.....: "+CLASS$(ASC(USER.CLASS$))+"   Lvl:"+STR$(ASC(USER.LEVEL$))
	IF FNW%(I$+"   Str:"+STR$(ASC(USER.STR$))+"   Int:"+STR$(ASC(USER.INT$))+"   Agl:"+STR$(ASC(USER.AGL$))+"   Chr:"+STR$(ASC(USER.CHR$))+CR$) THEN GOTO Hungup
	IF FNW%("Gang member.........:"+STR$(ASC(USER.GANG$))+" - "+GANG$(ASC(USER.GANG$))+CR$) THEN GOTO Hungup
	M!=CVD(USER.BANK$)
	IF FNW%("Gold in bank/on loan:"+STR$(M!)) THEN GOTO Hungup
	M!=CVD(USER.LOAN$)
	IF FNW%(" /"+STR$(M!)+CR$) THEN GOTO Hungup
	IF FNW%("Jousts Won/Lost.....:"+STR$(CVI(USER.JW$))+" /"+STR$(CVI(USER.JL$))+CR$) THEN GOTO Hungup
	IF FNW%("Real Estate/Security:"+STR$(ASC(USER.RE$))+" - "+RE$(ASC(USER.RE$))+" /"+STR$(ASC(USER.SECURITY$))+" - "+SECURITY$(ASC(USER.SECURITY$))+CR$) THEN GOTO Hungup
	IF FNW%("Warship hull size...:"+STR$(CVI(USER.HULL$))) THEN GOTO Hungup
	IF FNW%("     Cannons:"+STR$(ASC(USER.CANNON$))) THEN GOTO Hungup
	IF FNW%("     Ram: "+USER.RAM$+CR$) THEN GOTO Hungup
	IF FNW%("Weapon/Armor........:"+STR$(ASC(USER.WEAPON$))+" - "+WEAPON$(ASC(USER.WEAPON$))+" /"+STR$(ASC(USER.ARMOR$))+" - "+ARMOR$(ASC(USER.ARMOR$))+CR$+CR$) THEN GOTO Hungup
	IF FNW%("Enter field to edit (1-6), <C>hange password, <D>elete: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	ZZ%=FNW%(CR$+CR$)
	IF REC%=1 AND USER%<>1 THEN GOTO Top
	IF UCASE$(I$)="C" THEN
		IF FNW%("Enter new password: ") THEN GOTO Hungup
		NOECHO%=1:IF FNR% THEN GOTO Hungup
		NOECHO%=0:IF LEN(I$)<2 OR LEN(I$)>LEN(USER.PASSWORD$) THEN ZZ%=FNW%(BELL$):GOTO Top
		LSET USER.PASSWORD$=UCASE$(I$)+STRING$(LEN(USER.PASSWORD$),0)
		PUT #2,REC%
		GOTO Top
	END IF
	IF UCASE$(I$)="D" AND REC%>1 THEN
		LSET USER.REC$=STRING$(LEN(USER.REC$),0)
		PUT #2,REC%
		CLOSE #4
		OPEN "A",#4,"NOTES\ARENA"+MID$(STR$(REC%),2)+".MSG":CLOSE #4
		KILL "NOTES\ARENA"+MID$(STR$(REC%),2)+".MSG"
		OPEN "A",#4,"EMAIL\EMAIL"+MID$(STR$(REC%),2)+".MSG":CLOSE #4
		KILL "EMAIL\EMAIL"+MID$(STR$(REC%),2)+".MSG"
		IF FNW%("Deleted!"+CR$) THEN GOTO Hungup
		GOTO Top
	END IF
	F%=VAL(I$)
	IF F% THEN ZZ%=FNW%("Enter new "):ON F% GOSUB Handle,Username,City,State,Phone,UserAccess:GOTO Edit1
	PUT #2,REC%
	GOTO Top
Handle:
	IF FNW%("Character's handle: ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	IF LEN(I$)<2 OR LEN(I$)>LEN(USER.HANDLE$) THEN GOTO Handle
	LSET USER.HANDLE$=I$+STRING$(LEN(USER.HANDLE$),0)
	RETURN
Username:
	IF FNW%("User's real name: ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	IF LEN(I$)<5 OR LEN(I$)>LEN(USER.NAME$) THEN GOTO Username
	LSET USER.NAME$=I$+STRING$(LEN(USER.NAME$),0)
	RETURN
City:
	IF FNW%("City: ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	IF LEN(I$)<2 OR LEN(I$)>LEN(USER.CITY$) THEN GOTO City
	LSET USER.CITY$=I$+STRING$(LEN(USER.CITY$),0)
	RETURN
State:
	IF FNW%("State: ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	IF LEN(I$)<>LEN(USER.STATE$) THEN GOTO State
	LSET USER.STATE$=UCASE$(I$)
	RETURN
Phone:
	IF FNW%("Phone number: ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	IF LEN(I$)<>12 THEN GOTO Phone
	I$=LEFT$(I$,3)+MID$(I$,5,3)+RIGHT$(I$,4)
	D#=INT(VAL(I$)):IF D#<1011010000 THEN GOTO Phone
	LSET USER.PHONE$=MKD$(D#)
	RETURN
UserAccess:
	IF FNW%("Access level (1-10): ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	I%=VAL(I$):IF I%<1 OR I%>10 THEN RETURN
	IF I%=10 AND USER%<>1 THEN RETURN
	LSET USER.ACCESS$=CHR$(I%)
	RETURN

ReRoll:
	IF FNW%(CR$+"Rerolling") THEN GOTO Hungup
	FOR REC%=1 TO 96
		IF (REC% MOD 10)=0 THEN ZZ%=FNW%(".")
		GET #2,REC%
		LSET USER.CALLS$=CHR$(0)
		LSET USER.STATUS$=CHR$(0)
		LSET USER.CLASS$=CHR$(0)
		LSET USER.LEVEL$=CHR$(1)
		LSET USER.HP$=MKI$(15)
		LSET USER.SP$=MKI$(0)
		LSET USER.JW$=MKI$(0)
		LSET USER.JL$=MKI$(0)
		LSET USER.EXP$=MKD$(0)
		LSET USER.STR$=CHR$(50)
		LSET USER.INT$=CHR$(50)
		LSET USER.AGL$=CHR$(50)
		LSET USER.CHR$=CHR$(50)
		LSET USER.WEAPON$=CHR$(1)
		LSET USER.ARMOR$=CHR$(1)
		LSET USER.SPELL$=MKI$(0)
		LSET USER.POISON$=MKI$(0)
		LSET USER.RE$=CHR$(0)
		LSET USER.SECURITY$=CHR$(0)
		LSET USER.GOLD$=MKD$(1)
		LSET USER.BANK$=MKD$(5)
		LSET USER.LOAN$=MKD$(0)
		LSET USER.HULL$=MKI$(0)
		LSET USER.CANNON$=CHR$(0)
		LSET USER.RAM$="N"
		LSET USER.BLESSED$=CHR$(0)
		LSET USER.CURSED$=CHR$(0)
		PUT #2,REC%
	NEXT REC%
	ZZ%=FNW%("Ok."+CR$)

	ZZ%=FNW%("Blessing...")
	DO:REC%=FND(96):GET #2,REC%:LOOP WHILE ASC(USER.ACCESS$)<4
	LSET USER.BLESSED$=CHR$(1)
	PUT #2,REC%

	ZZ%=FNW%("Cursing...")
	DO:REC%=FND(96):GET #2,REC%:LOOP WHILE ASC(USER.ACCESS$)<4 OR ASC(USER.BLESSED$)<>0
	LSET USER.CURSED$=CHR$(1)
	PUT #2,REC%

	ZZ%=FNW%("Done!"+CR$)
	RETURN

Newuser:
	CLS 2:IF LOCL%=0 THEN PRINT #1,CHR$(27);"[H";CHR$(27);"[J";
	IF FNW%("New User Listing:"+CR$+CR$) THEN RETURN Hungup
	FOR REC%=1 TO 96
		GET #2,REC%
		IF ASC(USER.ACCESS$)>0 AND ASC(USER.ACCESS$)<4 OR ASC(USER.BLESSED$) OR ASC(USER.CURSED$) THEN
			I$=STR$(CVD(USER.PHONE$))
			I$=MID$(I$,2,3)+"-"+MID$(I$,5,3)+"-"+RIGHT$(I$,4)
			IF ASC(USER.BLESSED$) THEN I$=I$+" (blessed)"
			IF ASC(USER.CURSED$) THEN I$=I$+" (cursed)"
			ZZ%=FNW%(STR$(REC%)+".  "+FNL$(USER.NAME$)+", "+FNL$(USER.CITY$)+", "+FNL$(USER.STATE$)+", "+I$+CR$)
		END IF
	NEXT REC%
	ZZ%=FNW%(CR$+"Press RETURN: "):ZZ%=FNR%
	RETURN

ReadGang:
	OPEN "I",#4,"GANG.DAT"
	FOR I%=1 TO 24
		INPUT #4,GANG$(I%),GM%(I%,1),GM%(I%,2),GM%(I%,3),GM%(I%,4)
	NEXT I%
	CLOSE #4
	RETURN

SaveGang:
	OPEN "O",#4,"GANG.DAT"
	FOR I%=1 TO 24
		WRITE #4,GANG$(I%),GM%(I%,1),GM%(I%,2),GM%(I%,3),GM%(I%,4)
	NEXT I%
	CLOSE #4
	RETURN
