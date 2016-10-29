	'$INCLUDE: 'COMMON.BAS'

	DIM MWR%(21),EX#(21)

Top:
	GET #2,USER%
	IF ASC(USER.CLASS$)=0 AND ASC(USER.ACCESS$)<>3 THEN
		ZZ%=FNW%("You have been rerolled."+CR$)
		HERO%=0:GOSUB ReRoll
	END IF

CheckExp:
	IF CVD(USER.EXP$)>(2^(ASC(USER.LEVEL$)-1)*(1100-2*ASC(USER.INT$))) THEN
		LSET USER.LEVEL$=CHR$(ASC(USER.LEVEL$)+1)
		IF FNW%(CR$+"-=>*<=-"+CR$+CR$) THEN GOTO Hungup
		IF FNW%("Level ="+STR$(ASC(USER.LEVEL$))+CR$+CR$) THEN GOTO Hungup
		IF ASC(USER.LEVEL$)>99 THEN
			LSET USER.IMMORTAL$=MKI$(CVI(USER.IMMORTAL$)+1)
			PUT #2,USER%
			IF FNW%("You have become so powerful that you are now a god.  You must get a new mortal") THEN GOTO Hungup
			IF FNW%("character.  Press return to start over in this world: ") THEN GOTO Hungup
			IF FNR% THEN GOTO Hungup
			IF ASC(USER.CLASS$)<>11 THEN
				IF FNW%(CR$+CR$) THEN GOTO Hungup
				IF FNW%("Ol' Mighty One!  Solve the Ancient Riddle of the Keys and be given the"+CR$) THEN GOTO Hungup
				IF FNW%("chance to become a Hero.  You have a Gold, Silver, and Copper key that"+CR$) THEN GOTO Hungup
				IF FNW%("must be inserted in the proper order to unleash the Power of the Hero."+CR$) THEN GOTO Hungup
				HERO%=1:I%=FND(3):K$=CHR$(48+I%)
				DO:J%=FND(3):LOOP WHILE J%=I%:K$=K$+CHR$(48+J%)
				IF K$="12" OR K$="21" THEN K$=K$+"3"
				IF K$="13" OR K$="31" THEN K$=K$+"2"
				IF K$="23" OR K$="32" THEN K$=K$+"1"
				L$="G,S,C"
				FOR X%=1 TO 3
					DO
						IF FNW%(CR$+"Insert key #"+MID$(STR$(X%),2)+" ("+L$+") > ") THEN GOTO Hungup
						IF FNR% THEN GOTO Hungup
						I$=UCASE$(I$):F%=-(I$="G")-3*(I$="S")-5*(I$="C")
						IF MID$(L$,F%,1)="+" THEN F%=0
					LOOP WHILE F%=0
					L$=LEFT$(L$,F%-1)+"+"+MID$(L$,F%+1):F%=INT(F%/2+.5)
					IF FNW%("...you insert and turn the key...") THEN GOTO Hungup
					SLEEP 2
					IF F%<>VAL(MID$(K$,X%,1)) THEN
						ZZ%=FNW%("Boooom!  You die."+CR$+"The correct sequence was: ")
						FOR I%=1 TO 3
							ZZ%=FNW%(MID$("GSC",VAL(MID$(K$,I%,1)),1))
							IF I%<3 THEN ZZ%=FNW%(", ")
						NEXT I%
						HERO%=0:EXIT FOR
					END IF
					IF FNW%("click!"+CR$) THEN GOTO Hungup
				NEXT X%
			END IF
			GOSUB ReRoll
			GOTO Top
		END IF
		I%=ASC(USER.STR$)/10+FND(ASC(USER.LEVEL$))+ASC(USER.LEVEL$)
		IF FNW%("Hit points = Hit points +"+STR$(I%)+CR$+CR$) THEN GOTO Hungup
		LSET USER.HP$=MKI$(CVI(USER.HP$)+I%)
		A%=ASC(USER.CLASS$)
		IF A%=2 OR A%=4 OR A%=5 OR A%=7 OR A%=9 OR A%=11 THEN
			I%=ASC(USER.INT$)/10+FND(ASC(USER.LEVEL$))+ASC(USER.LEVEL$)
			IF FNW%("Spell power = Spell power +"+STR$(I%)+CR$+CR$) THEN GOTO Hungup
			LSET USER.SP$=MKI$(CVI(USER.SP$)+I%)
		END IF
		I%=1-(A%=1 OR A%=8 OR A%=11):IF ASC(USER.STR$)+I%>99 THEN I%=99-ASC(USER.STR$)
		IF I% THEN LSET USER.STR$=CHR$(ASC(USER.STR$)+I%):IF FNW%("Stamina = Stamina +"+STR$(I%)+CR$+CR$) THEN GOTO Hungup
		I%=1-(A%=2 OR A%=9 OR A%=10 OR A%=11):IF ASC(USER.INT$)+I%>99 THEN I%=99-ASC(USER.INT$)
		IF I% THEN LSET USER.INT$=CHR$(ASC(USER.INT$)+I%):IF FNW%("Intellect = Intellect +"+STR$(I%)+CR$+CR$) THEN GOTO Hungup
		I%=1-(A%=3 OR A%=6 OR A%=11):IF ASC(USER.AGL$)+I%>99 THEN I%=99-ASC(USER.AGL$)
		IF I% THEN LSET USER.AGL$=CHR$(ASC(USER.AGL$)+I%):IF FNW%("Agility = Agility +"+STR$(I%)+CR$+CR$) THEN GOTO Hungup
		I%=1-(A%=4 OR A%=5 OR A%=7 OR A%=11):IF ASC(USER.CHR$)+I%>99 THEN I%=99-ASC(USER.CHR$)
		IF I% THEN LSET USER.CHR$=CHR$(ASC(USER.CHR$)+I%):IF FNW%("Charisma = Charisma +"+STR$(I%)+CR$+CR$) THEN GOTO Hungup
		PUT #2,USER%:SLEEP 1
		GOSUB GetStats
		GOTO CheckExp
	END IF

	IF FNT% THEN GOTO Expired
	IF FNW%(CR$) THEN GOTO Hungup
	IF FNW%(MAINMSG$+CR$+CR$) THEN GOTO Hungup
	IF FNW%("Time left:"+STR$(LOGOFF%-T%)+" min."+CR$) THEN GOTO Hungup
	IF FNW%("[Main] Command (?=Menu): ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$)
	IF I$="?" THEN
		CLS 2:IF LOCL%=0 THEN PRINT #1,CHR$(27);"[H";CHR$(27);"[J";
		FILE$="MAINMENU.TXT":GOSUB PrintFile
		GOTO Top
	END IF
	IF I$="@" AND (ASC(USER.ACCESS$)>8 OR USER%=1) THEN CHAIN "SYSOP"
	IF I$="A" AND ASC(USER.ACCESS$)<>3 THEN CHAIN "ARENA"
	IF I$="B" THEN FILE$="POST.TXT":GOSUB PrintFile:GOTO Top
	IF I$="C" THEN
		LOCATE 25,72:COLOR 0,7:PRINT " ³ CHAT ";:LOCATE 24,1:COLOR 7,0
		IF FNW%("Paging the sysop...") THEN GOTO Hungup
		FOR I%=1 TO 2:PLAY "CDEFGAG":SLEEP 1:NEXT I%
		IF FNW%("Ok.  The sysop may come online later."+CR$) THEN GOTO Hungup
		GOTO Top
	END IF
	IF I$="D" AND ASC(USER.ACCESS$)>3 THEN CHAIN "DUNGEON"
	IF I$="E" THEN CHAIN "EMAIL"
	IF I$="G" AND ASC(USER.ACCESS$)<>3 THEN CHAIN "CASINO"
	IF I$="H" THEN FILE$="HINTS.TXT":GOSUB PrintFile:GOTO Top
	IF I$="I" THEN
		IF FNW%(CR$+"20 top Immortals."+CR$+CR$) THEN GOTO Hungup
		IF FNW%("Num           Username           Immortal  Access"+CR$) THEN GOTO Hungup
		IF FNW%("---  --------------------------  --------  ------"+CR$) THEN GOTO Hungup
		M%=0:FOR I%=0 TO 21:MWR%(I%)=0:EX#(I%)=-1:NEXT I%
		FOR REC%=1 TO 96
			GET #2,REC%
			IF ASC(USER.ACCESS$)>0 THEN
				J%=M%+1:MWR%(J%)=REC%:EX#(J%)=CVI(USER.IMMORTAL$)
				WHILE EX#(J%)>EX#(J%-1) AND J%>1
					SWAP EX#(J%),EX#(J%-1)
					SWAP MWR%(J%),MWR%(J%-1)
					J%=J%-1
				WEND
				IF M%<20 THEN M%=M%+1
			END IF
		NEXT REC%
		FOR I%=1 TO M%
			IF MWR%(I%) THEN
				GET #2,MWR%(I%)
				IF FNW%(RIGHT$(" "+STR$(MWR%(I%)),2)+".  ") THEN GOTO Hungup
				IF FNW%(LEFT$(FNL$(USER.HANDLE$)+SPACE$(26),26)+"  ") THEN GOTO Hungup
				IF FNW%(RIGHT$("     "+STR$(CVI(USER.IMMORTAL$)),6)+"    ") THEN GOTO Hungup
				IF FNW%(LEVEL$(ASC(USER.ACCESS$))+CR$) THEN GOTO Hungup
			END IF
		NEXT I%
		GOTO Top
	END IF
	IF I$="J" THEN
		IF FNW%(CR$+"20 top Joust Champions, sorted by wins."+CR$+CR$) THEN GOTO Hungup
		IF FNW%("Num           Username           Won/Lost  Ratio  Access"+CR$) THEN GOTO Hungup
		IF FNW%("---  --------------------------  --------  -----  ------"+CR$) THEN GOTO Hungup
		M%=0:FOR I%=0 TO 21:MWR%(I%)=0:EX#(I%)=-1:NEXT I%
		FOR REC%=1 TO 96
			GET #2,REC%
			IF ASC(USER.ACCESS$)>0 THEN
				J%=M%+1:MWR%(J%)=REC%:EX#(J%)=CVI(USER.JW$)
				WHILE EX#(J%)>EX#(J%-1) AND J%>1
					SWAP EX#(J%),EX#(J%-1)
					SWAP MWR%(J%),MWR%(J%-1)
					J%=J%-1
				WEND
				IF M%<20 THEN M%=M%+1
			END IF
		NEXT REC%
		FOR I%=1 TO M%
			IF MWR%(I%) THEN
				GET #2,MWR%(I%)
				IF FNW%(RIGHT$(" "+STR$(MWR%(I%)),2)+".  ") THEN GOTO Hungup
				IF FNW%(LEFT$(FNL$(USER.HANDLE$)+SPACE$(26),26)+"  ") THEN GOTO Hungup
				IF CVI(USER.JW$)+CVI(USER.JL$) THEN R%=100*CVI(USER.JW$)/(CVI(USER.JW$)+CVI(USER.JL$)) ELSE R%=0
				IF FNW%(RIGHT$("  "+STR$(CVI(USER.JW$)),3)+"/"+LEFT$(MID$(STR$(CVI(USER.JL$)),2)+SPACE$(6),6)+RIGHT$("   "+STR$(R%),4)+"%") THEN GOTO Hungup
				IF FNW%("  "+LEVEL$(ASC(USER.ACCESS$))+CR$) THEN GOTO Hungup
			END IF
		NEXT I%
		GOTO Top
	END IF
	IF I$="K" THEN
		FILE$="LAST50.TXT":GOSUB PrintFile
		GOTO Top
	END IF
	IF I$="L" THEN
		X%=ASC(USER.LEVEL$)-3:IF X%<1 THEN X%=1
		Y%=ASC(USER.LEVEL$)+3:IF Y%>99 THEN Y%=99
		IF FNW%(CR$+"Starting level <"+MID$(STR$(X%),2)+">: ") THEN GOTO Hungup
		IF FNR% THEN GOTO Hungup
		I%=VAL(I$):IF I%>0 AND I%<100 THEN X%=I%
		IF FNW%(CR$+"  Ending level <"+MID$(STR$(Y%),2)+">: ") THEN GOTO Hungup
		IF FNR% THEN GOTO Hungup
		I%=VAL(I$):IF I%>=X% AND I%<100 THEN Y%=I%
		IF FNW%(CR$+CR$) THEN GOTO Hungup
		IF FNW%("Num           Username             Class   Lvl  Status  Last Call On  Access"+CR$) THEN GOTO Hungup
		IF FNW%("---  --------------------------  ---------  --  ------  ------------  ------"+CR$) THEN GOTO Hungup
		FOR REC%=1 TO 96
			GET #2,REC%
			IF ASC(USER.ACCESS$)>0 AND ASC(USER.LEVEL$)>=X% AND ASC(USER.LEVEL$)<=Y% THEN
				IF FNW%(RIGHT$(" "+STR$(REC%),2)+".  ") THEN GOTO Hungup
				IF FNW%(LEFT$(FNL$(USER.HANDLE$)+SPACE$(26),26)+"  ") THEN GOTO Hungup
				IF FNW%(LEFT$(CLASS$(ASC(USER.CLASS$))+SPACE$(9),9)+"  ") THEN GOTO Hungup
				IF FNW%(RIGHT$(" "+STR$(ASC(USER.LEVEL$)),2)+"  ") THEN GOTO Hungup
				IF ASC(USER.STATUS$) THEN ZZ%=FNW%(" Dead ") ELSE ZZ%=FNW%("Alive!")
				IF FNW%("  "+LEFT$(FND$(USER.LDATE$)+" ",12)+"  ") THEN GOTO Hungup
				IF FNW%(LEVEL$(ASC(USER.ACCESS$))+CR$) THEN GOTO Hungup
			END IF
		NEXT REC%
		GOTO Top
	END IF
	IF I$="M" THEN
		IF FNW%(CR$+"20 top users, sorted by experience."+CR$+CR$) THEN GOTO Hungup
		IF FNW%("Num           Username             Class   Lvl  Status         Party"+CR$) THEN GOTO Hungup
		IF FNW%("---  --------------------------  ---------  --  ------  --------------------"+CR$) THEN GOTO Hungup
		M%=0:FOR I%=0 TO 21:MWR%(I%)=0:EX#(I%)=-1:NEXT I%
		FOR REC%=1 TO 96
			GET #2,REC%
			IF ASC(USER.ACCESS$)>0 THEN
				J%=M%+1:MWR%(J%)=REC%:EX#(J%)=CVD(USER.EXP$)
				WHILE EX#(J%)>EX#(J%-1) AND J%>1
					SWAP EX#(J%),EX#(J%-1)
					SWAP MWR%(J%),MWR%(J%-1)
					J%=J%-1
				WEND
				IF M%<20 THEN M%=M%+1
			END IF
		NEXT REC%
		FOR I%=1 TO M%
			IF MWR%(I%) THEN
				GET #2,MWR%(I%)
				IF FNW%(RIGHT$(" "+STR$(MWR%(I%)),2)+".  ") THEN GOTO Hungup
				IF FNW%(LEFT$(FNL$(USER.HANDLE$)+SPACE$(26),26)+"  ") THEN GOTO Hungup
				IF FNW%(LEFT$(CLASS$(ASC(USER.CLASS$))+SPACE$(9),9)+"  ") THEN GOTO Hungup
				IF FNW%(RIGHT$(" "+STR$(ASC(USER.LEVEL$)),2)+"  ") THEN GOTO Hungup
				IF ASC(USER.STATUS$) THEN ZZ%=FNW%(" Dead ") ELSE ZZ%=FNW%("Alive!")
				IF FNW%("  "+LEFT$(GANG$(ASC(USER.GANG$))+SPACE$(20),20)+CR$) THEN GOTO Hungup
			END IF
		NEXT I%
		GOTO Top
	END IF
	IF I$="N" AND ASC(USER.ACCESS$)<>3 THEN CHAIN "NAVAL"
	IF I$="P" AND ASC(USER.ACCESS$)<>3 THEN CHAIN "GANGWARS"
	IF I$="R" AND ASC(USER.ACCESS$)>3 THEN
		ZZ%=FNW%(CR$+"It is a hot, moonless night."+CR$)
		ZZ%=FNW%("A city guard walks down another street."+CR$+CR$)
		U#=INT((WC(ASC(USER.WEAPON$))+AC(ASC(USER.ARMOR$)))*TCHR%/100+CVD(USER.GOLD$)+CVD(USER.BANK$)-CVD(USER.LOAN$))
		CL%=ASC(USER.CLASS$):LVL%=ASC(USER.LEVEL$)
		IF FNW%("Who are you going to rob? ") THEN GOTO Hungup
		REC%=FNUSER%:IF REC%=0 THEN GOTO Top
		GET #2,REC%
		IF FNW%("You case "+FNL$(USER.HANDLE$)+"'s joint out."+CR$) THEN GOTO Hungup
		W%=ASC(USER.WEAPON$)*4/5:A%=ASC(USER.ARMOR$)*4/5
		D#=INT((WC(W%)+AC(A%))*TCHR%/100+CVD(USER.GOLD$))
		IF D#<INT(U#/5) OR LVL%-ASC(USER.LEVEL$)>3 THEN ZZ%=FNW%("You decide it is not worth the effort."+CR$):GOTO Top
		V%=ASC(USER.SECURITY$):Y%=ASC(USER.RE$):WALL%=5*(V%+1)+Y%+INT((ASC(USER.LEVEL$)-LVL%)/5)
		IF FNW%("The goods are in a "+RE$(Y%)+" protected by a "+SECURITY$(V%)+"."+CR$) THEN GOTO Hungup
		IF FNW%("Attempt to steal (Y/N)? ") THEN GOTO Hungup
		IF FNR% THEN GOTO Hungup
		ZZ%=FNW%(CR$)
		IF UCASE$(I$)<>"Y" THEN GOTO Top
		IF FNW%("You slide into the shadows and make your attempt...") THEN GOTO Hungup
		SLEEP 2
		ZZ%=FNW%(CR$)
		SKILL%=0:FOR I%=1 TO 1-(CL%=4 OR CL%=7)-2*(CL%=3):SKILL%=SKILL%+FND(LVL%+1):NEXT I%
		IF SKILL%<=WALL% THEN GOTO Caught
		LSET USER.WEAPON$=CHR$(W%)
		LSET USER.ARMOR$=CHR$(A%)
		LSET USER.GOLD$=MKD$(0)
		PUT #2,REC%
		GET #2,USER%
		LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+D#)
		PUT #2,USER%
		M!=D#
		IF FNW%(CR$+"You make it out with"+STR$(M!)+" gold pieces worth of stuff!"+CR$) THEN GOTO Hungup
		GOTO Top
	END IF
	IF I$="S" AND ASC(USER.ACCESS$)<>3 THEN CHAIN "SQUARE"
	IF I$="T" AND ASC(USER.ACCESS$)<>3 THEN CHAIN "TAVERN"
	IF I$="U" THEN
		IF FNW%("Change your password (Y/N)? ") THEN GOTO Hungup
		IF FNR% THEN GOTO Hungup
		IF UCASE$(I$)<>"Y" THEN GOTO Top
		IF FNW%(CR$+"Enter new password: ") THEN GOTO Top
		NOECHO%=1:IF FNR% THEN GOTO Hungup
		NOECHO%=0:IF LEN(I$)=0 THEN GOTO Top
		LSET USER.PASSWORD$=UCASE$(I$)+STRING$(LEN(USER.PASSWORD$),0)
		IF FNW%(CR$+"Enter password again to verify: ") THEN GOTO Top
		NOECHO%=1:IF FNR% THEN GOTO Hungup
		NOECHO%=0:IF UCASE$(I$)<>FNL$(USER.PASSWORD$) THEN GOTO Top
		PUT #2,USER%
		IF FNW%(CR$+"Saved new password."+CR$) THEN GOTO Hungup
	END IF
	IF I$="X" AND ASC(USER.ACCESS$)<>3 THEN
		IF FNW%("Are you sure you want to reroll (Y/N)? ") THEN GOTO Hungup
		IF FNR% THEN GOTO Hungup
		IF UCASE$(I$)="Y" THEN HERO%=0:GOSUB ReRoll
		GOTO Top
	END IF
	IF I$="Y" THEN
		IF FNW%("Usernumber:"+STR$(USER%)+CR$) THEN GOTO Hungup
		IF FNW%("Username: "+FNL$(USER.HANDLE$)+CR$) THEN GOTO Hungup
		IF FNW%("Access: "+LEVEL$(ASC(USER.ACCESS$))+CR$) THEN GOTO Hungup
		IF FNW%("Calls today:"+STR$(ASC(USER.CALLS$))+CR$) THEN GOTO Hungup
		IF FNW%("Total calls made:"+STR$(CVI(USER.TCALL$))+CR$) THEN GOTO Hungup
		IF FNW%("Class: "+CLASS$(ASC(USER.CLASS$))+CR$) THEN GOTO Hungup
		IF FNW%("Level:"+STR$(ASC(USER.LEVEL$))+CR$) THEN GOTO Hungup
		M!=CVD(USER.EXP$)
		IF FNW%("Exper:"+STR$(M!)+CR$) THEN GOTO Hungup
		M!=2^(ASC(USER.LEVEL$)-1)*(1100-ASC(USER.INT$)*2)
		IF FNW%("Need: "+STR$(M!)+CR$) THEN GOTO Hungup
		IF FNW%("Stamina:  "+STR$(TSTR%)+" ("+MID$(STR$(ASC(USER.STR$)),2)+")"+CR$) THEN GOTO Hungup
		IF FNW%("Intellect:"+STR$(TINT%)+" ("+MID$(STR$(ASC(USER.INT$)),2)+")"+CR$) THEN GOTO Hungup
		IF FNW%("Agility:  "+STR$(TAGL%)+" ("+MID$(STR$(ASC(USER.AGL$)),2)+")"+CR$) THEN GOTO Hungup
		IF FNW%("Charisma: "+STR$(TCHR%)+" ("+MID$(STR$(ASC(USER.CHR$)),2)+")"+CR$) THEN GOTO Hungup
		IF FNW%("Hit points:"+STR$(THP%)+" out of"+STR$(CVI(USER.HP$))+CR$) THEN GOTO Hungup
		IF TSP% THEN IF FNW%("Spell points:"+STR$(TSP%)+" out of"+STR$(CVI(USER.SP$))+CR$) THEN GOTO Hungup
		IF FNW%(CR$+"Press RETURN: ") THEN GOTO Hungup
		IF FNR% THEN GOTO Hungup
		IF FNW%(CR$+CR$) THEN GOTO Hungup
		IF FNW%("Weapon: "+WEAPON$(ASC(USER.WEAPON$))) THEN GOTO Hungup
		A$=""
		IF THIT%>0 THEN A$="(+"+MID$(STR$(THIT%),2)+")"
		IF THIT%<0 THEN A$="("+STR$(THIT%)+")"
		IF FNW%(" "+A$+CR$) THEN GOTO Hungup
		IF FNW%("Armor:  "+ARMOR$(ASC(USER.ARMOR$))) THEN GOTO Hungup
		A$=""
		IF TSHIELD%>0 THEN A$="(+"+MID$(STR$(TSHIELD%),2)+")"
		IF TSHIELD%<0 THEN A$="("+STR$(TSHIELD%)+")"
		IF FNW%(" "+A$+CR$) THEN GOTO Hungup
		IF TSP% THEN
			IF FNW%("Spells: ") THEN GOTO Hungup
			V%=CVI(USER.SPELL$):IF V%=0 THEN ZZ%=FNW%("None")
			FOR I%=0 TO 11
				IF (V% AND 2^I%) THEN ZZ%=FNW%(SPELL$(I%+1)+CR$+SPACE$(8))
			NEXT I%
			ZZ%=FNW%(CR$)
		END IF
		I%=ASC(USER.CLASS$)
		IF I%=3 OR I%=6 OR I%=10 THEN
			IF FNW%("Poison: ") THEN GOTO Hungup
			V%=CVI(USER.POISON$):IF V%=0 THEN ZZ%=FNW%("None")
			FOR I%=0 TO 11
				IF (V% AND 2^I%) THEN ZZ%=FNW%("Type"+STR$(I%+1)+CR$+SPACE$(8))
			NEXT I%
			ZZ%=FNW%(CR$)
		END IF
		IF FNW%("Lives in: "+RE$(ASC(USER.RE$))+CR$) THEN GOTO Hungup
		IF FNW%("Security: "+SECURITY$(ASC(USER.SECURITY$))+CR$) THEN GOTO Hungup
		M!=CVD(USER.GOLD$):IF FNW%("Gold in hand:"+STR$(M!)+CR$) THEN GOTO Hungup
		M!=CVD(USER.BANK$):IF FNW%("Gold in bank:"+STR$(M!)+CR$) THEN GOTO Hungup
		M!=CVD(USER.LOAN$):IF FNW%("Gold on loan:"+STR$(M!)+CR$) THEN GOTO Hungup
		IF FNW%("Party: "+GANG$(ASC(USER.GANG$))+CR$) THEN GOTO Hungup
		IF ASC(USER.BLESSED$) THEN ZZ%=FNW%("You were blessed by User #"+MID$(STR$(ASC(USER.BLESSED$)),2)+CR$)
		IF ASC(USER.CURSED$) THEN ZZ%=FNW%("You were cursed by User #"+MID$(STR$(ASC(USER.CURSED$)),2)+CR$)
		GOTO Top
	END IF
	IF I$="Z" AND ASC(USER.ACCESS$)<>3 THEN
		IF FNW%("The Round Table was written in QuickBASIC for an IBM 386sx with 1meg RAM and"+CR$) THEN GOTO Hungup
		IF FNW%("a 1.44meg floppy drive.  576k RAM is allocated for the RAM disks."+CR$+CR$) THEN GOTO Hungup
		IF ASC(USER.ACCESS$)<5 THEN GOTO Top
		IF FNW%("Change border message (Y/N)? ") THEN GOTO Hungup
		IF FNR% THEN GOTO Hungup
		IF UCASE$(I$)<>"Y" THEN GOTO Top
		IF FNW%(CR$+"Enter new border message:"+CR$) THEN GOTO Hungup
		IF FNR% THEN GOTO Hungup
		IF LEN(I$) THEN MAINMSG$=I$
		GOTO Top
	END IF
	GOTO Top

ReRoll:
	IF FNW%("You must pick a class."+CR$+CR$) THEN GOTO Hungup
	FOR I%=1 TO 10+HERO%
		IF FNW%("<"+MID$(STR$(I%),2)+"> "+CLASS$(I%)+CR$) THEN GOTO Hungup
	NEXT I%
Class:
	IF FNW%(CR$+"Enter digit: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	A%=VAL(I$)
	IF A%<1 OR A%>(10+HERO%) THEN GOTO Class
	LSET USER.STATUS$=CHR$(0)
	LSET USER.CLASS$=CHR$(A%)
	LSET USER.LEVEL$=CHR$(1)
	LSET USER.EXP$=MKD$(0)
	LSET USER.HP$=MKI$(15)
	LSET USER.SP$=MKI$(-15*(A%=2 OR A%=4 OR A%=5 OR A%=7 OR A%=9 OR A%=11))
	LSET USER.JW$=MKI$(0)
	LSET USER.JL$=MKI$(0)
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
	THULL%=0
Ability:
	A%=200
	IF FNW%(CR$+"You have"+STR$(A%)+" ability points to distribute between 4 abilities: Stamina,"+CR$) THEN GOTO Hungup
	IF FNW%("Intellect, Agility, and  Charisma.  Each ability must be between 20 and 80."+CR$) THEN GOTO Hungup
Stamina:
	IF FNW%(CR$+CR$+"Enter your stamina: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	I%=VAL(I$)
	IF I%<20 OR I%>80 THEN GOTO Stamina
	LSET USER.STR$=CHR$(I%)
	A%=A%-I%
Intellect:
	IF FNW%(CR$+CR$+"Enter your intellect: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	I%=VAL(I$)
	IF I%<20 OR I%>80 THEN GOTO Intellect
	LSET USER.INT$=CHR$(I%)
	A%=A%-I%
Agility:
	IF FNW%(CR$+CR$+"Enter your agility: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	I%=VAL(I$)
	IF I%<20 OR I%>80 THEN GOTO Agility
	LSET USER.AGL$=CHR$(I%)
	A%=A%-I%
Charisma:
	IF FNW%(CR$+CR$+"You have"+STR$(200-ASC(USER.STR$)-ASC(USER.INT$)-ASC(USER.AGL$))+" points left.") THEN GOTO Hungup
	IF FNW%(CR$+CR$+"Enter your charisma: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	I%=VAL(I$)
	IF I%<20 OR I%>80 THEN GOTO Charisma
	LSET USER.CHR$=CHR$(I%)
	A%=A%-I%
	IF A%<>0 THEN ZZ%=FNW%(CR$+CR$+BELL$+"You did not assign 200 points!"+CR$):GOTO Ability
	PUT #2,USER%
	GOSUB GetStats
	RETURN

GetStats:
	I%=(ASC(USER.CURSED$)<>0)-(ASC(USER.BLESSED$)<>0)
	THP%=CVI(USER.HP$):TSP%=CVI(USER.SP$)
	TSTR%=ASC(USER.STR$)+10*I%:IF TSTR%>100 THEN TSTR%=100
	TAGL%=ASC(USER.AGL$)+10*I%:IF TAGL%>100 THEN TAGL%=100
	TINT%=ASC(USER.INT$)+10*I%:IF TINT%>100 THEN TINT%=100
	TCHR%=ASC(USER.CHR$)+10*I%:IF TCHR%>100 THEN TCHR%=100
	THIT%=10*I%*ASC(USER.WEAPON$)/100
	TSHIELD%=10*I%*ASC(USER.ARMOR$)/100
	RETURN
