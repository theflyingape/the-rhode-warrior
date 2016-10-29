	'$INCLUDE: 'COMMON.BAS'

	DEF FNPUNCH%(L%,S%)
		A%=INT((L%+S%/10)/2)+INT(RND(1)*(L%+S%/10)/2)
		IF A%<1 THEN A%=1
		FNPUNCH%=A%
	END DEF

	GET #2,USER%:LVL%=ASC(USER.LEVEL$)
	IF CVD(USER.EXP$)>(2^(LVL%-1)*(1100-2*ASC(USER.INT$))) THEN CHAIN "MAIN"
	IF FNW%(CR$) THEN GOTO Hungup
	IF FNW%("Welcome to Tiny's Tavern."+CR$+CR$) THEN GOTO Hungup
	IF FNW%("<E> Eavesdrop on the arguments"+CR$) THEN GOTO Hungup
	IF FNW%("<J> Jump into the arguments"+CR$) THEN GOTO Hungup
	IF FNW%("<G> Guzzle Beer"+CR$) THEN GOTO Hungup
	IF FNW%("<B> Brawl"+CR$+CR$) THEN GOTO Hungup
	IF FNT% THEN GOTO Expired
	IF FNW%("[Tavern] Option (Q=Quit): ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$)
	IF I$="Q" THEN CHAIN "MAIN"
	F%=-(I$="E")-2*(I$="J")-3*(I$="G")-4*(I$="B")
	ON F% GOSUB Eavesdrop,Jump,Guzzle,Brawl
	ZZ%=FNW%(CR$)
	GOTO Start

Eavesdrop:
	IF FNW%("You eavesdrop on the conversation."+CR$+CR$) THEN RETURN Hungup
	FILE$="TAVERN.TXT":GOSUB PrintFile
	ZZ%=FNW%("Press RETURN: "):IF FNR% THEN RETURN Hungup
	ZZ%=FNW%(CR$)
	RETURN

Jump:
	IF ASC(USER.ACCESS$)<5 THEN
		ZZ%=FNW%("The crowd gets ugly by your remarks, so you leave."+CR$)
		RETURN
	END IF
	GOSUB Message
	IF MSG%=0 THEN RETURN
	CLOSE #4:OPEN "I",#4,"TAVERN.TXT"
	WHILE (NOT EOF(4)) AND A$<>"***":LINE INPUT #4,A$:WEND
	IF NOT EOF(4) THEN LINE INPUT #4,A$
	CLOSE #5:OPEN "O",#5,"TAVERN.NEW"
	WHILE NOT EOF(4):LINE INPUT #4,A$:PRINT #5,A$:WEND
	PRINT #5,FNL$(USER.HANDLE$);" says,"
	FOR I%=1 TO MSG%
		IF MSG$(I%)<>"***" THEN PRINT #5,MSG$(I%)
	NEXT I%
	PRINT #5,"":PRINT #5,"***":PRINT #5,""
	CLOSE #4:CLOSE #5
	KILL "TAVERN.TXT"
	NAME "TAVERN.NEW" AS "TAVERN.TXT"
	RETURN

Guzzle:
	ZZ%=FNW%("Tiny, the 7'2"+CHR$(34)+" barkeeper, hands you a beer."+CR$+CR$)
	ZZ%=FNW%("How much do you pay? ")
	IF FNR% THEN RETURN Hungup
	D#=INT(ABS(VAL(I$))):ZZ%=FNW%(CR$+CR$)
	IF D#=0 THEN ZZ%=FNW%("Tiny pours the beer on you and kicks you out of his bar."+CR$):CHAIN "MAIN"
	IF D#>CVD(USER.GOLD$) THEN ZZ%=FNW%("You don't have that much!"+CR$+CR$):GOTO Guzzle
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-D#)
	PUT #2,USER%
	ZZ%=FNW%("He grunts and hands you your beer."+CR$)
	IF D#>15 THEN D#=FND(15)
	RESTORE Hints
	FOR I%=1 TO D#:READ A$:NEXT I%
	ZZ%=FNW%("He says, '"+A$+".'"+CR$)
	RETURN
Hints:
	DATA "More stamina will yield more hit points"
	DATA "More intellect will yield more spell power"
	DATA "You can sell things for better prices with higher charisma"
	DATA "You don't miss as often with higher agility"
	DATA "You can do more damage in battle with higher stamina"
	DATA "Spells don't fail as often with higher intellect"
	DATA "Higher agility yields higher jousting ability"
	DATA "Fishing can get better results with a lot of luck"
	DATA "Real estate and security help protect your investments"
	DATA "Entering large, invalid amounts will cause an Overflow error"
	DATA "Higher baud rates yield faster screen displays"
	DATA "Higher intellect calculates opponent's hit points more accurately"
	DATA "Backstabs swish more than you wish"
	DATA "Dungeon maps fall more into the hands of the lucky"
	DATA "I'll have more hints tomorrow"
Brawl:
	GET #2,USER%:LVL%=ASC(USER.LEVEL$)
	IF BRAWL%>2 THEN ZZ%=FNW%("You have run out of brawls."+CR$):RETURN
	IF ASC(USER.ACCESS$)<4 THEN ZZ%=FNW%("You are not allowed to brawl."+CR$):RETURN
	ZZ%=FNW%("Pick a fight with what user? ")
	NME%=FNUSER%:ZZ%=FNW%(CR$)
	IF NME%=0 THEN RETURN
	IF NME%=USER% THEN ZZ%=FNW%("You want to hit yourself?"+CR$+CR$):GOTO Brawl
	IF ASC(USER.ACCESS$)<4 THEN ZZ%=FNW%("That person does not brawl."+CR$):RETURN
	NME.NAME$=FNL$(USER.HANDLE$):NME.BP%=CVI(USER.HP$)/10:NME.LVL%=ASC(USER.LEVEL$):NME.STR%=ASC(USER.STR$):NME.AGL%=ASC(USER.AGL$)
	IF USER.SEX$="M" THEN HE$="He":HIM$="him" ELSE HE$="She":HIM$="her"
	IF NME.LVL%+3<LVL% THEN ZZ%=FNW%("You can only brawl someone higher or up to three levels below you."+CR$+CR$):GOTO Brawl
	GET #2,USER%:BP%=THP%/10
	ZZ%=FNW%(LEFT$("Name: "+NME.NAME$+SPACE$(32),32)+"You:"+CR$)
	ZZ%=FNW%(LEFT$("Level:"+STR$(NME.LVL%)+SPACE$(32),32)+STR$(LVL%)+CR$)
	ZZ%=FNW%(LEFT$("Knock out points:"+STR$(NME.BP%)+SPACE$(32),32)+STR$(BP%)+CR$+CR$)
	ZZ%=FNW%("Are you sure (Y/N)? ")
	IF FNR% THEN RETURN Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$)
	IF I$<>"Y" THEN GOTO Brawl
	BRAWL%=BRAWL%+1:REC%=0
	IF (TAGL%+FND(100))>(NME.AGL%+FND(100)) THEN ZZ%=FNW%("You get the first punch."+CR$+CR$):GOTO YOURturn
	ZZ%=FNW%(HE$+" gets the first punch."+CR$+CR$):GOTO NMEturn
Bloop:
	ZZ%=FNW%("<P> Punch "+HIM$+", <G> Give it up, <Y> Your status: ")
	IF FNR% THEN RETURN Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$)
	IF I$="G" THEN ZZ%=FNW%("We can't all be Rocky, eh?"+CR$):RETURN
	IF I$="Y" THEN ZZ%=FNW%("Your knock out points:"+STR$(BP%)+CR$+CR$)
	IF I$<>"P" THEN GOTO Bloop
YOURturn:
	A%=FNPUNCH%(LVL%,TSTR%)
	IF FND(100)>TAGL% THEN
		ZZ%=FNW%(HE$+" ducks your punch."+CR$+CR$):SLEEP 1
		DO:REC%=FND(96):GET #2,REC%:LOOP WHILE REC%=USER% OR REC%=NME% OR ASC(USER.ACCESS$)<4
		ZZ%=FNW%("You hit "+FNL$(USER.HANDLE$)+"!"+CR$+CR$):SLEEP 1
		IF A%>=INT(CVI(USER.HP$)/10) THEN
			SWAP REC%,NME%:GOSUB NMEko:SWAP REC%,NME%
			GOTO NMEturn
		END IF
		ZZ%=FNW%("Uh oh!  Here comes "+FNL$(USER.HANDLE$)+"!"+CR$+CR$):SLEEP 1
		IF FND(100)>ASC(USER.AGL$) THEN
			ZZ%=FNW%("You dodge the reprisal!"+CR$+CR$):SLEEP 1
			GOTO NMEturn
		END IF
		A%=FNPUNCH%(ASC(USER.LEVEL$),ASC(USER.STR$))
		ZZ%=FNW%(MID$("SheHe",1-3*(USER.SEX$="M"),3)+" hits you for"+STR$(A%)+" points."+CR$+CR$)
		BP%=BP%-A%
		IF BP%<1 THEN NME.NAME$=FNL$(USER.HANDLE$):SWAP REC%,NME%:GOTO YOUko
		GOTO NMEturn
	END IF
	ZZ%=FNW%("You hit "+HIM$+" for"+STR$(A%)+" points."+CR$+CR$)
	NME.BP%=NME.BP%-A%
	IF NME.BP%<1 THEN GOTO NMEko
NMEturn:
	A%=FNPUNCH%(NME.LVL%,NME.STR%)
	IF FND(100)>NME.AGL% THEN
		ZZ%=FNW%("You dodge "+NME.NAME$+"'s fist."+CR$+CR$):SLEEP 1
		DO:REC%=FND(96):GET #2,REC%:LOOP WHILE REC%=USER% OR REC%=NME% OR ASC(USER.ACCESS$)<4
		ZZ%=FNW%(NME.NAME$+" hits "+FNL$(USER.HANDLE$)+"!"+CR$+CR$):SLEEP 1
		IF A%>=INT(CVI(USER.HP$)/10) THEN
			ZZ%=FNW%(NME.NAME$+" knocks out "+FNL$(USER.HANDLE$)+"!"+CR$+CR$)
			GOTO Bloop
		END IF
		ZZ%=FNW%("Uh oh!  Here comes "+FNL$(USER.HANDLE$)+"!"+CR$+CR$):SLEEP 1
		IF FND(100)>ASC(USER.AGL$) THEN
			ZZ%=FNW%(HE$+" dodges the reprisal!"+CR$+CR$):SLEEP 1
			GOTO Bloop
		END IF
		A%=FNPUNCH%(ASC(USER.LEVEL$),ASC(USER.STR$))
		ZZ%=FNW%(MID$("SheHe",1-3*(USER.SEX$="M"),3)+" hits "+NME.NAME$+" for"+STR$(A%)+" points."+CR$+CR$)
		NME.BP%=NME.BP%-A%
		IF NME.BP%<1 THEN ZZ%=FNW%(FNL$(USER.HANDLE$)+" knocks out "+NME.NAME$+"!"+CR$+CR$):RETURN
		GOTO YOURturn
	END IF
	ZZ%=FNW%(NME.NAME$+" hits you for"+STR$(A%)+" points."+CR$+CR$)
	BP%=BP%-A%
	IF BP%<1 THEN GOTO YOUko
	GOTO Bloop
NMEko:
	GET #2,NME%
	ZZ%=FNW%("You knocked out "+FNL$(USER.HANDLE$)+"!"+CR$+CR$)
	EX#=INT((2^(ASC(USER.LEVEL$)-2)*1000)/9):X!=EX#
	GOLD#=CVD(USER.GOLD$):M!=GOLD#:LSET USER.GOLD$=MKD$(0)
	PUT #2,NME%
	ZZ%=FNW%("You get"+STR$(X!)+" experience and"+STR$(M!)+" gold pieces!"+CR$)
	GET #2,USER%
	LSET USER.EXP$=MKD$(CVD(USER.EXP$)+EX#)
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+GOLD#)
	PUT #2,USER%
	CLOSE #4:OPEN "A",#4,"NOTES\ARENA"+MID$(STR$(NME%),2)+".MSG"
	PRINT #4,"-=>*<=-":PRINT #4,""
	PRINT #4,FNL$(USER.HANDLE$)+" knocked you out and got"+STR$(M!)+" gold pieces."
	PRINT #4,"":CLOSE #4
	RETURN
YOUko:
	GET #2,USER%
	EX#=INT((2^(LVL%-2)*1000)/9):X!=EX#
	GOLD#=CVD(USER.GOLD$):M!=GOLD#:LSET USER.GOLD$=MKD$(0)
	PUT #2,USER%
	ZZ%=FNW%(NME.NAME$+" has knocked you out"):IF GOLD# THEN ZZ%=FNW%(" and takes all your gold")
	ZZ%=FNW%("!"+CR$+CR$)
	CLOSE #4:OPEN "A",#4,"NOTES\ARENA"+MID$(STR$(NME%),2)+".MSG"
	PRINT #4,"-=>*<=-":PRINT #4,""
	PRINT #4,"You knocked out "+FNL$(USER.HANDLE$)+"!"
	PRINT #4,"You got"+STR$(X!)+" experience and"+STR$(M!)+" gold pieces."
	PRINT #4,"":CLOSE #4
	GET #2,NME%
	LSET USER.EXP$=MKD$(CVD(USER.EXP$)+EX#)
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+GOLD#)
	PUT #2,NME%
	ZZ%=FNW%("You are unconscious for 3 minutes..."):SLEEP 2:LOGOFF%=LOGOFF%-3:ZZ%=FNW%(CR$+CR$)
	RETURN
