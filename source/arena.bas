	'$INCLUDE: 'COMMON.BAS'

	DEF FNA%(W%,L%,S%,A%)
		A%=(4*ABS(W%)+L%+S%/10-ABS(A%))/2
		A%=A%+RND(1)*A%
		IF A%<1 THEN A%=1
		FNA%=A%
	END DEF

	DEF FNBURN%(A%)
		ZZ%=FNW%(HIS$+" scroll burns as it is read."+CR$+CR$)
		NME.SPELL%=NME.SPELL%-A%
		IF NME% THEN
			GET #2,NME%
			LSET USER.SPELL$=MKI$(NME.SPELL%)
			PUT #2,NME%
			GET #2,USER%
		END IF
	END DEF

	DIM L50$(50)
	GET #2,USER%:CL%=ASC(USER.CLASS$)

Top:
	REC%=0:GOSUB GetRec
	IF CVD(USER.EXP$)>(2^(ASC(USER.LEVEL$)-1)*(1100-ASC(USER.INT$)*2)) THEN CHAIN "MAIN"
	IF FNW%(CR$) THEN GOTO Hungup
	IF FNW%("<U> User fights"+CR$) THEN GOTO Hungup
	IF FNW%("<M> Monster fights"+CR$) THEN GOTO Hungup
	IF FNW%("<J> Joust users"+CR$) THEN GOTO Hungup
	IF FNW%("<C> Cast a spell"+CR$) THEN GOTO Hungup
	IF FNW%("<P> Poison your weapon"+CR$) THEN GOTO Hungup
	IF FNW%("<G> Goto the square"+CR$) THEN GOTO Hungup
	IF FNW%("<Y> Your status"+CR$) THEN GOTO Hungup
	IF FNW%(CR$+"[Arena] Option (Q=Quit): ") THEN GOTO Hungup
	IF FNT% THEN GOTO Expired
	IF FNR% THEN GOTO Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$)
	IF I$="G" THEN CHAIN "SQUARE"
	IF I$="Q" THEN CHAIN "MAIN"
	F%=-(I$="U")-2*(I$="M")-3*(I$="J")-4*(I$="C")-5*(I$="P")-6*(I$="Y")
	ON F% GOSUB User,Monster,Joust,Cast,Poison,Status
	GOTO Top

User:
	IF ASC(USER.ACCESS$)<4 THEN ZZ%=FNW%("You cannot fight other users yet, go fight a monster."+CR$):RETURN
	IF FIGHT%>2 THEN ZZ%=FNW%("You have run out of fights."+CR$):RETURN
	IF FNW%("Fight what user? ") THEN RETURN Hungup
	REC%=FNUSER%
	IF FNW%(CR$+CR$) THEN RETURN Hungup
	IF REC%=0 THEN RETURN
	GOSUB GetRec:IF NME%=0 THEN RETURN
	IF NME%=USER% THEN ZZ%=FNW%("You can't fight a wimp like "+MID$("himher",-(NME.SEX$="M")-4*(NME.SEX$="F"),3)+"."+CR$+CR$):GOTO User
	IF NME.ACCESS%<4 THEN ZZ%=FNW%("That user does not fight."+CR$):RETURN
	IF ASC(USER.LEVEL$)-NME.LVL%>3 THEN ZZ%=FNW%("You can only attack someone higher or up to three levels below you."+CR$+CR$):GOTO User
	IF NME.STATUS% THEN
		ZZ%=FNW%(NME.NAME$+" was killed by ")
		REC%=NME.STATUS%:GOSUB GetRec:IF NME%=0 THEN GOTO User
		ZZ%=FNW%(NME.NAME$+"."+CR$+CR$)
		GOTO User
	END IF
	IF FNW%(LEFT$("Name: "+NME.NAME$+SPACE$(33),33)+"You: "+CR$+CR$) THEN RETURN Hungup
	IF FNW%(LEFT$("Class: "+CLASS$(NME.CL%)+SPACE$(33),33)+CLASS$(ASC(USER.CLASS$))+CR$) THEN RETURN Hungup
	IF FNW%(LEFT$("Level:"+STR$(NME.LVL%)+SPACE$(33),33)+MID$(STR$(ASC(USER.LEVEL$)),2)+CR$) THEN RETURN Hungup
	IF FNW%(LEFT$("Weapon: "+WEAPON$(NME.WEAPON%)+SPACE$(33),33)+WEAPON$(ASC(USER.WEAPON$))+CR$) THEN RETURN Hungup
	IF FNW%(LEFT$("Armor: "+ARMOR$(NME.ARMOR%)+SPACE$(33),33)+ARMOR$(ASC(USER.ARMOR$))+CR$) THEN RETURN Hungup
	IF FNW%(LEFT$("Hit points: "+STR$(NME.HP%)+SPACE$(33),33)+MID$(STR$(THP%),2)+CR$) THEN RETURN Hungup
	IF NME.SP%+CVI(USER.SP$) THEN IF FNW%(LEFT$("Spell points:"+STR$(NME.SP%)+SPACE$(33),33)+MID$(STR$(TSP%),2)+CR$) THEN RETURN Hungup
	IF FNW%(CR$+"Are you sure (Y/N)? ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	ZZ%=FNW%(CR$+CR$)
	IF UCASE$(I$)<>"Y" THEN GOTO User
	FIGHT%=FIGHT%+1
	GOTO Battle

Monster:
	IF FIGHT%>2 THEN ZZ%=FNW%("You have run out of fights."+CR$):RETURN
	OPEN "I",#4,"MONSTERS.TXT":INPUT #4,MM%:CLOSE #4
	IF FNW%("Fight what monster (1-"+MID$(STR$(MM%),2)+",<D>emon)? ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	ZZ%=FNW%(CR$+CR$)
	IF UCASE$(I$)="D" THEN
		IF ASC(USER.LEVEL$)<50 THEN
			ZZ%=FNW%("You are not powerful enough to fight demons yet.  Go fight some monsters."+CR$+CR$)
			GOTO Monster
		END IF
		D#=INT(2^((ASC(USER.LEVEL$)-1)/(100/MW%))*WC(1)*10):M!=D#
		IF FNW%("The ancient necromancer will summon you a demon for"+STR$(M!)+" gold pieces."+CR$+CR$) THEN RETURN Hungup
		IF CVD(USER.GOLD$)<D# THEN ZZ%=FNW%("You don't have enough."+CR$+CR$):GOTO Monster
		IF FNW%("Will you pay (Y/N)? ") THEN RETURN Hungup
		IF FNR% THEN RETURN Hungup
		ZZ%=FNW%(CR$+CR$)
		IF UCASE$(I$)<>"Y" THEN ZZ%=FNW%("His eyes glow red and he says, "+CHR$(34)+"I don't make deals!"+CHR$(34)+CR$+CR$):GOTO Monster
		IF FNW%("As you hand him the money, it disappears into thin air."+CR$+CR$) THEN RETURN Hungup
		LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-D#)
		PUT #2,USER%
		IF FNW%("The old necromancer summons you a demon."+CR$+CR$) THEN RETURN Hungup
		IF FND(4)=1 THEN NME.CL%=FND(11) ELSE NME.CL%=CL%
		NME.NAME$="summoned Demon":NME.LVL%=ASC(USER.LEVEL$)+FND(7)-4:IF NME.LVL%>99 THEN NME.LVL%=99
		NME.EXP#=2^(NME.LVL%-2)*1000:NME.STR%=99:NME.INT%=99:NME.AGL%=99:NME.CHR%=0
		NME.HP%=15:FOR I%=2 TO NME.LVL%:NME.HP%=NME.HP%+9+(I%<5)+(I%<15)+(I%<25)+FND(I%)+I%:NEXT I%
		NME.SP%=0:IF NME.CL%=2 OR NME.CL%=4 OR NME.CL%=5 OR NME.CL%=7 OR NME.CL%=9 OR NME.CL%=11 THEN NME.SPELL%=NOT 0:NME.SP%=15:FOR I%=2 TO NME.LVL%:NME.SP%=NME.SP%+9+(I%<5)+(I%<15)+(I%<25)+FND(I%)+I%:NEXT I%
		NME.WEAPON%=ASC(USER.WEAPON$)+FND(3)-2:IF NME.WEAPON%<1 OR NME.WEAPON%>MW% THEN NME.WEAPON%=MW%
		NME.ARMOR%=ASC(USER.ARMOR$)+FND(3)-2:IF NME.ARMOR%<1 OR NME.ARMOR%>MA% THEN NME.ARMOR%=MA%
		NME.GOLD#=INT(WC(NME.WEAPON%)/10):NME.GOLD#=NME.GOLD#+FND(NME.GOLD#)
		IF FND(2)=1 THEN NME.WEAPON%=-NME.WEAPON%
		IF FND(2)=1 THEN NME.ARMOR%=-NME.ARMOR%
		GOTO Mstat
	END IF
	X%=VAL(I$):IF X%<1 OR X%>MM% THEN RETURN
	OPEN "I",#4,"MONSTERS.TXT":INPUT #4,MM%
	FOR I%=1 TO X%
		INPUT #4,NME.NAME$,NME.CL%,NME.LVL%,NME.EXP#,NME.STR%,NME.INT%,NME.AGL%,NME.CHR%,NME.HP%,NME.SP%,NME.WEAPON%,NME.ARMOR%,NME.SPELL%,NME.GOLD#
	NEXT I%
	CLOSE #4
	IF FNW%("You encounter a "+NME.NAME$+"!"+CR$+CR$) THEN RETURN Hungup
Mstat:
	IF NME.WEAPON%>0 THEN ZZ%=FNW%("It's carrying a "+WEAPON$(NME.WEAPON%)+"."+CR$+CR$)
	IF NME.ARMOR%>0 THEN ZZ%=FNW%("It has a "+ARMOR$(NME.ARMOR%)+"."+CR$+CR$)
	IF FNW%("It has"+STR$(NME.HP%)+" hit points."+CR$+CR$) THEN RETURN Hungup
	IF FNW%("Will you fight it (Y/N)? ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	ZZ%=FNW%(CR$+CR$)
	IF UCASE$(I$)<>"Y" THEN GOTO Monster
	FIGHT%=FIGHT%+1:NME.SEX$="I":NME.MAX%=NME.HP%
	GOTO Battle

Joust:
	IF JOUST%>2 THEN ZZ%=FNW%("You have run out of jousts."+CR$):RETURN
	IF FNW%("You grab a horse and prepare yourself to joust."+CR$+CR$) THEN RETURN Hungup
Juser:
	GET #2,USER%
	Z1%=TAGL%*ASC(USER.LEVEL$)/10+2*CVI(USER.JW$)-CVI(USER.JL$)+10
	IF FNW%("Joust what user or <S>tatus? ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$)
	IF I$="S" THEN
		IF FNW%("Jousts won:"+STR$(CVI(USER.JW$))+CR$) THEN RETURN Hungup
		IF FNW%("Jousts lost:"+STR$(CVI(USER.JL$))+CR$) THEN RETURN Hungup
		IF FNW%("Jousting ability:"+STR$(Z1%)+CR$) THEN RETURN Hungup
		IF FNW%(CR$+"<R> Reset jousting stats"+CR$+CR$) THEN RETURN Hungup
		GOTO Juser
	END IF
	IF I$="R" THEN
		LSET USER.JW$=MKI$(0):LSET USER.JL$=MKI$(0)
		PUT #2,USER%
		GOTO Juser
	END IF
	REC%=VAL(I$):IF REC%<1 OR REC%>96 THEN RETURN
	GOSUB GetRec:IF NME%=0 THEN GOTO Juser
	Z2%=NME.AGL%*NME.LVL%/10+2*NME.JW%-NME.JL%+10
	IF Z2%<1 THEN ZZ%=FNW%("That knight is out practicing right now."+CR$+CR$):GOTO Juser
	IF NME%=USER% THEN ZZ%=FNW%("You can't joust a wimp like "+MID$("himher",-(NME.SEX$="M")-4*(NME.SEX$="F"),3)+"."+CR$+CR$):GOTO Juser
	IF ASC(USER.LEVEL$)-NME.LVL%>3 THEN ZZ%=FNW%("You can only joust someone higher or up to three levels below you."+CR$+CR$):GOTO Juser
	IF FNW%("Jousting ability:"+CR$+CR$) THEN RETURN Hungup
	IF FNW%(LEFT$(NME.NAME$+":"+SPACE$(27),27)+STR$(Z2%)+CR$) THEN RETURN Hungup
	IF FNW%(LEFT$(FNL$(USER.HANDLE$)+":"+SPACE$(27),27)+STR$(Z1%)+CR$) THEN RETURN Hungup
	IF FNW%(CR$+"Are you sure (Y/N)? ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	ZZ%=FNW%(CR$+CR$)
	IF UCASE$(I$)<>"Y" THEN GOTO Juser
	JOUST%=JOUST%+1:P$=MID$("SheHe",-(NME.SEX$="F")-4*(NME.SEX$="M"),3)
	P%=0:JW%=0:JL%=0
	IF FNW%("The trumpets blare! You and your opponent ride into the arena. The crowd roars!"+CR$+CR$) THEN RETURN Hungup
Jpass:
	IF FNW%("--=:)) Round"+STR$(P%)+"/5: Won:"+MID$(STR$(JW%),2)+" ^ Lost:"+MID$(STR$(JL%),2)+" ((:=--"+CR$) THEN RETURN Hungup
	IF FNW%("       <J> Joust * <F> Forfeit: ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$):IF I$<>"J" AND I$<>"F" THEN GOTO Jpass
	IF I$="F" THEN
		IF FNW%(CR$+"The crowd throws rocks at you as you ride out of the arena."+CR$) THEN RETURN Hungup
		GET #2,NME%
		LSET USER.JW$=MKI$(CVI(USER.JW$)+1)
		PUT #2,NME%
		GET #2,USER%
		LSET USER.JL$=MKI$(CVI(USER.JL$)+1)
		PUT #2,USER%
		GOTO Joust
	END IF
	P%=P%+1
	IF FNW%(CR$+"You spur the horse.  The tension mounts."+CR$) THEN RETURN Hungup
	SLEEP 1
	DO:Z%=Z1%+FND(ASC(USER.LEVEL$)*10)-(Z2%+FND(NME.LVL%*10)):LOOP WHILE Z%=0
	IF Z%>0 THEN ZZ%=FNW%("-*> Thud! <*- A hit!  You win this pass!"+CR$+CR$):JW%=JW%+1
	IF Z%<0 THEN ZZ%=FNW%("^> Oof! <^ "+P$+" hits! You lose this pass!"+CR$+CR$):JL%=JL%+1
	IF JW%=3 THEN
		IF FNW%("You have won the joust!"+CR$) THEN RETURN Hungup
		IF FNW%("The crowd cheers!"+CR$+CR$) THEN RETURN Hungup
		D#=INT(2^((ASC(USER.LEVEL$)-1)/(100/MW%))*WC(1)*10):M!=D#
		IF FNW%("You win"+STR$(M!)+" gold pieces!"+CR$+CR$) THEN RETURN Hungup
		GET #2,NME%
		LSET USER.JL$=MKI$(CVI(USER.JL$)+1)
		PUT #2,NME%
		GET #2,USER%
		LSET USER.JW$=MKI$(CVI(USER.JW$)+1)
		LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+D#)
		PUT #2,USER%
		OPEN "A",#4,"NOTES\ARENA"+MID$(STR$(NME%),2)+".MSG"
		PRINT #4,"-=>*<=-":PRINT #4,""
		PRINT #4,FNL$(USER.HANDLE$)+" defeated you in a joust.":PRINT #4,""
		CLOSE #4
		GOTO Joust
	END IF
	IF JL%=3 THEN
		IF FNW%("You have lost the joust."+CR$) THEN RETURN Hungup
		IF FNW%("The crowd boo's you."+CR$+CR$) THEN RETURN Hungup
		IF FNW%(NME.NAME$+" spits in your face."+CR$+CR$) THEN RETURN Hungup
		D#=INT(2^((NME.LVL%-1)/(100/MW%))*WC(1)*10):M!=D#
		GET #2,NME%
		LSET USER.JW$=MKI$(CVI(USER.JW$)+1)
		LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+D#)
		PUT #2,NME%
		GET #2,USER%
		LSET USER.JL$=MKI$(CVI(USER.JL$)+1)
		PUT #2,USER%
		OPEN "A",#4,"NOTES\ARENA"+MID$(STR$(NME%),2)+".MSG"
		PRINT #4,"-=>*<=-":PRINT #4,""
		PRINT #4,"You defeated ";FNL$(USER.HANDLE$);" in a joust.":PRINT #4,""
		CLOSE #4
		GOTO Joust
	END IF
	GOTO Jpass

Cast:
	SP%=0
	IF CL%<>2 AND CL%<>4 AND CL%<>5 AND CL%<>7 AND CL%<>9 AND CL%<>11 THEN ZZ%=FNW%("You are not a spell caster!"+CR$):RETURN
	IF CVI(USER.SPELL$)=0 THEN ZZ%=FNW%("You don't have any spells!"+CR$+CR$):RETURN
	IF FNW%("Enter spell number (?=Menu): ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	ZZ%=FNW%(CR$+CR$)
	IF I$="?" THEN
		FOR I%=0 TO 11
			IF (CVI(USER.SPELL$) AND 2^I%) THEN IF FNW%(RIGHT$(" <"+MID$(STR$(I%+1),2)+"> ",5)+LEFT$(SPELL$(I%+1)+SPACE$(20),20)+STR$(SP%(I%+1+(CL%=9 OR CL%=11)))+CR$) THEN RETURN Hungup
		NEXT I%
		IF FNW%(CR$) THEN RETURN Hungup
		GOTO Cast
	END IF
	SP%=VAL(I$):IF SP%<1 THEN RETURN
	IF SP%>12 THEN GOTO Cast
	IF (CVI(USER.SPELL$) AND 2^(SP%-1))=0 THEN
		IF FNW%("You don't know that spell!"+CR$+CR$) THEN RETURN Hungup
		GOTO Cast
	END IF
	IF SP%(SP%+(CL%=9 OR CL%=11))>TSP% THEN ZZ%=FNW%("You don't have enough spell power to cast that spell!"+CR$+CR$):GOTO Cast
	IF SP%=7 THEN ZZ%=FNW%("Teleport can only be used in the dungeon."+CR$+CR$):GOTO Cast
	IF SP%=9 AND NME.SEX$="" THEN ZZ%=FNW%("You can only blast during battle."+CR$+CR$):GOTO Cast
	IF SP%=10 AND NME.SEX$<>"" THEN ZZ%=FNW%("Resurrect can only be used in the Arena."+CR$+CR$):SP%=0:RETURN
	TSP%=TSP%-SP%(SP%+(CL%=9 OR CL%=11))
	IF CL%=4 AND FND(5-(NME.CL%=8))=1 THEN
		ZZ%=FNW%("Your scroll burns as you cast the spell."+CR$+CR$)
		GET #2,USER%
		LSET USER.SPELL$=MKI$(CVI(USER.SPELL$)-2^(SP%-1))
		PUT #2,USER%
	END IF
	IF (FND(100)-10*(CL%<>2)+20*(CL%=9 OR CL%=11)+10*(NME.CL%=8))>TINT% THEN
		IF FND(10)>1 THEN ZZ%=FNW%("Fssst! Spell fails!"+CR$) ELSE ZZ%=FNW%("Pfssst!  You crapped your britches!"+CR$)
		RETURN
	END IF
	IF SP%=1 THEN
		TCHR%=TCHR%+FND(10):IF TCHR%>100 THEN TCHR%=100
		IF FND(10)>1 THEN ZZ%=FNW%("You feel much more charismatic."+CR$) ELSE ZZ%=FNW%("{Sigh} I really don't need more charm."+CR$)
	END IF
	IF SP%=2 THEN
		TINT%=TINT%+FND(10):IF TINT%>100 THEN TINT%=100
		IF FND(10)>1 THEN ZZ%=FNW%("You feel much more intelligent."+CR$) ELSE ZZ%=FNW%("You feel less like a turd and more like a nerd."+CR$)
	END IF
	IF SP%=3 THEN
		TSTR%=TSTR%+FND(10):IF TSTR%>100 THEN TSTR%=100
		IF FND(10)>1 THEN ZZ%=FNW%("You feel much more stronger."+CR$) ELSE ZZ%=FNW%("My!  What bulging muscles!"+CR$)
	END IF
	IF SP%=4 THEN
		TAGL%=TAGL%+FND(10):IF TAGL%>100 THEN TAGL%=100
		IF FND(10)>1 THEN ZZ%=FNW%("You feel much more agile."+CR$) ELSE ZZ%=FNW%("Can't touch this!"+CR$)
	END IF
	IF SP%=5 THEN
		TSHIELD%=TSHIELD%+1-(CL%=11)
		IF FND(10)>1 THEN ZZ%=FNW%("A magic field shimmers around you."+CR$) ELSE ZZ%=FNW%("A magic field SMELLS around you."+CR$)
	END IF
	IF SP%=6 THEN
		THIT%=THIT%+1-(CL%=11)
		IF FND(10)>1 THEN ZZ%=FNW%("Your "+WEAPON$(ASC(USER.WEAPON$))+" now glows with a magical sharpness."+CR$) ELSE ZZ%=FNW%("Your "+WEAPON$(ASC(USER.WEAPON$))+" sings, "+CHR$(34)+"Bring on the turkey!"+CHR$(34)+"."+CR$)
	END IF
	IF SP%=8 THEN
		A%=0:FOR I%=1 TO ASC(USER.LEVEL$):A%=A%+FND(15)-2*(CL%=2):NEXT I%
		THP%=THP%+A%
		ZZ%=FNW%("Hit points = Hit points +"+STR$(A%)+CR$)
	END IF
	IF SP%=9 THEN
		A%=0:FOR I%=1 TO ASC(USER.LEVEL$):A%=A%+FND(17-17*(NME.CL%=8)-2*(CL%=2)):NEXT I%
		NME.HP%=NME.HP%-A%
		ZZ%=FNW%("You blast "+HIM$+" for"+STR$(A%)+" hit points of damage."+CR$)
	END IF
	IF SP%=10 THEN
		REC%=0
		WHILE REC%=0
			ZZ%=FNW%("Resurrect who? ")
			REC%=FNUSER%
		WEND
		ZZ%=FNW%(CR$)
		GET #2,REC%
		IF ASC(USER.STATUS$) THEN 
			ZZ%=FNW%("Now raising "+FNL$(USER.HANDLE$)+"...")
			LSET USER.STATUS$=CHR$(0)
			PUT #2,REC%
		END IF
		GET #2,USER%
		ZZ%=FNW%("Done."+CR$)
	END IF
	IF SP%=11 THEN
		GET #2,USER%
		THP%=CVI(USER.HP$)
		ZZ%=FNW%("You feel your vitality completely restored."+CR$)
	END IF
	IF SP%=12 THEN
		NME.HP%=0
		IF FND(10)>1 THEN ZZ%=FNW%(NME.NAME$+" is completely atomized."+CR$) ELSE ZZ%=FNW%(NME.NAME$+" lights up like a Roman candle."+CR$)
	END IF
	RETURN

Poison:
	CL%=ASC(USER.CLASS$)
	IF CL%<>3 AND CL%<>6 AND CL%<>10 THEN
		IF FNW%("Only thieves, assassins, and alchemists may do that!"+CR$) THEN GOTO Hungup
		RETURN
	END IF
	IF CVI(USER.POISON$)=0 THEN
		IF FNW%("You don't have any poison!"+CR$) THEN RETURN Hungup
		RETURN
	END IF
	IF FNW%("Enter type (?=Menu): ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	ZZ%=FNW%(CR$+CR$)
	IF I$="?" THEN
		FOR I%=1 TO 12
			IF (CVI(USER.POISON$) AND 2^(I%-1)) THEN
				IF FNW%(RIGHT$(" <"+MID$(STR$(I%),2)+"> ",5)+"Type"+STR$(I%)+CR$) THEN RETURN Hungup
			END IF
		NEXT I%
		IF FNW%(CR$) THEN RETURN Hungup
		GOTO Poison
	END IF
	SP%=VAL(I$):IF SP%<1 THEN RETURN
	IF SP%>12 THEN GOTO Poison
	IF (CVI(USER.POISON$) AND 2^(SP%-1))=0 THEN
		IF FNW%("You don't have any type"+STR$(SP%)+" poison."+CR$+CR$) THEN RETURN Hungup
		GOTO Poison
	END IF
	IF FNW%("You pour some poison on your "+WEAPON$(ASC(USER.WEAPON$))+"."+CR$+CR$) THEN RETURN Hungup
	THIT%=SP%*(2-2*(CL%=10))
	IF FND(3+(CL%=3))=1 THEN
		LSET USER.POISON$=MKI$(CVI(USER.POISON$)-2^(SP%-1))
		PUT #2,USER%
		IF FNW%("You toss the empty vial aside."+CR$+CR$) THEN RETURN Hungup
	END IF
	IF THIT%>ASC(USER.WEAPON$) THEN
		THIT%=ASC(USER.WEAPON$)
		IF FNW%("Your weapon has as much poison as it can hold.  The rest just drips off."+CR$+CR$) THEN RETURN Hungup
	END IF
	I%=(ASC(USER.CURSED$)<>0)-(ASC(USER.BLESSED$)<>0)
	THIT%=THIT%+10*I%*ASC(USER.WEAPON$)/100
	RETURN

Status:
	IF FNW%(LEFT$("Stamina:"+STR$(TSTR%)+"/"+MID$(STR$(ASC(USER.STR$)),2)+SPACE$(20),20)) THEN RETURN Hungup
	IF FNW%(LEFT$("Intellect:"+STR$(TINT%)+"/"+MID$(STR$(ASC(USER.INT$)),2)+SPACE$(20),20)) THEN RETURN Hungup
	IF FNW%(LEFT$("Agility:"+STR$(TAGL%)+"/"+MID$(STR$(ASC(USER.AGL$)),2)+SPACE$(20),20)) THEN RETURN Hungup
	IF FNW%(LEFT$("Charisma:"+STR$(TCHR%)+"/"+MID$(STR$(ASC(USER.CHR$)),2)+SPACE$(20),20)) THEN RETURN Hungup
	IF FNW%(LEFT$("Hit points:"+STR$(THP%)+"/"+MID$(STR$(CVI(USER.HP$)),2)+SPACE$(40),40)) THEN RETURN Hungup
	IF FNW%("Spell power:"+STR$(TSP%)+"/"+MID$(STR$(CVI(USER.SP$)),2)+CR$) THEN RETURN Hungup
	A$=""
	IF THIT%>0 THEN A$="(+"+MID$(STR$(THIT%),2)+")"
	IF THIT%<0 THEN A$="("+STR$(THIT%)+")"
	IF FNW%(LEFT$("Weapon: "+WEAPON$(ASC(USER.WEAPON$))+" "+A$+SPACE$(40),40)) THEN RETURN Hungup
	A$=""
	IF TSHIELD%>0 THEN A$="(+"+MID$(STR$(TSHIELD%),2)+")"
	IF TSHIELD%<0 THEN A$="("+STR$(TSHIELD%)+")"
	IF FNW%("Armor: "+ARMOR$(ASC(USER.ARMOR$))+" "+A$+CR$) THEN RETURN Hungup
	RETURN

GetRec:
	IF REC% THEN GET #2,REC% ELSE LSET USER.REC$=STRING$(LEN(USER.REC$),0)
	IF ASC(USER.ACCESS$)=0 THEN
		NME%=0:NME.ACCESS%=0:NME.CL%=0:NME.STR%=0:NME.INT%=0:NME.AGL%=0:NME.CHR%=0:NME.HP%=0:NME.SP%=0
		NME.LVL%=0:NME.STATUS%=0:NME.JW%=0:NME.JL%=0
		NME.WEAPON%=0:NME.ARMOR%=0:NME.SPELL%=0:NME.NAME$="":NME.SEX$=""
		NME.MAX%=0:NME.GOLD#=0
		NME.BLESSED%=0:NME.CURSED%=0:NME.THIT%=0:NME.TSHIELD%=0
	ELSE
		NME%=REC%:NME.ACCESS%=ASC(USER.ACCESS$):NME.CL%=ASC(USER.CLASS$):NME.STR%=ASC(USER.STR$):NME.INT%=ASC(USER.INT$):NME.AGL%=ASC(USER.AGL$):NME.CHR%=ASC(USER.CHR$):NME.HP%=CVI(USER.HP$):NME.SP%=CVI(USER.SP$)
		NME.LVL%=ASC(USER.LEVEL$):NME.STATUS%=ASC(USER.STATUS$):NME.JW%=CVI(USER.JW$):NME.JL%=CVI(USER.JL$)
		NME.WEAPON%=ASC(USER.WEAPON$):NME.ARMOR%=ASC(USER.ARMOR$):NME.SPELL%=CVI(USER.SPELL$):NME.NAME$=FNL$(USER.HANDLE$):NME.SEX$=USER.SEX$
		NME.MAX%=NME.HP%:NME.GOLD#=INT(CVD(USER.GOLD$))
		NME.BLESSED%=ASC(USER.BLESSED$):NME.CURSED%=ASC(USER.CURSED$)
		I%=(NME.CURSED%<>0)-(NME.BLESSED%<>0)
		NME.STR%=NME.STR%+10*I%:IF NME.STR%>100 THEN NME.STR%=100
		NME.INT%=NME.INT%+10*I%:IF NME.INT%>100 THEN NME.INT%=100
		NME.AGL%=NME.AGL%+10*I%:IF NME.AGL%>100 THEN NME.AGL%=100
		NME.CHR%=NME.CHR%+10*I%:IF NME.CHR%>100 THEN NME.CHR%=100
		NME.THIT%=10*I%*NME.WEAPON%/100
		NME.TSHIELD%=10*I%*NME.ARMOR%/100
	END IF
	GET #2,USER%
	RETURN

Battle:
	IF NME.SEX$="M" THEN HE$="He":HIM$="him":HIS$="His"
	IF NME.SEX$="F" THEN HE$="She":HIM$="her":HIS$="Her"
	IF NME.SEX$="I" THEN HE$="It":HIM$="it":HIS$="Its"
	CURE%=0
	IF FND(15+5*(NME.CL%=2)+10*(NME.CL%=9 OR NME.CL%=11))=1 THEN CURE%=(((NME.SPELL% AND 1024)=1024) AND (NME.SP%>=SP%(11+(NME.CL%=9 OR NME.CL%=11))))
	IF (TAGL%+10*(NME.CL%=7)+FND(100))<(NME.AGL%+10*(CL%=7)+FND(100)) THEN
		IF FNW%(HE$+" gets the first swing."+CR$) THEN RETURN Hungup
		GOTO NMEturn
	END IF
	IF FNW%("You get the first swing."+CR$+CR$) THEN RETURN Hungup
	IF CL%<>3 AND CL%<>4 AND CL%<>6 AND CL%<>7 THEN GOTO YOURturn
	DO
		IF FNW%("Attempt to backstab (Y/N)? ") THEN RETURN Hungup
		IF FNR% THEN RETURN Hungup
		I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$)
	LOOP UNTIL I$="Y" OR I$="N"
	IF I$="N" THEN GOTO YOURturn
	IF FND(100)>(TAGL%+10*(NME.CL%=7)) THEN
		IF FNW%("Attempt fails!  "+HE$+" gets the first swing."+CR$+CR$) THEN RETURN Hungup
		SLEEP 1
		GOTO NMEturn
	END IF
	BS%=2-2*(CL%=3)
	A%=BS%*FNA%(ASC(USER.WEAPON$)+THIT%,ASC(USER.LEVEL$),TSTR%,NME.ARMOR%+NME.TSHIELD%)
	IF FNW%("You stab "+HIM$+", causing"+STR$(A%)+" hit points of damage."+CR$) THEN RETURN Hungup
	NME.HP%=NME.HP%-A%
	GOTO NMEturn
Bloop:
	IF THP%<1 THEN GOTO YOUdie
	ZZ%=FNW%("["+MID$(STR$(THP%),2)+",")
	IF TINT%>=50 AND TINT%<100 THEN N%=2*(100-TINT%):I$=MID$(STR$(INT(NME.HP%/N%+.5)*N%),2)
	IF TINT%<50 OR I$="0" THEN IF NME.HP%>(NME.MAX%*2/3) THEN I$="Healthy" ELSE IF NME.HP%>(NME.MAX%*1/3) THEN I$="Hurting" ELSE I$="Weak"
	IF TINT%=100 THEN I$=MID$(STR$(NME.HP%),2)
	IF FNW%(I$+"] <A> Attack, <C> Cast spell, <R> Retreat, <Y> Your status: ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$)
	IF I$<>"A" AND I$<>"C" AND I$<>"R" AND I$<>"Y" THEN GOTO Bloop
	IF I$="C" THEN
		GOSUB Cast:IF SP%=0 THEN GOTO Bloop
		GOTO NMEturn
	END IF
	IF I$="R" THEN
		IF CL%=11 AND NME%<>0 THEN ZZ%=FNW%("A Hero can never retreat from a user battle!"+CR$+CR$):GOTO Bloop
		IF FNW%("He who fights and runs away, lives to fight another day."+CR$+CR$) THEN RETURN Hungup
		IF NME% THEN
			OPEN "A",#4,"NOTES\ARENA"+MID$(STR$(NME%),2)+".MSG"
			PRINT #4,"-=>*<=-":PRINT #4,""
			PRINT #4,FNL$(USER.HANDLE$);", the coward, retreated from you in battle.":PRINT #4,""
			CLOSE #4
		END IF
		REASON$=FNL$(USER.HANDLE$)+" retreated from"
		IF NME.SEX$="I" THEN REASON$=REASON$+" a":IF INSTR("AEIOU",LEFT$(NME.NAME$,1)) THEN REASON$=REASON$+"n"
		REASON$=REASON$+" "+NME.NAME$+".":GOSUB Last50
		RETURN
	END IF
	IF I$="Y" THEN
		IF FNW%("Your hit points:"+STR$(THP%)+CR$) THEN RETURN Hungup
		IF FNW%("    spell power:"+STR$(TSP%)+CR$+CR$) THEN RETURN Hungup
		GOTO Bloop
	END IF
YOURturn:
	IF FND(100)>INT(50+10*(NME.CL%=7)+TAGL%/2) THEN
		IF FNW%("Your "+WEAPON$(ASC(USER.WEAPON$))+" passes through thin air."+CR$) THEN RETURN Hungup
		GOTO NMEturn
	END IF
	A%=FNA%(ASC(USER.WEAPON$)+THIT%,ASC(USER.LEVEL$),TSTR%,NME.ARMOR%+NME.TSHIELD%)
	IF CL%=1 OR CL%=4 OR CL%=5 OR CL%=6 OR CL%=8 OR CL%=11 THEN A%=A%+FND(ASC(USER.LEVEL$))*(1-2*(CL%=1)-3*(CL%=8 OR CL%=11))
	IF CL%=9 THEN A%=A%*.8
	IF FNW%("You hit "+HIM$+", causing"+STR$(A%)+" hit points of damage."+CR$) THEN RETURN Hungup
	NME.HP%=NME.HP%-A%
NMEturn:
	ZZ%=FNW%(CR$)
	IF NME.HP%<1 THEN GOTO NMEdies
	IF (CURE%=0) AND ((NME.SPELL% AND 64)=64) AND (NME.HP%<NME.MAX%/8) AND (NME.SP%>=SP%(7+(NME.CL%=9 OR NME.CL%=11))) AND (FND(15+5*(NME.CL%=2)+10*(NME.CL%=9 OR NME.CL%=11))=1) THEN
		NME.SP%=NME.SP%-SP%(7+(NME.CL%=9 OR NME.CL%=11))
		IF NME.CL%=4 AND FND(5-5*(CL%=8))=1 THEN ZZ%=FNBURN%(64)
		IF (FND(100)-10*(NME.CL%<>2)+20*(NME.CL%=9 OR NME.CL%=11)+10*(CL%=8))>NME.INT% THEN
			IF FNW%("Fssst! "+HIS$+" spell fails!"+CR$+CR$) THEN RETURN Hungup
			GOTO Bloop
		END IF
		IF FNW%(HE$+" teleports "+HIM$+"self away from the battle!"+CR$+CR$) THEN RETURN Hungup
		IF NME% THEN
			OPEN "A",#4,"NOTES\ARENA"+MID$(STR$(NME%),2)+".MSG"
			PRINT #4,"-=>*<=-":PRINT #4,""
			PRINT #4,FNL$(USER.HANDLE$);" cursed as you teleported away.":PRINT #4,""
			CLOSE #4
		END IF
		REASON$=FNL$(USER.HANDLE$)+" watched"
		IF NME.SEX$="I" THEN REASON$=REASON$+" a":IF INSTR("AEIOU",LEFT$(NME.NAME$,1)) THEN REASON$=REASON$+"n"
		REASON$=REASON$+" "+NME.NAME$+" teleport away!":GOSUB Last50
		RETURN
	END IF
	IF (CURE%=0) AND ((NME.SPELL% AND 128)=128) AND (NME.HP%<NME.MAX%/2) AND (NME.SP%>=SP%(8+(NME.CL%=9 OR NME.CL%=11))) AND (FND(4+(NME.CL%=2)+2*(NME.CL%=9 OR NME.CL%=11))=1) THEN
		NME.SP%=NME.SP%-SP%(8+(NME.CL%=9 OR NME.CL%=11))
		IF NME.CL%=4 AND FND(5-5*(CL%=8))=1 THEN ZZ%=FNBURN%(128)
		IF (FND(100)-10*(NME.CL%<>2)+20*(NME.CL%=9 OR NME.CL%=11)+10*(CL%=8))>NME.INT% THEN
			IF FNW%("Fssst! "+HIS$+" spell fails!"+CR$+CR$) THEN RETURN Hungup
			GOTO Bloop
		END IF
		A%=0:FOR I%=1 TO NME.LVL%:A%=A%+FND(15)-2*(NME.CL%=2):NEXT I%
		NME.HP%=NME.HP%+A%:IF NME.HP%>NME.MAX% THEN A%=A%-(NME.HP%-NME.MAX%):NME.HP%=NME.MAX%
		IF FNW%(HE$+" heals "+HIM$+"self! Hit points = hit points +"+STR$(A%)+CR$+CR$) THEN RETURN Hungup
		GOTO Bloop
	END IF
	IF (CURE%=0) AND ((NME.SPELL% AND 256)=256) AND (NME.SP%>=SP%(9+(NME.CL%=9 OR NME.CL%=11))) AND (FND(4+(NME.CL%=2)+2*(NME.CL%=9 OR NME.CL%=11))=1) THEN
		NME.SP%=NME.SP%-SP%(9+(NME.CL%=9 OR NME.CL%=11))
		IF NME.CL%=4 AND FND(5-5*(CL%=8))=1 THEN ZZ%=FNBURN%(256)
		IF (FND(100)-10*(NME.CL%<>2)+20*(NME.CL%=9 OR NME.CL%=11)+10*(CL%=8))>NME.INT% THEN
			IF FNW%("Fssst! "+HIS$+" spell fails!"+CR$+CR$) THEN RETURN Hungup
			GOTO Bloop
		END IF
		A%=0:FOR I%=1 TO NME.LVL%:A%=A%+FND(17-17*(CL%=8))-2*(NME.CL%=2):NEXT I%
		THP%=THP%-A%
		IF FNW%(HE$+" blasts you for"+STR$(A%)+" hit points of damage!"+CR$+CR$) THEN RETURN Hungup
		GOTO Bloop
	END IF
	IF (CURE%<>0) AND (NME.HP%<NME.MAX%/8) THEN
		NME.SP%=NME.SP%-SP%(11+(NME.CL%=9 OR NME.CL%=11))
		IF NME.CL%=4 AND FND(5-5*(CL%=8))=1 THEN ZZ%=FNBURN%(1024)
		IF (FND(100)-10*(NME.CL%<>2)+20*(NME.CL%=9 OR NME.CL%=11)+10*(CL%=8))>NME.INT% THEN
			IF FNW%("Fssst! "+HIS$+" spell fails!"+CR$+CR$) THEN RETURN Hungup
			GOTO Bloop
		END IF
		NME.HP%=NME.MAX%
		IF FNW%(HE$+" completely heals "+HIM$+"self!"+CR$+CR$) THEN RETURN Hungup
		GOTO Bloop
	END IF
	IF ((NME.SPELL% AND 2048)=2048) AND (NME.SP%>=SP%(12+(NME.CL%=9 OR NME.CL%=11))) AND (FND(15+5*(NME.CL%=2)+10*(NME.CL%=9 OR NME.CL%=11))=1) THEN
		NME.SP%=NME.SP%-SP%(9+(NME.CL%=9 OR NME.CL%=11))
		IF NME.CL%=4 AND FND(5-5*(CL%=8))=1 THEN ZZ%=FNBURN%(2048)
		IF (FND(100)-10*(NME.CL%<>2)+20*(NME.CL%=9 OR NME.CL%=11)+10*(CL%=8))>NME.INT% THEN
			IF FNW%("Fssst! "+HIS$+" spell fails!"+CR$+CR$) THEN RETURN Hungup
			GOTO Bloop
		END IF
		THP%=0
		IF FNW%(HE$+" zaps your soul with one blow!"+CR$+CR$) THEN RETURN Hungup
		GOTO Bloop
	END IF
	IF FND(100)>INT(50+10*(CL%=7)+NME.AGL%/2) THEN
		IF NME.WEAPON%>=0 THEN IF FNW%(HIS$+" "+WEAPON$(NME.WEAPON%)+" whistles by you."+CR$+CR$) THEN RETURN Hungup
		IF NME.WEAPON%<0 THEN IF FNW%("It attacks you, but misses."+CR$+CR$) THEN RETURN Hungup
		GOTO Bloop
	END IF
	A%=FNA%(NME.WEAPON%+NME.THIT%,NME.LVL%,NME.STR%,ASC(USER.ARMOR$)+TSHIELD%)
	IF NME.CL%=1 OR NME.CL%=4 OR NME.CL%=5 OR NME.CL%=6 OR NME.CL%=8 OR NME.CL%=11 THEN A%=A%+FND(NME.LVL%)*(1-2*(NME.CL%=1)-3*(NME.CL%=8 OR NME.CL%=11))
	IF NME.CL%=9 THEN A%=A%*.8
	IF FNW%(HE$+" hits you, causing"+STR$(A%)+" hit points of damage."+CR$+CR$) THEN RETURN Hungup
	THP%=THP%-A%
	GOTO Bloop
YOUdie:
	HW%=ASC(USER.WEAPON$):HA%=ASC(USER.ARMOR$):HG#=INT(CVD(USER.GOLD$)):HL%=ASC(USER.LEVEL$):HB%=ASC(USER.BLESSED$)
	IF NME% THEN
		ZZ%=FNW%(NME.NAME$+" has killed you!"+CR$+CR$)
		OPEN "A",#4,"NOTES\ARENA"+MID$(STR$(NME%),2)+".MSG"
		PRINT #4,"-=>*<=-":PRINT #4,""
		PRINT #4,"You killed ";FNL$(USER.HANDLE$);"!"
		GET #2,NME%
		IF HW%>NME.WEAPON% THEN
			ZZ%=FNW%(HE$+" also gets your "+WEAPON$(HW%)+CR$)
			PRINT #4,"You also got the ";WEAPON$(HW%);"."
			LSET USER.WEAPON$=CHR$(HW%)
		ELSE
			D#=INT(WC(HW%)*NME.CHR%/100):M!=D#
			ZZ%=FNW%(HE$+" also gets"+STR$(M!)+" for your "+WEAPON$(HW%)+CR$)
			PRINT #4,"You got"+STR$(M!)+" for the ";WEAPON$(HW%);"."
			LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+D#)
		END IF
		IF HA%>NME.ARMOR% THEN
			ZZ%=FNW%(HE$+" also gets your "+ARMOR$(HA%)+CR$)
			PRINT #4,"You also got the ";ARMOR$(HA%);"."
			LSET USER.ARMOR$=CHR$(HA%)
		ELSE
			D#=INT(AC(HA%)*NME.CHR%/100):M!=D#
			ZZ%=FNW%(HE$+" also gets"+STR$(M!)+" for your "+ARMOR$(HA%)+CR$)
			PRINT #4,"You got"+STR$(M!)+" for the ";ARMOR$(HA%);"."
			LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+D#)
		END IF
		M!=HG#
		ZZ%=FNW%(HE$+" gets"+STR$(M!)+" gold and"+STR$(INT(2^(HL%-2)*1000/3))+" experience."+CR$)
		PRINT #4,"You got"+STR$(M!)+" gold and";STR$(INT(2^(HL%-2)*1000/3));" experience."
		LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+HG#)
		LSET USER.EXP$=MKD$(CVD(USER.EXP$)+INT(2^(HL%-2)*1000/3))
		PRINT #4,""
		CLOSE #4
		IF HB% AND NME.ACCESS%>3 THEN LSET USER.BLESSED$=CHR$(USER%)
		LSET USER.CURSED$=CHR$(0)
		PUT #2,NME%
	ELSE
		ZZ%=FNW%("The "+NME.NAME$+" has killed you!"+CR$+CR$)
		ZZ%=FNW%("It gets all your gold!"+CR$+CR$)
		GET #2,USER%
		LSET USER.GOLD$=MKD$(0)
		LSET USER.BLESSED$=CHR$(0)
		PUT #2,USER%
		REASON$=FNL$(USER.HANDLE$)+" was killed by a "+NME.NAME$+".":GOSUB Last50
		IF HB% THEN
			DO:REC%=FND(96):GET #2,REC%:LOOP WHILE ASC(USER.ACCESS$)<4 OR ASC(USER.CURSED$)>0
			LSET USER.BLESSED$=CHR$(USER%)
			PUT #2,REC%
		END IF
		RETURN Logoff
	END IF
	GET #2,USER%
	IF ASC(USER.WEAPON$)>NME.WEAPON% THEN LSET USER.WEAPON$=CHR$(NME.WEAPON%)
	IF ASC(USER.ARMOR$)>NME.ARMOR% THEN LSET USER.ARMOR$=CHR$(NME.ARMOR%)
	LSET USER.GOLD$=MKD$(0)
	LSET USER.BLESSED$=CHR$(0)
	IF NME.CURSED% THEN LSET USER.CURSED$=CHR$(NME%)
	PUT #2,USER%
	REASON$=FNL$(USER.HANDLE$)+" was killed by "+NME.NAME$+".":GOSUB Last50
	RETURN Logoff
NMEdies:
	HW%=ASC(USER.WEAPON$):HA%=ASC(USER.ARMOR$):HG#=0:HC%=ASC(USER.CURSED$):U$=MID$("SheHe",1-3*(USER.SEX$="M"),3)
	IF NME% THEN
		ZZ%=FNW%("You have killed "+NME.NAME$+"!"+CR$+CR$)
		OPEN "A",#4,"NOTES\ARENA"+MID$(STR$(NME%),2)+".MSG"
		PRINT #4,"-=>*<=-":PRINT #4,""
		PRINT #4,FNL$(USER.HANDLE$);" killed you!"
		GET #2,NME%
		IF HW%<NME.WEAPON% THEN
			PRINT #4,U$+" also got your ";WEAPON$(NME.WEAPON%);"."
			ZZ%=FNW%("You also get "+LCASE$(HIS$)+" "+WEAPON$(NME.WEAPON%)+"."+CR$)
			LSET USER.WEAPON$=CHR$(HW%)
		ELSE
			D#=INT(WC(NME.WEAPON%)*TCHR%/100):HG#=HG#+D#:M!=D#
			PRINT #4,U$+" got";STR$(M!);" for your ";WEAPON$(NME.WEAPON%)
			ZZ%=FNW%("You get"+STR$(M!)+" for "+LCASE$(HIS$)+" "+WEAPON$(NME.WEAPON%)+"."+CR$)
		END IF
		IF HA%<NME.ARMOR% THEN
			PRINT #4,U$+" also got your ";ARMOR$(NME.ARMOR%);"."
			ZZ%=FNW%("You also get "+LCASE$(HIS$)+" "+ARMOR$(NME.ARMOR%)+"."+CR$)
			LSET USER.ARMOR$=CHR$(HA%)
		ELSE
			D#=INT(AC(NME.ARMOR%)*TCHR%/100):HG#=HG#+D#:M!=D#
			PRINT #4,U$+" got";STR$(M!);" for your ";ARMOR$(NME.ARMOR%);"."
			ZZ%=FNW%("You get"+STR$(M!)+" for "+LCASE$(HIS$)+" "+ARMOR$(NME.ARMOR%)+"."+CR$)
		END IF
		IF NME.GOLD# THEN
			HG#=HG#+NME.GOLD#:M!=NME.GOLD#
			PRINT #4,U$+" got"+STR$(M!)+" gold pieces."
			LSET USER.GOLD$=MKD$(0)
			ZZ%=FNW%("You get"+STR$(M!)+" gold pieces."+CR$)
		END IF
		LSET USER.STATUS$=CHR$(USER%)
		IF HC% AND NME.ACCESS%>3 THEN LSET USER.CURSED$=CHR$(USER%)
		LSET USER.BLESSED$=CHR$(0)
		PUT #2,NME%
	ELSE
		ZZ%=FNW%("You have killed the "+NME.NAME$+"!"+CR$+CR$)
		IF HW%<NME.WEAPON% THEN
			ZZ%=FNW%("You also get its "+WEAPON$(NME.WEAPON%)+"."+CR$)
		ELSE
			IF NME.WEAPON%>0 THEN
				D#=INT(WC(NME.WEAPON%)*TCHR%/100):HG#=HG#+D#:M!=D#
				ZZ%=FNW%("You get"+STR$(M!)+" for its "+WEAPON$(NME.WEAPON%)+"."+CR$)
			END IF
		END IF
		IF HA%<NME.ARMOR% THEN
			ZZ%=FNW%("You also get its "+ARMOR$(NME.ARMOR%)+"."+CR$)
		ELSE
			IF NME.ARMOR%>0 THEN
				D#=INT(AC(NME.ARMOR%)*TCHR%/100):HG#=HG#+D#:M!=D#
				ZZ%=FNW%("You get"+STR$(M!)+" for its "+ARMOR$(NME.ARMOR%)+"."+CR$)
			END IF
		END IF
		HG#=HG#+NME.GOLD#:M!=NME.GOLD#
		ZZ%=FNW%("You get"+STR$(M!)+" gold pieces."+CR$)
	END IF
	GET #2,USER%
	IF ASC(USER.WEAPON$)<NME.WEAPON% THEN LSET USER.WEAPON$=CHR$(NME.WEAPON%):THIT%=0
	IF ASC(USER.ARMOR$)<NME.ARMOR% THEN LSET USER.ARMOR$=CHR$(NME.ARMOR%):TSHIELD%=0
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+HG#)
	LSET USER.EXP$=MKD$(CVD(USER.EXP$)+INT(2^(NME.LVL%-2)*1000/3))
	ZZ%=FNW%("You get"+STR$(INT(2^(NME.LVL%-2)*1000/3))+" experience."+CR$)
	IF NME.BLESSED% THEN
		LSET USER.BLESSED$=CHR$(NME%)
		TSTR%=TSTR%+10:IF TSTR%>100 THEN TSTR%=100
		TINT%=TINT%+10:IF TINT%>100 THEN TINT%=100
		TAGL%=TAGL%+10:IF TAGL%>100 THEN TAGL%=100
		TCHR%=TCHR%+10:IF TCHR%>100 THEN TCHR%=100
		ZZ%=FNW%("You suddenly feel blessed!"+CR$)
	END IF
	IF ASC(USER.BLESSED$) AND THIT%=0 THEN THIT%=10*ASC(USER.WEAPON$)/100
	IF ASC(USER.BLESSED$) AND TSHIELD%=0 THEN TSHIELD%=10*ASC(USER.ARMOR$)/100
	IF HC% AND NME.ACCESS%>3 THEN
		TSTR%=TSTR%+10:IF TSTR%>100 THEN TSTR%=100
		TINT%=TINT%+10:IF TINT%>100 THEN TINT%=100
		TAGL%=TAGL%+10:IF TAGL%>100 THEN TAGL%=100
		TCHR%=TCHR%+10:IF TCHR%>100 THEN TCHR%=100
		IF THIT%<0 THEN THIT%=0
		IF TSHIELD%<0 THEN TSHIELD%=0
		LSET USER.CURSED$=CHR$(0)
		ZZ%=FNW%("Your curse is removed!"+CR$)
	END IF
	PUT #2,USER%
	IF NME% THEN GOSUB ArenaMsg
	CLOSE #4
	REASON$=FNL$(USER.HANDLE$)+" killed"
	IF NME.SEX$="I" THEN REASON$=REASON$+" a":IF INSTR("AEIOU",LEFT$(NME.NAME$,1)) THEN REASON$=REASON$+"n"
	REASON$=REASON$+" "+NME.NAME$+".":GOSUB Last50
	RETURN

ArenaMsg:
	PRINT #4,""
	ZZ%=FNW%(CR$+"Would you like to leave a comment (y/N)? ")
	IF FNR% THEN CLOSE #4:RETURN Hungup
	IF UCASE$(I$)="Y" THEN
		ZZ%=FNW%(CR$+"Enter a comment on the line below."+CR$)
		IF FNR% THEN CLOSE #4:RETURN Hungup
		PRINT #4,FNL$(USER.HANDLE$)+" says,"
		PRINT #4,CHR$(34)+I$+CHR$(34):PRINT #4,""
	END IF
	ZZ%=FNW%(CR$)
	RETURN

Last50:
	OPEN "I",#4,"LAST50.TXT"
	LINE INPUT #4,A$:X%=0
	WHILE NOT EOF(4):X%=X%+1:LINE INPUT #4,L50$(X%):WEND
	CLOSE #4
	OPEN "O",#4,"LAST50.TXT"
	PRINT #4,A$
	PRINT #4,FND$(USER.LDATE$);"  ";LEFT$(TIME$,5);"  ";REASON$
	IF X%>49 THEN X%=49
	FOR I%=1 TO X%:PRINT #4,L50$(I%):NEXT I%
	CLOSE #4
	RETURN
