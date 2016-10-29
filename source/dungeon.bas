	'$INCLUDE: 'COMMON.BAS'

	DEF FNA%(W%,L%,S%,A%)
		A%=(4*ABS(W%)+L%+S%/10-ABS(A%))/2
		A%=A%+RND(1)*A%
		IF A%<1 THEN A%=1
		FNA%=A%
	END DEF
	DIM DUNGEON%(7,7)
	GET #2,USER%
	CL%=ASC(USER.CLASS$):LVL%=ASC(USER.LEVEL$)
	DL%=LVL%-1:IF DL%<1 THEN DL%=1
	IF FNW%(CR$+"Generating dungeon, please wait..."+CR$) THEN GOTO Hungup
	DX%=FND(7):DY%=FND(7)

NextLevel:
	FOR I%=1 TO 7:FOR J%=1 TO 7:DUNGEON%(I%,J%)=FND(8):NEXT J%:NEXT I%
	DUNGEON%(FND(7),FND(7))=9
	DUNGEON%(FND(7),FND(7))=10
	MAP%=0
	IF FNW%("Now entering dungeon level"+STR$(DL%)+"."+CR$+CR$) THEN GOTO Hungup
	GOSUB Examine

NextMove:
	ZZ%=FNW%(CR$)
	HY%=DY%:HX%=DX%:NME.NAME$=""
	IF FNT% THEN GOTO Expired
	IF FNW%("<N>orth, <S>outh, <E>ast, <W>est, <C>ast, <P>oison, <M>ap, <Y>our stats: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$):NME.SEX$=""
	F%=-(I$="N")-2*(I$="S")-3*(I$="E")-4*(I$="W")-5*(I$="C")-6*(I$="P")-7*(I$="M")-8*(I$="Y")
	ON F% GOSUB North,South,East,West,Cast,Poison,Map,YourStats
	GOTO NextMove

North:
	IF DY%=1 THEN ZZ%=FNW%("There is a wall to the north."+CR$):RETURN
	IF DUNGEON%(DY%,DX%)=7 THEN ZZ%=FNW%("This is an east-west corridor.  You may only go east or west."+CR$):RETURN
	DY%=DY%-1:GOTO Examine

South:
	IF DY%=7 THEN ZZ%=FNW%("There is a wall to the south."+CR$):RETURN
	IF DUNGEON%(DY%,DX%)=7 THEN ZZ%=FNW%("This is an east-west corridor.  You may only go east or west."+CR$):RETURN
	DY%=DY%+1:GOTO Examine

East:
	IF DX%=7 THEN ZZ%=FNW%("There is a wall to the east."+CR$):RETURN
	IF DUNGEON%(DY%,DX%)=6 THEN ZZ%=FNW%("This is a north-south corridor.  You may only go north or south."+CR$):RETURN
	DX%=DX%+1:GOTO Examine

West:
	IF DX%=1 THEN ZZ%=FNW%("There is a wall to the west."+CR$):RETURN
	IF DUNGEON%(DY%,DX%)=6 THEN ZZ%=FNW%("This is a north-south corridor.  You may only go north or south."+CR$):RETURN
	DX%=DX%-1:GOTO Examine

Examine:
	ON DUNGEON%(DY%,DX%) GOTO Empty,Cave,Monster,Monster,Thief,NS,EW,TrapDoor,Teleport,Cleric

Empty:
	I%=FND(3)
	IF I%=1 THEN ZZ%=FNW%("You are in an empty chamber."+CR$)
	IF I%=2 THEN ZZ%=FNW%("You are in a barren chamber."+CR$)
	IF I%=3 THEN ZZ%=FNW%("You are in a quiet chamber."+CR$)
	IF FND(20)>1 THEN RETURN
	SLEEP 1:IF FNW%(CR$+"You hear a noise up ahead."+CR$+CR$) THEN RETURN Hungup
	SLEEP 1:IF FNW%("Cautiously, you approach to investigate..."+CR$+CR$) THEN RETURN Hungup
	SLEEP 1:IF FNW%("Suddenly, from out of the darkness, ") THEN RETURN Hungup
	SLEEP 1:IF FND(10)=1 THEN ZZ%=FNW%("a bat flies by and soils your cloak!"+CR$+CR$):RETURN
	IF FNW%("a monster attacks!"+CR$+CR$) THEN RETURN Hungup
	GOTO Monster

Cave:
	ZZ%=FNW%("You cautiously enter a hidden cavern."+CR$+CR$)
	IF FND(10)=1 THEN
		ZZ%=FNW%("On the ground, you find a small ")
		I%=FND(3)
		IF I%=1 THEN ZZ%=FNW%("bottle")
		IF I%=2 THEN ZZ%=FNW%("flask")
		IF I%=3 THEN ZZ%=FNW%("vial")
		ZZ%=FNW%(", containing a potion."+CR$+CR$)
		ZZ%=FNW%("Will you drink it (y/N)? ")
		IF FNR% THEN RETURN Hungup
		ZZ%=FNW%(CR$)
		IF UCASE$(I$)="Y" THEN
			I%=FND(6)
			IF I%=1 THEN
				IF TSP% THEN
					ZZ%=FNW%("It was a Potion of Magic.  Your spell power is temporarily increased."+CR$)
					TSP%=TSP%+FND(CVI(USER.SP$)-TSP%)
				ELSE
					I%=2
				END IF
			END IF
			IF I%=2 OR I%=3 THEN
				ZZ%=FNW%("It was a Potion of Healing.  Some of your wounds disappear."+CR$)
				THP%=THP%+FND(CVI(USER.HP$)-THP%)
			END IF
			IF I%=4 THEN
				IF TSP% THEN
					ZZ%=FNW%("It was an Anti-Magic Potion!  Your spell power is temporarily decreased."+CR$)
					TSP%=TSP%-FND(TSP%)
				ELSE
					I%=5
				END IF
			END IF
			IF I%=5 OR I%=6 THEN
				ZZ%=FNW%("It contained poison!  Your hit points are temporarily decreased."+CR$)
				THP%=THP%-FND(THP%)
			END IF
		END IF
	END IF
	IF CL%=4 AND FND(20)=1 AND CVI(USER.SPELL$)<4095 THEN
		DO:I%=FND(12):LOOP WHILE (CVI(USER.SPELL$) AND 2^(I%-1))=2^(I%-1)
		ZZ%=FNW%(CR$+"You've found a "+SPELL$(I%)+" scroll!"+CR$)
		LSET USER.SPELL$=MKI$(CVI(USER.SPELL$) OR 2^(I%-1))
		PUT #2,USER%
		RETURN
	END IF
	IF FND(10)>1 THEN RETURN

Monster:
	IF DUNGEON%(DY%,DX%)>1 THEN ZZ%=FNW%("There's something lurking in this chamber."+CR$+CR$)
	RESTORE BadGuys
	J%=DL%+FND(11)-6
	IF J%<1 THEN J%=FND(5)
	IF J%>100 THEN J%=95+FND(5)
	FOR I%=1 TO J%:READ NME.NAME$,NME.CL%:NEXT I%
	IF NME.CL%=0 THEN NME.CL%=CL%
	NME.SEX$="I":PN$="a":IF INSTR("aeiou",LEFT$(NME.NAME$,1)) THEN PN$="an"
	IF FNW%("It's "+PN$+" "+NME.NAME$+"!"+CR$+CR$) THEN RETURN Hungup
	NME.LVL%=DL%:IF NME.LVL%>99 THEN NME.LVL%=99
	NME.STR%=ASC(USER.STR$):NME.INT%=ASC(USER.INT$)
	NME.AGL%=ASC(USER.AGL$):NME.CHR%=ASC(USER.CHR$)
	NME.HP%=15:FOR I%=2 TO NME.LVL%:NME.HP%=NME.HP%+9-(I%<5)-(I%<15)-(I%<25)+FND(I%)+I%:NEXT I%
	NME.HP%=NME.HP%/4:NME.MAX%=NME.HP%
	NME.SP%=0:NME.SPELL%=0
	IF NME.CL%=2 OR NME.CL%=4 OR NME.CL%=5 OR NME.CL%=7 OR NME.CL%=9 OR NME.CL%=11 THEN
		NME.SP%=15:FOR I%=2 TO NME.LVL%:NME.SP%=NME.SP%+9-(I%<5)-(I%<15)-(I%<25)+FND(I%)+I%:NEXT I%
		NME.SP%=NME.SP%/4
		NME.SPELL%=NOT 0
	END IF
	NME.WEAPON%=-ASC(USER.WEAPON$):NME.ARMOR%=-ASC(USER.ARMOR$)
	NME.GOLD#=INT(WC(ASC(USER.WEAPON$))/10*TCHR%/100)+1:NME.GOLD#=NME.GOLD#+FND(NME.GOLD#)
	ZZ%=FNW%("It has"+STR$(NME.HP%)+" hit points, and it doesn't look friendly..."+CR$+CR$)
Battle:
	IF (TAGL%+10*(NME.CL%=7)+FND(100))<(NME.AGL%+10*(CL%=7)+FND(100)) THEN
		IF FNW%("It gets the first swing."+CR$+CR$) THEN RETURN Hungup
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
		IF FNW%("Attempt fails!  It gets the first swing."+CR$+CR$) THEN RETURN Hungup
		SLEEP 1
		GOTO NMEturn
	END IF
	BS%=2-2*(CL%=3)
	A%=BS%*FNA%(ASC(USER.WEAPON$)+THIT%,ASC(USER.LEVEL$),TSTR%,NME.ARMOR%-INT(TSHIELD%/4))
	IF FNW%("You stab it, causing"+STR$(A%)+" hit points of damage."+CR$+CR$) THEN RETURN Hungup
	NME.HP%=NME.HP%-A%
	IF NME.HP%<1 THEN GOTO NMEdies
	GOTO NMEturn
Bloop:
	IF THP%<1 THEN GOTO YOUdie
	IF I$="R" THEN I$="":ZZ%=FNW%("You make good on your escape!"+CR$+CR$):SLEEP 1:GOTO Examine
	ZZ%=FNW%("["+MID$(STR$(THP%),2)+",")
	IF TINT%>=50 AND TINT%<100 THEN N%=2*(100-TINT%):I$=MID$(STR$(INT(NME.HP%/N%+.5)*N%),2)
	IF TINT%<50 OR I$="0" THEN IF NME.HP%>(NME.MAX%*2/3) THEN I$="Healthy" ELSE IF NME.HP%>(NME.MAX%*1/3) THEN I$="Hurting" ELSE I$="Weak"
	IF TINT%=100 THEN I$=MID$(STR$(NME.HP%),2)
	IF FNW%(I$+"] <A> Attack, <C> Cast spell, <R> Retreat, <S> Status: ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$)
	IF I$<>"A" AND I$<>"C" AND I$<>"R" AND I$<>"S" THEN GOTO Bloop
	IF I$="C" THEN
		GOSUB Cast:IF SP%=0 THEN GOTO Bloop
		GOTO NMEturn
	END IF
	IF I$="R" THEN
		IF FNW%("Argh!  You run like crazy out the nearest exit!"+CR$+CR$) THEN RETURN Hungup
		DY%=HY%:DX%=HX%:SLEEP 1
		IF FND(INT(TINT%/20)+1)>1 THEN GOTO Bloop
		ZZ%=FNW%("The "+NME.NAME$+" leaps at you!"+CR$+CR$):SLEEP 1
		GOTO NMEturn
	END IF
	IF I$="S" THEN
		IF FNW%("Your hit points:"+STR$(THP%)+CR$) THEN RETURN Hungup
		IF FNW%("    spell power:"+STR$(TSP%)+CR$+CR$) THEN RETURN Hungup
		GOTO Bloop
	END IF
YOURturn:
	IF FND(100)>INT(50+10*(NME.CL%=7)+TAGL%/2) THEN
		IF FNW%("Your "+WEAPON$(ASC(USER.WEAPON$))+" passes through thin air."+CR$+CR$) THEN RETURN Hungup
		GOTO NMEturn
	END IF
	A%=FNA%(ASC(USER.WEAPON$)+THIT%,ASC(USER.LEVEL$),TSTR%,NME.ARMOR%-INT(TSHIELD%/4))
	IF CL%=1 OR CL%=4 OR CL%=5 OR CL%=6 OR CL%=8 OR CL%=11 THEN A%=A%+FND(ASC(USER.LEVEL$))*(1-2*(CL%=1)-3*(CL%=8 OR CL%=11))
	IF CL%=9 THEN A%=A%*.8
	IF FNW%("You hit it, causing"+STR$(A%)+" hit points of damage."+CR$+CR$) THEN RETURN Hungup
	NME.HP%=NME.HP%-A%
NMEturn:
	IF NME.HP%<1 THEN GOTO NMEdies
	IF (NME.SPELL% AND 128)=128 AND NME.HP%<NME.MAX%/2 AND NME.SP%>=SP%(8+(NME.CL%=9 OR NME.CL%=11)) AND FND(4)=1 THEN
		NME.SP%=NME.SP%-SP%(8+(NME.CL%=9 OR NME.CL%=11))
		IF NME.CL%=4 AND FND(5-5*(CL%=8))=1 THEN
			IF FNW%("Its scroll burns as it is read."+CR$+CR$) THEN RETURN Hungup
			NME.SPELL%=NME.SPELL%-128
		END IF
		IF (FND(100)-10*(NME.CL%<>2)+20*(NME.CL%=9 OR NME.CL%=11)+10*(CL%=8))>NME.INT% THEN
			IF FNW%("Fssst!  Its spell fails!"+CR$+CR$) THEN RETURN Hungup
			GOTO Bloop
		END IF
		A%=0:FOR I%=1 TO NME.LVL%:A%=A%+FND(15)-2*(NME.CL%=2):NEXT I%
		NME.HP%=NME.HP%+A%:IF NME.HP%>NME.MAX% THEN A%=A%-(NME.HP%-NME.MAX%):NME.HP%=NME.MAX%
		IF FNW%("It heals itself!  Hit points = hit points +"+STR$(A%)+CR$+CR$) THEN RETURN Hungup
		GOTO Bloop
	END IF
	IF (NME.SPELL% AND 256)=256 AND NME.SP%>=SP%(9+(NME.CL%=9 OR NME.CL%=11)) AND FND(4)=1 THEN
		NME.SP%=NME.SP%-SP%(9+(NME.CL%=9 OR NME.CL%=11))
		IF NME.CL%=4 AND FND(5-5*(CL%=8))=1 THEN
			IF FNW%("Its scroll burns as it is read."+CR$+CR$) THEN RETURN Hungup
			NME.SPELL%=NME.SPELL%-256
		END IF
		IF (FND(100)-10*(NME.CL%<>2)+20*(NME.CL%=9 OR NME.CL%=11)+10*(CL%=8))>NME.INT% THEN
			IF FNW%("Fssst!  Its spell fails!"+CR$+CR$) THEN RETURN Hungup
			GOTO Bloop
		END IF
		A%=0:FOR I%=1 TO NME.LVL%:A%=A%+FND(17-4*(CL%=8))-2*(NME.CL%=2):NEXT I%
		THP%=THP%-A%
		IF FNW%("It blasts you for"+STR$(A%)+" hit points of damage!"+CR$+CR$) THEN RETURN Hungup
		GOTO Bloop
	END IF
	IF FND(100)>INT(50+10*(CL%=7)+NME.AGL%/2) THEN
		IF FNW%("It attacks you, but misses."+CR$+CR$) THEN RETURN Hungup
		GOTO Bloop
	END IF
	A%=FNA%(NME.WEAPON%-INT(THIT%/4),NME.LVL%,NME.STR%,ASC(USER.ARMOR$)+TSHIELD%)
	IF NME.CL%=1 OR NME.CL%=4 OR NME.CL%=5 OR NME.CL%=6 OR NME.CL%=8 OR NME.CL%=11 THEN A%=A%+FND(NME.LVL%)*(1-2*(NME.CL%=1)-3*(NME.CL%=8 OR NME.CL%=11))
	IF NME.CL%=9 THEN A%=A%*.8
	IF FNW%("It hits you, causing"+STR$(A%)+" hit points of damage."+CR$+CR$) THEN RETURN Hungup
	THP%=THP%-A%
	GOTO Bloop
YOUdie:
	ZZ%=FNW%("The "+NME.NAME$+" has killed you!"+CR$+CR$)
	ZZ%=FNW%("It gets all your gold!"+CR$+CR$)
	LSET USER.GOLD$=MKD$(0)
	PUT #2,USER%
	RETURN Logoff
NMEdies:
	ZZ%=FNW%("You have killed the "+NME.NAME$+"!"+CR$+CR$)
	LSET USER.EXP$=MKD$(CVD(USER.EXP$)+INT(2^(DL%-2)*1000/15))
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+NME.GOLD#)
	PUT #2,USER%
	X!=INT(2^(DL%-2)*1000/15):M!=NME.GOLD#
	ZZ%=FNW%("You get"+STR$(M!)+" gold and"+STR$(X!)+" experience."+CR$)
	IF DUNGEON%(DY%,DX%)=3 OR DUNGEON%(DY%,DX%)=4 THEN DUNGEON%(DY%,DX%)=1
	IF MAP%=0 AND FND(11-INT(LVL%/20)-INT(TCHR%/20))=1 THEN MAP%=NOT 0:ZZ%=FNW%(CR$+"You find a map!"+CR$)
	RETURN

Thief:
	ZZ%=FNW%("There is a thief in this chamber!"+CR$+CR$)
	NME.GOLD#=FND(CVD(USER.GOLD$)/4):M!=NME.GOLD#
	IF FND(3)=1 THEN
		ZZ%=FNW%("You surprise the thief!  As he runs out, he drops"+STR$(M!)+" gold pieces."+CR$)
	ELSE
		ZZ%=FNW%("He surprises you!  As he passes by, he steals"+STR$(M!)+" gold pieces."+CR$)
		NME.GOLD#=-NME.GOLD#
	END IF
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+NME.GOLD#)
	PUT #2,USER%
	RETURN

NS:
	ZZ%=FNW%("You have entered a north-south passage")
	IF I$="E" OR I$="W" THEN ZZ%=FNW%(" through a secret door."+CR$):ZZ%=FNW%("You hear the door lock behind you")
	ZZ%=FNW%("."+CR$+CR$)
	RETURN

EW:
	ZZ%=FNW%("You have entered an east-west passage")
	IF I$="N" OR I$="S" THEN ZZ%=FNW%(" through a secret door."+CR$):ZZ%=FNW%("You hear the door lock behind you")
	ZZ%=FNW%("."+CR$+CR$)
	RETURN

TrapDoor:
	ZZ%=FNW%("You have stepped onto a trapdoor!"+CR$+CR$)
	IF FND(100)+20<(TAGL%-10*(CL%=7)) THEN ZZ%=FNW%("You manage to catch the edge and stop yourself from falling."+CR$+CR$):RETURN
	ZZ%=FNW%("You've fallen down a level!"+CR$+CR$)
	DL%=DL%+1
	RETURN NextLevel

Teleport:
	ZZ%=FNW%("You have entered a magical teleport chamber."+CR$+CR$)
	ZZ%=FNW%("What do you wish to do?"+CR$+CR$)
	IF DL%>1 THEN ZZ%=FNW%("<U> Teleport up 1 level"+CR$)
	ZZ%=FNW%("<D> Teleport down 1 level"+CR$)
	ZZ%=FNW%("<O> Teleport out of the dungeon"+CR$)
	ZZ%=FNW%("<R> Random teleport"+CR$)
	ZZ%=FNW%("<S> Stay on this level"+CR$+CR$)
	ZZ%=FNW%("Teleport: ")
	IF FNR% THEN RETURN Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$)
	IF I$="U" AND DL%>1 THEN
		DL%=DL%-1
		ZZ%=FNW%("Now teleporting up to level"+STR$(DL%)+"."+CR$+CR$)
		ZZ%=FNW%("Everything spins around, and suddenly you're elsewhere..."+CR$+CR$)
		RETURN NextLevel
	END IF
	IF I$="D" THEN
		DL%=DL%+1
		ZZ%=FNW%("Now teleporting down to level"+STR$(DL%)+"."+CR$+CR$)
		ZZ%=FNW%("Everything spins around, and suddenly you're elsewhere..."+CR$+CR$)
		RETURN NextLevel
	END IF
	IF I$="O" THEN
		ZZ%=FNW%("Now teleporting out of the dungeon."+CR$+CR$)
		ZZ%=FNW%("As you leave, you hear a voice boom, 'Next time, you will not escape so easily!"+CR$)
		ZZ%=FNW%("Moohahahaha!...'"+CR$)
		CHAIN "MAIN"
	END IF
	IF I$="R" THEN
		ZZ%=FNW%("Everything spins around, and suddenly you're elsewhere..."+CR$+CR$)
		FOR I%=1 TO 100:DY%=FND(7):DX%=FND(7):NEXT I%
		RETURN
	END IF
	IF I$<>"S" THEN GOTO Teleport
	RETURN

Cleric:
	ZZ%=FNW%("There is an old cleric in this room."+CR$+CR$)
	NME.GOLD#=INT(WC(1)*(CVI(USER.HP$)-THP%)*DL%):M!=NME.GOLD#
	ZZ%=FNW%("He says, 'I can heal all of your wounds for"+STR$(M!)+" gold pieces."+CR$+CR$)
	ZZ%=FNW%("Do you accept his offer (Y/N)? ")
	IF FNR% THEN RETURN Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$)
	IF I$="Y" THEN
		IF NME.GOLD#>CVD(USER.GOLD$) THEN ZZ%=FNW%("You don't have enough!"+CR$):RETURN
		THP%=CVI(USER.HP$)
		LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-NME.GOLD#)
		PUT #2,USER%
		ZZ%=FNW%("He casts a Cure spell on you."+CR$)
		RETURN
	END IF
	IF I$="N" THEN
		ZZ%=FNW%("He says, 'I shall pray for you.'"+CR$)
		RETURN
	END IF
	GOTO Cleric

Cast:
	SP%=0
	IF CL%<>2 AND CL%<>4 AND CL%<>5 AND CL%<>7 AND CL%<>9 AND CL%<>11 THEN
		IF FNW%("You are not a spell caster!"+CR$) THEN RETURN Hungup
		RETURN
	END IF
	IF CVI(USER.SPELL$)=0 THEN
		IF FNW%("You don't have any spells!"+CR$) THEN RETURN Hungup
		RETURN
	END IF
	IF FNW%("Enter spell number (?=Menu): ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	IF FNW%(CR$+CR$) THEN RETURN Hungup
	IF I$="?" THEN
		FOR I%=0 TO 11
			IF (CVI(USER.SPELL$) AND 2^I%) THEN IF FNW%(RIGHT$(" <"+MID$(STR$(I%+1),2)+"> ",5)+LEFT$(SPELL$(I%+1)+SPACE$(20),20)+STR$(SP%(I%+1+(CL%=9 OR CL%=11)))+CR$) THEN RETURN Hungup
		NEXT I%
		IF FNW%(CR$) THEN RETURN Hungup
		GOTO Cast
	END IF
	SP%=VAL(I$):IF SP%<1 THEN RETURN
	IF SP%>12 THEN GOTO Cast
	IF (CVI(USER.SPELL$) AND 2^(SP%-1))=0 THEN ZZ%=FNW%("You don't know that spell!"+CR$+CR$):GOTO Cast
	IF SP%(SP%+(CL%=9 OR CL%=11))>TSP% THEN ZZ%=FNW%("You don't have enough spell power to cast that spell!"+CR$+CR$):GOTO Cast
	IF SP%=9 AND NME.SEX$="" THEN ZZ%=FNW%("You can only blast during battle."+CR$+CR$):GOTO Cast
	IF SP%=10 AND NME.SEX$<>"" THEN ZZ%=FNW%("Resurrecting the dead won't help you now!"+CR$+CR$):GOTO Cast
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
	IF SP%=7 THEN
		IF NME.NAME$="" THEN GOTO Teleport
		ZZ%=FNW%("You randomly teleport away from the battle."+CR$)
		DY%=FND(7):DX%=FND(7)
	END IF
	IF SP%=8 THEN
		A%=0:FOR I%=1 TO ASC(USER.LEVEL$):A%=A%+FND(15)-2*(CL%=2):NEXT I%
		THP%=THP%+A%
		ZZ%=FNW%("Hit points = Hit points +"+STR$(A%)+CR$)
	END IF
	IF SP%=9 THEN
		A%=0:FOR I%=1 TO ASC(USER.LEVEL$):A%=A%+FND(17-17*(NME.CL%=8)):NEXT I%
		NME.HP%=NME.HP%-A%
		ZZ%=FNW%("You blast "+HIM$+" for"+STR$(A%)+" hit points of damage."+CR$)
	END IF
	IF SP%=10 THEN
		REC%=0
		WHILE REC%=0
			ZZ%=FNW%("Resurrect who? ")
			REC%=FNUSER%
		WEND
		GET #2,REC%
		ZZ%=FNW%(CR$+"Now raising "+FNL$(USER.HANDLE$)+"...")
		LSET USER.STATUS$=CHR$(0)
		PUT #2,REC%
		GET #2,USER%
		ZZ%=FNW%("Done."+CR$)
	END IF
	IF SP%=11 THEN
		THP%=CVI(USER.HP$)
		ZZ%=FNW%("You feel your vitality completely restored."+CR$)
	END IF
	IF SP%=12 THEN
		NME.HP%=0
		IF FND(10)>1 THEN ZZ%=FNW%(NME.NAME$+" is completely atomized."+CR$) ELSE ZZ%=FNW%(NME.NAME$+" lights up like a Roman candle."+CR$)
	END IF
	RETURN

Poison:
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

Map:
	IF NOT MAP% THEN ZZ%=FNW%("You don't have a map of this level!"+CR$):RETURN
	CLS 2:IF LOCL%=0 THEN PRINT #1,CHR$(27);"[H";CHR$(27);"[J";
	FOR I%=1 TO 7:FOR J%=1 TO 7
		IF I%=DY% AND J%=DX% THEN
			IF LOCL%=0 THEN PRINT #1,CHR$(27);"[7m";
			COLOR 0,7:ZZ%=FNW%(" YOU "):COLOR 7,0
			IF LOCL%=0 THEN PRINT #1,CHR$(27);"[m";
		ELSE
			IF DUNGEON%(I%,J%)>8 THEN COLOR 15,0:IF LOCL%=0 THEN PRINT #1,CHR$(27);"[1m";
			ZZ%=FNW%(MID$(" Emp  Cav  Mon  Mon  ???  N-S  E-W  ???  TEL  CLR ",5*(DUNGEON%(I%,J%)-1)+1,5))
			IF DUNGEON%(I%,J%)>8 THEN COLOR 7,0:IF LOCL%=0 THEN PRINT #1,CHR$(27);"[m";
		END IF
	NEXT J%:ZZ%=FNW%(CR$+CR$):NEXT I%
	RETURN

YourStats:
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
	M!=CVD(USER.GOLD$):ZZ%=FNW%("Gold:"+STR$(M!)+CR$)
	RETURN

BadGuys:
	DATA goblin,1,orc,1,kobold,1,hobgoblin,7,bullywug,1,xvart,1,caveman,1,norker,1,skeleton,1,zombie,1
	DATA giant centipede,1,gnoll,1,stirge,1,troglodyte,1,lizard man,1,crabman,1,mongrelman,1,orgrillon,8,githzerai,5,kuo-toa,5
	DATA bugbear,8,ghoul,4,ogre,8,firedrake,4,drow,4,firenewt,4,harpy,1,ophidian,1,phantom,4,worg,1
	DATA gargoyle,4,rust monster,1,ghast,4,werewolf,4,owlbear,1,firetoad,4,hall hound,4,hook horror,1,anhkheg,6,githyanki,5
	DATA cave bear,8,cockatrice,4,minotaur,8,displacer beast,1,doppleganger,0,imp,4,quasit,4,ice lizard,1,svirfneblin,1,yeti,1
	DATA carrion crawler,1,manticore,1,troll,4,wight,4,wraith,4,basilisk,1,wyvern,8,medusa,4,drider,1,ogre mage,4
	DATA hill giant,8,tunnel worm,1,hydra,8,mimic,1,succubus,5,mind flayer,5,mummy,4,neo-otyugh,8,roper,1,umber hulk,8
	DATA pyrohydra,8,will-o-wisp,7,vampire,5,ghost,4,dracolisk,5,naga,4,xag-ya,4,xeg-yi,4,minor demon,4,green dragon,5
	DATA red dragon,5,stone golem,8,nycadaemon,5,titan,8,demilich,9,pit fiend,5,lernaean hydra,8,major demon,5,mist dragon,5,grey slaad,4
	DATA beholder,5,iron golem,8,death slaad,4,cloud dragon,5,lich,9,elder titan,8,slaad lord,5,demon prince,5,arch devil,5,elemental prince,11
