	'$INCLUDE: 'COMMON.BAS'

	GET #2,USER%:HS%=CVI(USER.HULL$)
	IF FNW%(CR$) THEN GOTO Hungup
	IF FNW%("<S> Shipyards"+CR$) THEN GOTO Hungup
	IF FNW%("<B> Battle other users"+CR$) THEN GOTO Hungup
	IF FNW%("<H> Hunt Sea Monsters"+CR$) THEN GOTO Hungup
	IF FNW%("<F> Go fishing"+CR$) THEN GOTO Hungup
	IF FNW%("<Y> Your ship's status"+CR$) THEN GOTO Hungup
	IF FNW%("<L> List users ships"+CR$) THEN GOTO Hungup
	IF FNW%(CR$+"[Naval] Option (Q=Quit): ") THEN GOTO Hungup
	IF FNT% THEN GOTO Expired
	IF FNR% THEN GOTO Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$)
	IF I$="Q" THEN CHAIN "MAIN"
	F%=-(I$="S")-2*(I$="B")-3*(I$="H")-4*(I$="F")-5*(I$="Y")-6*(I$="L")
	ON F% GOSUB Shipyards,Battle,Hunt,Fishing,Status,Users
	GOTO Start

Shipyards:
	IF FNW%("<B> Buy a new ship"+CR$) THEN RETURN Hungup
	IF FNW%("<F> Fix battle damage"+CR$) THEN RETURN Hungup
	IF FNW%("<C> Mount cannons"+CR$) THEN RETURN Hungup
	IF FNW%("<R> Mount a ram"+CR$) THEN RETURN Hungup
	IF FNW%(CR$+"[Shipyards] Option (Q=Quit): ") THEN GOTO Hungup
	IF FNT% THEN RETURN Expired
	IF FNR% THEN RETURN Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$)
	IF I$="Q" THEN RETURN
	F%=-(I$="B")-2*(I$="F")-3*(I$="C")-4*(I$="R")
	ON F% GOSUB Buy,Repair,Cannons,Ram
	GOTO Shipyards
Buy:
	IF HS%+50>9999 THEN ZZ%=FNW%("They don't make ships any bigger than the one you have now."+CR$+CR$):RETURN
	ZZ%=FNW%("List of affordable ships:"+CR$+CR$)
	FOR AFFORD%=HS%+50 TO 9950 STEP 50
		COST#=INT(2^(AFFORD%/150)*7937*WC(1)+.5):M!=COST#
		IF COST#>CVD(USER.GOLD$) THEN AFFORD%=AFFORD%-50:EXIT FOR
		IF FNW%(LEFT$("Hull size:"+STR$(AFFORD%)+SPACE$(20),20)+"Cost:"+STR$(M!)+CR$) THEN RETURN Hungup
	NEXT AFFORD%
	IF FNW%("Enter size to buy: ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	ZZ%=FNW%(CR$+CR$):IF I$="" THEN RETURN
	HULL%=VAL(I$)
	IF (HULL% MOD 50) THEN ZZ%=FNW%("We don't make ships with that hull size, but only in multiples of 50."+CR$+CR$):RETURN
	IF HULL%<=HS% THEN ZZ%=FNW%("You already have a"+STR$(HS%)+" hull size ship!"+CR$+CR$):RETURN
	IF HULL%>9950 THEN ZZ%=FNW%("We don't make ships that big!"+CR$+CR$):RETURN
	COST#=INT(2^(HULL%/150)*7937*WC(1)+.5)
	IF COST#>CVD(USER.GOLD$) THEN ZZ%=FNW%("You can't afford that hull size!"+CR$+CR$):RETURN
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-COST#)
	LSET USER.HULL$=MKI$(HULL%):HS%=HULL%:THULL%=HS%
	LSET USER.RAM$="N"
	PUT #2,USER%
	ZZ%=FNW%("You now have a brand new"+STR$(HS%)+" hull point ship, with no ram."+CR$+CR$)
	RETURN
Repair:
	IF HS%=0 THEN ZZ%=FNW%("You don't have a ship!"+CR$+CR$):RETURN
	ZZ%=FNW%("You need"+STR$(HS%-THULL%)+" hull points of repair."+CR$)
	COST#=INT(2^(HS%/150)*7937*WC(1)+.5):COST#=INT(COST#/HS%/10):M!=COST#
	ZZ%=FNW%("Each hull point costs"+STR$(M!)+" gold pieces."+CR$+CR$)
	M!=INT(CVD(USER.GOLD$)/COST#):IF M!>(HS%-THULL%) THEN M!=HS%-THULL%
	AFFORD%=M!
	ZZ%=FNW%("How many points (MAX="+MID$(STR$(AFFORD%),2)+")? ")
	IF FNR% THEN RETURN Hungup
	ZZ%=FNW%(CR$+CR$)
	I%=VAL(I$):IF UCASE$(I$)="MAX" THEN I%=AFFORD%
	IF I%<1 THEN RETURN
	IF I%>AFFORD% THEN ZZ%=FNW%("You can't afford THAT many!"+CR$+CR$)
	THULL%=THULL%+I%
	COST#=INT(2^(HS%/150)*7937*WC(1)+.5):COST#=INT(COST#/HS%/10)*I%
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-COST#)
	PUT #2,USER%
	ZZ%=FNW%("Hull points ="+STR$(THULL%)+CR$+CR$)
	RETURN
Cannons:
	IF HS%=0 THEN ZZ%=FNW%("You don't have a ship!"+CR$+CR$):RETURN
	ZZ%=FNW%("You can mount up to"+STR$(HS%/50-ASC(USER.CANNON$))+" more cannons."+CR$)
	COST#=INT(2^(HS%/150)*7937*WC(1)+.5):COST#=INT(COST#/250):M!=COST#
	ZZ%=FNW%("Each cannon costs"+STR$(M!)+" gold pieces."+CR$+CR$)
	M!=INT(CVD(USER.GOLD$)/COST#):IF M!>(HS%/50-ASC(USER.CANNON$)) THEN M!=HS%/50-ASC(USER.CANNON$)
	AFFORD%=M!
	ZZ%=FNW%("How many cannons (MAX="+MID$(STR$(AFFORD%),2)+")? ")
	IF FNR% THEN RETURN Hungup
	ZZ%=FNW%(CR$+CR$)
	I%=VAL(I$):IF UCASE$(I$)="MAX" THEN I%=AFFORD%
	IF I%<1 THEN RETURN
	IF I%>AFFORD% THEN ZZ%=FNW%("You can't afford THAT many!"+CR$+CR$):RETURN
	LSET USER.CANNON$=CHR$(ASC(USER.CANNON$)+I%)
	COST#=INT(2^(HS%/150)*7937*WC(1)+.5):COST#=INT(COST#/250)*I%
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-COST#)
	PUT #2,USER%
	ZZ%=FNW%("Cannons ="+STR$(ASC(USER.CANNON$))+CR$+CR$)
	RETURN
Ram:
	IF HS%=0 THEN ZZ%=FNW%("You don't have a ship!"+CR$+CR$):RETURN
	IF USER.RAM$="Y" THEN ZZ%=FNW%("But your ship already has a ram!"+CR$+CR$):RETURN
	COST#=INT(2^(HS%/150)*7937*WC(1)+.5):COST#=INT(COST#/10):M!=COST#
	ZZ%=FNW%("We can equip your ship with a ram for"+STR$(M!)+" gold pieces."+CR$+CR$)
	IF COST#>CVD(USER.GOLD$) THEN ZZ%=FNW%("You don't have enough gold!"+CR$+CR$):RETURN
	ZZ%=FNW%("Ok (Y/N)? ")
	IF FNR% THEN RETURN Hungup
	ZZ%=FNW%(CR$+CR$)
	IF UCASE$(I$)<>"Y" THEN RETURN
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-COST#)
	LSET USER.RAM$="Y"
	PUT #2,USER%
	ZZ%=FNW%("You now have a ram."+CR$+CR$)
	RETURN

Battle:
	IF HS%=0 THEN ZZ%=FNW%("You don't have a ship!"+CR$+CR$):RETURN
	IF NAVAL%>2 THEN ZZ%=FNW%("You have run out of battles."+CR$+CR$):RETURN
	IF ASC(USER.ACCESS$)<4 THEN ZZ%=FNW%("You are not allowed to battle other users."+CR$+CR$):RETURN
	ZZ%=FNW%("Battle which user? ")
	NME%=FNUSER%:ZZ%=FNW%(CR$)
	IF NME%<1 THEN RETURN
	NME.NAME$=FNL$(USER.HANDLE$):NME.GOLD#=CVD(USER.GOLD$):NME.INT%=ASC(USER.INT$)
	NME.HS%=CVI(USER.HULL$):NME.CANNON%=ASC(USER.CANNON$):NME.RAM$=USER.RAM$
	IF NME.HS%=0 THEN ZZ%=FNW%(NME.NAME$+" does not have a ship."+CR$+CR$):RETURN
	ZZ%=FNW%("You sail out until you spot "+NME.NAME$+"'s ship on the horizon."+CR$+CR$)
	ZZ%=FNW%("It has"+STR$(NME.HS%)+" hull points and is armed with"+STR$(NME.CANNON%)+" cannons."+CR$+CR$)
	IF NME.RAM$="Y" THEN ZZ%=FNW%("It also has a ram."+CR$+CR$)
	ZZ%=FNW%("Continue (Y/N)? ")
	IF FNR% THEN RETURN Hungup
	ZZ%=FNW%(CR$+CR$)
	IF UCASE$(I$)<>"Y" THEN GOTO Battle
	NAVAL%=NAVAL%+1:NME.MAX%=NME.HS%
	GET #2,USER%
	IF (FND(100)+NME.INT%)<=(FND(100)+ASC(USER.INT$)) THEN ZZ%=FNW%("You approach them and quickly open fire."+CR$+CR$):I$="F":GOTO YOURturn
	ZZ%=FNW%("They spot you coming and open fire."+CR$+CR$):GOTO NMEturn
Bloop:
	GET #2,USER%
	ZZ%=FNW%("<F> Fire cannons, <R> Ram their ship, <S> Sail off, <Y> Your status: ")
	IF FNR% THEN RETURN Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$)
	IF I$<>"F" AND I$<>"R" AND I$<>"S" AND I$<>"Y" THEN GOTO Bloop
	IF I$="Y" THEN
		ZZ%=FNW%("Hull points:"+STR$(THULL%)+CR$)
		ZZ%=FNW%("Cannons:"+STR$(ASC(USER.CANNON$))+CR$)
		ZZ%=FNW%("Ram: "+MID$("YesNo",1-3*(USER.RAM$<>"Y"),3)+CR$+CR$)
		GOTO Bloop
	END IF
	IF I$="S" THEN
		IF FND(100)>(INT(NME.HS%/(NME.HS%+THULL%)*50)+50) THEN ZZ%=FNW%("They outmaneuver you and stop your retreat!"+CR$+CR$):GOTO NMEturn
		ZZ%=FNW%("You sail away safely out of range."+CR$+CR$)
		RETURN
	END IF
YOURturn:
	IF I$="F" THEN
		GET #2,USER%
		ZZ%=FNW%("Attacker: ")
		SHOTS%=ASC(USER.CANNON$):C%=NME.CANNON%:RAM$=NME.RAM$:GOSUB Volley
		ZZ%=FNW%(CR$+"You hit them"+STR$(HIT%)+" times for"+STR$(HULL%)+" hull points of damage."+CR$):NME.HS%=NME.HS%-HULL%
		IF CANNON% THEN ZZ%=FNW%("You also hit"+STR$(CANNON%)+" of their cannons."+CR$):NME.CANNON%=NME.CANNON%-CANNON%
		IF RAM% THEN ZZ%=FNW%("You also hit their ram."+CR$):NME.RAM$="N"
		GET #2,NME%
		LSET USER.CANNON$=CHR$(NME.CANNON%):LSET USER.RAM$=NME.RAM$
		PUT #2,NME%
		ZZ%=FNW%(CR$)
	END IF
	IF I$="R" THEN
		IF USER.RAM$<>"Y" THEN ZZ%=FNW%("You don't have a ram!"+CR$+CR$):GOTO Bloop
		IF FND(100)>INT(NME.HS%/(NME.HS%+THULL%)*100) THEN ZZ%=FNW%("They quickly outmaneuver your ship."+CR$+CR$):GOTO NMEturn
		D%=FND(INT(HS%/2))+FND(INT(THULL%/2))
		ZZ%=FNW%("You ram them for"+STR$(D%)+" hull points of damage!"+CR$+CR$)
		NME.HS%=NME.HS%-D%
	END IF
	IF NME.HS%<1 THEN GOTO NMEdies
NMEturn:
	I$="F"
	IF NME.CANNON%=0 THEN I$="S":ZZ%=FNW%("They are defenseless and attempt to flee..."+CR$+CR$)
	IF NME.HS%<THULL% AND FND(20)=1 THEN I$="S":ZZ%=FNW%(NME.NAME$+" acknowledges your superior tactics and tries to retreat..."+CR$+CR$)
	IF NME.RAM$="Y" AND FND(5)=1 THEN I$="R"
	IF I$="S" THEN
		SLEEP 2
		IF FND(100)>(INT(THULL%/(THULL%+NME.HS%)*50)+50) THEN ZZ%=FNW%("You outmaneuver them and stop their retreat!"+CR$+CR$):GOTO Bloop
		ZZ%=FNW%("They sail away over the horizon."+CR$+CR$)
		RETURN
	END IF
	IF I$="F" THEN
		GET #2,USER%
		ZZ%=FNW%("Defender: ")
		SHOTS%=NME.CANNON%:C%=ASC(USER.CANNON$):RAM$=USER.RAM$:GOSUB Volley
		ZZ%=FNW%(CR$+"They hit you"+STR$(HIT%)+" times for"+STR$(HULL%)+" hull points of damage."+CR$):THULL%=THULL%-HULL%
		IF CANNON% THEN ZZ%=FNW%("They also hit"+STR$(CANNON%)+" of your cannons."+CR$):LSET USER.CANNON$=CHR$(C%-CANNON%)
		IF RAM% THEN ZZ%=FNW%("They also hit your ram."+CR$):LSET USER.RAM$="N"
		PUT #2,USER%
		ZZ%=FNW%(CR$)
	END IF
	IF I$="R" THEN
		ZZ%=FNW%("They attempt to ram you!"+CR$+CR$):SLEEP 2
		IF FND(100)>INT(THULL%/(THULL%+NME.HS%)*100) THEN ZZ%=FNW%("You quickly outmaneuver their ship."+CR$+CR$):GOTO Bloop
		D%=FND(INT(NME.MAX%/2))+FND(INT(NME.HS%/2))
		ZZ%=FNW%("They ram you for"+STR$(D%)+" hull points of damage!"+CR$+CR$)
		THULL%=THULL%-D%
	END IF
	IF THULL%<1 THEN GOTO YOUdie
	GOTO Bloop
NMEdies:
	D%=FND(5)
	IF D%=1 THEN ZZ%=FNW%(CR$+"You've sunk "+NME.NAME$+"'s ship!"+CR$+CR$)
	IF D%=2 THEN ZZ%=FNW%(CR$+"You've sunk "+NME.NAME$+"'s leaky, old tub!"+CR$+CR$)
	IF D%=3 THEN ZZ%=FNW%(CR$+"You've made splinters out of "+NME.NAME$+"'s ship!"+CR$+CR$)
	IF D%=4 THEN ZZ%=FNW%(CR$+NME.NAME$+" is now sleeping with the fishes!"+CR$+CR$)
	IF D%=5 THEN ZZ%=FNW%(CR$+NME.NAME$+" is now chum for the sharks!"+CR$+CR$)
	GET #2,NME%
	LSET USER.GOLD$=MKD$(0)
	PUT #2,NME%
	IF HS%>=CVI(USER.HULL$) THEN I%=CVI(USER.HULL$) ELSE I%=HS%
	COST#=INT(2^(I%/150)*7937*WC(1)+.5):COST#=INT(COST#/250)
	NME.GOLD#=NME.GOLD#+INT(COST#*TCHR%/100)*ASC(USER.CANNON$)
	M!=NME.GOLD#:ZZ%=FNW%("You get"+STR$(M!)+" gold pieces of booty!"+CR$+CR$)
	GET #2,USER%
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+NME.GOLD#)
	PUT #2,USER%
	OPEN "A",#4,"NOTES\ARENA"+MID$(STR$(NME%),2)+".MSG"
	PRINT #4,"-=>*<=-"
	PRINT #4,FNL$(USER.HANDLE$)+" sunk your ship and got"+STR$(M!)+" gold pieces!"
	PRINT #4,""
	CLOSE #4
	RETURN
YOUdie:
	ZZ%=FNW%(NME.NAME$+" smiles as a shark approaches you..."+CR$)
	GET #2,USER%
	NME.GOLD#=CVD(USER.GOLD$):M!=NME.GOLD#
	LSET USER.GOLD$=MKD$(0)
	PUT #2,USER%
	GET #2,NME%
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+NME.GOLD#)
	PUT #2,NME%
	GET #2,USER%
	OPEN "A",#4,"NOTES\ARENA"+MID$(STR$(NME%),2)+".MSG"
	PRINT #4,"-=>*<=-"
	PRINT #4,"You've sunk "+FNL$(USER.HANDLE$)+"'s ship and got"+STR$(M!)+" gold pieces!"
	PRINT #4,""
	CLOSE #4
	RETURN Logoff

Hunt:
	IF HS%=0 THEN ZZ%=FNW%("You don't have a ship!"+CR$+CR$):RETURN
	IF NAVAL%>2 THEN ZZ%=FNW%("You have run out of battles."+CR$+CR$):RETURN
	IF ASC(USER.ACCESS$)<4 THEN ZZ%=FNW%("You are not allowed hunt sea monsters."+CR$+CR$):RETURN
	RESTORE SeaMonster
	FOR I%=1 TO 5
		READ NME.NAME$,NME.INT%,NME.HS%,NME.SHOTS%,NME.POWER%,NME.RAM$,NME.GOLD#
		ZZ%=FNW%("<"+MID$(STR$(I%),2)+"> "+NME.NAME$+CR$)
	NEXT I%
	ZZ%=FNW%(CR$+"Hunt what monster? ")
	IF FNR% THEN RETURN Hungup
	ZZ%=FNW%(CR$+CR$)
	N%=VAL(I$):IF N%<1 OR N%>5 THEN RETURN
	RESTORE SeaMonster:FOR I%=1 TO N%:READ NME.NAME$,NME.INT%,NME.HS%,NME.SHOTS%,NME.POWER%,NME.RAM$,NME.GOLD#:NEXT I%
	ZZ%=FNW%("You sail out until you spot a "+NME.NAME$+" on the horizon."+CR$+CR$)
	ZZ%=FNW%("It has"+STR$(NME.HS%)+" hull points."+CR$+CR$)
	ZZ%=FNW%("Continue (Y/N)? ")
	IF FNR% THEN RETURN Hungup
	ZZ%=FNW%(CR$+CR$)
	IF UCASE$(I$)<>"Y" THEN GOTO Hunt
	NAVAL%=NAVAL%+1:NME.MAX%=NME.HS%
	IF (FND(100)+NME.INT%)<=(FND(100)+ASC(USER.INT$)) THEN ZZ%=FNW%("You approach it and quickly open fire."+CR$+CR$):I$="F":GOTO YOURhunt
	ZZ%=FNW%("It spots you coming and open fire."+CR$+CR$):GOTO NMEhunt
Hloop:
	ZZ%=FNW%("<F> Fire cannons, <R> Ram it, <S> Sail off, <Y> Your status: ")
	IF FNR% THEN RETURN Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$)
	IF I$<>"F" AND I$<>"R" AND I$<>"S" AND I$<>"Y" THEN GOTO Hloop
	IF I$="Y" THEN
		ZZ%=FNW%("Hull points:"+STR$(THULL%)+CR$)
		ZZ%=FNW%("Cannons:"+STR$(ASC(USER.CANNON$))+CR$)
		ZZ%=FNW%("Ram: "+MID$("YesNo",1-3*(USER.RAM$<>"Y"),3)+CR$+CR$)
		GOTO Hloop
	END IF
	IF I$="S" THEN
		IF FND(100)>(INT(NME.HS%/(NME.HS%+THULL%)*50)+50) THEN ZZ%=FNW%("They outmaneuver you and stop your retreat!"+CR$+CR$):GOTO NMEhunt
		ZZ%=FNW%("You sail away safely out of range."+CR$+CR$)
		RETURN
	END IF
YOURhunt:
	IF I$="F" THEN
		ZZ%=FNW%("Attacker: ")
		SHOTS%=ASC(USER.CANNON$):C%=NME.CANNON%:RAM$=NME.RAM$
		HIT%=0:HULL%=0:CANNON%=0:RAM%=0
		FOR I%=1 TO SHOTS%
			N%=FND(100):N%=-(N%>74)
			IF N%=1 THEN HIT%=HIT%+1:D%=FND(25):HULL%=HULL%+D%
			ZZ%=FNW%(MID$("-*",N%+1,1))
			FOR J%=1 TO 800:NEXT J%
		NEXT I%
		ZZ%=FNW%(CR$+"You hit it"+STR$(HIT%)+" times for"+STR$(HULL%)+" hull points of damage."+CR$+CR$):NME.HS%=NME.HS%-HULL%
	END IF
	IF I$="R" THEN
		IF USER.RAM$<>"Y" THEN ZZ%=FNW%("You don't have a ram!"+CR$+CR$):GOTO Hloop
		IF FND(100)>INT(NME.HS%/(NME.HS%+THULL%)*100) THEN ZZ%=FNW%("It quickly outmaneuvers your ship."+CR$+CR$):GOTO NMEhunt
		D%=FND(INT(HS%/2))+FND(INT(THULL%/2))
		ZZ%=FNW%("You ram it for"+STR$(D%)+" hull points of damage!"+CR$+CR$)
		NME.HS%=NME.HS%-D%
	END IF
	IF NME.HS%<1 THEN
		M!=NME.GOLD#
		ZZ%=FNW%("You get"+STR$(M!)+" for bringing home the carcass."+CR$+CR$)
		LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+NME.GOLD#)
		PUT #2,USER%:LSET USER.DISK$=USER.REC$:PUT #2,USER%
		RETURN
	END IF
NMEhunt:
	I$="F"
	IF NME.RAM$="Y" AND FND(5)=1 THEN I$="R"
	IF I$="F" THEN
		HULL%=0:FOR I%=1 TO NME.SHOTS%:HULL%=HULL%+FND(NME.POWER%)+FND(NME.POWER%):NEXT I%
		ZZ%=FNW%(CR$+"The "+NME.NAME$+" attacks your ship, causing"+STR$(HULL%)+" hull points of damage."+CR$+CR$):THULL%=THULL%-HULL%
	END IF
	IF I$="R" THEN
		ZZ%=FNW%("The "+NME.NAME$+" attempts to ram you!"+CR$+CR$):SLEEP 2
		IF FND(100)>INT(THULL%/(THULL%+NME.HS%)*100) THEN ZZ%=FNW%("You quickly outmaneuver it."+CR$+CR$):GOTO Hloop
		D%=FND(INT(NME.MAX%/2))+FND(INT(NME.HS%/2))
		ZZ%=FNW%("The "+NME.NAME$+" rams you for"+STR$(D%)+" hull points of damage!"+CR$+CR$)
		THULL%=THULL%-D%
	END IF
	IF THULL%<1 THEN
		ZZ%=FNW%("The "+NME.NAME$+" has sunk your ship!"+CR$)
		GET #2,USER%
		LSET USER.GOLD$=MKD$(0)
		PUT #2,USER%
		RETURN Logoff
	END IF
	GOTO Hloop

Fishing:
	IF HS%=0 THEN ZZ%=FNW%("You don't have a ship!"+CR$+CR$):RETURN
	IF ASC(USER.ACCESS$)<4 THEN ZZ%=FNW%("You are not allowed to fish."+CR$+CR$):RETURN
	ZZ%=FNW%("It is a fine day for sailing.  You cast your reel into the ocean and feel"+CR$)
	ZZ%=FNW%("a gentle tug..."):SLEEP 3
	N%=FND(TCHR%)
	ZZ%=FNW%("you caught a"):SLEEP 2
	IF N%<15 THEN
		ZZ%=FNW%(" fish and you eat it."+CR$):SLEEP 2
		ZZ%=FNW%("Ugh!  You feel sick and die!"+CR$)
		RETURN Logoff
	END IF
	IF N%<49 THEN
		ZZ%=FNW%(" fish and you eat it."+CR$):SLEEP 2
		ZZ%=FNW%("Yum!  You feel stronger and healthier."+CR$)
		TSTR%=TSTR%+10:IF TSTR%>100 THEN TSTR%=100
		THP%=THP%+INT(CVI(USER.HP$)/(FND(11)+9))
		IF TSP% THEN TSP%=TSP%+INT(CVI(USER.SP$)/(FND(11)+9))
		RETURN
	END IF
	IF N%<74 THEN
		ZZ%=FNW%("n oyster and you eat it."+CR$):SLEEP 2
		ZZ%=FNW%("Ouch!  You bit into a pearl."+CR$)
		COST#=INT(2^(HS%/150)*FND(7937)*WC(1)+.5):M!=COST#
		LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+COST#)
		PUT #2,USER%
		ZZ%=FNW%("You sell it for"+STR$(M!)+" gold pieces."+CR$)
		RETURN
	END IF
	IF N%<89 THEN
		ZZ%=FNW%("n oyster and you eat it."+CR$):SLEEP 2
		ZZ%=FNW%("Ouch!  You bit into a diamond."+CR$)
		COST#=10*INT(2^(HS%/150)*FND(7937)*WC(1)+.5):M!=COST#
		LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+COST#)
		PUT #2,USER%
		ZZ%=FNW%("You sell it for"+STR$(M!)+" gold pieces."+CR$)
		RETURN
	END IF
	IF N%<94 THEN
		ZZ%=FNW%(" turtle and you let it go."+CR$):SLEEP 2
		ZZ%=FNW%("The turtle turns and smiles and enhances your "+ARMOR$(ASC(USER.ARMOR$))+"."+CR$)
		TSHIELD%=TSHIELD%+ASC(USER.ARMOR$)/5+1
		RETURN
	END IF
	IF N%<99 THEN
		ZZ%=FNW%(" turtle and you let it go."+CR$):SLEEP 2
		ZZ%=FNW%("The turtle turns and smiles and enhances your "+WEAPON$(ASC(USER.WEAPON$))+"."+CR$)
		THIT%=THIT%+ASC(USER.WEAPON$)/5+1
		RETURN
	END IF
	ZZ%=FNW%(" mermaid!  "):SLEEP 2
	ZZ%=FNW%("She grants you an extra call for today!"+CR$)
	IF ASC(USER.CALLS$)>0 THEN LSET USER.CALLS$=CHR$(ASC(USER.CALLS$)-1)
	PUT #2,USER%
	RETURN

Status:
	IF HS%=0 THEN ZZ%=FNW%("You don't have a ship!"+CR$+CR$):RETURN
	ZZ%=FNW%("Ship's Status:"+CR$+CR$)
	ZZ%=FNW%("Hull points:"+STR$(THULL%)+" out of"+STR$(HS%)+CR$)
	ZZ%=FNW%("Cannons:"+STR$(ASC(USER.CANNON$))+CR$)
	ZZ%=FNW%("Ram: "+MID$("YesNo",1-3*(USER.RAM$<>"Y"),3)+CR$)
	RETURN

Users:
	ZZ%=FNW%("Num              Username               Size      Cannons      Ram"+CR$)
	ZZ%=FNW%("---      -------------------------      ----      -------      ---"+CR$)
	FOR REC%=1 TO 96
		GET #2,REC%
		IF CVI(USER.HULL$) THEN
			I$=MID$("YesNo",1-3*(USER.RAM$<>"Y"),3)
			ZZ%=FNW%(RIGHT$(" "+MID$(STR$(REC%),2)+".",3)+SPACE$(6)+LEFT$(FNL$(USER.HANDLE$)+SPACE$(31),31)+RIGHT$("   "+MID$(STR$(CVI(USER.HULL$)),2),4)+SPACE$(6)+RIGHT$("    "+MID$(STR$(ASC(USER.CANNON$)),2),5)+SPACE$(8)+I$+CR$)
		END IF
	NEXT REC%
	ZZ%=FNW%(CR$+"Press RETURN: ")
	IF FNR% THEN RETURN Hungup
	ZZ%=FNW%(CR$+CR$)
	RETURN

Volley:
	HIT%=0:HULL%=0:CANNON%=0:RAM%=0
	FOR I%=1 TO SHOTS%
		N%=FND(100):N%=-(N%>65)-(N%>94)-(N%>98)
		IF N%=3 AND RAM$<>"Y" THEN N%=2
		IF N%=2 AND C%=0 THEN N%=1
		IF N%=1 THEN HIT%=HIT%+1:D%=FND(50):HULL%=HULL%+D%
		IF N%=2 THEN CANNON%=CANNON%+1:C%=C%-1
		IF N%=3 THEN RAM%=1:RAM$="N"
		ZZ%=FNW%(MID$("-*@^",N%+1,1))
		FOR J%=1 TO 1000:NEXT J%
	NEXT I%
	RETURN

SeaMonster:
	DATA Mad Whale,60,250,1,125,Y,1E+07
	DATA Giant Sea Turtle,30,500,1,250,Y,1E+08
	DATA Sea Dragon,90,1000,2,250,Y,1E+09
	DATA Kraken,50,2000,2,1000,N,5E+14
	DATA Marine Titan,99,3000,2,1500,Y,5E+16
