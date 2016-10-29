	'$INCLUDE: 'COMMON.BAS'

	GET #2,USER%
Top:
	PUT #2,USER%
	IF FNW%(CR$) THEN GOTO Hungup
	IF FNW%("<A> Armoury"+CR$) THEN GOTO Hungup
	IF FNW%("<W> Weapons Shoppe"+CR$) THEN GOTO Hungup
	IF FNW%("<R> Real Estate"+CR$) THEN GOTO Hungup
	IF FNW%("<S> Security"+CR$) THEN GOTO Hungup
	IF FNW%("<M> Mages Guild"+CR$) THEN GOTO Hungup
	IF FNW%("<B> Ye Olde Stone Bank"+CR$) THEN GOTO Hungup
	IF FNW%("<H> Butler Hospital"+CR$) THEN GOTO Hungup
	IF FNW%("<P> Pick pockets"+CR$) THEN GOTO Hungup
	IF FNW%("<V> Visit the Apothecary"+CR$) THEN GOTO Hungup
	IF FNW%("<G> Goto the Arena"+CR$) THEN GOTO Hungup
	IF FNW%(CR$+"[Square] Option (Q=Quit): ") THEN GOTO Hungup
	IF FNT% THEN GOTO Expired
	IF FNR% THEN GOTO Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$)
	IF I$="Q" THEN CHAIN "MAIN"
	F%=-(I$="A")-2*(I$="B")-3*(I$="G")-4*(I$="H")-5*(I$="M")-6*(I$="P")-7*(I$="R")-8*(I$="S")-9*(I$="V")-10*(I$="W")
	ON F% GOTO Armory,Bank,Arena,Hospital,Mage,Pick,RE,Security,Apothecary,Weapon
	GOTO Top

Armory:
	GOSUB BuySell
	IF FNW%("You currently own <"+MID$(STR$(ASC(USER.ARMOR$)),2)+"> "+ARMOR$(ASC(USER.ARMOR$))+"."+CR$) THEN GOTO Hungup
	D#=INT(AC(ASC(USER.ARMOR$))*TCHR%/100):M!=D#
	IF I$="S" THEN
		IF FNW%("I'll give you"+STR$(M!)+" for it."+CR$) THEN GOTO Hungup
		IF FNW%("Do you accept (Y/N)? ") THEN GOTO Hungup
		IF FNR% THEN GOTO Hungup
		IF UCASE$(I$)="Y" THEN
			LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+D#)
			LSET USER.ARMOR$=CHR$(0)
			TSHIELD%=0
		END IF
		ZZ%=FNW%(CR$):GOTO Top
	END IF
	FOR AFFORD%=1 TO MA%
		IF CVD(USER.GOLD$)<AC(AFFORD%) THEN EXIT FOR
	NEXT AFFORD%
	AFFORD%=AFFORD%-1
	IF FNW%("You can afford the first"+STR$(AFFORD%)+" of"+STR$(MA%)+" armor."+CR$+CR$) THEN GOTO Hungup
	V%=ASC(USER.ARMOR$):IF V%>AFFORD% THEN V%=AFFORD%
	IF V%<1 THEN V%=1
	IF FNW%("Start list at <"+MID$(STR$(V%),2)+">: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	X%=VAL(I$):IF I$="" THEN X%=V%
	IF X%<1 OR X%>MA% THEN GOTO Top
	V%=AFFORD%:IF V%<1 THEN V%=1
	IF FNW%(CR$+"  End list at <"+MID$(STR$(V%),2)+">: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	Y%=VAL(I$):IF I$="" THEN Y%=V%
	IF Y%<X% OR Y%>MA% THEN GOTO Top
	IF FNW%(CR$+CR$) THEN GOTO Hungup
	FOR I%=X% TO Y%
		M!=AC(I%)
		IF FNW%(RIGHT$(" <"+MID$(STR$(I%),2)+"> ",5)+LEFT$(ARMOR$(I%)+SPACE$(32),32)+STR$(M!)+CR$) THEN GOTO Hungup
	NEXT I%
	IF FNW%(CR$+"Buy which? ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	ZZ%=FNW%(CR$)
	X%=VAL(I$):IF X%<1 OR X%>AFFORD% THEN GOTO Top
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-AC(X%))
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+D#)
	LSET USER.ARMOR$=CHR$(X%)
	TSHIELD%=0
	IF ASC(USER.BLESSED$) THEN TSHIELD%=X%/10
	IF ASC(USER.CURSED$) THEN TSHIELD%=-X%/10
	ZZ%=FNW%(CR$+"Done."+CR$)
	GOTO Top

Bank:
	PUT #2,USER%
	IF FNW%(CR$+"Welcome to Ye Olde Stone Bank"+CR$+CR$) THEN GOTO Hungup
	IF FNW%("<D> Deposit"+CR$) THEN GOTO Hungup
	IF FNW%("<L> Loan"+CR$) THEN GOTO Hungup
	IF FNW%("<R> Rob the bank"+CR$) THEN GOTO Hungup
	IF FNW%("<S> Status"+CR$) THEN GOTO Hungup
	IF FNW%("<W> Withdrawal"+CR$) THEN GOTO Hungup
	IF FNW%(CR$+"[Bank] Option (Q=Quit): ") THEN GOTO Hungup
	IF FNT% THEN GOTO Expired
	IF FNR% THEN GOTO Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$)
	IF I$="Q" THEN GOTO Top
	F%=-(I$="D")-2*(I$="L")-3*(I$="R")-4*(I$="S")-5*(I$="W")
	ON F% GOTO Deposit,Loan,Rob,Status,Withdrawal
	GOTO Bank
Deposit:
	M!=INT(CVD(USER.GOLD$)):IF FNW%("Deposit [MAX="+MID$(STR$(M!),2)+"]: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	ZZ%=FNW%(CR$)
	IF UCASE$(I$)="MAX" THEN I$=MID$(STR$(CVD(USER.GOLD$)),2)
	D#=INT(VAL(I$)):IF D#<1 OR D#>CVD(USER.GOLD$) THEN GOTO Bank
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-D#)
	IF CVD(USER.LOAN$) THEN
		LSET USER.LOAN$=MKD$(CVD(USER.LOAN$)-D#):D#=0
		IF CVD(USER.LOAN$)<0 THEN D#=-CVD(USER.LOAN$):LSET USER.LOAN$=MKD$(0)
	END IF
	LSET USER.BANK$=MKD$(CVD(USER.BANK$)+D#)
	GOTO Bank
Loan:
	IF FNW%("How much of loan? ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	ZZ%=FNW%(CR$)
	D#=INT(VAL(I$)):IF D#<1 THEN GOTO Bank
	J%=0:FOR I%=1 TO ASC(USER.LEVEL$):J%=J%+I%:NEXT I%
	IF (D#+CVD(USER.LOAN$))>(50*J%) THEN ZZ%=FNW%("Your credit is not THAT good!"+CR$):GOTO Bank
	IF FNW%("Your loan has been approved."+CR$) THEN GOTO Hungup
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+D#)
	LSET USER.LOAN$=MKD$(CVD(USER.LOAN$)+D#)
	GOTO Bank
Rob:
	D#=FND(ASC(USER.LEVEL$)^3*900)+ASC(USER.LEVEL$)^3*100
	I%=ASC(USER.CLASS$):A%=(INT(ASC(USER.LEVEL$)/10)+1)*(1-2*(I%=4 OR I%=7)-5*(I%=3))
	IF FNW%("You attempt to sneak into the vault...") THEN GOTO Hungup
	SLEEP 3:IF FNW%(CR$+CR$) THEN GOTO Hungup
	IF FND(100)>A% THEN GOTO Caught
	M!=D#:IF FNW%("You loot"+STR$(M!)+" pieces of gold!") THEN GOTO Hungup
	SLEEP 3:IF FNW%(CR$+CR$) THEN GOTO Hungup
	IF FNW%("You slowly try to make your way out...") THEN GOTO Hungup
	SLEEP 3:IF FNW%(CR$+CR$) THEN GOTO Hungup
	IF FND(100)>A% THEN ZZ%=FNW%("something jingles!"):SLEEP 2:GOTO Caught
	IF FNW%("You make good with your escape!"+CR$) THEN GOTO Hungup
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+D#):SLEEP 2
	GOTO Bank
Status:
	M!=INT(CVD(USER.GOLD$)):IF FNW%("Gold in hand:"+STR$(M!)+CR$) THEN GOTO Hungup
	M!=INT(CVD(USER.BANK$)):IF FNW%("Gold in bank:"+STR$(M!)+CR$) THEN GOTO Hungup
	M!=INT(CVD(USER.LOAN$)):IF FNW%("Gold on loan:"+STR$(M!)+CR$) THEN GOTO Hungup
	GOTO Bank
Withdrawal:
	M!=INT(CVD(USER.BANK$)):IF FNW%("Withdrawal [MAX="+MID$(STR$(M!),2)+"]: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	ZZ%=FNW%(CR$)
	IF UCASE$(I$)="MAX" THEN I$=MID$(STR$(CVD(USER.BANK$)),2)
	D#=INT(VAL(I$)):IF D#<1 OR D#>CVD(USER.BANK$) THEN GOTO Bank
	LSET USER.BANK$=MKD$(CVD(USER.BANK$)-D#)
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+D#)
	GOTO Bank

Arena:
	CHAIN "ARENA"

Hospital:
	IF FNW%("Welcome to Butler Hospital."+CR$+CR$) THEN GOTO Hungup
	IF FNW%("Hit points cost"+STR$(ASC(USER.LEVEL$)*WC(1))+"."+CR$) THEN GOTO Hungup
	X%=CVI(USER.HP$)-THP%
	IF FNW%("You need"+STR$(X%)+" hit points."+CR$) THEN GOTO Hungup
	M!=INT(CVD(USER.GOLD$)/ASC(USER.LEVEL$))
	IF FNW%("You can afford"+STR$(M!)+" hit points."+CR$+CR$) THEN GOTO Hungup
	IF M!<X% THEN X%=M!
	IF FNW%("How many do you want [MAX="+MID$(STR$(X%),2)+"]? ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	ZZ%=FNW%(CR$)
	I%=VAL(I$):IF UCASE$(I$)="MAX" THEN I%=X%
	IF I%<1 OR I%>X% THEN GOTO Top
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-ASC(USER.LEVEL$)*I%)
	THP%=THP%+I%
	IF FNW%(CR$+"Hit points ="+STR$(THP%)+CR$) THEN GOTO Hungup
	GOTO Top

Mage:
	CL%=ASC(USER.CLASS$)
	IF CL%<>2 AND CL%<>4 AND CL%<>5 AND CL%<>7 AND CL%<>9 AND CL%<>11 THEN
		IF FNW%("Your character can't cast spells!"+CR$) THEN GOTO Hungup
		GOTO Top
	END IF
	IF FNW%("The old mage offers to ") THEN GOTO Hungup
	IF CL%=4 THEN ZZ%=FNW%("make you a scroll."+CR$) ELSE ZZ%=FNW%("teach you a spell."+CR$)
	FOR I%=0 TO 11
		IF (CVI(USER.SPELL$) AND 2^I%)=0 THEN M!=10^(I%+1):IF FNW%(RIGHT$(" <"+MID$(STR$(I%+1),2)+"> ",5)+LEFT$(SPELL$(I%+1)+SPACE$(32),32)+STR$(M!)+CR$) THEN GOTO Hungup
	NEXT I%
	IF FNW%(CR$+"Buy which? ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	ZZ%=FNW%(CR$)
	X%=VAL(I$):IF X%<1 OR X%>12 THEN GOTO Top
	IF (CVI(USER.SPELL$) AND 2^(X%-1)) THEN ZZ%=FNW%(CR$+"You already have that spell!"):GOTO Top
	IF CVD(USER.GOLD$)<10^X% THEN ZZ%=FNW%(CR$+"You don't have enough gold!"+CR$):GOTO Top
	LSET USER.SPELL$=MKI$(CVI(USER.SPELL$) OR 2^(X%-1))
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-10^X%)
	IF FNW%(CR$+"The deed is done."+CR$) THEN GOTO Hungup
	GOTO Top

Pick:
	IF FNW%("You attempt to pick a passerby's pocket...") THEN GOTO Hungup
	SLEEP 3:ZZ%=FNW%(CR$+CR$)
	D#=0:FOR I%=1 TO ASC(USER.LEVEL$):D#=D#+FND(20)+5:NEXT I%
	CL%=ASC(USER.CLASS$):A%=10-23*(CL%=4 OR CL%=7)-56*(CL%=3)
	IF FND(100)>A% THEN GOTO Caught
	M!=D#:IF FNW%("You manage to steal"+STR$(M!)+" gold pieces!"+CR$) THEN GOTO Hungup
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+D#)
	GOTO Top

RE:
	GOSUB BuySell
	IF FNW%("You currently live in a "+RE$(ASC(USER.RE$))+"."+CR$) THEN GOTO Hungup
	D#=INT(RC(ASC(USER.RE$))*TCHR%/100)
	IF I$="S" THEN
		M!=D#:IF FNW%("I'll give you"+STR$(M!)+" for it."+CR$) THEN GOTO Hungup
		IF FNW%("Do you accept (Y/N)? ") THEN GOTO Hungup
		IF FNR% THEN GOTO Hungup
		IF UCASE$(I$)="Y" THEN
			LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+D#)
			LSET USER.RE$=CHR$(0)
		END IF
		ZZ%=FNW%(CR$):GOTO Top
	END IF
	FOR AFFORD%=1 TO MR%
		IF CVD(USER.GOLD$)<RC(AFFORD%) THEN EXIT FOR
	NEXT AFFORD%
	AFFORD%=AFFORD%-1
	IF FNW%("You can afford the first"+STR$(AFFORD%)+" of"+STR$(MR%)+" real estate."+CR$+CR$) THEN GOTO Hungup
	IF FNW%("Start list at <1>: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	X%=VAL(I$):IF I$="" THEN X%=1
	IF X%<1 OR X%>MR% THEN GOTO Top
	V%=AFFORD%:IF V%<1 THEN V%=1
	IF FNW%(CR$+"  End list at <"+MID$(STR$(V%),2)+">: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	Y%=VAL(I$):IF I$="" THEN Y%=V%
	IF Y%<X% OR Y%>MR% THEN GOTO Top
	IF FNW%(CR$+CR$) THEN GOTO Hungup
	FOR I%=X% TO Y%
		M!=RC(I%)
		IF FNW%(RIGHT$(" <"+MID$(STR$(I%),2)+"> ",5)+LEFT$(RE$(I%)+SPACE$(32),32)+STR$(M!)+CR$) THEN GOTO Hungup
	NEXT I%
	IF FNW%(CR$+"Buy which? ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	ZZ%=FNW%(CR$)
	X%=VAL(I$):IF X%<1 OR X%>AFFORD% THEN GOTO Top
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-RC(X%))
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+D#)
	LSET USER.RE$=CHR$(X%)
	ZZ%=FNW%(CR$+"Done."+CR$)
	GOTO Top

Security:
	GOSUB BuySell
	IF FNW%("You currently own a "+SECURITY$(ASC(USER.SECURITY$))+"."+CR$) THEN GOTO Hungup
	D#=INT(SC(ASC(USER.SECURITY$))*TCHR%/100)
	IF I$="S" THEN
		M!=D#:IF FNW%("I'll give you"+STR$(M!)+" for it."+CR$) THEN GOTO Hungup
		IF FNW%("Do you accept (Y/N)? ") THEN GOTO Hungup
		IF FNR% THEN GOTO Hungup
		IF UCASE$(I$)="Y" THEN
			LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+D#)
			LSET USER.SECURITY$=CHR$(0)
		END IF
		ZZ%=FNW%(CR$):GOTO Top
	END IF
	FOR AFFORD%=1 TO MS%
		IF CVD(USER.GOLD$)<SC(AFFORD%) THEN EXIT FOR
	NEXT AFFORD%
	AFFORD%=AFFORD%-1
	IF FNW%("You can afford the first"+STR$(AFFORD%)+" of"+STR$(MS%)+" security."+CR$+CR$) THEN GOTO Hungup
	IF FNW%("Start list at <1>: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	X%=VAL(I$):IF I$="" THEN X%=1
	IF X%<1 OR X%>MS% THEN GOTO Top
	V%=AFFORD%:IF V%<1 THEN V%=1
	IF FNW%(CR$+"  End list at <"+MID$(STR$(V%),2)+">: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	Y%=VAL(I$):IF I$="" THEN Y%=AFFORD%
	IF Y%<X% OR Y%>MS% THEN GOTO Top
	IF FNW%(CR$+CR$) THEN GOTO Hungup
	FOR I%=X% TO Y%
		M!=SC(I%)
		IF FNW%(RIGHT$(" <"+MID$(STR$(I%),2)+"> ",5)+LEFT$(SECURITY$(I%)+SPACE$(32),32)+STR$(M!)+CR$) THEN GOTO Hungup
	NEXT I%
	IF FNW%(CR$+"Buy which? ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	ZZ%=FNW%(CR$)
	X%=VAL(I$):IF X%<1 OR X%>AFFORD% THEN GOTO Top
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-SC(X%))
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+D#)
	LSET USER.SECURITY$=CHR$(X%)
	ZZ%=FNW%(CR$+"Done."+CR$)
	GOTO Top

Apothecary:
	CL%=ASC(USER.CLASS$)
	IF CL%<>3 AND CL%<>6 AND CL%<>10 AND CL%<>11 THEN
		IF FNW%("Your character can't use poisons!"+CR$) THEN GOTO Hungup
		GOTO Top
	END IF
	IF FNW%("You enter a secret back door of the shop."+CR$) THEN GOTO Hungup
	FOR I%=0 TO 11
		IF (CVI(USER.POISON$) AND 2^I%)=0 THEN IF FNW%(RIGHT$(" <"+MID$(STR$(I%+1),2)+"> ",5)+LEFT$("Poison Type "+MID$(STR$(I%+1),2)+SPACE$(32),32)+STR$(10^(I%+1))+CR$) THEN GOTO Hungup
	NEXT I%
	IF FNW%(CR$+"Buy which? ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	ZZ%=FNW%(CR$)
	X%=VAL(I$):IF X%<1 OR X%>12 THEN GOTO Top
	IF (CVI(USER.POISON$) AND 2^(X%-1)) THEN ZZ%=FNW%(CR$+"You already have that poison!"):GOTO Top
	IF CVD(USER.GOLD$)<10^X% THEN ZZ%=FNW%(CR$+"You don't have enough gold!"+CR$):GOTO Top
	LSET USER.POISON$=MKI$(CVI(USER.POISON$) OR 2^(X%-1))
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-10^X%)
	IF FNW%(CR$+"The Apothecary slips you the vial."+CR$) THEN GOTO Hungup
	GOTO Top

Weapon:
	GOSUB BuySell
	IF FNW%("You currently own <"+MID$(STR$(ASC(USER.WEAPON$)),2)+"> "+WEAPON$(ASC(USER.WEAPON$))+"."+CR$) THEN GOTO Hungup
	D#=INT(WC(ASC(USER.WEAPON$))*TCHR%/100)
	IF I$="S" THEN
		M!=D#:IF FNW%("I'll give you"+STR$(M!)+" for it."+CR$) THEN GOTO Hungup
		IF FNW%("Do you accept (Y/N)? ") THEN GOTO Hungup
		IF FNR% THEN GOTO Hungup
		IF UCASE$(I$)="Y" THEN
			LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+D#)
			LSET USER.WEAPON$=CHR$(0)
			THIT%=0
		END IF
		ZZ%=FNW%(CR$):GOTO Top
	END IF
	FOR AFFORD%=1 TO MW%
		IF CVD(USER.GOLD$)<WC(AFFORD%) THEN EXIT FOR
	NEXT AFFORD%
	AFFORD%=AFFORD%-1
	IF FNW%("You can afford the first"+STR$(AFFORD%)+" of"+STR$(MW%)+" weapons."+CR$+CR$) THEN GOTO Hungup
	V%=ASC(USER.WEAPON$):IF V%>AFFORD% THEN V%=AFFORD%
	IF V%<1 THEN V%=1
	IF FNW%("Start list at <"+MID$(STR$(V%),2)+">: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	X%=VAL(I$):IF I$="" THEN X%=V%
	IF X%<1 OR X%>MW% THEN GOTO Top
	V%=AFFORD%:IF V%<1 THEN V%=1
	IF FNW%(CR$+"  End list at <"+MID$(STR$(V%),2)+">: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	Y%=VAL(I$):IF I$="" THEN Y%=AFFORD%
	IF Y%<X% OR Y%>MW% THEN GOTO Top
	IF FNW%(CR$+CR$) THEN GOTO Hungup
	FOR I%=X% TO Y%
		M!=WC(I%)
		IF FNW%(RIGHT$(" <"+MID$(STR$(I%),2)+"> ",5)+LEFT$(WEAPON$(I%)+SPACE$(32),32)+STR$(M!)+CR$) THEN GOTO Hungup
	NEXT I%
	IF FNW%(CR$+"Buy which? ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	ZZ%=FNW%(CR$)
	X%=VAL(I$):IF X%<1 OR X%>AFFORD% THEN GOTO Top
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-WC(X%))
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+D#)
	LSET USER.WEAPON$=CHR$(X%)
	THIT%=0
	IF ASC(USER.BLESSED$) THEN THIT%=X%/10
	IF ASC(USER.CURSED$) THEN THIT%=-X%/10
	ZZ%=FNW%(CR$+"Done."+CR$)
	GOTO Top

BuySell:
	IF FNW%("<B>uy or <S>ell? ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$)
	IF I$<>"B" AND I$<>"S" THEN RETURN Top
	RETURN
