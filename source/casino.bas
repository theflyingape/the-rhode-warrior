	'$INCLUDE: 'COMMON.BAS'

	DIM DECK$(52),YH$(10),DH$(10)
	GET #2,USER%
Top:
	PUT #2,USER%
	IF FNW%(CR$+CR$+"Welcome to the Gambling Casino"+CR$+CR$) THEN GOTO Hungup
	IF FNW%("<B> Blackjack"+CR$) THEN GOTO Hungup
	IF FNW%("<C> Craps"+CR$) THEN GOTO Hungup
	IF FNW%("<G> Greyhound race"+CR$) THEN GOTO Hungup
	IF FNW%("<H> High card"+CR$) THEN GOTO Hungup
	IF FNW%("<I> Instant Cash Machine"+CR$) THEN GOTO Hungup
	IF FNW%("<R> Roulette"+CR$) THEN GOTO Hungup
	IF FNW%(CR$+"[Casino] Option (Q=Quit): ") THEN GOTO Hungup
	IF FNT% THEN GOTO Expired
	IF FNR% THEN GOTO Hungup
	I$=UCASE$(I$):ZZ%=FNW%(CR$+CR$)
	IF I$="Q" THEN CHAIN "MAIN"
	F%=-1*(I$="B")-2*(I$="C")-3*(I$="G")-4*(I$="H")-5*(I$="I")-6*(I$="R")
	ON F% GOTO Blackjack,Craps,Greyhound,HiCard,ATM,Roulette
	GOTO Top

ATM:
	IF FNW%(CR$+CR$) THEN GOTO Hungup
	IF FNW%("<D>eposit"+CR$) THEN GOTO Hungup
	IF FNW%("<W>ithdrawal"+CR$+CR$) THEN GOTO Hungup
	M!=CVD(USER.GOLD$):IF FNW%("Money in hand:"+STR$(M!)+CR$) THEN GOTO Hungup
	M!=CVD(USER.BANK$):IF FNW%("Money in bank:"+STR$(M!)+CR$) THEN GOTO Hungup
	M!=CVD(USER.LOAN$):IF FNW%("Money on loan:"+STR$(M!)+CR$) THEN GOTO Hungup
	IF FNW%(CR$+"[ATM] Option (Q=Quit): ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	CH$=UCASE$(I$)
	IF CH$="Q" THEN GOTO Top
	IF CH$<>"D" AND CH$<>"W" THEN GOTO ATM
	IF CH$="D" THEN M!=CVD(USER.GOLD$) ELSE M!=CVD(USER.BANK$)
	IF FNW%(CR$+"Amount [MAX="+MID$(STR$(M!),2)+"]? ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	IF UCASE$(I$)="MAX" AND CH$="D" THEN I$=MID$(STR$(CVD(USER.GOLD$)),2)
	IF UCASE$(I$)="MAX" AND CH$="W" THEN I$=MID$(STR$(CVD(USER.BANK$)),2)
	A#=INT(VAL(I$)):IF A#<1 THEN GOTO ATM
	IF CH$="D" AND A#<=CVD(USER.GOLD$) THEN
		LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-A#)
		IF CVD(USER.LOAN$) THEN
			LSET USER.LOAN$=MKD$(CVD(USER.LOAN$)-A#)
			IF CVD(USER.LOAN$)<0 THEN
				A#=-CVD(USER.LOAN$)
				LSET USER.LOAN$=MKD$(0)
			END IF
		END IF
		LSET USER.BANK$=MKD$(CVD(USER.BANK$)+A#)
	END IF
	IF CH$="W" AND A#<=CVD(USER.BANK$) THEN
		LSET USER.BANK$=MKD$(CVD(USER.BANK$)-A#)
		LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+A#)
	END IF
	GOTO ATM

Blackjack:
	GOSUB Shuffle
	GOSUB Bet
	YH%=0:DH%=0:NC%=0:M%=1:YH$="":DH$=""
	GOSUB Draw1:YH$=YH$+A$:YH%=YH%+1:YH$(YH%)=I$
	GOSUB Draw1:DH$=DH$+A$:DH%=DH%+1:DH$(DH%)=I$
	GOSUB DH
	GOSUB Draw1:YH$=YH$+A$:YH%=YH%+1:YH$(YH%)=I$
	GOSUB Draw1:DH$=DH$+A$:DH%=DH%+1:DH$(DH%)=I$
	GOSUB YH:IF V%=21 THEN ZZ%=FNW%(" - Blackjack!"):M%=2:GOTO BJpayoff
Card:
	IF V%>21 THEN ZZ%=FNW%(" - You bust!"):LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-BET#):GOTO Top
	IF YH%=5 THEN ZZ%=FNW%(" - 5-cards you win!"):M%=2:GOTO BJpayoff
	IF FNW%(CR$+CR$+"<H>it or <S>tay: ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	I$=UCASE$(I$):IF I$<>"H" AND I$<>"S" THEN GOTO Card
	IF I$="H" THEN GOSUB Draw1:YH$=YH$+A$:YH%=YH%+1:YH$(YH%)=I$:GOSUB YH:GOTO Card
	Y%=V%:M%=1
	GOSUB DH
	WHILE V%<17 AND V%<21
		GOSUB Draw1:DH$=DH$+A$:DH%=DH%+1:DH$(DH%)=I$
		GOSUB DH
	WEND
	IF V%>21 THEN ZZ%=FNW%(" - Dealer bust!"):GOTO BJpayoff
	IF V%=Y% THEN ZZ%=FNW%(" - Push"):GOTO Top
	IF V%>Y% THEN ZZ%=FNW%(" - You lose!"):LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-BET#):GOTO Top
BJpayoff:
	M!=M%*BET#:IF FNW%(CR$+"You win"+STR$(M!)+" gold pieces!") THEN GOTO Hungup
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+M%*BET#)
	GOTO Top
Draw1:
	NC%=NC%+1
	I$=LEFT$(DECK$(NC%),1):A$=" "+MID$(DECK$(NC%),2)+" "
	RETURN
YH:	IF FNW%(CR$+"Your hand: "+YH$) THEN RETURN Hungup
	V%=0:T%=0
	FOR I%=1 TO YH%
		IF YH$(I%)="A" THEN T%=T%+1:V%=V%+1
		IF YH$(I%)="0" THEN V%=V%+10
		IF VAL(YH$(I%)) THEN V%=V%+VAL(YH$(I%))
	NEXT I%
	WHILE T%>0 AND V%<12:T%=T%-1:V%=V%+10:WEND
	IF FNW%("="+STR$(V%)) THEN RETURN Hungup
	RETURN
DH:	IF FNW%(CR$+"Dealer's hand: "+DH$) THEN RETURN Hungup
	V%=0:T%=0
	FOR I%=1 TO DH%
		IF DH$(I%)="A" THEN T%=T%+1:V%=V%+1
		IF DH$(I%)="0" THEN V%=V%+10
		IF VAL(DH$(I%)) THEN V%=V%+VAL(DH$(I%))
	NEXT I%
	IF NC%=2 THEN ZZ%=FNW%(" DOWN ")
	WHILE T%>0 AND V%<12:T%=T%-1:V%=V%+10:WEND
	IF FNW%("="+STR$(V%)) THEN RETURN Hungup
	SLEEP 1
	RETURN

Craps:
	GOSUB Bet
	M%=1
	GOSUB Roll
	IF V%=7 OR V%=11 THEN ZZ%=FNW%("A Natural!"):M%=2:GOTO Cpay
	IF V%=2 OR V%=12 THEN ZZ%=FNW%("Crapped-out -- you lose!"):LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-BET#):GOTO Top
	P%=V%:V%=0
	WHILE P%<>V% AND V%<>7
		IF FNW%(CR$+"Your point is:"+STR$(P%)+"  Press RETURN: ") THEN GOTO Hungup
		IF FNR% THEN GOTO Hungup
		GOSUB Roll
	WEND
	IF V%=7 THEN ZZ%=FNW%("Crapped-out -- you lose!"):LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-BET#):GOTO Top
	IF FNW%("You made your point!") THEN GOTO Hungup
Cpay:
	M!=M%*BET#:IF FNW%(CR$+"You win"+STR$(M!)+" gold pieces!") THEN GOTO Hungup
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+M%*BET#)
	GOTO Top

Roll:
	X%=FND(6):Y%=FND(6):V%=X%+Y%
	IF FNW%(CR$+CR$+"Your roll: "+STR$(X%)+" "+STR$(Y%)+" ="+STR$(V%)+"  ") THEN RETURN Hungup
	RETURN

Greyhound:
	DATA Armored Tank,Knight Rider,Wham's Wings,Go For Grunt,Let It Ride,Stinky,Lucky Lois,Rhody Runner,Beetle Bomb
	RESTORE Greyhound
	FOR I%=1 TO 9:READ DECK$(I%):YH%(I%)=0:DH%(I%)=0:NEXT I%
	GOSUB Bet
	CLS 2:IF LOCL%=0 THEN PRINT #1,CHR$(27);"[H";CHR$(27);"[J";
	GOSUB Track
	IF FNW%("Which dog number? ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	D%=VAL(I$):IF D%<1 OR D%>9 THEN GOTO Top
	IF FNW%(" - "+DECK$(D%)+CR$) THEN GOTO Hungup
	IF FNW%("<W>in, <P>lace, or <S>how? ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	T$=UCASE$(I$):IF T$<>"W" AND T$<>"P" AND T$<>"S" THEN GOTO Top
	IF FNW%(" - will pay"+STR$(-8*(T$="W")-4*(T$="P")-2*(T$="S"))+":1"+CR$) THEN GOTO Hungup
	YH%=0
	FOR P%=10 TO 50 STEP 10
		F%=0
		WHILE NOT F%
			X%=FND(9)
			IF DH%(X%)<50 THEN:DH%(X%)=DH%(X%)+1:IF DH%(X%)=50 AND YH%<3 THEN YH%=YH%+1:YH%(YH%)=X%
			F%=(DH%(X%)=P%)
		WEND
		GOSUB Track:SLEEP 2
	NEXT P%
	WHILE YH%<3
		X%=FND(9)
		IF DH%(X%)<50 THEN DH%(X%)=DH%(X%)+1:IF DH%(X%)=50 AND YH%<3 THEN YH%=YH%+1:YH%(YH%)=X%
	WEND
	GOSUB Track:ZZ%=FNW%(CR$+CR$):SLEEP 2
	A#=0
	IF T$="W" AND YH%(1)=D% THEN A#=8*BET#
	IF T$="P" AND (YH%(1)=D% OR YH%(2)=D%) THEN A#=4*BET#
	IF T$="S" AND (YH%(1)=D% OR YH%(2)=D% OR YH%(3)=D%) THEN A#=2*BET#
	IF A#=0 THEN ZZ%=FNW%("You lose."):LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-BET#):GOTO Top
	M!=A#:IF FNW%("You win"+STR$(M!)+" gold pieces!") THEN GOTO Hungup
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+A#)
	GOTO Top
Track:
	LOCATE 1,1:IF LOCL%=0 THEN PRINT #1,CHR$(27);"[H";
	IF FNW%("[==============*=========+=========+=========+=========+=========F"+CR$) THEN GOTO Hungup
	FOR I%=1 TO 9
		IF FNW%("[ "+LEFT$(DECK$(I%)+SPACE$(12),12)+" :"+STRING$(DH%(I%),"-")+MID$(STR$(I%),2)) THEN GOTO Hungup
		FOR J%=1 TO 3
			IF YH%(J%)=I% THEN ZZ%=FNW%(" ("+MID$("1st2nd3rd",3*(J%-1)+1,3)+")")
		NEXT J%
		ZZ%=FNW%(CR$)
	NEXT I%
	IF FNW%("[==============*=========+=========+=========+=========+=========F"+CR$+CR$) THEN GOTO Hungup
	RETURN

HiCard:
	GOSUB Shuffle
	GOSUB Bet
	IF FNW%("Pick a card (1-52)? ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	V%=VAL(I$)
	IF V%<1 OR V%>52 THEN GOTO Top
	IF FNW%(" - "+MID$(DECK$(V%),2)+CR$+CR$) THEN GOTO Hungup
	DO:I%=FND(52):LOOP WHILE I%=V%
	IF FNW%("My card: "+MID$(DECK$(I%),2)+CR$+CR$) THEN GOTO Hungup
	X%=VAL(DECK$(V%))
	IF X%=0 THEN
		IF MID$(DECK$(V%),2,1)="T" THEN X%=10
		IF MID$(DECK$(V%),2,1)="J" THEN X%=11
		IF MID$(DECK$(V%),2,1)="Q" THEN X%=12
		IF MID$(DECK$(V%),2,1)="K" THEN X%=13
		IF MID$(DECK$(V%),2,1)="A" THEN X%=14
	END IF
	Y%=VAL(DECK$(I%))
	IF Y%=0 THEN
		IF MID$(DECK$(I%),2,1)="T" THEN Y%=10
		IF MID$(DECK$(I%),2,1)="J" THEN Y%=11
		IF MID$(DECK$(I%),2,1)="Q" THEN Y%=12
		IF MID$(DECK$(I%),2,1)="K" THEN Y%=13
		IF MID$(DECK$(I%),2,1)="A" THEN Y%=14
	END IF
	SLEEP 1
	IF X%=Y% THEN ZZ%=FNW%("We tie.")
	IF X%<Y% THEN ZZ%=FNW%("You lose."):LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-BET#)
	IF X%>Y% THEN ZZ%=FNW%("You win!"):LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+BET#)
	GOTO Top

Roulette:
	GOSUB Bet
	IF FNW%("Enter number (1-36), <O>dd, or <E>ven? ") THEN GOTO Hungup
	IF FNR% THEN GOTO Hungup
	I$=UCASE$(I$):V%=VAL(I$)
	IF I$<>"O" AND I$<>"E" AND (V%<1 OR V%>36) THEN GOTO Top
	IF FNW%(CR$+"The wheel spins...") THEN GOTO Hungup
	FOR I%=1 TO 3
		SLEEP 1
		IF FNW%("["+MID$(STR$(FND(36)),2)+"]...") THEN GOTO Hungup
	NEXT I%
	SLEEP 2
	I%=FND(36)
	IF FNW%("["+MID$(STR$(I%),2)+"] - ") THEN GOTO Hungup
	IF V%=I% THEN
		M!=35*BET#:IF FNW%("You win"+STR$(M!)+" gold pieces!") THEN GOTO Hungup
		LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+35*BET#)
		GOTO Top
	END IF
	IF (I$="O" AND I% MOD 2=1) OR (I$="E" AND I% MOD 2=0) THEN
		M!=BET#:IF FNW%("You win"+STR$(M!)+" gold pieces!") THEN GOTO Hungup
		LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)+BET#)
		GOTO Top
	END IF
	IF FNW%("You lose!") THEN GOTO Hungup
	LSET USER.GOLD$=MKD$(CVD(USER.GOLD$)-BET#)
	GOTO Top

Bet:
	M!=CVD(USER.GOLD$):IF FNW%(CR$+"Bet [MAX="+MID$(STR$(M!),2)+"]? ") THEN RETURN Hungup
	IF FNR% THEN RETURN Hungup
	IF UCASE$(I$)="MAX" THEN I$=MID$(STR$(CVD(USER.GOLD$)),2)
	BET#=INT(VAL(I$)):ZZ%=FNW%(CR$)
	IF BET#<1 OR BET#>CVD(USER.GOLD$) THEN RETURN Top
	RETURN

Shuffle:
	DATA 2Two,3Three,4Four,5Five,6Six,7Seven,8Eight,9Nine,0Ten,0Jack,0Queen,0King,AAce
	IF FNW%("Shuffling deck...") THEN GOTO Hungup
	FOR I%=0 TO 3:RESTORE Shuffle:FOR J%=1 TO 13:READ DECK$(13*I%+J%):NEXT J%:NEXT I%
	FOR I%=1 TO 100:X%=FND(52):Y%=FND(52):SWAP DECK$(X%),DECK$(Y%):NEXT I%
	IF FNW%("Ok."+CR$) THEN GOTO Hungup
	RETURN
