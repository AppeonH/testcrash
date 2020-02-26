$PBExportHeader$dfscl_n_cst_random.sru
$PBExportComments$DFSCL Zufallszahlengenerator
forward
global type dfscl_n_cst_random from nonvisualobject
end type
end forward

global type dfscl_n_cst_random from nonvisualobject autoinstantiate
end type

type variables

protected:
// Startwert zur Ermittlung der nächsten Zufallszahl
  ulong il_startvalue

// Konstanten zur Ermittlung der Zufallszahl
  constant ulong icl_modulo1 = 100000000
  constant ulong icl_modulo2 = 10000
  constant ulong icl_division = 31415821

end variables

forward prototypes
public function unsignedlong of_rand (unsignedlong al_range)
public function integer of_randomize (unsignedlong al_startvalue)
protected function unsignedlong of_generatekey ()
end prototypes

public function unsignedlong of_rand (unsignedlong al_range);/////////////////////////////////////////////////////////////////////////
//
// public Funktion of_rand
//
// Parameter: 
//    	unsignedlong	al_range		obere Bereichsgrenze
//
// Rückgabewert:
//    	unsignedlong	Zufallszahl
// 
// Beschreibung: 
//			Erzeugt eine Zufallszahl zwischen 0 und al_range
//			Wenn al_range = 0 ist, dann wird eine Zufallszahl im unsignedlong-Bereich
//			erzeugt
//
// Autor:
//     	René Ullrich
//
/////////////////////////////////////////////////////////////////////////
//
// History:
//			03/97	Ersterstellung
//
/////////////////////////////////////////////////////////////////////////

ulong ll_random
decimal ll_help

IF IsNull(al_range) THEN return al_range

ll_random = of_GenerateKey()

IF al_range > 0 THEN
	ll_help = ll_random / icl_modulo1
	ll_random = ll_help * al_range
END IF

return ll_random
end function

public function integer of_randomize (unsignedlong al_startvalue);/////////////////////////////////////////////////////////////////////////
//
// public Funktion of_randomize
//
// Parameter: 
//    	unsignedlong	al_startvalue		Startwert
//
// Rückgabewert:
//    	NONE
// 
// Beschreibung: 
// 		Setzt den Startwert für die Randomize-Funktion
//		 	Wenn der Startwert 0 ist, wird der Startwert aus der aktuellen 
//			Systemzeit ermittelt
//
//
// Autor:
//     	René Ullrich
//
/////////////////////////////////////////////////////////////////////////
//
// History:
//		03/97	Ersterstellung
//		01/98	BugFix: Endlosschleife bei Startwertermittlung aus dem aktuellen Datum
//		06/99	RU: Verwendung des Jahres vierstellig.
//					 0-Werte bei HH, MM, SS, FF beeinflussen Startwert nicht
//
/////////////////////////////////////////////////////////////////////////

string ls_now
integer li_pos, li_newpos
ulong ll_current

IF IsNull (al_startvalue) THEN Return al_startvalue

IF al_startvalue = 0 THEN
	ls_now = string (datetime (today(), now()), "DD.MM.YYYY.HH.MM.SS.FF")
	li_pos = 1
	DO While li_Pos > 0
		li_newpos = Pos (ls_now, ".", li_pos)
		IF li_newpos = 0 THEN
			// letzter Teilstring
			ll_current = Long (Left (ls_now, 1 + Len (ls_now) - li_pos))
			IF ll_current = 0 THEN ll_current = 1
			li_pos = 0
		ELSE
			ll_current = Long (Mid (ls_now, li_pos, li_newpos - li_pos))
			IF ll_current = 0 THEN ll_current = 1
			li_pos = li_newpos + 1
		END IF
		al_startvalue = Mod (al_startvalue * ll_current, 31415821)
	LOOP
END IF

il_startvalue = al_startvalue

return 1

end function

protected function unsignedlong of_generatekey ();/////////////////////////////////////////////////////////////////////////
//
// protected Funktion of_generatekey
//
// Parameter: 
//    	NONE
//
// Rückgabewert:
//    	unsignedlong	Zufallszahl
// 
// Beschreibung: 
//			Erzeugt eine Zufallszahl, die gleichzeitig Startwert für die nächste 
//			Zufallszahl ist
//
// Autor:
//     	René Ullrich
//
/////////////////////////////////////////////////////////////////////////
//
// History:
//			03/97	Ersterstellung
//
/////////////////////////////////////////////////////////////////////////

ulong ll_p0, ll_p1, ll_q0, ll_q1
ulong ll_help

ll_p1 = il_startvalue / icl_modulo2
ll_p0= Mod (il_startvalue, icl_modulo2)
ll_q1 = icl_division / icl_modulo2
ll_q0 = Mod (icl_division, icl_modulo2)
ll_help = Mod (ll_p0 * ll_q1 + ll_p1 * ll_q0, icl_modulo2)
il_startvalue = Mod (Mod (ll_help * icl_modulo2 + ll_p0 * ll_q0, icl_modulo1) + 1, icl_modulo1)

return il_startvalue

end function

on dfscl_n_cst_random.create
call super::create
TriggerEvent( this, "constructor" )
end on

on dfscl_n_cst_random.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event constructor;/////////////////////////////////////////////////////////////////////////
//
// Event constructor
//
// Parameter: 
//     	NONE
// 
// Rückgabewert: 
//     	NONE
// 
// Beschreibung: 
//			Initialisierung des Zufallszahlengenerators
//
// Autor:
//     	René Ullrich
//
/////////////////////////////////////////////////////////////////////////
//
// History:
//			03/97		Ersterstellung
//
/////////////////////////////////////////////////////////////////////////

Randomize (0)
end event

