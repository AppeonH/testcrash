$PBExportHeader$n_cst_auswertwf1_attrib.sru
$PBExportComments$Auswertungsparameter WF1
forward
global type n_cst_auswertwf1_attrib from nonvisualobject
end type
end forward

global type n_cst_auswertwf1_attrib from nonvisualobject autoinstantiate
end type

type variables

public:
// Name der Auswertung
string is_auswertung
string is_title

// Hinweis:
// nicht alle der nachfolgenden Einstellungen 
// werden für alle Auswertungen benötigt


// spezieller Benutzer (sonst leer)
string is_username

// Zeitraum (Mehrfachangabe)
integer ii_jahre []
integer ii_monate []
boolean ib_zeitmehrfach = TRUE

// Kostenstellen von_kst (Mehrfachauswahl)
string is_vonkst []

// Umlagen (Mehrfachauswahl)
string is_schluessel []
string is_umlagen []
string is_zusatz []
string is_zusatz1 []
string is_zusatz2 []

// KST als Bezugsgröße (Mehrfachauswahl)
string is_bezugkst []
boolean ib_bezug = FALSE  // Auswahl KST als Bezugsgröße

end variables

on n_cst_auswertwf1_attrib.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_cst_auswertwf1_attrib.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

