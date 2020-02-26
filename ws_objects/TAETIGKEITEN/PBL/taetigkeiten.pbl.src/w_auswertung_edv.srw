$PBExportHeader$w_auswertung_edv.srw
$PBExportComments$Auswertungen EDV
forward
global type w_auswertung_edv from window
end type
type dw_info from datawindow within w_auswertung_edv
end type
type dw_auswertung from datawindow within w_auswertung_edv
end type
end forward

global type w_auswertung_edv from window
integer x = 50
integer y = 52
integer width = 3287
integer height = 2168
boolean titlebar = true
string title = "Auswertungen"
string menuname = "m_auswertung_edv"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 29347791
string icon = "AppIcon!"
event ue_detail ( boolean ab_more )
event ue_export ( )
event rechnungsaufteilung ( )
event ue_export_sap ( )
dw_info dw_info
dw_auswertung dw_auswertung
end type
global w_auswertung_edv w_auswertung_edv

type variables

public s_auswert istr_auswert

end variables

forward prototypes
public function long of_retrieve ()
end prototypes

event ue_detail(boolean ab_more);
string ls_desc, ls_temp, ls_rc
long ll_group

IF ab_more THEN
	// Detail-Daten aufblenden (von oben nach unten)
	DO
		ll_group ++
		ls_desc = "DataWindow.Header." + string (ll_group) + ".Height"
		ls_rc = dw_auswertung.Describe (ls_desc)
	LOOP UNTIL ls_rc = "!" OR ls_rc = "?" OR Long (ls_rc) < 10
	
	ls_temp = String (ll_group)
	IF ls_rc = "!" OR ls_rc = "?" THEN
		// keine weitere Gruppe mehr
		// dann Detail-Band anzeigen
		ls_temp = "d"
		ls_desc = "DataWindow.Detail.Height"
		ls_rc = dw_auswertung.Describe (ls_desc)
	END IF
	
	IF Long (ls_rc) < 10 THEN
		ls_temp = dw_auswertung.Describe ("t_height" + ls_temp + ".Text")
		IF ls_temp = "!" OR ls_temp = "?" THEN ls_temp = "64"
		dw_auswertung.Modify (ls_desc + "='" + ls_temp + "' " + ls_desc + ".AutoSize=yes ")
	ELSE
		MessageBox ("Hinweis", "Keine weiteren Detail-Daten vorhanden")
	END IF

ELSE
	// Detail-Daten wegblenden (von unten nach oben)
	ls_desc = "DataWindow.Detail.Height"
	ls_rc = dw_auswertung.Describe (ls_desc)
	
	IF Long (ls_rc) < 10 THEN
		ls_temp = ""
		DO
			ls_desc = ls_temp
			ll_group ++
			ls_temp = "DataWindow.Header." + string (ll_group) + ".Height"
			ls_rc = dw_auswertung.Describe (ls_temp)
		LOOP WHILE ls_rc <> "!" AND ls_rc <> "?" AND Long (ls_rc) >= 10
	END IF
	
	IF ls_desc <> "" THEN
		dw_auswertung.Modify (ls_desc + "='0' " + ls_desc + ".AutoSize=no ")
	ELSE
		MessageBox ("Hinweis", "Keine weiteren Detail-Daten vorhanden")
	END IF
	
END IF

end event

event ue_export();
// Export der Daten
dw_auswertung.Saveas ("", XLSX!, TRUE)

end event

event rechnungsaufteilung();
OpenWithParm (w_rechnungsaufteilung, this)

end event

event ue_export_sap();string ls_path, ls_file, ls_empfkst, ls_empfauft, ls_leistungsart, ls_verrechart, ls_verrechartalt
string ls_header, ls_detail, ls_header_statkz, ls_detail_statkz, ls_daten
string ls_variant, ls_variant_statkz
string ls_username, ls_activityun, ls_orgnr, ls_orgnralt
integer li_rc, li_FileNum, li_co_area , li_doc_no, li_month, li_year, li_save
long ll_nbrRows, ll_row
double ldbl_actvty_qty
date ldt_date, ldt_date_last, ldt_date_alt
n_cst_string lnv_string


li_co_area = 1000
li_doc_no = 0
ls_variant  = "ZTAB2"
ls_variant_statkz = "01SAP"
ls_username  = "BATCH"
ls_activityun = "TAG"


//Dateiname
ls_path =  "CODatenübernahme_" + string (today(), 'yyyymmdd') + ".txt"

//Anzahl Zeilen im DataWindow 
ll_nbrRows = dw_auswertung.RowCount()

//Speichern unter
li_rc = GetFileSaveName ( "Speichern unter", ls_path, ls_file, "TXT", "All Files (*.*),*.*", "", 32770)

IF FileExists(ls_file) THEN
	li_save = Messagebox("Speichern unter","Die Datei " + ls_file + " besteht bereits. Möchten Sie sie ersetzen?",Information!,YesNo!,2)
	IF li_save = 2 THEN RETURN
END IF

IF li_rc = 0 THEN
	Messagebox("Hinweis","Speichern fehlgeschlagen")
	RETURN
END IF

//Tabellenrumpf
FOR ll_row = 1 TO ll_nbrRows
	ls_verrechart = dw_auswertung.getItemString( ll_row, "verrechart")
	ls_leistungsart = dw_auswertung.getItemString( ll_row, "leistungsart")
	IF IsNull (ls_verrechart) OR IsNull(ls_leistungsart) OR left (Trim (ls_leistungsart), 1) = "(" THEN CONTINUE

	ls_empfkst = dw_auswertung.getItemString( ll_row, "empfstelle")
	IF IsNull(ls_empfkst) THEN 
		ls_empfkst = ""
	ELSEIF IsNumber (ls_empfkst) THEN
		ls_empfkst = Right (Fill ("0", 10) + ls_empfkst, 10)
	END IF
	
	ls_empfauft = Right (Fill ("0", 12) + dw_auswertung.getItemString( ll_row, "empfauftrag"), 12)
	IF IsNull(ls_empfauft) THEN ls_empfauft = ""
	
	ldbl_actvty_qty = dw_auswertung.getItemNumber( ll_row, "zeit")	
	ldt_date = Date(dw_auswertung.getItemDateTime( ll_row, "monat"))
	
	ls_orgnr = dw_auswertung.getItemString( ll_row, "orgnr")
	IF IsNumber (ls_orgnr) THEN
		ls_orgnr = Right (Fill ("0", 10) + ls_orgnr, 10)
	END IF
	
	li_month = Month (ldt_date) + 1
	li_year = Year (ldt_date)
	
	IF li_month > 12 THEN
		li_month -= 12
		li_year ++
	END IF
	
	ldt_date_last = RelativeDate (Date (li_year, li_month, 1), -1)
	
	IF (ls_verrechart <> ls_verrechartalt) OR (ls_orgnr <> ls_orgnralt) OR (ldt_date_last <> ldt_date_alt) THEN
		li_doc_no ++
		IF ls_verrechart = "Statist. Kennzahlen" THEN
			// Statistische Kennzahlen
			ls_header_statkz += string(li_co_area) + "~t" + string(li_doc_no,"0000000000") + "~t" + string(ldt_date_last,"yyyymmdd") + "~t" + &
										string(ldt_date_last,"yyyymmdd") + "~tEUPRÜF " + string(ldt_date_last,"mm/yyyy") + "~t" + ls_variant_statkz + "~t" + ls_username + "~r~n"
		ELSE
			// Leistungsverrechnung
			ls_header += string(li_co_area) + "~t" + string(li_doc_no,"0000000000") + "~t" + string(ldt_date_last,"yyyymmdd") + "~t" + &
									string(ldt_date_last,"yyyymmdd") + "~tILV " + ls_orgnr + " " + string(ldt_date_last,"mm/yyyy") + "~t" + ls_variant + "~t" + ls_username + "~r~n"
		END IF
	END IF
	
	IF ls_verrechart = "Statist. Kennzahlen" THEN
		// Statistische Kennzahlen
		ls_detail_statkz += string(li_co_area) + "~t" + string(li_doc_no,"0000000000") + "~t" + ls_empfkst + "~t" +	&
									lnv_string.of_GlobalReplace (string(ldbl_actvty_qty,"0.000"), ",", ".") + "~t" + ls_leistungsart + "~r~n" 
	ELSE
		// Leistungsverrechnung
		ls_detail += string(li_co_area) + "~t" + string(li_doc_no,"0000000000") + "~t" + ls_orgnr + "~t" + &
						ls_leistungsart + "~t"  + ls_empfkst + "~t" + ls_empfauft + "~t" +	&
						lnv_string.of_GlobalReplace (string(ldbl_actvty_qty,"0.000"), ",", ".") + "~t" + ls_activityun + "~r~n" 
	END IF
	
	ls_verrechartalt = ls_verrechart
	ls_orgnralt = ls_orgnr
	ldt_date_alt = ldt_date_last
NEXT

//Tabelleninhalt
ls_daten = ""
IF ls_header <> "" THEN
	ls_daten += "BAPIAAHDR~r~n" + &
					"CO_AREA~tDOC_NO~tDOCDATE~tPOSTGDATE~tDOC_HDR_TX~tVARIANT~tUSERNAME~r~n" + &
					ls_header + &
					"~r~n~r~nBAPIAAITM~r~n" + &
					"CO_AREA~tDOC_NO~tSEND_CCTR~tACTTYPE~tREC_CCTR~tREC_ORDER~tACTVTY_QTY~tACTIVITYUN~r~n" + &
					ls_detail
END IF
IF ls_header_statkz <> "" THEN
	IF ls_daten <> "" THEN ls_daten += "~r~n~r~n"
	
	ls_daten +=	"BAPISKFHDR~r~n" + &
					"CO_AREA~tDOC_NO~tDOCDATE~tPOSTGDATE~tDOC_HDR_TX~tVARIANT~tUSERNAME~r~n" + &
					ls_header_statkz + &
					"~r~n~r~nBAPISKFITM~r~n" + &
					"CO_AREA~tDOC_NO~tREC_CCTR~tSTAT_QTY~tSTATKEYFIG~r~n" + &
					ls_detail_statkz
END IF


//Datei öffnen
li_FileNum = FileOpen (ls_file, TextMode!, Write!, LockWrite!, Replace!)

IF li_FileNum = -1 THEN
	Messagebox("Hinweis","Datei konnte nicht geöffnet werden")
	RETURN
END IF

//Daten in Datei schreiben
IF FileWriteEx(li_FileNum, ls_daten) < 0 THEN
	Messagebox("Hinweis","Datei konnte nicht gespeichert werden")
END IF

//Datei schließen
FileClose(li_FileNum)

end event

public function long of_retrieve ();
long ll_row


dw_auswertung.SetTransObject (SQLCA)

IF Left (dw_auswertung.dataobject, 17) = "d_auswert_projekt" THEN
	ll_row = dw_auswertung.retrieve (DateTime (istr_auswert.id_von), DateTime (istr_auswert.id_bis), istr_auswert.is_projekt, istr_auswert.is_schluessel)

ELSEIF dw_auswertung.dataobject = "d_auswahl_projekt" OR &
			dw_auswertung.dataobject = "d_auswahl_projekt_fb" THEN
	// Für Projekt Retrieve über Modus und Username
	ll_row = dw_auswertung.retrieve (gs_modus, istr_auswert.is_username)

ELSEIF dw_auswertung.dataobject = "d_auswahl_jira" THEN
	// Für JIRA-Tickets Retrieve ohen Parameter
	ll_row = dw_auswertung.retrieve ()

ELSEIF dw_auswertung.dataobject = "d_auswert_jira" THEN
	ll_row = dw_auswertung.retrieve (istr_auswert.is_projekt)

ELSE
	ll_row = dw_auswertung.retrieve (DateTime (istr_auswert.id_von), DateTime (istr_auswert.id_bis), istr_auswert.is_username, istr_auswert.is_kst)
	dw_auswertung.GroupCalc ()
END IF
dw_auswertung.setFocus()

if ll_row <1 then
	messagebox ("Keine Daten", "Es konnten keine Daten gefunden werden.")
end if

return ll_row

end function

on w_auswertung_edv.create
if this.MenuName = "m_auswertung_edv" then this.MenuID = create m_auswertung_edv
this.dw_info=create dw_info
this.dw_auswertung=create dw_auswertung
this.Control[]={this.dw_info,&
this.dw_auswertung}
end on

on w_auswertung_edv.destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_info)
destroy(this.dw_auswertung)
end on

event resize;
dw_auswertung.resize (this.workspacewidth(), this.workspaceheight() - 100)

end event

event open;
istr_auswert = Message.PowerObjectParm

this.title = istr_auswert.is_title
dw_auswertung.tag = istr_auswert.is_tag
dw_auswertung.dataobject = istr_auswert.is_datawindow

MenuId.dynamic of_checkrights()

dw_auswertung.setTransObject(sqlca)
post of_retrieve ()

end event

type dw_info from datawindow within w_auswertung_edv
boolean visible = false
integer x = 347
integer y = 992
integer width = 2802
integer height = 628
integer taborder = 40
boolean titlebar = true
string title = "Detail-Infos"
boolean controlmenu = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean hsplitscroll = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_auswertung from datawindow within w_auswertung_edv
event type integer pfc_print ( )
integer width = 3241
integer height = 1996
integer taborder = 30
string title = "none"
boolean hscrollbar = true
boolean vscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event type integer pfc_print();//////////////////////////////////////////////////////////////////////////////
//	Event:			pfc_print
//	Arguments:		None
//	Returns:			Integer - 1 if it succeeds and -1 if an error occurs
//	Description:		Opens the print dialog to allow user to change print settings,
//						and then prints the DataWindow.
//////////////////////////////////////////////////////////////////////////////
//	Rev. History		Version
//						5.0			Initial version
//						5.0.01		Modified script to avoid 64K segment problem with 16bit machine code executables
//						5.0.04		Destroy local datastore prior to returning in error condition.
//						8.0			Return code of pfc_printdlg has changed for Cancel Action.  Changed
//							 		code to test on success rather than failure.
//						8.0			Set Printer chosen in print dialog
// 						10.0 		Use new overloaded function dw.Print(canceldlg, showPrintDlg) which could display Print Dialog,
//////////////////////////////////////////////////////////////////////////////
/*
 * Open Source PowerBuilder Foundation Class Libraries
 *
 * Copyright (c) 2004-2005, All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted in accordance with the GNU Lesser General
 * Public License Version 2.1, February 1999
 *
 * http://www.gnu.org/copyleft/lesser.html
 *
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals and was originally based on software copyright (c) 
 * 1996-2004 Sybase, Inc. http://www.sybase.com.  For more
 * information on the Open Source PowerBuilder Foundation Class
 * Libraries see http://pfc.codexchange.sybase.com
*/
//////////////////////////////////////////////////////////////////////////////
//boolean	lb_rowselection
integer	li_rc
long		ll_s, ll_selected[]
long		ll_selectedcount
long		ll_cnt
datastore				lds_selection

// Print selection
if this.Object.DataWindow.Print.Page.Range = "selection" then
	// Get selected count
//	lb_rowselection = IsValid (inv_RowSelect)
//	if not lb_rowselection then of_SetRowSelect (true)
//	ll_selectedcount = inv_RowSelect.of_SelectedCount (ll_selected)
//	if not lb_rowselection then of_SetRowSelect (false)
	DO
		ll_s = GetSelectedRow ( ll_s )
		IF ll_s > 0 THEN
			ll_selectedcount++
			ll_selected[ll_selectedcount] = ll_s
		END IF
	LOOP WHILE ll_s > 0

	if ll_selectedcount > 0 then
		// Create a datastore to print selected rows
		lds_selection = create datastore
		lds_selection.dataobject = this.DataObject

		// First discard any data in the dataobject
		lds_selection.Reset()

		// Copy selected rows
		for ll_cnt = 1 to ll_selectedcount
			if this.RowsCopy (ll_selected[ll_cnt], ll_selected[ll_cnt], primary!, &
				lds_selection, 2147483647, primary!) < 0 then
				destroy lds_selection
				return -1
			end if
		next

	end if
end if

// Print
if IsValid (lds_selection) then
	li_rc = lds_selection.Print (true, true)
	destroy lds_selection
else
	li_rc = this.Print (true, true)
end if

return li_rc
end event

event clicked;long ll_selected

ll_selected = this.getSelectedRow(0)
this.selectRow(ll_selected, false)

if row > 0 then
	this.selectRow(row, true)
end if
end event

event doubleclicked;m_auswertung_edv lm_menu
string ls_dwo

if (row > 0) and &
		((dw_auswertung.dataobject = 'd_auswahl_projekt') or &
		 (dw_auswertung.dataobject = 'd_auswahl_projekt_fb') or &
		 (dw_auswertung.dataobject = 'd_auswahl_jira') or &
		 (dw_auswertung.dataobject = 'd_auswahl_ts_projekt')) then
	IF dw_auswertung.dataobject = 'd_auswahl_jira' THEN
		istr_auswert.is_schluessel = '' // Schlüssel spielt in der Auswertung keine Rolle
		istr_auswert.is_projekt = getItemString(row, "ticket")
	ELSE
		istr_auswert.is_projekt = getItemString(row, "umlage")
		IF dw_auswertung.dataobject = 'd_auswahl_ts_projekt' THEN
			istr_auswert.is_schluessel = 't'
		ELSE
			istr_auswert.is_schluessel = getItemString(row, "schluessel")
		END IF
	END IF

	ls_dwo = dw_auswertung.tag

	IF dw_auswertung.dataobject = 'd_auswahl_projekt' THEN
		// für IT immer die Detail-variante
		ls_dwo += "_detail"
	ELSEIF dw_auswertung.dataobject = 'd_auswahl_projekt_fb' THEN
		// Projektauswertung nur nach Kostenstelle oder nach Mitarbeitern
		// ist abhängig von der Berechtigung oder Projektleitung
		IF GetItemString (row, "variante") = 'pd' THEN
			ls_dwo += "_detail"
		END IF
	END IF
	
	dw_auswertung.dataobject = ls_dwo

	parent.MenuId.dynamic of_checkrights()
	
	of_retrieve ()
	
elseif dwo.name = "zeitraum" or Pos (dwo.tag, "zeitraum") > 0 then
	lm_menu = parent.Menuid
	IF lm_menu.m_file.m_zeitraum.enabled AND lm_menu.m_file.m_zeitraum.visible THEN
		lm_menu.m_file.m_zeitraum.Event clicked ()
	END IF

elseif dwo.name = "projektnr" or Pos (dwo.tag, "projektnr") > 0 then
	lm_menu = parent.Menuid
	IF lm_menu.m_file.m_projekt.enabled AND lm_menu.m_file.m_projekt.visible THEN
		lm_menu.m_file.m_projekt.Event clicked ()
	END IF

elseif dwo.name = "kstnr" or Pos (dwo.tag, "kstnr") > 0 then
	lm_menu = parent.Menuid
	IF lm_menu.m_file.m_kst.enabled AND lm_menu.m_file.m_kst.visible THEN
		lm_menu.m_file.m_kst.Event clicked ()
	END IF
end if

end event

event buttonclicked;
long ll_i, ll_start, ll_end, ll_row, ll_rows
string ls_liste[], ls_akt, ls_last, ls_typ, ls_colname
decimal ldc_pt

IF dwo.name = "b_detail" AND row > 0 THEN
	// "Evaluate" führt hier zu einer Zerstörung des Layouts (Bug?)
	ll_start = GetItemNumber (row, "first_row")
	ll_end = GetItemNumber (row, "last_row")
	ls_typ = GetItemString (row, "detail_typ")
	
	FOR ll_i = ll_start TO ll_end
		ls_akt = GetItemString (ll_i, "detail")
		IF NOT IsNull (ls_akt) THEN
			IF ls_akt <> ls_last THEN
				ls_liste [UpperBound (ls_liste) + 1] = ls_akt
				ls_last = ls_akt
			END IF
		END IF
	NEXT
	
	IF ls_typ = "t" THEN
		// TAB-Systems
		ls_colname = "umlage"
		dw_info.DataObject = "d_info_ts_umlage"
		dw_info.SetTransObject (SQLCA)
		ll_rows = dw_info.Retrieve (ls_liste)
	ELSEIF ls_typ = "j" THEN
		// JIRA
		ls_colname = "ticket"
		dw_info.DataObject = "d_info_jira"
		dw_info.SetTransObject (SQLCA)
		ll_rows = dw_info.Retrieve (ls_liste)
	ELSE
		// Aufträge
		ls_colname = "nr"
		dw_info.DataObject = "d_info_auftraege"
		dw_info.SetTransObject (SQLCA)
		ll_rows = dw_info.Retrieve (ls_liste)
	END IF
	
	IF ll_rows > 0 THEN
		// Aufwände zusteuern
		FOR ll_i = ll_start TO ll_end
			ls_akt = GetItemString (ll_i, "detail")
			IF IsNull (ls_akt) THEN ls_akt = "????"
			IF ls_akt <> ls_last THEN
				IF ls_last <> "" THEN
					ll_row = dw_info.Find (ls_colname + "='" + ls_last + "'", 1, ll_rows)
					IF ll_row > 0 THEN
						// Wert des vorherigen Eintrags setzen
						dw_info.SetItem (ll_row, "pt", ldc_pt)
					END IF
				END IF
				ldc_pt = 0
				ls_last = ls_akt
			END IF
			ldc_pt += GetItemNumber (ll_i, "pt")
		NEXT
		
		// Wert des allerletzen Eintrags setzen
		IF ls_last <> "" THEN
			ll_row = dw_info.Find (ls_colname + "='" + ls_last + "'", 1, ll_rows)
			IF ll_row > 0 THEN
				// Wert des vorherigen Eintrags setzen
				dw_info.SetItem (ll_row, "pt", ldc_pt)
			END IF
		END IF
		
		dw_info.visible = TRUE
	END IF

ELSEIF dwo.name = "b_bemerkung" THEN
	// Bemerkung ein-/ausblenden (= Detail ein/aus)
	IF Describe ("datawindow.detail.height") = "0" THEN
		Modify ("datawindow.detail.height='60'")
	ELSE
		Modify ("datawindow.detail.height='0'")
	END IF
END IF

end event

