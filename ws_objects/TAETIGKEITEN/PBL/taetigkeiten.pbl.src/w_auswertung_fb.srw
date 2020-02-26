$PBExportHeader$w_auswertung_fb.srw
$PBExportComments$Auswertungen Fachbereich
forward
global type w_auswertung_fb from window
end type
type dw_auswertung from datawindow within w_auswertung_fb
end type
end forward

global type w_auswertung_fb from window
integer x = 50
integer y = 52
integer width = 3287
integer height = 2168
boolean titlebar = true
string title = "Auswertungen"
string menuname = "m_auswertung_fb"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 29347791
string icon = "AppIcon!"
event ue_export ( )
event ue_detail ( boolean ab_more )
dw_auswertung dw_auswertung
end type
global w_auswertung_fb w_auswertung_fb

type variables

public s_auswert istr_auswert

end variables

forward prototypes
public function string of_selectkst ()
public function long of_retrieve ()
public function integer of_selectbericht ()
end prototypes

event ue_export();
SaveAsType lsat_format


IF dw_auswertung.DataObject = "d_auswert_iresf_zeiten_bericht" THEN
	lsat_format = PDF!
ELSE
	lsat_format = XLSX!
END IF

// Export der Daten
IF dw_auswertung.SaveAs ("", lsat_format, TRUE) < 1 THEN
	MessageBox ("Achtung", "Die Daten konnten nicht gespeichert werden!")
END IF

end event

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

public function string of_selectkst ();
string ls_kst
s_auswahl lstr_auswahl

lstr_auswahl.dataobject = "d_dddw_kst"
lstr_auswahl.return_column = "umlage"

OpenWithParm (w_auswahl, lstr_auswahl)

ls_kst = Message.StringParm

return ls_kst

end function

public function long of_retrieve ();
long ll_row


dw_auswertung.SetTransObject (SQLCA)

IF Left (dw_auswertung.dataobject, 17) = "d_auswert_projekt" THEN
	ll_row = dw_auswertung.retrieve (DateTime (istr_auswert.id_von), DateTime (istr_auswert.id_bis), istr_auswert.is_projekt, istr_auswert.is_schluessel)

ELSEIF dw_auswertung.DataObject = "d_auswahl_projekt_fb" THEN
	// Für Projekt Retrieve über Modus und Username
	ll_row = dw_auswertung.retrieve (gs_modus, istr_auswert.is_username)
	
ELSEIF dw_auswertung.DataObject = "d_auswert_iresf_zeiten_bericht" THEN
	// Für IRESF-Bericht Retrieve über KST und Bericht
	ll_row = dw_auswertung.retrieve (istr_auswert.is_kst, istr_auswert.is_projekt)

ELSE
	// sonst über Zeitraum und Username/KST
	ll_row = dw_auswertung.retrieve (Datetime (istr_auswert.id_von), Datetime (istr_auswert.id_bis), istr_auswert.is_username)
	dw_auswertung.GroupCalc ()
END IF
dw_auswertung.setFocus()

if ll_row < 1 then
	messagebox ("Keine Daten", "Es konnten keine Daten gefunden werden.")
end if

return ll_row

end function

public function integer of_selectbericht ();
// Bericht auswählen
SetPointer (HourGlass!)
OpenWithParm (w_auswahl_bericht_iresf, istr_auswert)

IF IsNull (Message.PowerObjectParm) OR NOT IsValid (Message.PowerObjectParm) THEN return -1
istr_auswert = Message.PowerObjectParm

return 1
end function

event open;
istr_auswert = Message.PowerObjectParm

this.title = istr_auswert.is_title
dw_auswertung.tag = istr_auswert.is_tag
dw_auswertung.dataobject = istr_auswert.is_datawindow

MenuId.dynamic of_checkrights()

dw_auswertung.setTransObject(sqlca)

IF istr_auswert.is_datawindow = "d_plausi_username_fb" THEN
	// Plausi nicht über Username, sondern über KST
	IF istr_auswert.is_username = "" OR IsNull (istr_auswert.is_username) THEN
		istr_auswert.is_username = of_selectkst ()
		IF istr_auswert.is_username = "" OR IsNull (istr_auswert.is_username) THEN
			close (this)
			return
		END IF
	END IF
	
ELSEIF istr_auswert.is_datawindow = "d_auswert_iresf_zeiten_bericht" THEN
	IF istr_auswert.is_kst = "" OR IsNull (istr_auswert.is_kst) THEN
		of_selectbericht ()
		IF istr_auswert.is_kst = "" OR IsNull (istr_auswert.is_kst) THEN
			close (this)
			return
		END IF
	END IF
END IF

post of_retrieve ()

end event

on w_auswertung_fb.create
if this.MenuName = "m_auswertung_fb" then this.MenuID = create m_auswertung_fb
this.dw_auswertung=create dw_auswertung
this.Control[]={this.dw_auswertung}
end on

on w_auswertung_fb.destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_auswertung)
end on

event resize;
dw_auswertung.resize (this.workspacewidth(), this.workspaceheight() - 100)

end event

type dw_auswertung from datawindow within w_auswertung_fb
event type integer pfc_print ( )
integer width = 3241
integer height = 1988
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

event doubleclicked;m_auswertung_fb lm_menu
string ls_dwo


if (row > 0) and (dw_auswertung.dataobject = 'd_auswahl_projekt_fb') then
	istr_auswert.is_projekt = getItemString (row, "umlage")
	istr_auswert.is_schluessel = getItemString (row, "schluessel")
	
	ls_dwo = dw_auswertung.tag
	
	// Projektauswertung nur nach Kostenstelle oder nach Mitarbeitern
	// ist abhängig von der Berechtigung oder Projektleitung
	IF GetItemString (row, "variante") = 'pd' THEN
		ls_dwo += "_detail"
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

