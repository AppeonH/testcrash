$PBExportHeader$n_cst_pflege_katalog_attrib.sru
$PBExportComments$Parameter Katalog-Pflege
forward
global type n_cst_pflege_katalog_attrib from nonvisualobject
end type
end forward

global type n_cst_pflege_katalog_attrib from nonvisualobject autoinstantiate
end type

type variables

public:
string is_kat_typ
string is_title
string is_dataobject
boolean ib_autokey
end variables

on n_cst_pflege_katalog_attrib.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_cst_pflege_katalog_attrib.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

