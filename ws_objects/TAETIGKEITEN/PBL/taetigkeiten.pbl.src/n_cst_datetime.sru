$PBExportHeader$n_cst_datetime.sru
$PBExportComments$Extension Date and/or Datetime service class
forward
global type n_cst_datetime from pfc_n_cst_datetime
end type
type os_systemtime from structure within n_cst_datetime
end type
type os_time_zone_information from structure within n_cst_datetime
end type
end forward

type os_systemtime from structure
	unsignedinteger		wyear
	unsignedinteger		wmonth
	unsignedinteger		wdayofweek
	unsignedinteger		wday
	unsignedinteger		whour
	unsignedinteger		wminute
	unsignedinteger		wsecond
	unsignedinteger		wmilliseconds
end type

type os_time_zone_information from structure
	long		bias
	integer		standardname[32]
	os_systemtime		standarddate
	long		standardbias
	integer		daylightname[32]
	os_systemtime		daylightdate
	long		daylightbias
end type

global type n_cst_datetime from pfc_n_cst_datetime
end type

type prototypes

protected Function boolean GetTimeZoneInformationForYear &
		(uint year, &
		 char tz, &
		 ref os_time_zone_information lpTimeZoneInformation) Library "kernel32"

protected Function boolean SystemTimeToTzSpecificLocalTime &
		(os_time_zone_information lpTimeZone, &
		 os_systemtime lpUniversalTime, &
		 ref os_systemtime lpLocalTime ) Library "kernel32"

end prototypes

forward prototypes
public function datetime of_utc2local (datetime adtm_utc)
end prototypes

public function datetime of_utc2local (datetime adtm_utc);/////////////////////////////////////////////////////////////////////////
//
// public Function of_utc2local
//
// Parameter:
//			datetime adtm_utc		auf UTC basierendes Datum/Uhrzeit
//
// Rückgabewert:
//			datetime			lokales Datum/Uhrzeit
//
// Beschreibung:
//			Wandelt ein Datum/Zeit auf UTC-basis in den zugehörigen Wert auf 
//			Basis der lokalen Einstellungen
//
// Autor:
//			Ullrich
//
/////////////////////////////////////////////////////////////////////////
//
// History:
//		12/2015 erste Version
//
/////////////////////////////////////////////////////////////////////////

datetime ldtm_local
os_time_zone_information lstr_tzi
os_systemtime lstr_utc
os_systemtime lstr_local
char lc_c
boolean lb_rc


SetNull (ldtm_local)

//local time zone
lb_rc = GetTimeZoneInformationForYear (Year(Date(adtm_utc)), lc_c, lstr_tzi)
IF NOT lb_rc THEN return ldtm_local

//PowerBuilder datetime -> SYSTEMTIME (für UTC)
lstr_UTC.wYear = Year(Date(adtm_utc))
lstr_UTC.wMonth = Month(Date(adtm_utc))
lstr_UTC.wDay = Day(Date(adtm_utc))
lstr_UTC.wHour = Hour(Time(adtm_utc))
lstr_UTC.wMinute = Minute(Time(adtm_utc))
lstr_UTC.wSecond = Second(Time(adtm_utc))
lstr_UTC.wMilliseconds = 0

//Convert to local time zone
lb_rc = SystemTimeToTzSpecificLocalTime (lstr_tzi, lstr_UTC, lstr_local)
IF NOT lb_rc THEN return ldtm_local

//Convert SYSTEMTIME to PowerBuilder datetime
ldtm_local = DateTime (Date (lstr_local.wYear, lstr_local.wMonth, lstr_local.wDay), &
						     Time (lstr_local.wHour, lstr_local.wMinute, lstr_local.wSecond))

return ldtm_local

end function

on n_cst_datetime.create
call super::create
end on

on n_cst_datetime.destroy
call super::destroy
end on

