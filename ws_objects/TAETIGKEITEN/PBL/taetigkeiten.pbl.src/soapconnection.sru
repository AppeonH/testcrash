$PBExportHeader$soapconnection.sru
forward
global type soapconnection from nonvisualobject
end type
end forward

global type soapconnection from nonvisualobject native "pbwsclient170.pbx"
public function  string generateproxy(string wsdl, string servicename)
public function  long createinstance(ref powerobject obj, string proxyname, string portname)
public function  long createinstance(ref powerobject obj, string proxyname)
public function  long setoptions(string options)
public function  long setproxyserveroptions(string proxyoptions)
public function  long SetSoapLogFile(string Filename)
public function  long UseIntegratedWindowsAuthentication(boolean useIWA)
public function  long SetClientCertificateFile(string Filename)
public function  long SetTimeout(long Seconds)
public function  long SetBasicAuthentication(string Domain, string userID, string Password)
public function  long SetProxyServer(string hostname, long port, string userID, string Password)
public function  long SetProxyServer(string Address, string userID, string Password)
public function  long RemoveAuthentication()
public function  long UseConnectionCache (boolean cache)
public function  powerobject dynamiccast(powerobject proxyobject, readonly string proxyname)
public function  long SetBypassProxyOnLocal(boolean bypassProxyOnLocal)
public function  long AddToBypassList(string value)
public function  long RemoveBypassList()
public function  long SetUseDefaultProxySetting(boolean UseDefaultProxySetting)
public function  long SetKeepAlive(boolean KeepAlive)
public function  long EnablePreAuthentication()
end type
global soapconnection soapconnection

on soapconnection.create
call super::create
TriggerEvent( this, "constructor" )
end on

on soapconnection.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

