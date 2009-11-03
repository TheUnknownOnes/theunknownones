unit DelphiRemoteServer_TLB;

// ************************************************************************ //
// WARNUNG                                                                    
// -------                                                                    
// Die in dieser Datei deklarierten Typen wurden aus Daten einer Typbibliothek
// generiert. Wenn diese Typbibliothek explizit oder indirekt (ueber eine     
// andere Typbibliothek) reimportiert wird oder wenn der Befehl            
// 'Aktualisieren' im Typbibliotheks-Editor waehrend des Bearbeitens der     
// Typbibliothek aktiviert ist, wird der Inhalt dieser Datei neu generiert und 
// alle manuell vorgenommenen Aenderungen gehen verloren.                                        
// ************************************************************************ //

// PASTLWTR : 1.2
// Datei generiert am 03.11.2009 15:23:12 aus der unten beschriebenen Typbibliothek.

// ************************************************************************  //
// Typbib: C:\Users\MaWarm\Documents\Projects\TUO\trunk\Delphi\DelphiRemote\DelphiRemoteServer\Project\DelphiRemoteServer.tlb (1)
// LIBID: {4B987DA2-F15C-4A9D-AEAC-F2B56E9E714D}
// LCID: 0
// Hilfedatei: 
// Hilfe-String: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit muss ohne Typueberpruefung fuer Zeiger compiliert werden. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// In dieser Typbibliothek deklarierte GUIDS . Es werden folgende        
// Praefixe verwendet:                                                     
//   Typbibliotheken     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Nicht-DISP-Interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // Haupt- und Nebenversionen der Typbibliothek
  DelphiRemoteServerMajorVersion = 1;
  DelphiRemoteServerMinorVersion = 0;

  LIBID_DelphiRemoteServer: TGUID = '{4B987DA2-F15C-4A9D-AEAC-F2B56E9E714D}';

  IID_IService: TGUID = '{CF69BEFB-C2E7-4652-84EA-36D397DE2C51}';
  CLASS_Service: TGUID = '{2755AA8A-EE23-4A1A-A6B3-55B526D317DB}';
type

// *********************************************************************//
// Forward-Deklaration von in der Typbibliothek definierten Typen                    
// *********************************************************************//
  IService = interface;
  IServiceDisp = dispinterface;

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten CoClasses             
// (HINWEIS: Hier wird jede CoClass ihrem Standard-Interface zugewiesen)              
// *********************************************************************//
  Service = IService;


// *********************************************************************//
// Interface: IService
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CF69BEFB-C2E7-4652-84EA-36D397DE2C51}
// *********************************************************************//
  IService = interface(IDispatch)
    ['{CF69BEFB-C2E7-4652-84EA-36D397DE2C51}']
    procedure RegisterIDE(const IDE: IDispatch); safecall;
    procedure UnregisterIDE(const IDE: IDispatch); safecall;
    function Get_IDE(idx: Integer): IDispatch; safecall;
    function Get_IDECount: Integer; safecall;
    function Get_IDEs: IEnumVARIANT; safecall;
    property IDE[idx: Integer]: IDispatch read Get_IDE;
    property IDECount: Integer read Get_IDECount;
    property IDEs: IEnumVARIANT read Get_IDEs;
  end;

// *********************************************************************//
// DispIntf:  IServiceDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CF69BEFB-C2E7-4652-84EA-36D397DE2C51}
// *********************************************************************//
  IServiceDisp = dispinterface
    ['{CF69BEFB-C2E7-4652-84EA-36D397DE2C51}']
    procedure RegisterIDE(const IDE: IDispatch); dispid 201;
    procedure UnregisterIDE(const IDE: IDispatch); dispid 202;
    property IDE[idx: Integer]: IDispatch readonly dispid 203;
    property IDECount: Integer readonly dispid 204;
    property IDEs: IEnumVARIANT readonly dispid 205;
  end;

// *********************************************************************//
// Die Klasse CoService stellt die Methoden Create und CreateRemote zur      
// Verfuegung, um Instanzen des Standard-Interface IService, dargestellt 
// von CoClass Service, zu erzeugen. Diese Funktionen koennen                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// will, die von dieser Typbibliothek dargestellt werden.                                            
// *********************************************************************//
  CoService = class
    class function Create: IService;
    class function CreateRemote(const MachineName: string): IService;
  end;

implementation

uses ComObj;

class function CoService.Create: IService;
begin
  Result := CreateComObject(CLASS_Service) as IService;
end;

class function CoService.CreateRemote(const MachineName: string): IService;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Service) as IService;
end;

end.
