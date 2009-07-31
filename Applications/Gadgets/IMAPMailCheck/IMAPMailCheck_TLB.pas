unit IMAPMailCheck_TLB;

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
// Datei generiert am 31.07.2009 10:48:41 aus der unten beschriebenen Typbibliothek.

// ************************************************************************  //
// Typbib: E:\Libs\tuo\Applications\Gadgets\IMAPMailCheck\IMAPMailCheck.tlb (1)
// LIBID: {02DC6A5D-28FA-46D5-9D66-DE45E9FC13B0}
// LCID: 0
// Hilfedatei: 
// Hilfe-String: IMAPMailCheck Bibliothek
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
  IMAPMailCheckMajorVersion = 1;
  IMAPMailCheckMinorVersion = 0;

  LIBID_IMAPMailCheck: TGUID = '{02DC6A5D-28FA-46D5-9D66-DE45E9FC13B0}';

  IID_IIMAPMailChecker: TGUID = '{8BF98B0D-FC0C-426A-AA7D-2E2E7C79EF78}';
  DIID_IIMAPMailCheckerEvents: TGUID = '{847A8624-B8E1-4499-8E9E-CEFA0CCFC454}';
  CLASS_IMAPMailChecker: TGUID = '{776EB96E-1A49-455A-BC2B-B1E90C81E8B1}';
type

// *********************************************************************//
// Forward-Deklaration von in der Typbibliothek definierten Typen                    
// *********************************************************************//
  IIMAPMailChecker = interface;
  IIMAPMailCheckerDisp = dispinterface;
  IIMAPMailCheckerEvents = dispinterface;

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten CoClasses             
// (HINWEIS: Hier wird jede CoClass ihrem Standard-Interface zugewiesen)              
// *********************************************************************//
  IMAPMailChecker = IIMAPMailChecker;


// *********************************************************************//
// Interface: IIMAPMailChecker
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8BF98B0D-FC0C-426A-AA7D-2E2E7C79EF78}
// *********************************************************************//
  IIMAPMailChecker = interface(IDispatch)
    ['{8BF98B0D-FC0C-426A-AA7D-2E2E7C79EF78}']
    procedure ShowConfigDialog; safecall;
    function Get_UnseenMessages: Integer; safecall;
    procedure RunMailTool; safecall;
    property UnseenMessages: Integer read Get_UnseenMessages;
  end;

// *********************************************************************//
// DispIntf:  IIMAPMailCheckerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8BF98B0D-FC0C-426A-AA7D-2E2E7C79EF78}
// *********************************************************************//
  IIMAPMailCheckerDisp = dispinterface
    ['{8BF98B0D-FC0C-426A-AA7D-2E2E7C79EF78}']
    procedure ShowConfigDialog; dispid 201;
    property UnseenMessages: Integer readonly dispid 202;
    procedure RunMailTool; dispid 203;
  end;

// *********************************************************************//
// DispIntf:  IIMAPMailCheckerEvents
// Flags:     (4096) Dispatchable
// GUID:      {847A8624-B8E1-4499-8E9E-CEFA0CCFC454}
// *********************************************************************//
  IIMAPMailCheckerEvents = dispinterface
    ['{847A8624-B8E1-4499-8E9E-CEFA0CCFC454}']
  end;

// *********************************************************************//
// Die Klasse CoIMAPMailChecker stellt die Methoden Create und CreateRemote zur      
// Verfuegung, um Instanzen des Standard-Interface IIMAPMailChecker, dargestellt 
// von CoClass IMAPMailChecker, zu erzeugen. Diese Funktionen koennen                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// will, die von dieser Typbibliothek dargestellt werden.                                            
// *********************************************************************//
  CoIMAPMailChecker = class
    class function Create: IIMAPMailChecker;
    class function CreateRemote(const MachineName: string): IIMAPMailChecker;
  end;

implementation

uses ComObj;

class function CoIMAPMailChecker.Create: IIMAPMailChecker;
begin
  Result := CreateComObject(CLASS_IMAPMailChecker) as IIMAPMailChecker;
end;

class function CoIMAPMailChecker.CreateRemote(const MachineName: string): IIMAPMailChecker;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_IMAPMailChecker) as IIMAPMailChecker;
end;

end.
