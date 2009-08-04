unit IBTransactionCheck_TLB;

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
// Datei generiert am 04.08.2009 07:07:56 aus der unten beschriebenen Typbibliothek.

// ************************************************************************  //
// Typbib: E:\Libs\tuo\Applications\Gadgets\IBTransactionCheck\IBTransactionCheck.tlb (1)
// LIBID: {0A40C4F0-2C61-426F-A20C-5626202AEB4B}
// LCID: 0
// Hilfedatei: 
// Hilfe-String: IBTransactionCheck Bibliothek
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
  IBTransactionCheckMajorVersion = 1;
  IBTransactionCheckMinorVersion = 0;

  LIBID_IBTransactionCheck: TGUID = '{0A40C4F0-2C61-426F-A20C-5626202AEB4B}';

  IID_ITIBTransactionCheck: TGUID = '{3C0D3E43-E8CD-4ABA-B1EA-F771B7A6FD29}';
  CLASS_TIBTransactionCheck: TGUID = '{9EBFE7D7-8AB6-49D8-903E-CB8E68B9F654}';
type

// *********************************************************************//
// Forward-Deklaration von in der Typbibliothek definierten Typen                    
// *********************************************************************//
  ITIBTransactionCheck = interface;
  ITIBTransactionCheckDisp = dispinterface;

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten CoClasses             
// (HINWEIS: Hier wird jede CoClass ihrem Standard-Interface zugewiesen)              
// *********************************************************************//
  TIBTransactionCheck = ITIBTransactionCheck;


// *********************************************************************//
// Interface: ITIBTransactionCheck
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3C0D3E43-E8CD-4ABA-B1EA-F771B7A6FD29}
// *********************************************************************//
  ITIBTransactionCheck = interface(IDispatch)
    ['{3C0D3E43-E8CD-4ABA-B1EA-F771B7A6FD29}']
    function Get_Transactions: WideString; safecall;
    procedure Shutdown(Attachment: Integer); safecall;
    procedure ShowOptionsDialog; safecall;
    function Get_RefreshInterval: Integer; safecall;
    property Transactions: WideString read Get_Transactions;
    property RefreshInterval: Integer read Get_RefreshInterval;
  end;

// *********************************************************************//
// DispIntf:  ITIBTransactionCheckDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3C0D3E43-E8CD-4ABA-B1EA-F771B7A6FD29}
// *********************************************************************//
  ITIBTransactionCheckDisp = dispinterface
    ['{3C0D3E43-E8CD-4ABA-B1EA-F771B7A6FD29}']
    property Transactions: WideString readonly dispid 202;
    procedure Shutdown(Attachment: Integer); dispid 201;
    procedure ShowOptionsDialog; dispid 203;
    property RefreshInterval: Integer readonly dispid 204;
  end;

// *********************************************************************//
// Die Klasse CoTIBTransactionCheck stellt die Methoden Create und CreateRemote zur      
// Verfuegung, um Instanzen des Standard-Interface ITIBTransactionCheck, dargestellt 
// von CoClass TIBTransactionCheck, zu erzeugen. Diese Funktionen koennen                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// will, die von dieser Typbibliothek dargestellt werden.                                            
// *********************************************************************//
  CoTIBTransactionCheck = class
    class function Create: ITIBTransactionCheck;
    class function CreateRemote(const MachineName: string): ITIBTransactionCheck;
  end;

implementation

uses ComObj;

class function CoTIBTransactionCheck.Create: ITIBTransactionCheck;
begin
  Result := CreateComObject(CLASS_TIBTransactionCheck) as ITIBTransactionCheck;
end;

class function CoTIBTransactionCheck.CreateRemote(const MachineName: string): ITIBTransactionCheck;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_TIBTransactionCheck) as ITIBTransactionCheck;
end;

end.
