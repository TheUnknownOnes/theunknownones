unit QRPreview_TLB;

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
// Datei generiert am 26.04.2006 13:01:59 aus der unten beschriebenen Typbibliothek.

// ************************************************************************  //
// Typbib: L:\PROJECT\SOFTWARE\Marco\QRShellExtension\QRPreview.tlb (1)
// LIBID: {ACE34B01-3555-4BDD-9101-1C96A2418990}
// LCID: 0
// Hilfedatei: 
// Hilfe-String: QRPreview2 Bibliothek
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// Fehler
//   Hinweis: QRPreview geändert zu QRPreview_
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
  QRPreviewMajorVersion = 1;
  QRPreviewMinorVersion = 0;

  LIBID_QRPreview: TGUID = '{ACE34B01-3555-4BDD-9101-1C96A2418990}';

  IID_IQRPreview: TGUID = '{8BBCF110-61B4-4357-9BCC-6C2079F2658D}';
  CLASS_QRPreview_: TGUID = '{E31F84F5-2ACD-4A4A-84F7-556835197272}';
type

// *********************************************************************//
// Forward-Deklaration von in der Typbibliothek definierten Typen                    
// *********************************************************************//
  IQRPreview = interface;
  IQRPreviewDisp = dispinterface;

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten CoClasses             
// (HINWEIS: Hier wird jede CoClass ihrem Standard-Interface zugewiesen)              
// *********************************************************************//
  QRPreview_ = IQRPreview;


// *********************************************************************//
// Interface: IQRPreview
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8BBCF110-61B4-4357-9BCC-6C2079F2658D}
// *********************************************************************//
  IQRPreview = interface(IDispatch)
    ['{8BBCF110-61B4-4357-9BCC-6C2079F2658D}']
  end;

// *********************************************************************//
// DispIntf:  IQRPreviewDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8BBCF110-61B4-4357-9BCC-6C2079F2658D}
// *********************************************************************//
  IQRPreviewDisp = dispinterface
    ['{8BBCF110-61B4-4357-9BCC-6C2079F2658D}']
  end;

// *********************************************************************//
// Die Klasse CoQRPreview_ stellt die Methoden Create und CreateRemote zur      
// Verfuegung, um Instanzen des Standard-Interface IQRPreview, dargestellt 
// von CoClass QRPreview_, zu erzeugen. Diese Funktionen koennen                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// will, die von dieser Typbibliothek dargestellt werden.                                            
// *********************************************************************//
  CoQRPreview_ = class
    class function Create: IQRPreview;
    class function CreateRemote(const MachineName: string): IQRPreview;
  end;

implementation

uses ComObj;

class function CoQRPreview_.Create: IQRPreview;
begin
  Result := CreateComObject(CLASS_QRPreview_) as IQRPreview;
end;

class function CoQRPreview_.CreateRemote(const MachineName: string): IQRPreview;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_QRPreview_) as IQRPreview;
end;

end.
