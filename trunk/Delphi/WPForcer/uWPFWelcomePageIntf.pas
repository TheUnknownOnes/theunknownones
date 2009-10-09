unit uWPFWelcomePageIntf;

interface
type
  TWindowClosingEvent = procedure (Sender: TObject; CanClose: Boolean; Data: Integer);

  IURLModule = interface
  ['{9D215B02-6073-45DC-B007-1A2DBCE2D693}']
    procedure Close;
    function GetURL: string;                    // tested
    procedure SetURL(const AURL: string);       // tested
    procedure SourceActivated;
    function GetWindowClosingEvent: TWindowClosingEvent; // WARNING!!! DO NOT CALL!!!
    procedure Proc1;
    procedure Proc2;
    procedure Proc3;
    procedure Proc4;
    procedure Proc5;
    property URL: string read GetURL write SetURL;
  end;

  IValidateSource = interface
  ['{ED7F0BEA-DFFC-4048-B9C0-70DC1DE9D031}']
    procedure SourceValidated; // function?     // partially tested
  end;

  IDocModule = interface
['{60AE6F18-62AD-4E39-A999-29504CF2632A}']
    procedure AddToProject;
    function GetFileName: string;
    procedure GetIsModified;
    function GetModuleName: string;
    procedure Save;
    procedure Show;  // doesn't seem to work properly...
    procedure ShowEditor(Visible: Boolean; const Filename: string);
    procedure GetProjectCount;
    procedure GetProject;
    procedure GetActiveProject;
    property Filename: string read GetFilename;
    property ModuleName: string read GetModuleName;
  end;

  IUnknown3 = interface
['{15D3FB81-EF27-488E-B2B4-26B59CA89D9D}']
    procedure AddNotifier;
    procedure AddToInterface;
  end;


  IUnknown6 = interface
['{9D215B02-6073-45DC-B007-1A2DBCE2D693}']
    procedure Proc1;
    procedure Proc2;
    procedure Proc3;
    procedure Proc4;
  end;


  IUnknown7 = interface
['{9E9C81A0-EDDC-11D1-9504-00608CCBF153}']
    procedure GetDocModule;
    procedure AddModuleHandler;
    procedure RemoveModuleHandler;
  end;

  IUnknown8 = interface
['{DEF9DC3E-E68A-4341-90C4-426178AED0AA}']
    procedure Proc1;
    procedure Proc2;
    procedure Proc3;
    procedure Proc4;
  end;

  IUnknown11 = interface
  ['{F9D448F2-50BC-11D1-9FB5-0020AF3D82DA}']
    function GetInstance: TObject;
  end;

  IUnknown12 = interface
  ['{FFD0A5AF-49CB-4EC2-A658-957146030CEC}']
    procedure HasObjects;
  end;

type
  TGUIDs = array of TGUID;

function GoURL(const URL: string): Boolean;
function GetGUID(const AObject: TObject): TGUIDs; overload;

implementation
uses
  ToolsAPI, SysUtils, ActiveX, Clipbrd, Controls;

// From Hallvard 
function GetImplementingObject(const I: IInterface): TObject;
const
  AddByte = $04244483;
  AddLong = $04244481;
type
  PAdjustSelfThunk = ^TAdjustSelfThunk;
  TAdjustSelfThunk = packed record
    case AddInstruction: longint of
      AddByte : (AdjustmentByte: shortint);
      AddLong : (AdjustmentLong: longint);
  end;
  PInterfaceMT = ^TInterfaceMT;
  TInterfaceMT = packed record
    QueryInterfaceThunk: PAdjustSelfThunk;
  end;
  TInterfaceRef = ^PInterfaceMT;
var
  QueryInterfaceThunk: PAdjustSelfThunk;
begin
  Result := Pointer(I);
  if Assigned(Result) then
    try
      QueryInterfaceThunk := TInterfaceRef(I)^. QueryInterfaceThunk;
      case QueryInterfaceThunk.AddInstruction of
        AddByte: Inc(PChar(Result), QueryInterfaceThunk.AdjustmentByte);
        AddLong: Inc(PChar(Result), QueryInterfaceThunk.AdjustmentLong);
      else
        Result := nil;
      end;
    except
      Result := nil;
    end;
end;

function GetGUID(const AInterface: IInterface): TGUIDs; overload;
begin
  Result := GetGUID(GetImplementingObject(AInterface));
end;

procedure ConvertError(const S: string);
begin
  raise EConvertError.Create(S);
end;

function GUIDToString(const GUID: TGUID): string;
var
  P: PWideChar;
begin
  if not Succeeded(StringFromCLSID(GUID, P)) then
    ConvertError('SInvalidGUID');
  Result := P;
  CoTaskMemFree(P);
end;

function GetGUID(const AObject: TObject): TGUIDs;
var
  ClassPtr: TClass;
  IntfTable: PInterfaceTable;
  ResultI, I: Integer;
  pie: PInterfaceEntry;
  Text: string;
begin
  Result := nil;
  ClassPtr := AObject.ClassType;
  ResultI := 0;
  while ClassPtr <> nil do
  begin
    IntfTable := ClassPtr.GetInterfaceTable;
    if IntfTable <> nil then
      begin
        SetLength(Result, IntfTable.EntryCount + Length(Result));
        for I := 0 to IntfTable.EntryCount-1 do
        begin
          Result[ResultI] := IntfTable.Entries[I].IID;
          Inc(ResultI);
        end;
      end;
    ClassPtr := ClassPtr.ClassParent;
  end;
  Text := '';
  for I := Low(Result) to High(Result) do
    Text := Text +   '['''+
  GuidToString(Result[I])+
  ''']'#13#10;
  Clipboard.AsText := Text;
  Result := nil;
end;


function GoURL(const URL: string): Boolean;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  Editor: IOTAEditor;
  SourceEditor: IOTASourceEditor;
  I: Integer;
  Project: IOTAProject;
  S: string;
  URLModule: IURLModule;
  Unknown1: IValidateSource;
  DocModule: IDocModule;
  Unknown3: IUnknown3;
  Unknown6: IUnknown6;
  Unknown7: IUnknown7;
  Unknown8: IUnknown8;
  Unknown9: IValidateSource;
  Unknown11: IUnknown11;
  Unknown12: IUnknown12;
  MS: IOTAMessageServices;
  Obj: TObject;
begin
  Result := False;
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  for I := 0 to ModuleServices.ModuleCount-1 do
    begin
      Module := ModuleServices.Modules[I];
      if Supports(Module, IURLModule, URLModule) then
        begin
          if Supports(Module, IDocModule, DocModule) then
            begin
              URLModule.URL := URL;
              Result := True;
            end;
        end;
    end;
end;

end.
