//******************************************************************************
// ComLib.pas
// COM Utility Library
//
// Copyright (c) 1999-2001 Binh Ly
// All Rights Reserved
//
// http://www.techvanguards.com
//******************************************************************************
unit ComLib;

interface

uses
  Windows, ActiveX, AxCtrls, Classes, ComObj;

type
  //redefined here. D5's defn of Next seems a bit whacky to me :(
  IEnumVariant = interface(IUnknown)
    ['{00020404-0000-0000-C000-000000000046}']
    function Next(celt: LongWord; var rgvar: OleVariant;
      pceltFetched: PLongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumVariant): HResult; stdcall;
  end;

  { IEnumVariant wrapper
    This achieves similar functionality as Visual Basic's ForEach statement.

    Usage:

    Assume you have a COM object, Object1, that has a property, Items, that
    supports IEnumVariant and say each element in Items is a BSTR (widestring).
    The following will show each string in Items in message box:

    var
      vData: variant;
    begin
      // create Object1 somewhere here
      ...
      with TEnumVariant.Create (Object1.Items) do
      try
        while ForEach (vData) do
          ShowMessage (vData)
      finally
        Free;
      end;
    end;

    Notes:

    1) Object1.Items can either support IEnumVariant directly or IDispatch with
       _NewEnum (dispid = -4). TEnumVariant will handle both cases.

    2) If Object1.Items does not support IEnumVariant, an exception is raised
       right in the TEnumVariant constructor

    3) Since ForEach is dependent on IEnumVariant, its output parameter has been
       set to olevariant. If you know that you're enumerating a list of objects
       (interfaces), you can use the ForEachObject method instead where you have
       to pass in the correct interface id of each object:

       var
         pFoo: IFoo;

       with TEnumVariant.Create (Object1.Items) do
       try
         while ForEachObject (pFoo, IFoo) do
           pFoo.Bar;
       finally
         Free;
       end;

    4) You can use the Attach/Detach methods to attach to different enumerations
       at runtime. 
  }
  TDispNewEnum = dispinterface
    ['{97079E31-6957-11D2-9154-0000B4552A26}']  // dummy
    property _NewEnum: IUnknown readonly dispid -4;
    function _NewEnumFunc: IUnknown; dispid -4;
  end;

  TEnumVariant = class
  protected
    FEnumVariant: IEnumVariant;
  public
    constructor CreateUnknown (const Unk: IUnknown);
    constructor Create (const Unk: variant);
    procedure AttachUnknown (const Unk: IUnknown);
    procedure Attach (const Unk: variant);
    procedure Detach;
    function ForEach (out Data: olevariant): boolean; {$IFNDEF D3} overload; {$ENDIF}
    {$IFNDEF D3}
    function ForEach (out Obj; const IID: TGUID): boolean; overload;
    {$ENDIF}
    function ForEachObject (out Obj; const IID: TGUID): boolean;
    function Next (Count: integer; out Data; var Fetched: longint): boolean;
    procedure Reset;
    property EnumVariant: IEnumVariant read FEnumVariant;
  end;

  { IEnumVariant collection exporter
    Allows Delphi objects to export collection objects that support the
    IEnumVariant interface (can be used by VB clients using the ForEach keyword)

    Usage:

    Suppose you have an Object, Object1, that has an property, Items, and you
    want Items to support IEnumVariant. You can simply declare an instance of
    TVariantCollection as the object that implements Items:

    TObject1 = class (TAutoObject, IObject1)
    protected
      FItems: TVariantCollection;
      function Get_Items: IDispatch; safecall;
    public
      procedure Initialize; override;
      destructor Destroy; override;
    end;

    procedure TObject1.Initialize;
    begin
      inherited;
      FItems := TVariantCollection.Create (Self);
      FItems.Add ('1');  // parameter can be any olevariant compatible type!
    end;

    destructor TObject1.Destroy;
    begin
      FItems.Free;
      inherited;
    end;

    function TObject1.Get_Items: IDispatch;
    begin
      Result := FItems;
    end;

    Note: If you want to extend TVariantCollection - say export more automation
    methods, you can contain a TVariantCollection instance in a
    TAutoIntfObject-derived class and delegate the accessors and mutators to
    the contained instance. This should result in a very clean implementation!
  }
  IVariantCollection = interface
    //used by enumerator to lock list
    function GetController: IUnknown; stdcall;
    //used by enumerator to determine how many items
    function GetCount: integer; stdcall;
    //used by enumerator to retrieve items
    function GetItems (const Index: olevariant): olevariant; stdcall;
  end;

  TVariantCollection = class (TInterfacedObject, IUnknown, IDispatch, IVariantCollection)
  protected
    { IUnknown }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IDispatch }
    function GetTypeInfoCount (out Count: Integer): HResult; stdcall;
    function GetTypeInfo (Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames (const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke (DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  protected
    FController: pointer;  // weak ref to controller
    FItems: TStringList;
    FRefCounted: boolean;
    function GetController: IUnknown; stdcall;
    function GetCount: integer; stdcall;
    function GetItems (const Index: olevariant): olevariant; stdcall;
    procedure SetItems (const Index: olevariant; const Item: olevariant);
  public
    constructor Create (const Controller: IUnknown);
    destructor Destroy; override;
    function Add (const Item: olevariant): integer;
    function AddObject (const Key: string; const Item: olevariant): integer;
    procedure Clear;
    procedure Delete (const Index: olevariant);
    function GetEnum: IEnumVariant;
    property Controller: IUnknown read GetController;
    property Count: integer read GetCount;
    property Items [const Index: olevariant]: olevariant read GetItems write SetItems;
    property RefCounted: boolean read FRefCounted write FRefCounted;
  end;

  { reusable IEnumVariant enumerator. you can implement your own collection
    that implements IVariantCollection and be able export that collection
    as an IEnumVariant using TEnumVariantCollection.

    For example: TVariantCollection uses TEnumVariantCollection to implement
    its _NewEnum (dispid = -4) property.
  }
  TEnumVariantCollection = class (TInterfacedPersistent, IEnumVariant)
  protected
    { IEnumVariant }
    function Next(celt: LongWord; var rgvar: OleVariant;
      pceltFetched: PLongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone (out Enum: IEnumVariant): HResult; stdcall;
  protected
    FCollection: IVariantCollection;
    FIndex: integer;
  public
    constructor Create (const Collection: IVariantCollection);
    destructor Destroy; override;
  end;

  { AppId registration wrapper for EXE COM servers

    Usage: See implementation in ThreadComServ for usage.
  }
  TAppLocation = (alStorage, alLocal, alRemote);
  TAppIdentity = (aiInteractiveUser, aiLaunchingUser, aiThisUser);

  TAppId = class
  protected
    FAppId: TGUID;
    FDescription: string;
    FIdentity: TAppIdentity;
    FIdentityName: string;
    FLocation: TAppLocation;
    FRemoteServerName: string;
    FServiceName: string;
    procedure RegisterCoClass (cf: TComObjectFactory);
    procedure UnregisterCoClass (cf: TComObjectFactory);
  public
    constructor Create (const appid: TGUID; const sDescription: string);
    procedure Register (bRegister: boolean; cs: TComServerObject; cm: TComClassManager);
    property AppId: TGUID read FAppId write FAppId;
    property Description: string read FDescription write FDescription;
    property Identity: TAppIdentity read FIdentity write FIdentity;
    property IdentityName: string read FIdentityName write FIdentityName;
    property Location: TAppLocation read FLocation write FLocation;
    property RemoteServerName: string read FRemoteServerName write FRemoteServerName;
    property ServiceName: string read FServiceName write FServiceName;
  end;

  { IGlobalInterfaceTable wrappers }
const
  CLSID_StdGlobalInterfaceTable: TGUID = '{00000323-0000-0000-C000-000000000046}';

type
  IGlobalInterfaceTable = interface(IUnknown)
    ['{00000146-0000-0000-C000-000000000046}']
    function RegisterInterfaceInGlobal (pUnk: IUnknown; const riid: TIID;
      out dwCookie: DWORD): HResult; stdcall;
    function RevokeInterfaceFromGlobal (dwCookie: DWORD): HResult; stdcall;
    function GetInterfaceFromGlobal (dwCookie: DWORD; const riid: TIID; out ppv): HResult; stdcall;
  end;

  { TGlobalInterfacePointer

    Usage:

    Assuming you have an interface pointer, pObject1, whose interface, IObject1,
    you want to "globalize" in the GIT. This is accomplished by:

    var
      GIP1: TGIP;
    begin
      GIP1 := TGIP.Create (pObject1, IObject1);
    end;

    If you want to "localize" pObject1 later on or in some other thread, you
    will need to have access to the GIP1 object/variable:

    var
      pObject1: IObject1;
    begin
      GIP1.GetIntf (pObject1);
      pObject1.DoSomething;
    end;
  }
  TGIP = class
  protected
    FCookie: DWORD;
    FIID: TIID;
    function IsValid: boolean;
  public
    constructor Create (const Unk: IUnknown; const AIID: TIID);
    destructor Destroy; override;
    procedure GetIntf (out Intf);
    procedure RevokeIntf;
    procedure SetIntf (const Unk: IUnknown; const AIID: TIID);
    property Cookie: dword read FCookie;
    property IID: TGUID read FIID;
  end;

  { CoInitializeSecurity wrappers }
const
  { authentication levels }
  RPC_C_AUTHN_LEVEL_DEFAULT = 0;
  RPC_C_AUTHN_LEVEL_NONE = 1;
  RPC_C_AUTHN_LEVEL_CONNECT = 2;
  RPC_C_AUTHN_LEVEL_CALL = 3;
  RPC_C_AUTHN_LEVEL_PKT = 4;
  RPC_C_AUTHN_LEVEL_PKT_INTEGRITY = 5;
  RPC_C_AUTHN_LEVEL_PKT_PRIVACY = 6;

  alDefault = RPC_C_AUTHN_LEVEL_DEFAULT;
  alNone = RPC_C_AUTHN_LEVEL_NONE;
  alConnect = RPC_C_AUTHN_LEVEL_CONNECT;
  alCall = RPC_C_AUTHN_LEVEL_CALL;
  alPacket = RPC_C_AUTHN_LEVEL_PKT;
  alPacketIntegrity = RPC_C_AUTHN_LEVEL_PKT_INTEGRITY;
  alPacketPrivacy = RPC_C_AUTHN_LEVEL_PKT_PRIVACY;

  { impersonation levels }
  RPC_C_IMP_LEVEL_DEFAULT = 0;
  RPC_C_IMP_LEVEL_ANONYMOUS = 1;
  RPC_C_IMP_LEVEL_IDENTIFY = 2;
  RPC_C_IMP_LEVEL_IMPERSONATE = 3;
  RPC_C_IMP_LEVEL_DELEGATE = 4;

  ilDefault = RPC_C_IMP_LEVEL_DEFAULT;
  ilAnonymous = RPC_C_IMP_LEVEL_ANONYMOUS;
  ilIdentify = RPC_C_IMP_LEVEL_IDENTIFY;
  ilImpersonate = RPC_C_IMP_LEVEL_IMPERSONATE;
  ilDelegate = RPC_C_IMP_LEVEL_DELEGATE;

  { authentication capabilities }
  EOAC_NONE                    = $0;
  EOAC_DEFAULT                 = $800;
  EOAC_MUTUAL_AUTH             = $1;
  EOAC_STATIC_CLOAKING         = $20;
  EOAC_DYNAMIC_CLOAKING        = $40;
  EOAC_ANY_AUTHORITY           = $80;

  // These are only valid for CoInitializeSecurity
  EOAC_SECURE_REFS             = $2;
  EOAC_ACCESS_CONTROL          = $4;
  EOAC_APPID                   = $8;
  EOAC_MAKE_FULLSIC            = $100;
  EOAC_REQUIRE_FULLSIC         = $200;
  EOAC_AUTO_IMPERSONATE        = $400;

  RPC_C_AUTHN_WINNT = 10;

  RPC_C_AUTHZ_NONE = 0;
  RPC_C_AUTHZ_NAME = 1;
  RPC_C_AUTHZ_DCE = 2;
  
const
  CLSID_DCOMAccessControl: TGUID = '{0000031D-0000-0000-C000-000000000046}';

type
  { IAccessControl }
  IAccessControl = interface (IUnknown)
    ['{EEDD23E0-8410-11CE-A1C3-08002B2B8D8F}']
    function GrantAccessRights (pAccessList: pointer): HResult; stdcall;
    function SetAccessRights (pAccessList: pointer): HResult; stdcall;
    function SetOwner (pOwner: pointer; pGroup: pointer): HResult; stdcall;
    function RevokeAccessRights (lpProperty: pointer; cTrustees: ulong; prgTrustees: pointer): HResult; stdcall;
    function GetAllAccessRights (lpProperty: pointer; ppAccessList: pointer;
      ppOwner: pointer; ppGroup: pointer): HResult; stdcall;
    function IsAccessAllowed (pTrustee: pointer; lpProperty: pointer;
      AccessRights: ulong; out pfAccessAllowed: bool): HResult; stdcall;
  end;

  { Connection points enhancement wrapper classes for event handling }
type
  { TConnectionPoints and TConnectionPoint are included in here for the
    following fixes over the VCL versions:
    1) memory leak in the VCL IEnumConnections and IEnumConnectionPoints impl

    Hopefully, when Borland fixes the VCL, we can get rid of these!
  }
  TConnectionPoints = class;
  TConnectionPoint = class (AxCtrls.TConnectionPoint, IConnectionPoint)
  protected
    { IConnectionPoint }
    function EnumConnections (out Enum: IEnumConnections): HResult; stdcall;
    function Advise(const unkSink: IUnknown; out dwCookie: Longint): HResult; stdcall;
    function Unadvise(dwCookie: Longint): HResult; stdcall;
  protected
    FIID: TGUID;
    FSinkList: TList;  // dummy sink list
  public
    constructor Create(Container: TConnectionPoints;
      const IID: TGUID; Kind: TConnectionKind; OnConnect: TConnectEvent);
    destructor Destroy; override;
  end;

  TConnectionPoints = class (AxCtrls.TConnectionPoints {$IFDEF D3}, IConnectionPointContainer {$ENDIF})
  protected
    { IEnumConnectionPoints }
    function EnumConnectionPoints (out Enum: IEnumConnectionPoints): HResult; stdcall;
    function FindConnectionPoint(const iid: TIID; out cp: IConnectionPoint): HResult; stdcall;
  protected
    FConnectionPoints: TList;
  public
    constructor Create(const AController: IUnknown);
    destructor Destroy; override;
    function CreateConnectionPoint(const IID: TGUID; Kind: TConnectionKind;
      OnConnect: TConnectEvent): TConnectionPoint;
  end;

  TEventSinkForEachEvent = procedure (pUnk: IUnknown; var bContinue: boolean) of object;
  TEventSink = class (TObject, IDispatch)
  protected
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  protected
    FCP: TConnectionPoint;
    FSinkIID: TGUID;
  public
    constructor Create (cp: TConnectionPoint; const IID: TGUID);
    procedure ForEach (Event: TEventSinkForEachEvent);
    property SinkIID: TGUID read FSinkIID;
  end;

  TEventSinks = class (TObject, IUnknown {$IFDEF D3}, IConnectionPointContainer {$ENDIF})
  protected
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IConnectionPointContainer }
    function EnumConnectionPoints(out enumconn: IEnumConnectionPoints): HResult; stdcall;
    function FindConnectionPoint(const iid: TIID; out cp: IConnectionPoint): HResult; stdcall;
  protected
    FSinks: TList;
    FCPC: TConnectionPoints;
    procedure AttachSinks (const ClassTypeInfo: ITypeInfo);
    procedure Clear;
    function CreateSink (const IID: TGUID): TEventSink;
    function GetCount: integer;
    function GetSinkByIID (const IID: TGUID): TEventSink;
    function GetSinks (i: integer): TEventSink;
    procedure Initialize (AOwner: TObject; Simple: boolean);
  public
    constructor Create (AOwner: TObject);
    constructor CreateSimple (AOwner: TObject);
    destructor Destroy; override;
    function AddEvent (const EventIID: TGUID): TEventSink;
    property Count: integer read GetCount;
    property SinkByIID [const IID: TGUID]: TEventSink read GetSinkByIID;
    property Sinks [i: integer]: TEventSink read GetSinks;
  end;

  //simple TInterfaceList for D3
  {$IFDEF D3}
  TInterfaceList = class
  protected
    FList: TList;
    function GetCount: integer;
    function GetItems (Index: integer): IUnknown;
    procedure SetItems (Index: integer; const Item: IUnknown);
  public
    constructor Create;
    destructor Destroy; override;
    function Add (const Item: IUnknown): integer;
    procedure Clear;
    property Count: integer read GetCount;
    property Items [Index: integer]: IUnknown read GetItems write SetItems;
  end;
  {$ENDIF}

{ HKCR registry routines }
procedure DeleteRegValue (const sKey, sName: string);
function GetRegValue (const sKey, sName: string): string;
function RegKeyExists (const sKey: string): boolean;
function RegValueExists (const sKey, sName: string): boolean;

{ returns CLSID_StdGlobalInterfaceTable object }
function GIT: IGlobalInterfaceTable;

{ returns DCOMAccessControl object }
function DCOMAccessControl: IAccessControl;

{ simplified security initialization, must be called after first CoInitialize/Ex }
procedure InitializeCOMSecurity (iAuthLevel, iImpLevel: longint);

implementation

uses
  Registry, SysUtils, Variants;

procedure DeleteRegValue (const sKey, sName: string);
var
  hkTemp: HKey;
begin
  if (RegOpenKeyEx (HKEY_CLASSES_ROOT, PChar (sKey), 0,
      KEY_ALL_ACCESS, hkTemp) = ERROR_SUCCESS)
  then begin
    RegDeleteValue (hkTemp, PChar (sName));
    RegCloseKey (hkTemp);
  end;  { if }
end;

function GetRegValue (const sKey, sName: string): string;
var
  reg: TRegistry;
begin
  Result := '';
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    if (reg.OpenKey (sKey, FALSE)) then
      Result := reg.ReadString (sName);
  finally
    reg.Free;
  end;  { finally }
end;

function RegKeyExists (const sKey: string): boolean;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    Result := reg.KeyExists (sKey);
  finally
    reg.Free;
  end;  { finally }
end;

function RegValueExists (const sKey, sName: string): boolean;
var
  reg: TRegistry;
begin
  Result := FALSE;
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    if (reg.OpenKey (sKey, FALSE)) then
      Result := reg.ValueExists (sName);
  finally
    reg.Free;
  end;  { finally }
end;

{ returns CLSID_StdGlobalInterfaceTable object }
function GIT: IGlobalInterfaceTable;
var
  cGIT: IGlobalInterfaceTable;
begin
  if (cGIT = nil) then
    OleCheck (CoCreateInstance (CLSID_StdGlobalInterfaceTable, nil, CLSCTX_ALL,
      IGlobalInterfaceTable, cGIT));
  Result := cGIT;
end;

{ returns DCOMAccessControl object }
function DCOMAccessControl: IAccessControl;
begin
  OleCheck (CoCreateInstance (CLSID_DCOMAccessControl, nil, CLSCTX_ALL, IAccessControl, Result));
end;

{ simplified security initialization, must be called after first CoInitialize/Ex }
procedure InitializeCOMSecurity (iAuthLevel, iImpLevel: longint);
begin
  OleCheck (CoInitializeSecurity (nil, -1, nil, nil, iAuthLevel, iImpLevel, nil, EOAC_NONE, nil));
end;


{ TEnumVariant }

constructor TEnumVariant.CreateUnknown (const Unk: IUnknown);
begin
  inherited Create;
  if (Unk <> nil) then AttachUnknown (Unk);
end;

constructor TEnumVariant.Create (const Unk: variant);
begin
  inherited Create;
  if not (VarIsEmpty (Unk)) then Attach (Unk);
end;

procedure TEnumVariant.AttachUnknown (const Unk: IUnknown);
var
  pDisp: IDispatch;
  _NewEnumPropFailed: boolean;
  Unknown: IUnknown;
begin
  Detach;
  Unknown := Unk;
  { extract IEnumVariant }
  if (Unknown <> nil) then
  begin
    { try IEnumVariant }
    if not (Succeeded (Unknown.QueryInterface (IEnumVariant, FEnumVariant))) then
    begin
      FEnumVariant := nil;  // safety!

      { test _NewEnum prop and _NewEnum func }
      if (Succeeded (Unknown.QueryInterface (IDispatch, pDisp))) then
      begin
        _NewEnumPropFailed := False;
        try
          //property _NewEnum
          Unknown := TDispNewEnum (pDisp)._NewEnum;
          if not (Succeeded (Unknown.QueryInterface (IEnumVariant, FEnumVariant))) then
            FEnumVariant := nil;  // safety!
        except
          _NewEnumPropFailed := True;
        end;  { except }

        if (_NewEnumPropFailed) then
        try
          //function _NewEnum
          Unknown := TDispNewEnum (pDisp)._NewEnumFunc;
          if not (Succeeded (Unknown.QueryInterface (IEnumVariant, FEnumVariant))) then
            FEnumVariant := nil;  // safety!
        except
          { doesn't support _NewEnum! oh well! }
        end;
      end;  { if }
    end;  { if }
  end;  { if }

  { if IEnumVariant is undefined then error out! }
  if (FEnumVariant = nil) then
    raise Exception.Create ('Object does not support enumeration (IEnumVariant)');

  Reset;
end;

procedure TEnumVariant.Attach (const Unk: variant);
begin
  AttachUnknown (IUnknown (Unk));
end;

procedure TEnumVariant.Detach;
begin
  FEnumVariant := nil;
end;

function TEnumVariant.ForEach (out Data: olevariant): boolean;
var
  Fetched: longint;
begin
  Result := Next (1, Data, Fetched);
end;

{$IFNDEF D3}
function TEnumVariant.ForEach (out Obj; const IID: TGUID): boolean;
begin
  Result := ForEachObject (Obj, IID);
end;
{$ENDIF}

function TEnumVariant.ForEachObject (out Obj; const IID: TGUID): boolean;
var
  Fetched: longint;
  Data: olevariant;
begin
  Result := Next (1, Data, Fetched);
  while Result and VarIsEmpty (Data) do
    Result := Next (1, Data, Fetched);
  if (Result) then OleCheck (IUnknown (Data).QueryInterface (IID, Obj));
end;

function TEnumVariant.Next (Count: integer; out Data; var Fetched: longint): boolean;
begin
  Assert (FEnumVariant <> nil);
  Result := (FEnumVariant.Next (Count, olevariant (Data), @Fetched) = S_OK);
end;

procedure TEnumVariant.Reset;
begin
  Assert (FEnumVariant <> nil);
  OleCheck (FEnumVariant.Reset);
end;


{ TAppId }

procedure TAppId.RegisterCoClass (cf: TComObjectFactory);
var
  sClassKey, sLocalServer: string;
begin
  CreateRegKey ('CLSID\' + GuidToString (cf.ClassId), 'AppId', GuidToString (AppId));

  sClassKey := 'CLSID\' + GuidToString (cf.ClassId);
  case Location of
    alLocal :
      DeleteRegKey (sClassKey + '\_LocalServer32');  { just in case! }
    else
      { if Location is not Local, we have to rename CLSID\LocalServer32 key }
      if (RegKeyExists (sClassKey + '\LocalServer32')) then
      begin
        sLocalServer := GetRegValue (sClassKey + '\LocalServer32', '');
        DeleteRegKey (sClassKey + '\LocalServer32');
        CreateRegKey (sClassKey + '\_LocalServer32', '', sLocalServer);
      end;  { if }
  end;  { case }
end;

procedure TAppId.UnregisterCoClass (cf: TComObjectFactory);
var
  sClassKey: string;
begin
  sClassKey := 'CLSID\' + GuidToString (cf.ClassId);
  DeleteRegValue (sClassKey, 'AppId');
end;

constructor TAppId.Create (const appid: TGUID; const sDescription: string);
begin
  inherited Create;
  FAppId := appid;
  Identity := aiLaunchingUser;
  Location := alLocal;
  Description := sDescription;
end;

procedure TAppId.Register (bRegister: boolean; cs: TComServerObject; cm: TComClassManager);
var
  sAppIdKey: string;

 procedure RegisterLocation;
 begin
   { RemoteServerName, LocalService, ... }
   DeleteRegValue (sAppIdKey, 'ActivateAtStorage');
   DeleteRegValue (sAppIdKey, 'LocalService');
   DeleteRegValue (sAppIdKey, 'RemoteServerName');

   if (Location = alStorage) then
     CreateRegKey (sAppIdKey, 'ActivateAtStorage', 'Y');

   if (RemoteServerName <> '') then
     CreateRegKey (sAppIdKey, 'RemoteServerName', RemoteServerName)
   else
     if (ServiceName <> '') then CreateRegKey (sAppIdKey, 'LocalService', ServiceName);
 end;

 procedure RegisterIdentity;
 begin
   { RunAs }
   DeleteRegValue (sAppIdKey, 'RunAs');

   case Identity of
     aiInteractiveUser :
       CreateRegKey (sAppIdKey, 'RunAs', 'Interactive User');
     aiThisUser :
       if (IdentityName <> '') then CreateRegKey (sAppIdKey, 'RunAs', IdentityName);
   end;  { case }
 end;

begin
  sAppIdKey := 'AppId\' + GuidToString (AppId);
  if (bRegister) then
  begin
    { AppId }
    CreateRegKey (sAppIdKey, '', Description);
    { Location }
    RegisterLocation;
    { Identity }
    RegisterIdentity;

    if (cm <> nil) then cm.ForEachFactory (cs, RegisterCoClass);
  end
  else begin
    DeleteRegKey (sAppIdKey);
    if (cm <> nil) then cm.ForEachFactory (cs, UnregisterCoClass);
  end;  { else }
end;


{ TGIP }

function TGIP.IsValid: boolean;
begin
  Result := (FCookie <> 0);
end;

constructor TGIP.Create (const Unk: IUnknown; const AIID: TIID);
begin
  inherited Create;
  SetIntf (Unk, AIID);
end;

destructor TGIP.Destroy;
begin
  RevokeIntf;
  inherited;
end;

procedure TGIP.GetIntf (out Intf);
begin
  Assert (IsValid);
  OleCheck (GIT.GetInterfaceFromGlobal (FCookie, FIID, Intf));
end;

procedure TGIP.RevokeIntf;
begin
  if not (IsValid) then Exit;
  OleCheck (GIT.RevokeInterfaceFromGlobal (FCookie));
  FCookie := 0;
  FIID := GUID_NULL;
end;

procedure TGIP.SetIntf (const Unk: IUnknown; const AIID: TIID);
begin
  Assert ((Unk <> nil) and not (IsEqualGuid (AIID, GUID_NULL)));
  RevokeIntf;
  OleCheck (GIT.RegisterInterfaceInGlobal (Unk, AIID, FCookie));
  FIID := AIID;
end;


{ TEnumConnections }

type
  TEnumConnections = class (TInterfacedObject, IEnumConnections)
  private
    FConnectionPoint: TConnectionPoint;
    FIndex: Integer;
    FCount: Integer;
  protected
    { IEnumConnections }
    function Next(celt: Longint; out elt; pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enumconn: IEnumConnections): HResult; stdcall;
  public
    constructor Create(ConnectionPoint: TConnectionPoint; Index: Integer);
    destructor Destroy; override;
  end;

constructor TEnumConnections.Create(ConnectionPoint: TConnectionPoint;
  Index: Integer);
begin
  inherited Create;
  ConnectionPoint.Controller._AddRef;  // this is only the locking we need!
  FConnectionPoint := ConnectionPoint;
  FIndex := Index;
  FCount := ConnectionPoint.FSinkList.Count;
end;

destructor TEnumConnections.Destroy;
begin
  FConnectionPoint.Controller._Release;  // unlock!
  inherited;
end;

function TEnumConnections.Next(celt: Longint; out elt;
  pceltFetched: PLongint): HResult;
type
  TConnectDatas = array[0..1023] of TConnectData;
var
  I: Integer;
  P: Pointer;
begin
  I := 0;
  while (I < celt) and (FIndex < FCount) do
  begin
    P := FConnectionPoint.FSinkList[FIndex];
    if P <> nil then
    begin
      Pointer(TConnectDatas(elt)[I].pUnk) := nil;
      TConnectDatas(elt)[I].pUnk := IUnknown(P);
      TConnectDatas(elt)[I].dwCookie := FIndex + 1;
      Inc(I);
    end;
    Inc(FIndex);
  end;
  if pceltFetched <> nil then pceltFetched^ := I;
  if I = celt then Result := S_OK else Result := S_FALSE;
end;

function TEnumConnections.Skip(celt: Longint): HResult; stdcall;
begin
  Result := S_FALSE;
  while (celt > 0) and (FIndex < FCount) do
  begin
    if FConnectionPoint.FSinkList[FIndex] <> nil then Dec(celt);
    Inc(FIndex);
  end;
  if celt = 0 then Result := S_OK;
end;

function TEnumConnections.Reset: HResult; stdcall;
begin
  FIndex := 0;
  Result := S_OK;
end;

function TEnumConnections.Clone(out enumconn: IEnumConnections): HResult; stdcall;
begin
  try
    enumconn := TEnumConnections.Create(FConnectionPoint, FIndex);
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;


{ TEnumConnectionPoints }

type
  TEnumConnectionPoints = class(TInterfacedObject, IEnumConnectionPoints)
  private
    FContainer: TConnectionPoints;
    FIndex: Integer;
  protected
    { IEnumConnectionPoints }
    function Next(celt: Longint; out elt;
      pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enumconn: IEnumConnectionPoints): HResult; stdcall;
  public
    constructor Create(Container: TConnectionPoints; Index: Integer);
    destructor Destroy; override;
  end;

constructor TEnumConnectionPoints.Create(Container: TConnectionPoints;
  Index: Integer);
begin
  inherited Create;
  Container.Controller._AddRef;  // this is only the locking we need!
  FContainer := Container;
  FIndex := Index;
end;

destructor TEnumConnectionPoints.Destroy;
begin
  FContainer.Controller._Release;  // unlock
  inherited;
end;

type
  TPointerList = array[0..0] of Pointer;

function TEnumConnectionPoints.Next(celt: Longint; out elt;
  pceltFetched: PLongint): HResult;
var
  I: Integer;
  P: Pointer;
begin
  I := 0;
  while (I < celt) and (FIndex < FContainer.FConnectionPoints.Count) do
  begin
    P := Pointer(IConnectionPoint(TConnectionPoint(
      FContainer.FConnectionPoints[FIndex])));
    IConnectionPoint(P)._AddRef;
    TPointerList(elt)[I] := P;
    Inc(I);
    Inc(FIndex);
  end;
  if pceltFetched <> nil then pceltFetched^ := I;
  if I = celt then Result := S_OK else Result := S_FALSE;
end;

function TEnumConnectionPoints.Skip(celt: Longint): HResult; stdcall;
begin
  if FIndex + celt <= FContainer.FConnectionPoints.Count then
  begin
    FIndex := FIndex + celt;
    Result := S_OK;
  end else
  begin
    FIndex := FContainer.FConnectionPoints.Count;
    Result := S_FALSE;
  end;
end;

function TEnumConnectionPoints.Reset: HResult; stdcall;
begin
  FIndex := 0;
  Result := S_OK;
end;

function TEnumConnectionPoints.Clone(
  out enumconn: IEnumConnectionPoints): HResult; stdcall;
begin
  try
    enumconn := TEnumConnectionPoints.Create(FContainer, FIndex);
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;


{ TConnectionPoint }

function TConnectionPoint.EnumConnections(out Enum: IEnumConnections): HResult;
begin
  Enum := TEnumConnections.Create(Self, 0);
  Result := S_OK;
end;

function TConnectionPoint.Advise (const unkSink: IUnknown; out dwCookie: Longint): HResult;
var
  iIndex: integer;
begin
  Result := inherited Advise (unkSink, dwCookie);
  if (Result = S_OK) then
  begin
    //!warning: implicit assumption about internal implementation!
    iIndex := dwCookie - 1;
    if (iIndex = FSinkList.Count) then FSinkList.Add (nil);
    Assert ((iIndex >= 0) and (iIndex < FSinkList.Count));
    FSinkList [iIndex] := pointer (unkSink);
  end;  { if }
end;

function TConnectionPoint.Unadvise(dwCookie: Longint): HResult;
var
  iIndex: integer;
begin
  Result := inherited Unadvise (dwCookie);
  if (Result = S_OK) then
  begin
    //!warning: implicit assumption about internal implementation!
    iIndex := dwCookie - 1;
    Assert ((iIndex >= 0) and (iIndex < FSinkList.Count));
    FSinkList [iIndex] := nil;
  end;  { if }
end;

constructor TConnectionPoint.Create(Container: TConnectionPoints;
  const IID: TGUID; Kind: TConnectionKind; OnConnect: TConnectEvent);
begin
  inherited Create (Container, IID, Kind, OnConnect);
  FIID := IID;
  FSinkList := TList.Create;
end;

destructor TConnectionPoint.Destroy;
begin
  FSinkList.Free;
  inherited;
end;


{ TConnectionPoints }

function TConnectionPoints.EnumConnectionPoints (out Enum: IEnumConnectionPoints): HResult;
begin
  Enum := TEnumConnectionPoints.Create (Self, 0);
  Result := S_OK;
end;

{ have to reimplement FindConnectionPoint here like this because for some
  unknown reason, the default VCL implementation bypasses our virtualized
  IConnectionPoint interface pointer when the server is compiled as a DLL.
  It appears that the Delphi compiler flakes out generating the correct code!
}
function TConnectionPoints.FindConnectionPoint (const iid: TIID;
  out cp: IConnectionPoint): HResult;
var
  i: integer;
  cpTemp: TConnectionPoint;
begin
  for i := 0 to FConnectionPoints.Count - 1 do
  begin
    cpTemp := FConnectionPoints [i];
    if IsEqualGUID (cpTemp.FIID, iid) then
    begin
      cp := cpTemp;
      Result := S_OK;
      Exit;
    end;  { if }
  end;  { for }
  Result := CONNECT_E_NOCONNECTION;
end;

constructor TConnectionPoints.Create(const AController: IUnknown);
begin
  inherited Create (AController);
  FConnectionPoints := TList.Create;
end;

destructor TConnectionPoints.Destroy;
begin
  FConnectionPoints.Free;
  inherited;
end;

function TConnectionPoints.CreateConnectionPoint(const IID: TGUID; Kind: TConnectionKind;
  OnConnect: TConnectEvent): TConnectionPoint;
begin
  Result := TConnectionPoint.Create (Self, IID, Kind, OnConnect);
  FConnectionPoints.Add (Result);
end;


{ TEventSink }

function TEventSink.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := E_NOINTERFACE;
  if (GetInterface (IID, Obj)) then
    Result := S_OK
  else
  if (IsEqualGuid (IID, SinkIID) or IsEqualGuid (IDispatch, SinkIID)) then
  begin
    IDispatch (Obj) := Self;
    Result := S_OK;
    //if (GetInterface (IDispatch, Obj)) then Result := S_OK;
  end;
end;

function TEventSink._AddRef: Integer;
begin
  Result := 2;
end;

function TEventSink._Release: Integer;
begin
  Result := 1;
end;

function TEventSink.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Count := 0;
  Result := E_NOTIMPL;
end;

function TEventSink.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
begin
  pointer (TypeInfo) := nil;
  Result := E_NOTIMPL;
end;

function TEventSink.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TEventSink.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
  Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
var
  pec: IEnumConnections;
  cd: TConnectData;
begin
  { iterate and invoke }
  OleCheck ((FCP as IConnectionPoint).EnumConnections (pec));
  while (pec.Next (1, cd, nil) = S_OK) do
  begin
    try
      (cd.pUnk as IDispatch).Invoke (DispId, IID, LocaleId, Flags, Params,
                                     VarResult, ExcepInfo, ArgErr);
      cd.pUnk := nil;
    except
      cd.pUnk := nil;
    end;  { except }
  end;  { while }
  Result := S_OK;
end;

constructor TEventSink.Create (cp: TConnectionPoint; const IID: TGUID);
begin
  Assert ((cp <> nil) and not IsEqualGuid (IID, GUID_NULL));
  inherited Create;
  FCP := cp;
  FSinkIID := IID;
end;

procedure TEventSink.ForEach (Event: TEventSinkForEachEvent);
var
  pec: IEnumConnections;
  cd: TConnectData;
  bContinue: boolean;
begin
  if not Assigned (Event) then Exit;

  { iterate and invoke }
  bContinue := TRUE;
  OleCheck ((FCP as IConnectionPoint).EnumConnections (pec));
  while (pec.Next (1, cd, nil) = S_OK) do
  begin
    try
      Event (cd.pUnk, bContinue);
      cd.pUnk := nil;
    except
      cd.pUnk := nil;
    end;  { except }
    if not (bContinue) then Break;
  end;  { while }
end;


{ TEventSinks }

function TEventSinks.QueryInterface(const IID: TGUID; out Obj): HResult;
var
  es: TEventSink;
begin
  Result := E_NOINTERFACE;
  if (GetInterface (IID, Obj)) then
    Result := S_OK
  else begin
    es := SinkByIID [IID];
    if (es = nil) then Exit;
    Result := es.QueryInterface (IID, Obj);
  end;  { else }
end;

function TEventSinks._AddRef: Integer;
begin
  Result := 2;
end;

function TEventSinks._Release: Integer;
begin
  Result := 1;
end;

function TEventSinks.EnumConnectionPoints(out enumconn: IEnumConnectionPoints): HResult;
begin
  Result := FCPC.EnumConnectionPoints (enumconn);
end;

function TEventSinks.FindConnectionPoint(const iid: TIID; out cp: IConnectionPoint): HResult;
begin
  Result := FCPC.FindConnectionPoint (IID, cp);
end;

procedure TEventSinks.AttachSinks (const ClassTypeInfo: ITypeInfo);
var
  pta: PTypeAttr;
  iRefType: HRefType;
  i, iImplCount, iFlags: integer;
  pti, ptiTemp: ITypeInfo;
begin
  if (ClassTypeInfo = nil) then Exit;

  { load classinfo from type lib }
  //if not Succeeded (TypeLib.GetTypeInfoOfGUID (clsid, pti)) then Exit;
  pti := ClassTypeInfo;
  Assert (pti <> nil);

  { get impl count }
  OleCheck (pti.GetTypeAttr (pta));
  iImplCount := pta^.cImplTypes;
  pti.ReleaseTypeAttr (pta);

  { cycle impls and find ones that are marked SOURCE }
  for i := 0 to iImplCount - 1 do
  begin
    OleCheck (pti.GetImplTypeFlags (i, iFlags));
    if (iFlags and IMPLTYPEFLAG_FSOURCE = IMPLTYPEFLAG_FSOURCE) then
    begin
      OleCheck (pti.GetRefTypeOfImplType (i, iRefType));
      OleCheck (pti.GetRefTypeInfo (iRefType, ptiTemp));

      { load as new cp }
      ptiTemp.GetTypeAttr (pta);
      CreateSink (pta^.Guid);
      ptiTemp.ReleaseTypeAttr (pta);
    end;  { if }
  end;  { for }
end;

procedure TEventSinks.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do Sinks [i].Free;
  FSinks.Clear;
end;

function TEventSinks.CreateSink (const IID: TGUID): TEventSink;
var
  cp: TConnectionPoint;
begin
  cp := FCPC.CreateConnectionPoint (IID, ckMulti, nil);
  Result := TEventSink.Create (cp, IID);
  FSinks.Add (Result);
end;

function TEventSinks.GetCount: integer;
begin
  Result := FSinks.Count;
end;

function TEventSinks.GetSinkByIID (const IID: TGUID): TEventSink;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if (IsEqualGuid (Sinks [i].SinkIID, IID)) then
    begin
      Result := Sinks [i];
      Break;
    end;  { if }
end;

function TEventSinks.GetSinks (i: integer): TEventSink;
begin
  Assert ((i >= 0) and (i < Count));
  Result := FSinks [i];
end;

procedure TEventSinks.Initialize (AOwner: TObject; Simple: boolean);
var
  cf: TComObjectFactory;
  Controller: IUnknown;
  TypeInfo: ITypeInfo;
begin
  TypeInfo := nil;
  cf := nil;
  if (AOwner.InheritsFrom (TAutoIntfObject)) then
  begin
    Controller := TAutoIntfObject (AOwner);
  end
  else
  if (AOwner.InheritsFrom (TComObject)) then
  begin
    cf := TComObject (AOwner).Factory;
    Controller := TComObject (AOwner);
  end
  else
  if (AOwner.InheritsFrom (TComObjectFactory)) then
  begin
    cf := TComObjectFactory (AOwner);
    Controller := cf;
  end
  else
    raise Exception.Create ('Invalid owner for TEventSinks');

  FCPC := TConnectionPoints.Create (Controller);
  FSinks := TList.Create;

  if (cf = nil) then Exit;
  
  {$IFNDEF D3}
  if (Simple) then
  begin
    Assert (cf.InheritsFrom (TAutoObjectFactory));
    CreateSink (TAutoObjectFactory (cf).EventIID);
    Exit;
  end;  { if }
  {$ENDIF}

  if TypeInfo = nil then
    OleCheck (cf.ComServer.TypeLib.GetTypeInfoOfGuid (cf.ClassID, TypeInfo));
  AttachSinks (TypeInfo);
end;

{ standard ctor, handles all implemented interfaces that are marked as Source }
constructor TEventSinks.Create (AOwner: TObject);
begin
  Assert (AOwner <> nil);
  inherited Create;
  Initialize (AOwner, FALSE);
end;

{ optimized ctor for autoobjects that have 1 event interface only. In D4,
  CreateSimple can be very much faster instead of Create when creating a lot
  of autoobjects that have only 1 event interface (cached in the classfactory)
  because we don't need to reread the type library to get to the event dispintf!
}
constructor TEventSinks.CreateSimple (AOwner: TObject);
begin
  {$IFDEF D3}
  Create (AOwner);
  {$ELSE}
  Assert (AOwner <> nil);
  inherited Create;
  Initialize (AOwner, AOwner.InheritsFrom (TAutoObjectFactory));
  {$ENDIF}
end;

destructor TEventSinks.Destroy;
begin
  Clear;
  FSinks.Free;
  FCPC.Free;
  inherited;
end;

function TEventSinks.AddEvent(const EventIID: TGUID): TEventSink;
begin
  Result := CreateSink (EventIID);
end;

{ TVariantCollection }

type
  POleVariant = ^olevariant;

function TVariantCollection._AddRef: Integer;
begin
  if (RefCounted) then
    Result := inherited _AddRef
  else
    Result := 2;
end;

function TVariantCollection._Release: Integer;
begin
  if (RefCounted) then
    Result := inherited _Release
  else
    Result := 1;
end;

function TVariantCollection.GetTypeInfoCount (out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
  Count := 0;
end;

function TVariantCollection.GetTypeInfo (Index, LocaleID: Integer; out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
  pointer (TypeInfo) := nil;
end;

function TVariantCollection.GetIDsOfNames (const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; 
begin
  Result := E_NOTIMPL;
end;

function TVariantCollection.Invoke (DispID: Integer; const IID: TGUID; LocaleID: Integer;
  Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
begin
  if (DispId = -4) then
  begin
    { convenience! handle query for IEnumVariant }
    POleVariant (VarResult)^ := GetEnum as IUnknown;
    Result := S_OK;
  end
  else
    Result := E_NOTIMPL;
end;

function TVariantCollection.GetController: IUnknown;
begin
  Result := IUnknown (FController);
end;

function TVariantCollection.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TVariantCollection.GetItems (const Index: olevariant): olevariant;
var
  iIndex: integer;
begin
  if (VarType (Index) in [varInteger, varSmallInt]) then
    iIndex := Index
  else
    iIndex := FItems.IndexOf (Index);
  Result := POleVariant (FItems.Objects [iIndex])^;
end;

procedure TVariantCollection.SetItems (const Index: olevariant; const Item: olevariant);
var
  iIndex: integer;
begin
  if (VarType (Index) in [varInteger, varSmallInt]) then
    iIndex := Index
  else
    iIndex := FItems.IndexOf (Index);
  POleVariant (FItems.Objects [iIndex])^ := Item;
end;

constructor TVariantCollection.Create (const Controller: IUnknown);
begin
  inherited Create;
  FItems := TStringList.Create;
  { controller allows enumerator clients to lock the container object }
  FController := pointer (Controller);
end;

destructor TVariantCollection.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TVariantCollection.Add (const Item: olevariant): integer;
begin
  Result := AddObject ('', Item);
end;

function TVariantCollection.AddObject (const Key: string; const Item: olevariant): integer;
begin
  Result := FItems.AddObject (Key, TObject (new (POleVariant)));
  Items [Result] := Item;
end;

procedure TVariantCollection.Clear;
var
  i: integer;
begin
  for i := Count - 1 downto 0 do
    Delete (i);
end;

procedure TVariantCollection.Delete (const Index: olevariant);
var
  iIndex: integer;
begin
  if (VarType (Index) in [varInteger, varSmallInt]) then
    iIndex := Index
  else
    iIndex := FItems.IndexOf (Index);
  Items [iIndex] := Unassigned;
  Dispose (POleVariant (FItems.Objects [iIndex]));
  FItems.Delete (iIndex);
end;

function TVariantCollection.GetEnum: IEnumVariant;
begin
  Result := TEnumVariantCollection.Create (Self);
end;


{ TEnumVariantCollection }

function TEnumVariantCollection.Next (celt: LongWord; var rgvar: OleVariant;
  pceltFetched: PLongWord): HResult;
type
  TVariantList = array [0..0] of olevariant;
var
  i: longword;
begin
  i := 0;
  while (i < celt) and (FIndex < FCollection.GetCount) do
  begin
    TVariantList (rgvar) [i] := FCollection.GetItems (FIndex);
    inc (i);
    inc (FIndex);
  end;  { while }
  if (pceltFetched <> nil) then pceltFetched^ := i;
  if (i = celt) then Result := S_OK else Result := S_FALSE;
end;

function TEnumVariantCollection.Skip (celt: LongWord): HResult;
begin
  if ((FIndex + integer (celt)) <= FCollection.GetCount) then
  begin
    inc (FIndex, celt);
    Result := S_OK;
  end
  else begin
    FIndex := FCollection.GetCount;
    Result := S_FALSE;
  end;  { else }
end;

function TEnumVariantCollection.Reset: HResult;
begin
  FIndex := 0;
  Result := S_OK;
end;

function TEnumVariantCollection.Clone (out Enum: IEnumVariant): HResult;
begin
  Enum := TEnumVariantCollection.Create (FCollection);
  Result := S_OK;
end;

constructor TEnumVariantCollection.Create (const Collection: IVariantCollection);
begin
  Assert (Collection <> nil);
  inherited Create;
  FCollection := Collection;
  if (FCollection.GetController <> nil) then FCollection.GetController._AddRef;
end;

destructor TEnumVariantCollection.Destroy;
begin
  if (FCollection.GetController <> nil) then FCollection.GetController._Release;
  inherited;
end;

{$IFDEF D3}
//TInterfaceList
function TInterfaceList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TInterfaceList.GetItems (Index: integer): IUnknown;
begin
  Result := IUnknown (FList.Items [Index]);
end;

procedure TInterfaceList.SetItems (Index: integer; const Item: IUnknown);
begin
  Item._AddRef;
  FList.Items [Index] := pointer (Item);
end;

constructor TInterfaceList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TInterfaceList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TInterfaceList.Add (const Item: IUnknown): integer;
begin
  Item._AddRef;
  Result := FList.Add (pointer (Item));
end;

procedure TInterfaceList.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    IUnknown (FList.Items [i])._Release;
    FList.Items [i] := nil;
  end;
end;
{$ENDIF}

end.
