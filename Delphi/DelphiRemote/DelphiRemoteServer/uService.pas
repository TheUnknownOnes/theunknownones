unit uService;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ComLib, ActiveX, DelphiRemoteServer_TLB, StdVcl;

type
  TService = class(TAutoObject, IService)
  protected
    function Get_IDE(idx: Integer): IDispatch; safecall;
    function Get_IDECount: Integer; safecall;
    procedure RegisterIDE(const IDE: IDispatch); safecall;
    procedure UnregisterIDE(const IDE: IDispatch); safecall;
    function Get_IDEs: IEnumVARIANT; safecall;
    function Get_IDE_Properties(const IDE: IDispatch): IEnumVARIANT; safecall;
  end;

implementation

uses ComServ, uGlobal, SysUtils, Dialogs;

function TService.Get_IDE(idx: Integer): IDispatch;
begin
  Result:=Global.DelphiIDEs[idx] as IDispatch;
end;

function TService.Get_IDECount: Integer;
begin
  Result:=Global.DelphiIDEs.Count;
end;

procedure TService.RegisterIDE(const IDE: IDispatch);
begin
  Global.DelphiIDEs.Add(IDE);
end;

procedure TService.UnregisterIDE(const IDE: IDispatch);
begin
  Global.DelphiIDEs.Remove(IDE);
end;

function TService.Get_IDEs: IEnumVARIANT;
var
  varColl : TVariantCollection;
  idx : integer;
begin

  varColl:=TVariantCollection.Create(self);
  for idx := 0 to Global.DelphiIDEs.Count - 1 do
  begin
    varColl.Add(Self.Get_IDE(idx));
  end;
  Supports(varColl.GetEnum, IEnumVariant, Result);
end;

procedure RegTypeLibrary( Const FileName: String );
var
   wFileName:        WideString;
   DocName:          WideString;
   TypeLib:          ITypeLib;
begin

   if not( FileExists( FileName ) ) then
      Raise Exception.Create( 'Type library not found' );

   wFileName := FileName;
   OleCheck( LoadTypeLib( PWideChar( wFileName ), TypeLib ) );

   OleCheck( TypeLib.GetDocumentation( -1, nil, nil, nil, @DocName ) );
   DocName := ExtractFilePath( DocName );

   OleCheck( RegisterTypeLib( TypeLib, PWideChar( wFileName ), PWideChar( DocName ) ) );
end;

function TService.Get_IDE_Properties(const IDE: IDispatch): IEnumVARIANT;
begin

end;

initialization
  RegTypeLibrary(ParamStr(0));

  TAutoObjectFactory.Create(ComServer, TService, Class_Service,
    ciMultiInstance, tmApartment);
end.
