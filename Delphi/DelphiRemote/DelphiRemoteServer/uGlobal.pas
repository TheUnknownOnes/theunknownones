unit uGlobal;

interface

uses
  SysUtils, Classes;

type
  TGlobal = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FDelphiIDEs : TInterfaceList;
    FROTCookie: Longint;
  public
    property DelphiIDEs : TInterfaceList read FDelphiIDEs;
  end;

var
  Global: TGlobal;

implementation

uses ActiveX, comobj, DelphiRemoteServer_TLB, Dialogs;

{$R *.dfm}

procedure TGlobal.DataModuleCreate(Sender: TObject);
begin
  FDelphiIDEs:=TInterfaceList.Create;

  //Register object in ROT        
  OleCheck(RegisterActiveObject(Self, CLASS_Service, ActiveObject_Weak, FROTCookie));
end;

procedure TGlobal.DataModuleDestroy(Sender: TObject);
begin
  //Remove object from ROT
  OleCheck(RevokeActiveObject(FROTCookie, nil));

  FDelphiIDEs.Free;
end;

end.
