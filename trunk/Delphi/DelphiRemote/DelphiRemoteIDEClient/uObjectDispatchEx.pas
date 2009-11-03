unit uObjectDispatchEx;

interface

uses
  ActiveX, ObjComAuto, Classes, TypInfo, uDelphiRemoteIDEClientPlugin;

type
  TObjectDispatchEx = class(TObjectDispatch)
  public
    constructor Create(Instance: TObject; Owned: Boolean = True);
    destructor Destroy; override;
  end;

implementation

uses
  Dialogs, SysUtils;

{ TObjectDispatchEx }

constructor TObjectDispatchEx.Create(Instance: TObject; Owned: Boolean);
begin
  inherited Create(Instance, Owned);
end;

destructor TObjectDispatchEx.Destroy;
begin
  inherited;
end;

end.
