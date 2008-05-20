unit uSettingsComponentLinksDefault;

interface

uses
  Classes,
  Controls,
  SysUtils,
  uSettings;

type
  TCustomSettingsComponentLinkControl = class(TSettingsComponentLink)
  protected
    function ValidComponent(const AComponent : TComponent) : Boolean; override;
  end;


//==============================================================================


  TSettingsComponentLinkControl = class(TCustomSettingsComponentLinkControl)

  end;


//==============================================================================


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TUO', [TSettingsComponentLinkControl]);
end;

{ TCustomSettingsComponentLinkControl }

function TCustomSettingsComponentLinkControl.ValidComponent(
  const AComponent: TComponent): Boolean;
begin
  Result := AComponent is TControl;
end;

end.
