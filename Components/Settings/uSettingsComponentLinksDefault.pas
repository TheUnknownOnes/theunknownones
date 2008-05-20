unit uSettingsComponentLinksDefault;

interface

uses
  Classes,
  Controls,
  SysUtils,
  uSettings;

type
  TCustomSettingsComponentLinkControl = class(TCustomSettingsComponentLink)
  protected
    function ValidComponent(const AComponent : TComponent) : Boolean; override;
  end;

implementation

{ TCustomSettingsComponentLinkControl }

function TCustomSettingsComponentLinkControl.ValidComponent(
  const AComponent: TComponent): Boolean;
begin
  Result := AComponent is TControl;
end;

end.
