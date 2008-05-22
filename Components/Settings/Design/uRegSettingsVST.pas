unit uRegSettingsVST;

interface

uses
  Classes, uSettingsLinkVirtualTrees, uSettingsBase;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents(SettingsComponentGroup, [TSettingsLinkVST]);
end;


end.
