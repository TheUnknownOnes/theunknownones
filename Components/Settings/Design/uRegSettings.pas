unit uRegSettings;

interface

uses
  Classes, uSettingsLinksDefault, uSettingsBase, uSettingsStream, uSettingsXML;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents(SettingsComponentGroup, [TSettingsFile,
                                              TSettingsStream]);

  RegisterComponents(SettingsComponentGroup, [TSettingsXMLFile]);

  RegisterComponents(SettingsComponentGroup, [ TSettingsLinkComponent,
                                               TSettingsLinkForm,
                                               TSettingsLinkListView]);
end;

end.
