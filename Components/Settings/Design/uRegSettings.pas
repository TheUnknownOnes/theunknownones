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

  RegisterComponents(SettingsComponentGroup, [ TSettingsLinkControl,
                                               TSettingsLinkTabControl,
                                               TSettingsLinkForm,
                                               TSettingsLinkPageControl,
                                               TSettingsLinkListView]);
end;

end.
