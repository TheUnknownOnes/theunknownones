{----------------------------------------------------------------------------- 
 Project: Settings
 Purpose: Register components 
 Created: 12.08.2008 08:17:44
 
 (c) by TheUnknownOnes under Apache License 2.0
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

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
