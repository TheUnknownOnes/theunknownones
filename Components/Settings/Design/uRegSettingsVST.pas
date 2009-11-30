{-----------------------------------------------------------------------------
 Project: Settings_VirtualTrees
 Purpose: Register components
 Created: 12.08.2008 08:22:26
 
 (c) by TheUnknownOnes under dwywbdbu license - see http://theunknownones.googlecode.com/svn/ for the license
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}


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
