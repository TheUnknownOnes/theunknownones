//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit unitCNALangs;

{$I '..\CNA_Includes\main_include.pas'}

interface
  uses
    Classes, StrUtils, Controls, Dialogs;


  function GetLangString(AControl : TComponent; AProp : String) : String; overload;
  function GetLangString(AIdentifier : String) : String; overload;
  function LangStringAvailable(AControl : TComponent; AProp : String) : Boolean; overload;
  function LangStringAvailable(AIdentifier : String) : Boolean; overload;

var
  Strings : TStrings;

implementation

{.$region 'Helpers'}

function LangStringAvailable(AControl : TComponent; AProp : String) : Boolean;
begin
  Result:=LangStringAvailable(AControl.Name+'_'+AProp);
end;

function LangStringAvailable(AIdentifier : String) : Boolean;
begin
  if (Strings.IndexOfName(AIdentifier)>-1) then Result:=true
  else Result:=false;
end;

function GetLangString(AControl : TComponent; AProp : String) : String;
begin
  Result:=GetLangString(AControl.Name+'_'+AProp);
end;

function GetLangString(AIdentifier : String) : String;
var
  Index : Integer;
begin
  Result:='';
  Index:=Strings.IndexOfName(AIdentifier);
  if (Index>-1) then
    Result:=Strings.ValueFromIndex[Index];

end;

{.$endregion}

initialization
  Strings:=TStringList.Create;
  with Strings do
  begin
  {$IFDEF LANG_GERMAN}
    Add('gbOptions_Caption=Optionen');
    Add('cbExpertActive_Caption=Experte aktiv');
    Add('cbUseNA_Caption=Namens-Assistent benutzen');
    Add('cbNAPopIfDefined_Caption=Nur nach dem Namen fragen, wenn ein Pre-/Suffix definiert ist');
    Add('cbNACreateName_Caption=Als Vorgabe einen eindeutigen Namen erstellen');
    Add('lblNADelimiter_Caption=Trennzeichen zwischen Pre-/Suffix und dem Komponentennamen:');
    Add('cbUsePA_Caption=Eigenschafts-Assistenten benutzen');
    Add('lblProfile_Caption=Aktuelles Profil:');
    Add('gbsettings_Caption=Einstellungen');
    Add('gbValues_Caption=Werte');
    Add('lblComponents_Caption=Verfügbare Komponenten:');
    Add('btnAddComponentGroup_Caption=Komponente zur &gewählten Gruppe hinzufügen');
    Add('btnAddComponentNew_Caption=Komponente in eine &neue Gruppe einfügen');
    Add('btnOk_Caption=OK');
    Add('btnCancel_Caption=Abbrechen');
    Add('miAddProfile_Caption=Profil hinzufügen');
    Add('NewProfileBoxCaption=Neues Profil hinzufügen ...');
    Add('NewProfileBoxPrompt=Bitte geben Sie den Namen des neuen Profils ein:');
    Add('NewProfileBoxDefault=NeuesProfil');
    Add('NewProfileExists=Dieses Profil existiert schon');
    Add('NewProfileCouldNotAdd=Konnte dieses Profil nicht hinzufügen');
    Add('AddComponentSelectGroup=Bitte wählen Sie erst eine Gruppe');
    Add('AddComponentValidComponent=Bitte wählen Sie eine gültige Komponente');
    Add('AddComponentBoxText=Das Einfügen wird die Anzahl der verfügbaren Eigenschaften reduzieren. Wollen Sie fortfahren?');
    Add('AddComponentBoxCaption=Komponente einfügen ...');
    Add('AddComponentComponentExists=Diese Komponente existiert schon!');
    Add('AddComponentCouldNotAdd=Konnte diese Komponente nicht hinzufügen');
    Add('AddGroupSelectProfile=Bitte wählen Sie erst ein Profil!');
    Add('AddGroupBoxCaption=Neue Gruppe hinzufügen ...');
    Add('AddGroupBoxPrompt=Bitte geben Sie den Name der neuen Gruppe ein:');
    Add('AddGroupBoxDefault=NeueGruppe');
    Add('AddGroupExists=Diese Gruppe existiert schon in diesem Profil.');
    Add('AddGroupFailed=Konnte diese Gruppe nicht hinzufügen!');
    Add('miAddGroup_Caption=Gruppe hinzufügen');
    Add('miDelComponent_Caption=Komponente entfernen');
    Add('miDelGroup_Caption=Gruppe entfernen');
    Add('miDelProfile_Caption=Profil entfernen');
    Add('miDelete_Caption=Ausgewähltes entfernen');
    Add('DelComponentNoComponent=Bitte wählen Sie eine Komponente!');
    Add('DelGroupSelectGroup=Bitte wählen Sie die Gruppe, die Sie löschen wollen!');
    Add('DelGroupBoxText=Diese Gruppe ist nicht leer. Wollen Sie fortfahren?');
    Add('DelGroupBoxCaption=Gruppe löschen ...');
    Add('DelProfileBoxText=Dieses Profil ist nicht leer. Wollen Sie fortfahren?');
    Add('DelProfileBoxCaption=Profil löschen ...');
    Add('AddComponentSelectProfile=Bitte wählen Sie ein Profil');
    Add('miRenameGroup_Caption=Gruppe umbenennen');
    Add('miRenameProfile_Caption=Profil umbenennen');
    Add('RenameGroupNoGroup=Bitte wählen Sie erst ein Gruppe aus');
    Add('RenameGroupBoxCaption=Gruppe umbenennen ...');
    Add('RenameGroupBoxPrompt=Bitte geben Sie den neuen Namen an:');
    Add('RenameGroupExists=Diese Gruppe existiert schon!');
    Add('RenameProfileBoxCaption=Profil umbenennen ...');
    Add('RenameProfileBoxPrompt=Bitte geben Sie den neuen Namen an:');
    Add('RenameProfileExists=Dieses Profil existiert schon!');
    Add('Header_Fix=Prefix und Suffix');
    Add('Header_Properties=Eigenschaften');
    Add('Property_Not_Set=/');
    Add('Prefix=Prefix');
    Add('Suffix=Suffix');
    Add('ListColNameCaption=Name');
    Add('ListColValueCaption=Wert');
    Add('rgEnum_Caption=Elemente');
    Add('btnClearValue_Caption=Wert löschen');
    Add('E_NotSupported=Dieser Datentyp wird nicht unterstützt');
    Add('lblOldValue_Caption=Alter Wert:');
    Add('lblNewValue_Caption=Neuer Wert:');
    Add('MSG_DemandLoad=Kann die Klasse "%s" nicht laden! Es kann sein, das sie Teil eines "demand-load-package" is. (Versuche .cna Datei zu nutzen)');
    Add('MSG_FileLoad=Kann die Klasse "%s" nicht von "%s" laden. Die Eigenschaften-Liste bleibt leer!');
    Add('PropertyNotSet=*Nicht gesetzt*');
    Add('lblNewName_Caption=Neuer Komponentenname:');
    Add('MSG_SelectProfile=Bitte wählen Sie ein Profil!');
    Add('MSG_ComponentNotFound=Kann diese Komponente nicht im aktuelle Profil finden');
    Add('cbPAShowDLG_Caption=Nachfragen, bevor Standard-Eigenschaften geändert werden sollen');
    Add('PABox_ShouldChange=Standard-Eigenschaften werden geändert. Sind Sie damit einverstanden?');
    Add('miProfiles=Profil wählen');
    Add('miConfigure=Konfiguration');
    Add('ChooseActiveProfile=Bitte wählen Sie ein aktives Profil!');
    Add('MSG_SkippedProperty=%s.%s übersprungen. Grund: %s');
    Add('cbShowInputBox_Caption=Zeige Eingabe-Dialog');
    Add('Dialog=***Dialog***');
    Add('SpecifyProperty=Bitte Wert eingeben ...');
    Add('MSG_ObjectNotCreated=Die Komponente konnte nicht initialisiert werden. Fehler:"%s"');
    Add('MSG_EGettingPropList=Konnte die Eigenschafts-Liste für die Klasse "%s" nicht ermitteln.');
    Add('ErrorCaught=Die Debug-Funktion hat einen Fehler festgestellt. Die Beschreibung wurde in die Zwischenablage kopiert. Bitte senden Sie den Inhalt der Zwischenablage an chaosben@web.de.');

  {$ELSE} //english
    Add('gbOptions_Caption=Options');
    Add('cbExpertActive_Caption=Expert active');
    Add('cbUseNA_Caption=Use naming assistant');
    Add('cbNAPopIfDefined_Caption=Only popup if a pre-/suffix is defined');
    Add('cbNACreateName_Caption=Create unique name');
    Add('lblNADelimiter_Caption=Delimiter between pre-/suffix and the component-name:');
    Add('cbUsePA_Caption=Use property assistant');
    Add('lblProfile_Caption=Current profile:');
    Add('gbsettings_Caption=Settings');
    Add('gbValues_Caption=Values');
    Add('lblComponents_Caption=Available components:');
    Add('btnAddComponentGroup_Caption=Add component to &selected group');
    Add('btnAddComponentNew_Caption=Add component into &new group');
    Add('btnOk_Caption=OK');
    Add('btnCancel_Caption=Cancel');
    Add('miAddProfile_Caption=Add profile');
    Add('NewProfileBoxCaption=Add new profile ...');
    Add('NewProfileBoxPrompt=Please specify a name for the new profile:');
    Add('NewProfileBoxDefault=NewProfile');
    Add('NewProfileExists=This profile already exists');
    Add('NewProfileCouldNotAdd=Couldn''t add this profile');
    Add('AddComponentSelectGroup=Please select a group');
    Add('AddComponentValidComponent=Please specify a valid component');
    Add('AddComponentBoxText=This insert will reduce the amount of available properties in this group. Do you want to continue?');
    Add('AddComponentBoxCaption=Inserting component ...');
    Add('AddComponentComponentExists=This component already exists!');
    Add('AddComponentCouldNotAdd=Couldn''t add this component');
    Add('AddGroupSelectProfile=Please select a profile');
    Add('AddGroupBoxCaption=Add new group ...');
    Add('AddGroupBoxPrompt=Please specify a name for the new group:');
    Add('AddGroupBoxDefault=NewGroup');
    Add('AddGroupExists=This group already exists in this profile');
    Add('AddGroupFailed=Couldn''t add this group');
    Add('miAddGroup_Caption=Add group');
    Add('miDelComponent_Caption=Remove component');
    Add('miDelGroup_Caption=Remove group');
    Add('miDelProfile_Caption=Remove profile');
    Add('miDelete_Caption=Delete selected');
    Add('DelComponentNoComponent=Please select a component');
    Add('DelGroupSelectGroup=Please select the group you want to delete');
    Add('DelGroupBoxText=This group is not empty. Do you really want to continue?');
    Add('DelGroupBoxCaption=Delete group ...');
    Add('DelProfileBoxText=This profile is not empty. Do you really want to continue?');
    Add('DelProfileBoxCaption=Delete profile ...');
    Add('AddComponentSelectProfile=Please select a profile');
    Add('miRenameGroup_Caption=Rename group');
    Add('miRenameProfile_Caption=Rename profile');
    Add('RenameGroupNoGroup=Please select a group first');
    Add('RenameGroupBoxCaption=Rename group ...');
    Add('RenameGroupBoxPrompt=Please specify the new name:');
    Add('RenameGroupExists=This group already exists!');
    Add('RenameProfileBoxCaption=Rename profile ...');
    Add('RenameProfileBoxPrompt=Please specify the new name:');
    Add('RenameProfileExists=This profile already exists!');
    Add('Header_Fix=Prefix and suffix');
    Add('Header_Properties=Properties');
    Add('Property_Not_Set=/');
    Add('Prefix=Prefix');
    Add('Suffix=Suffix');
    Add('ListColNameCaption=Name');
    Add('ListColValueCaption=Value');
    Add('rgEnum_Caption=Elements');
    Add('btnClearValue_Caption=Clear value');
    Add('E_NotSupported=Not supported type');
    Add('lblOldValue_Caption=Old value:');
    Add('lblNewValue_Caption=New value:');
    Add('MSG_DemandLoad=Can not load class "%s"! This class may be contained in an "demand-load-package". (Trying to use cna-file.)');
    Add('MSG_FileLoad=Can not load "%s" from "%s". Empty property-list!');
    Add('PropertyNotSet=***Not set***');
    Add('lblNewName_Caption=New component-name:');
    Add('MSG_SelectProfile=Please select a CNA-profile.');
    Add('MSG_ComponentNotFound=Can not found this component in the current profile');
    Add('cbPAShowDLG_Caption=Ask before changing default-properties');
    Add('PABox_ShouldChange=Default properties will be changed. Do you agree?');
    Add('miProfiles=Select Profile');
    Add('miConfigure=Configure');
    Add('ChooseActiveProfile=Please choose the current profile!');
    Add('MSG_SkippedProperty=Skipped %s.%s because of exception: %s');
    Add('cbShowInputBox_Caption=Show input dialog');
    Add('Dialog=***dialog***');
    Add('SpecifyProperty=Please enter value ...');
    Add('MSG_ObjectNotCreated=Could not initialize component. Exception:"%s"');
    Add('MSG_EGettingPropList=Could not retrive property-list for Class "%s"');
    Add('ErrorCaught=The debug function has found an error. The Description was copied to the clipboard. Please send the content of the clipboard to chaosben@web.de.');
  {$ENDIF}
  end;


finalization
  Strings.Free;

end.
