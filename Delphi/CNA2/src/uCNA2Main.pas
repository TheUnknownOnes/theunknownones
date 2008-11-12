//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uCNA2Main;

{$R 'CNA2Resources.res'}

interface

uses
  ToolsAPI,
  SysUtils,
  Classes,
  Dialogs,
  uCNA2Settings,
  uCNA2Visualizers,
  uCNA2Profiles,
  uCNA2Actions;

type
  Tcna2Wizard = class(TInterfacedObject,
                      IOTAWizard,
                      IOTANotifier)
  private
    FIDENotifierIndex : Integer;

    //IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;

    //IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

procedure Register;

implementation

uses uCNA2ValueEditors, uCNA2FormEditorHooks, uCNA2IDENotifier;

procedure Register;
begin       
  RegisterPackageWizard(Tcna2Wizard.Create);
end;

{ Tcna2Wizard }

procedure Tcna2Wizard.AfterConstruction;
var
  Serv : IOTAServices;
begin
  inherited;

  cna2FormEditorHooks := Tcna2FormEditorHooks.Create;

  FIDENotifierIndex := -1;

  if Supports(BorlandIDEServices, IOTAServices, Serv) then
    FIDENotifierIndex := Serv.AddNotifier(Tcna2IDENotifier.Create);

  InitValueEditors;

  cna2Settings := Tcna2Settings.Create(nil);
  InitActions;
  cna2Profiles := Tcna2Profiles.Create;
  InitVisualizers;
end;

procedure Tcna2Wizard.AfterSave;
begin
  //Not used for IOTAWizard
end;

procedure Tcna2Wizard.BeforeDestruction;
var
  Serv : IOTAServices;
begin
  if (FIDENotifierIndex > -1) and
     Supports(BorlandIDEServices, IOTAServices, Serv) then
    Serv.RemoveNotifier(FIDENotifierIndex);

  if Assigned(cna2FormEditorHooks) then
  begin
    cna2FormEditorHooks.Free;
    cna2FormEditorHooks := nil;
  end;

  FreeVisualizers;

  if Assigned(cna2Profiles) then
  begin
    cna2Profiles.Free;
    cna2Profiles := nil;
  end;

  FreeActions;

  if Assigned(cna2Settings) then
  begin
    cna2Settings.Free;
    cna2Settings := nil;
  end;

  FreeValueEditors;

  inherited;
end;

procedure Tcna2Wizard.BeforeSave;
begin
  //Not used for IOTAWizard
end;

procedure Tcna2Wizard.Destroyed;
begin
  
end;

procedure Tcna2Wizard.Execute;
begin
  
end;

function Tcna2Wizard.GetIDString: string;
begin
  Result := 'TUO.CNA2';
end;

function Tcna2Wizard.GetName: string;
begin
  Result := 'CNA2';
end;

function Tcna2Wizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure Tcna2Wizard.Modified;
begin
  //Not used for IOTAWizard
end;

end.
