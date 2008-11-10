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
  uCNA2Profiles;

type
  Tcna2Wizard = class(TInterfacedObject,
                      IOTAWizard,
                      IOTANotifier)
  private

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

procedure Register;
begin       
  RegisterPackageWizard(Tcna2Wizard.Create);
end;

{ Tcna2Wizard }

procedure Tcna2Wizard.AfterConstruction;
begin
  inherited;

  cna2Settings := Tcna2Settings.Create(nil);
  cna2Profiles := Tcna2Profiles.Create;
  StartVisualizers;
end;

procedure Tcna2Wizard.AfterSave;
begin
  //Not used for IOTAWizard
end;

procedure Tcna2Wizard.BeforeDestruction;
begin
  StopVisualizers;

  if Assigned(cna2Profiles) then
  begin
    cna2Profiles.Free;
    cna2Profiles := nil;
  end;

  if Assigned(cna2Settings) then
  begin
    cna2Settings.Free;
    cna2Settings := nil;
  end;

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
