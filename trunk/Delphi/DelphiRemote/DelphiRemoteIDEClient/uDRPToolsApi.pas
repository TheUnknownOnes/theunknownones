unit uDRPToolsApi;

interface

uses
  uDelphiRemoteIDEClientPlugin,
  uDelphiRemoteIDEClient,
  ToolsAPI,
  SysUtils;

type
  {$TYPEINFO ON}
  {$METHODINFO ON}

  TOTAConsts = class(TDelphiRemoteIDEClientPlugin)
  private
    function GetCompileMode(const Index: Integer): Integer;
  published
    property CompileMode_Make : Integer index cmOTAMake read GetCompileMode;
    property CompileMode_Build : Integer index cmOTABuild read GetCompileMode;
    property CompileMode_Check : Integer index cmOTACheck read GetCompileMode;
    property CompileMode_MakeUnit : Integer index cmOTAMakeUnit read GetCompileMode;
  end;

  TOTAProjectBuilder = class(TDelphiRemoteIDEClientPlugin)
  private
    FProjectBuilder : IOTAProjectBuilder;
    function GetShouldBuild: Boolean;
  public
    constructor Create(AProjectBuilder : IOTAProjectBuilder);

    function BuildProject(CompileMode: Integer; Wait: Boolean): Boolean;
    function BuildProject2(CompileMode: Integer; Wait, ClearMessages: Boolean): Boolean;
  published
    property ShouldBuild: Boolean read GetShouldBuild;
  end;

  TOTAProject = class(TDelphiRemoteIDEClientPlugin)
  private
    FProject : IOTAProject;

    function GetPersonality: string;
    function GetProjectGUID: String;
  public
    constructor Create(AProject : IOTAProject);

    function Rename(const OldFileName, NewFileName: string): Boolean;

    procedure AddFileWithParent(const AFileName: string; IsUnitOrForm: Boolean;
      const Parent: string);

    procedure AddFile(const AFileName: string; IsUnitOrForm: Boolean);
    procedure RemoveFile(const AFileName: string);

    function GetProjectBuilder : IDispatch;
  published
    property Personality: string read GetPersonality;
    property ProjectGUID: String read GetProjectGUID;
  end;

  TModuleServices = class(TDelphiRemoteIDEClientPlugin)
  published
    function ActiveProject : IDispatch;
  end;

implementation

uses uObjectDispatchEx;

var
  ModuleServices : IOTAModuleServices;

{ TOTAConsts }

function TOTAConsts.GetCompileMode(const Index: Integer): Integer;
begin
  Result := Index;
end;

{ TOTAProject }

procedure TOTAProject.AddFile(const AFileName: string; IsUnitOrForm: Boolean);
begin
  FProject.AddFile(AFileName, IsUnitOrForm);
end;

procedure TOTAProject.AddFileWithParent(const AFileName: string;
  IsUnitOrForm: Boolean; const Parent: string);
begin
  FProject.AddFileWithParent(AFileName, IsUnitOrForm, Parent);
end;

constructor TOTAProject.Create(AProject: IOTAProject);
begin
  inherited Create; 
  FProject := AProject;
end;

function TOTAProject.GetPersonality: string;
begin
  Result := FProject.Personality;
end;

function TOTAProject.GetProjectBuilder: IDispatch;
var
  pb : IOTAProjectBuilder;
begin
  pb := FProject.ProjectBuilder;

  if Assigned(pb) then
    Result := TObjectDispatchEx.Create(TOTAProjectBuilder.Create(pb), true)
  else
    Result := nil;
end;

function TOTAProject.GetProjectGUID: String;
begin
  Result := GUIDToString(FProject.ProjectGUID);
end;

procedure TOTAProject.RemoveFile(const AFileName: string);
begin
  FProject.RemoveFile(AFileName);
end;

function TOTAProject.Rename(const OldFileName, NewFileName: string): Boolean;
begin
  Result := FProject.Rename(OldFileName, NewFileName);
end;

{ TModuleServices }

function TModuleServices.ActiveProject: IDispatch;
var
  FProject : IOTAProject;
begin
  FProject := ModuleServices.GetActiveProject;

  if Assigned(FProject) then
    Result := TObjectDispatchEx.Create(TOTAProject.Create(FProject), true)
  else
    Result := nil;
end;

{ TOTAProjectBuilder }

function TOTAProjectBuilder.BuildProject2(CompileMode: Integer; Wait,
  ClearMessages: Boolean): Boolean;
begin
  Result := FProjectBuilder.BuildProject(TOTACompileMode(CompileMode), Wait, ClearMessages);
end;

function TOTAProjectBuilder.BuildProject(CompileMode: Integer;
  Wait: Boolean): Boolean;
begin
  Result := FProjectBuilder.BuildProject(TOTACompileMode(CompileMode), Wait);
end;

constructor TOTAProjectBuilder.Create(AProjectBuilder: IOTAProjectBuilder);
begin
  inherited Create;
  FProjectBuilder := AProjectBuilder;
end;

function TOTAProjectBuilder.GetShouldBuild: Boolean;
begin
  Result := FProjectBuilder.ShouldBuild;
end;

initialization
  GlobalDelphiRemoteIDEClient.RegisterChild('OTAConsts', TOTAConsts.Create);

  if BorlandIDEServices.GetService(IOTAModuleServices, ModuleServices) then
    GlobalDelphiRemoteIDEClient.RegisterChild('ModuleServices', TModuleServices.Create);

end.
