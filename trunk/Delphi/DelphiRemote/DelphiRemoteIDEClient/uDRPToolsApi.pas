unit uDRPToolsApi;

interface

uses
  uDelphiRemoteIDEClientPlugin,
  uDelphiRemoteIDEClient,
  ToolsAPI,
  SysUtils,
  Classes;

type
  {$TYPEINFO ON}
  {$METHODINFO ON}

  TOTAConsts = class(TDelphiRemoteIDEClientPlugin)
  private
    function GetIntVal(const Index: Integer): Integer;
  published
    property CompileMode_Make : Integer index cmOTAMake read GetIntVal;
    property CompileMode_Build : Integer index cmOTABuild read GetIntVal;
    property CompileMode_Check : Integer index cmOTACheck read GetIntVal;
    property CompileMode_MakeUnit : Integer index cmOTAMakeUnit read GetIntVal;

    property ModuleType_Form : Integer index omtForm read GetIntVal;
    property ModuleType_DataModule : Integer index omtDataModule read GetIntVal;
    property ModuleType_ProjUnit : Integer index omtProjUnit read GetIntVal;
    property ModuleType_Unit : Integer index omtUnit read GetIntVal;
    property ModuleType_Rc : Integer index omtRc read GetIntVal;
    property ModuleType_Def : Integer index omtDef read GetIntVal;
    property ModuleType_Obj : Integer index omtObj read GetIntVal;
    property ModuleType_Res : Integer index omtRes read GetIntVal;
    property ModuleType_Lib : Integer index omtLib read GetIntVal;
    property ModuleType_TypeLib : Integer index omtTypeLib read GetIntVal;
    property ModuleType_PackageImport : Integer index omtPackageImport read GetIntVal;
    property ModuleType_FormResource : Integer index omtFormResource read GetIntVal;
    property ModuleType_Custom : Integer index omtCustom read GetIntVal;
    property ModuleType_IDL : Integer index omtIDL read GetIntVal;
  end;

  TOTAEditor = class(TDelphiRemoteIDEClientPlugin)
  private

  end;

  TOTAModule = class(TDelphiRemoteIDEClientPlugin)
  private
    FModule : IOTAModule;
    function GetOwnerModuleCount: Integer;
    function GetFileName: string;
    function GetFileSystem: string;
    procedure SetFileName(const Value: string);
    procedure SetFileSystem(const Value: string);
    function GetModuleFileCount: Integer;
    function GetOwnerCount: Integer;
  public
    constructor Create(AModule : IOTAModule);

    procedure Show;
    procedure ShowFilename(const FileName: string);

    procedure MarkModified;
    function GetOwnerModule(AIndex : Integer) : IDispatch;
    function GetCurrentEditor : IDispatch;

    function CloseModule(ForceClosed: Boolean): Boolean;
    function Save(ChangeName, ForceSave: Boolean): Boolean;
    function HasCoClasses: Boolean;
    function GetOwner(AIndex : Integer) : IDispatch;
    function Close: Boolean;
    procedure AddToInterface;
  published
    property OwnerModuleCount: Integer read GetOwnerModuleCount;

    property FileName: string read GetFileName write SetFileName;
    property FileSystem: string read GetFileSystem write SetFileSystem;
    property ModuleFileCount: Integer read GetModuleFileCount;

    property OwnerCount: Integer read GetOwnerCount;
  end;

  TOTAModuleInfo = class(TDelphiRemoteIDEClientPlugin)
  private
    FModuleInfo : IOTAModuleInfo;
    function GetCustomId: string;
    function GetAdditionalFiles: String;
    function GetDesignClass: string;
    function GetFileName: string;
    function GetFormName: string;
    function GetModuleType: Integer;
    function GetName: string;
  public
    constructor Create(AModuleInfo : IOTAModuleInfo);

    function OpenModule : IDispatch;
  published
    property CustomId: string read GetCustomId;
    property AdditionalFiles : String read GetAdditionalFiles;

    property ModuleType: Integer read GetModuleType;
    property Name: string read GetName;
    property FileName: string read GetFileName;
    property FormName: string read GetFormName;
    property DesignClass: string read GetDesignClass;
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

    function GetModuleCount: Integer;
    function GetModule(AIndex : Integer) : IDispatch;
  published
    property Personality: string read GetPersonality;
    property ProjectGUID: String read GetProjectGUID;
  end;

  TModuleServices = class(TDelphiRemoteIDEClientPlugin)
  published
    function GetActiveProject : IDispatch;
  end;

implementation

uses uObjectDispatchEx;

var
  ModuleServices : IOTAModuleServices;

{ TOTAConsts }

function TOTAConsts.GetIntVal(const Index: Integer): Integer;
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

function TOTAProject.GetModule(AIndex: Integer): IDispatch;
var
  module : IOTAModuleInfo;
begin
  module := FProject.GetModule(AIndex);

  if Assigned(module) then
    Result := TObjectDispatchEx.Create(TOTAModuleInfo.Create(module), true)
  else
    Result := nil;
end;

function TOTAProject.GetModuleCount: Integer;
begin
  Result := FProject.GetModuleCount;
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

function TModuleServices.GetActiveProject: IDispatch;
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

{ TOTAModuleInfo }

constructor TOTAModuleInfo.Create(AModuleInfo: IOTAModuleInfo);
begin
  inherited Create();
  FModuleInfo := AModuleInfo;
end;

function TOTAModuleInfo.GetAdditionalFiles: String;
var
  sl : TStringList;
begin
  sl := TStringList.Create;
  try
    FModuleInfo.GetAdditionalFiles(sl);
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

function TOTAModuleInfo.GetCustomId: string;
begin
  Result := FModuleInfo.CustomId;
end;

function TOTAModuleInfo.GetDesignClass: string;
begin
  Result := FModuleInfo.DesignClass;
end;

function TOTAModuleInfo.GetFileName: string;
begin
  Result := FModuleInfo.FileName;
end;

function TOTAModuleInfo.GetFormName: string;
begin
  Result := FModuleInfo.FormName;
end;

function TOTAModuleInfo.GetModuleType: Integer;
begin
  Result := Integer(FModuleInfo.ModuleType);
end;

function TOTAModuleInfo.GetName: string;
begin
  Result := FModuleInfo.Name;
end;

function TOTAModuleInfo.OpenModule: IDispatch;
var
  module : IOTAModule;
begin
  module := FModuleInfo.OpenModule;

  if Assigned(module) then
    Result := TObjectDispatchEx.Create(TOTAModule.Create(module), true)
  else
    Result := nil;
end;

{ TOTAModule }

procedure TOTAModule.AddToInterface;
begin
  FModule.AddToInterface;
end;

function TOTAModule.Close: Boolean;
begin
  Result := FModule.Close;
end;

function TOTAModule.CloseModule(ForceClosed: Boolean): Boolean;
begin
  Result := FModule.CloseModule(ForceClosed);
end;

constructor TOTAModule.Create(AModule: IOTAModule);
begin
  inherited Create();
  FModule := AModule;
end;

function TOTAModule.GetCurrentEditor: IDispatch;
begin
  Result := nil; //todo
end;

function TOTAModule.GetFileName: string;
begin
  Result := FModule.FileName;
end;

function TOTAModule.GetFileSystem: string;
begin
  Result := FModule.FileSystem;
end;

function TOTAModule.GetModuleFileCount: Integer;
begin
  Result := FModule.ModuleFileCount;
end;

function TOTAModule.GetOwner(AIndex: Integer): IDispatch;
var
  i : IOTAProject;
begin
  i := FModule.Owners[AIndex];

  if Assigned(i) then
    Result := TObjectDispatchEx.Create(TOTAProject.Create(i), true)
  else
    Result := nil;
end;

function TOTAModule.GetOwnerCount: Integer;
begin
  Result := FModule.OwnerModuleCount;
end;

function TOTAModule.GetOwnerModule(AIndex: Integer): IDispatch;
var
  module : IOTAModule;
begin
  module := FModule.OwnerModules[AIndex];

  if Assigned(module) then
    Result := TObjectDispatchEx.Create(TOTAModule.Create(module), true)
  else
    Result := nil;
end;

function TOTAModule.GetOwnerModuleCount: Integer;
begin
  Result := FModule.OwnerModuleCount;
end;

function TOTAModule.HasCoClasses: Boolean;
begin
  Result := FModule.HasCoClasses;
end;

procedure TOTAModule.MarkModified;
begin
  FModule.MarkModified;
end;

function TOTAModule.Save(ChangeName, ForceSave: Boolean): Boolean;
begin
  Result := FModule.Save(ChangeName, ForceSave);
end;

procedure TOTAModule.SetFileName(const Value: string);
begin
  FModule.FileName := Value;
end;

procedure TOTAModule.SetFileSystem(const Value: string);
begin
  FModule.FileSystem := Value;
end;

procedure TOTAModule.Show;
begin
  FModule.Show;
end;

procedure TOTAModule.ShowFilename(const FileName: string);
begin
  FModule.ShowFilename(FileName);
end;

initialization
  GlobalDelphiRemoteIDEClient.RegisterChild('OTAConsts', TOTAConsts.Create);

  if BorlandIDEServices.GetService(IOTAModuleServices, ModuleServices) then
    GlobalDelphiRemoteIDEClient.RegisterChild('ModuleServices', TModuleServices.Create);

end.
