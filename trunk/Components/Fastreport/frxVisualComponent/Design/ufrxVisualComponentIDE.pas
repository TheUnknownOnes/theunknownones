unit ufrxVisualComponentIDE;

{$R 'ufrxVisualComponentIDE.res'}

interface

uses
  Graphics,
  DesignIntf,
  ToolsAPI,
  Forms,
  Classes,
  SysUtils,
  Windows,
  Messages,
  DesignMenus,
  Dialogs,
  ufrxVisualComponent;

type
  TfrxVisualComponentWizard = class(TInterfacedObject,
                                    IOTANotifier,
                                    IOTAWizard,
                                    IOTAProjectWizard,
                                    IOTARepositoryWizard,
                                    IOTARepositoryWizard60,
                                    IOTARepositoryWizard80)
  private
    //IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    //IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    //IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;
    //IOTARepositoryWizard60
    function GetDesigner: string;
    //IOTARepositoryWizard80
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;
  end;

  TfrxVisualComponentModuleCreator = class(TInterfacedObject,
                                           IOTACreator,
                                           IOTAModuleCreator)
  private
    FClassIdent : String;

    //IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    //IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  public
    constructor Create(AOwner: IOTAModule; AClassIdent: string);
  end;

  TSrcFile = class(TInterfacedObject, IOTAFile)
  private
    FSource : String;
    function GetSource: string;
    function GetAge: TDateTime;
  public
    constructor Create(AResourceName : String);
  end;

  TDFMFile = class(TSrcFile)
  public
    function Replace(AFormIdent, AAncestorIdent : String) : TDFMFile;
  end;

  TUnitFile = class(TSrcFile)
  public
    function Replace(AModuleIdent, AFormIdent, AAncestorIdent, AIdent : String) : TUnitFile;
  end;

  TTfrxVisualComponentModule = class(TBaseCustomModule, ICustomModule)
  private
    function GetAttributes: TCustomModuleAttributes;
    procedure ExecuteVerb(Index: Integer);
    function GetVerb(Index: Integer): string;
    function GetVerbCount: Integer;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem);
    procedure Saving;
    procedure ValidateComponent(Component: TComponent);
    function ValidateComponentClass(ComponentClass: TComponentClass): Boolean;
    function Nestable: Boolean;
  public
    class function DesignClass: TComponentClass; override;
  end;


procedure Register;

implementation

const
  FORM_CLASS_NAME = 'frxVisualComponentFrame';

procedure Register;
begin
  RegisterPackageWizard(TfrxVisualComponentWizard.Create);
  RegisterCustomModule(TfrxVisualComponentFrame, TTfrxVisualComponentModule);
end;

function GetActiveProject: IOTAProject;
var
  AModuleServices: IOTAModuleServices;
  AModule: IOTAModule;
  AProjectGroup: IOTAProjectGroup;
  i: Integer;
begin
  Result := nil;

  AModuleServices := (BorlandIDEServices as IOTAModuleServices);

  for i := 0 to AModuleServices.ModuleCount - 1 do
  begin
    AModule := AModuleServices.Modules[i];
    if AModule.QueryInterface(IOTAProjectGroup, AProjectGroup) = S_OK then
      Break;
  end;

  if Assigned(AProjectGroup) then
    Result := AProjectGroup.ActiveProject;

  AModuleServices := nil;
  AModule := nil;
  AProjectGroup := nil;
end;

{ TfrxVisualComponentWizard }

procedure TfrxVisualComponentWizard.AfterSave;
begin end;

procedure TfrxVisualComponentWizard.BeforeSave;
begin end;

procedure TfrxVisualComponentWizard.Destroyed;
begin end;

procedure TfrxVisualComponentWizard.Execute;
var
  p : IOTAProject;
  cn,
  UnitIdent,
  FileName : String;
begin
  if Assigned(BorlandIDEServices) then
  begin
    (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName('', UnitIdent, cn, FileName);
    Delete(UnitIdent, 1, 4);

    p := GetActiveProject;
    if Assigned(p) then
    begin
      (BorlandIDEServices as IOTAModuleServices).CreateModule(TfrxVisualComponentModuleCreator.Create(p, UnitIdent));
    end;
  end;
end;

function TfrxVisualComponentWizard.GetAuthor: string;
begin
  Result := 'TheUnknownOnes'
end;

function TfrxVisualComponentWizard.GetComment: string;
begin
  Result := 'Creates a new frxVisualComponentFrame';
end;

function TfrxVisualComponentWizard.GetDesigner: string;
begin
  Result := dVCL;
end;

function TfrxVisualComponentWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := nil;
end;

function TfrxVisualComponentWizard.GetGlyph: Cardinal;
begin
  Result := LoadIcon(HInstance, 'FRXVISUALCOMPONENTIDE');
end;

function TfrxVisualComponentWizard.GetIDString: string;
begin
  Result := 'TfrxVisualComponentWizard.TheUnknownOnes.net'
end;

function TfrxVisualComponentWizard.GetName: string;
begin
  Result := 'frxVisualComponent';
end;

function TfrxVisualComponentWizard.GetPage: string;
begin
  Result := 'TUO.Fastreport';
end;

function TfrxVisualComponentWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

function TfrxVisualComponentWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TfrxVisualComponentWizard.Modified;
begin end;

{ TfrxVisualComponentModuleCreator }

constructor TfrxVisualComponentModuleCreator.Create(AOwner: IOTAModule;
  AClassIdent: string);
begin
  inherited Create;
  FClassIdent := AClassIdent;
end;

procedure TfrxVisualComponentModuleCreator.FormCreated(
  const FormEditor: IOTAFormEditor);
begin end;

function TfrxVisualComponentModuleCreator.GetAncestorName: string;
begin
  Result := 'frxVisualComponentFrame';
end;

function TfrxVisualComponentModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TfrxVisualComponentModuleCreator.GetExisting: Boolean;
begin
  Result := false;
end;

function TfrxVisualComponentModuleCreator.GetFileSystem: string;
begin
  Result := EmptyStr;
end;

function TfrxVisualComponentModuleCreator.GetFormName: string;
begin
  Result := FORM_CLASS_NAME + FClassIdent;
end;

function TfrxVisualComponentModuleCreator.GetImplFileName: string;
begin
  Result := EmptyStr;
end;

function TfrxVisualComponentModuleCreator.GetIntfFileName: string;
begin
  Result := EmptyStr;
end;

function TfrxVisualComponentModuleCreator.GetMainForm: Boolean;
begin
  Result := false;
end;

function TfrxVisualComponentModuleCreator.GetOwner: IOTAModule;
begin
  Result := GetActiveProject;
end;

function TfrxVisualComponentModuleCreator.GetShowForm: Boolean;
begin
  Result := true;
end;

function TfrxVisualComponentModuleCreator.GetShowSource: Boolean;
begin
  Result := true;
end;

function TfrxVisualComponentModuleCreator.GetUnnamed: Boolean;
begin
  Result := true;
end;

function TfrxVisualComponentModuleCreator.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TDFMFile.Create('DFM').Replace(FormIdent, AncestorIdent);
end;

function TfrxVisualComponentModuleCreator.NewImplSource(const ModuleIdent,
  FormIdent, AncestorIdent: string): IOTAFile;
var
  Ident : String;
begin
  Ident := Copy(FormIdent, Length(FORM_CLASS_NAME) + 1, Length(FormIdent));
  Result := TUnitFile.Create('UNIT').Replace(ModuleIdent, FormIdent, AncestorIdent, Ident);
end;

function TfrxVisualComponentModuleCreator.NewIntfSource(const ModuleIdent,
  FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

{ TSrcFile }

constructor TSrcFile.Create(AResourceName: String);
var
  rs : TResourceStream;
  sl : TStringList;
begin
  rs := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  sl := TStringList.Create;
  try
    sl.LoadFromStream(rs);
    FSource := sl.Text;
  finally
    sl.Free;
    rs.Free;
  end;
end;

function TSrcFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TSrcFile.GetSource: string;
begin
  Result := FSource;
end;

{ TTfrxVisualComponentModule }

class function TTfrxVisualComponentModule.DesignClass: TComponentClass;
begin
  Result := TfrxVisualComponentFrame;
end;

procedure TTfrxVisualComponentModule.ExecuteVerb(Index: Integer);
begin end;

function TTfrxVisualComponentModule.GetAttributes: TCustomModuleAttributes;
begin
  Result := [];
end;

function TTfrxVisualComponentModule.GetVerb(Index: Integer): string;
begin end;

function TTfrxVisualComponentModule.GetVerbCount: Integer;
begin
  Result := 0;
end;

function TTfrxVisualComponentModule.Nestable: Boolean;
begin
  Result := true;
end;

procedure TTfrxVisualComponentModule.PrepareItem(Index: Integer;
  const AItem: IMenuItem);
begin end;

procedure TTfrxVisualComponentModule.Saving;
begin end;

procedure TTfrxVisualComponentModule.ValidateComponent(Component: TComponent);
begin end;

function TTfrxVisualComponentModule.ValidateComponentClass(
  ComponentClass: TComponentClass): Boolean;
begin
  Result := true;
end;

{ TDFMFile }

function TDFMFile.Replace(AFormIdent, AAncestorIdent: String): TDFMFile;
begin
  FSource := StringReplace(FSource, '$FormIdent$', AFormIdent, [rfReplaceAll, rfIgnoreCase]);
  FSource := StringReplace(FSource, '$AncestorIdent$', AAncestorIdent, [rfReplaceAll, rfIgnoreCase]);
  Result := Self;
end;

{ TUnitFile }

function TUnitFile.Replace(AModuleIdent, AFormIdent,
  AAncestorIdent, AIdent: String): TUnitFile;
begin
  FSource := StringReplace(FSource, '$ModuleIdent$', AModuleIdent, [rfReplaceAll, rfIgnoreCase]);
  FSource := StringReplace(FSource, '$FormIdent$', AFormIdent, [rfReplaceAll, rfIgnoreCase]);
  FSource := StringReplace(FSource, '$AncestorIdent$', AAncestorIdent, [rfReplaceAll, rfIgnoreCase]);
  FSource := StringReplace(FSource, '$Ident$', AIdent, [rfReplaceAll, rfIgnoreCase]);
  Result := Self;
end;

end.
