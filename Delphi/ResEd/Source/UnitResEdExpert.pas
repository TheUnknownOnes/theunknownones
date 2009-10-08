//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitResEdExpert;

interface

uses EditIntf, ToolsApi, UnitResEdDataModule, IDEMessages, Graphics, 
     Classes
     {$IFDEF VER180}, CodeTemplateAPI, TUOScript{$ENDIF};

type
  {$region 'TWizardResEd'}
  TWizardResEd = class(TInterfacedObject, IOTAWizard, IOTANotifier)
  private
    WizardDataModule : TResEdDataModule;
    SplashBitmap     : TBitmap;
    function GetState: TWizardState;
  public
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Execute;
    procedure Modified;

    {$IFDEF VER180}
    function AddResource(Params : TStrings):Integer;
    function LoadResource(Params : TStrings):Integer;
    {$ENDIF}

    function GetIDString: string;
    function GetName: string;

    constructor Create;
    destructor Destroy; override;
  end;
  {$endregion}

  {$region 'TNotifyResEd'}
  TNotifyResEd = class(TNotifierObject, IOTAIDENotifier,IOTAIDENotifier80)
  protected
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);

    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean); overload; // Defined in IOTAIDENotifier80
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload; // Defined in IOTAIDENotifier

    procedure AfterCompile(Succeeded: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
    procedure AfterCompile(const Project: IOTAProject; Succeeded: Boolean; IsCodeInsight: Boolean); overload;
  end;
  {$endregion}

procedure Register;

implementation

uses UnitResEdMain, Dialogs, SysUtils, UnitResEdListForm, Controls,
     ResEdVirtualTrees, Menus, DeskUtil;

{$Region 'Register and unregister routines'}
var NotifierIndex : integer;

procedure Register;
var   
  Services   : IOTAServices;
  Wizard     : TWizardResEd;
begin
  Wizard:=TWizardResEd.Create;

  SplashScreenServices.AddPluginBitmap(
             'ResEd (Project Resource Editor)',
             Wizard.SplashBitmap.Handle,
             false,
             'Freeware without any warranty',
             'by TheUnknownOnes');
  (BorlandIDEServices as IOTAAboutBoxServices).AddPluginInfo(
             'ResEd (Project Resource Editor)',
             'This expert will help you to create and organize the resources contained in your Win32 projects.'+
             #13#10#13#10+
             'Contact: MarcoWarm@gmx.net; chaosben@web.de',
             Wizard.SplashBitmap.Handle,
             false,
             'Freeware without any warranty',
             'by TheUnknownOnes');

  RegisterPackageWizard(Wizard);
  Services := BorlandIDEServices as IOTAServices;
  Assert(Assigned(Services), 'IOTAServices not available');
  NotifierIndex := Services.AddNotifier(TNotifyResEd.Create);

  {$IFDEF VER180}
  TUOScriptEngine.RegisterFunction('AddResource', Integer(@TWizardResEd.AddResource));
  TUOScriptEngine.RegisterFunction('LoadResource', Integer(@TWizardResEd.LoadResource));
  {$ENDIF}
end;

procedure RemoveNotifier;
var
  Services: IOTAServices;
begin
  {$IFDEF VER180}
  TUOScriptEngine.UnregisterFunction('AddResource');
  TUOScriptEngine.UnregisterFunction('LoadResource');
  {$ENDIF}
  
  if NotifierIndex <> -1 then
  begin
    Services := BorlandIDEServices as IOTAServices;
    Assert(Assigned(Services), 'IOTAServices not available');
    try
      Services.RemoveNotifier(NotifierIndex);
    except
    end;
  end;
end;

{$EndRegion}

{$Region 'TWizardResEd'}

{$Region 'empty Methods ... are never used anyway'}


procedure TWizardResEd.Modified;
begin

end;

procedure TWizardResEd.AfterSave;
begin

end;

procedure TWizardResEd.BeforeSave;
begin

end;

procedure TWizardResEd.Execute;
begin

end;

procedure TWizardResEd.Destroyed;
begin
end;

{$EndRegion}

function TWizardResEd.GetIDString: string;
begin  
  Result := 'TUO.ResEd';
end;

function TWizardResEd.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

constructor TWizardResEd.Create;
begin  
  WizardDataModule:=TResEdDataModule.Create(nil);
  SplashBitmap:=Graphics.TBitmap.Create;
  WizardDataModule.imlSplash.GetBitmap(0,SplashBitmap);
end;

function TWizardResEd.GetName: string;
begin
  Result := 'ResEd';
end;

destructor TWizardResEd.Destroy;
begin
  WizardDataModule.Free;
  SplashBitmap.Free;
  inherited;
end;
{$EndRegion}

{$Region 'TNotifyResEd'}

{$Region 'Empty Methods .... are never used anyway'}

procedure TNotifyResEd.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin

end;

procedure TNotifyResEd.BeforeCompile(const Project: IOTAProject;
  IsCodeInsight: Boolean; var Cancel: Boolean);
begin

end;

procedure TNotifyResEd.AfterCompile(const Project: IOTAProject; Succeeded,
  IsCodeInsight: Boolean);
begin

end;

procedure TNotifyResEd.AfterCompile(Succeeded, IsCodeInsight: Boolean);
begin

end;

procedure TNotifyResEd.AfterCompile(Succeeded: Boolean);
begin

end;

{$EndRegion}

procedure TNotifyResEd.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  if NotifyCode = ofnActiveProjectChanged then
  begin
    try
      if Assigned(MainForm) and MainForm.Visible then
        TFormWizardResEd(MainForm).FormShow(nil);
    except
    end;
  end;
end;

{$EndRegion}

{$region 'ScriptStuff'}
{$IFDEF VER180}

function TWizardResEd.AddResource(Params : TStrings):Integer;
var
  i      : Integer;
  RELF   : TResEdListForm;
  MainFormVisible: Boolean;
begin
  result:=0;
  if (Params.Count<>1) then
    raise  Exception.Create('Invalid number of arguments '#13#10+
                            'found '+IntToStr(Params.Count)+#13#10+
                            'requested 1 (New Resourcename)');

  MainFormVisible:=Assigned(MainForm) and MainForm.Visible;
  TResEdDataModule.ShowResEdForm;

  RELF:=TResEdListForm.Create(nil);
  try
    RELF.Caption:='Select res-file';
    RELF.ListBox1.Clear;
    TFormWizardResEd(MainForm).ReturnResFiles(RELF.ListBox1.Items);
    RELF.ListBox1.ItemIndex:=0;
    if RELF.ShowModal=mrok then
      if (RELF.ListBox1.ItemIndex>=0) then
      begin
        if (RELF.ListBox1.Items.Objects[RELF.ListBox1.ItemIndex])<>nil then
        begin
          TFormWizardResEd(MainForm).TV.Selected[PVirtualNode(RELF.ListBox1.Items.Objects[RELF.ListBox1.ItemIndex])]:=True;

          RELF.Caption:='Select resource type';
          RELF.ListBox1.Clear;
          for i := 0 to  TFormWizardResEd(MainForm).miNewResource.Count-1 do
            RELF.ListBox1.Items.AddObject(
                      StringReplace(TFormWizardResEd(MainForm).miNewResource[i].Caption,
                                    '&','',[rfReplaceAll]),
                      TFormWizardResEd(MainForm).miNewResource[i]);
          
          RELF.ListBox1.ItemIndex:=0;
          if RELF.ShowModal=mrok then
            if (RELF.ListBox1.ItemIndex>=0) then
            begin
              ResNameGlob:=UpperCase(Params[0]);
              TMenuItem(RELF.ListBox1.Items.Objects[RELF.ListBox1.ItemIndex]).Click;
            end;
        end;
      end;
  finally
    ResNameGlob:='';
    FreeAndNil(RELF);
    if (not MainFormVisible) then
      MainForm.Visible:=False;
  end;
  result:=1;
end;

function TWizardResEd.LoadResource(Params : TStrings):Integer;
var
  RELF   : TResEdListForm;
  resType: TResType;

  Services     : IOTAModuleServices;
  Module       : IOTAModule;
  SourceEditor : IOTASourceEditor;
  EditView     : IOTAEditView;
  MainFormVisible: Boolean;
  ErrCode        : Integer;
begin
  result:=0;
  if (Params.Count<>2) then
    raise  Exception.Create('Invalid number of arguments '#13#10+
                            'found '+IntToStr(Params.Count)+#13#10+
                            'requested 2 (resourcetype,replacepattern)');

  //BITMAP, CURSOR, ICON, JPEG, PNG, GIF, MESSAGE, STRING, RCDATA
  resType:=GetResTypeFromString(Trim(Params[0]));

  MainFormVisible:=Assigned(MainForm) and MainForm.Visible;
  TResEdDataModule.ShowResEdForm;

  RELF:=TResEdListForm.Create(nil);
  try
    RELF.Caption:='Select resource';
    RELF.ListBox1.Clear;
    TFormWizardResEd(MainForm).ReturnResources(RELF.ListBox1.Items,resType);
    RELF.ListBox1.ItemIndex:=0;
    if RELF.ShowModal=mrok then
      if (RELF.ListBox1.ItemIndex>=0) then
      begin
        Services := BorlandIDEServices as IOTAModuleServices;
        Module:=Services.CurrentModule;

         if Supports(Module.CurrentEditor, IOTASourceEditor, SourceEditor) then
         begin
           EditView := SourceEditor.EditViews[0];
           SourceEditor.Show;
           SourceEditor.SwitchToView(0);


           if (RELF.ListBox1.Items.Objects[RELF.ListBox1.Itemindex]=nil) then
             EditView.Position.Replace(Params[1],
                                       RELF.ListBox1.Items[RELF.ListBox1.Itemindex],
                                       True, False, True,
                                       ToolsApi.sdBackward, ErrCode)
           else
             EditView.Position.Replace(Params[1],
                                       IntToStr(Integer(
                                       RELF.ListBox1.Items.Objects[RELF.ListBox1.Itemindex])-1),
                                       True, False, True,
                                       ToolsApi.sdBackward, ErrCode);
        end;
      end;
  finally
    ResNameGlob:='';
    FreeAndNil(RELF);
    if (not MainFormVisible) then
      MainForm.Visible:=False;
    FocusWindow(EditView.GetEditWindow.Form);
  end;
  result:=1;
end;

{$ENDIF}
{$EndRegion}

initialization

finalization
  RemoveNotifier;

end.
