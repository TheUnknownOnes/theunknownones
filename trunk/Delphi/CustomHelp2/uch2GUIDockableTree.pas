unit uch2GUIDockableTree;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DockForm, DeskUtil, uch2Main, StdCtrls, ExtCtrls, ComCtrls, ImgList,
  uch2FrameHelpTree;

type
  Tch2GUIDockableTree = class;

  Tch2FormGUIDockableTree = class(TDockableForm)
    Frame: Tch2FrameHelpTree;

    procedure FormCreate(Sender: TObject);
  end;

  Tch2GUIDockableTree = class(TInterfacedObject, Ich2GUI)
  private
    FForm : Tch2FormGUIDockableTree;

    {$REGION 'Ich2HelpGUI'}
    function GetName : String;
    function GetDescription : String;
    function GetGUID : TGUID;

    procedure Show(const AHelpString : String; const Ach2Keywords : TStringList);

    function AddHelpItem(AHelpItem : Ich2HelpItem; AParent : Pointer = nil) : Pointer;
    {$ENDREGION}
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;


type
  TDockableFormClass = Class Of TDockableForm;

procedure CreateDockableForm(var FormVar: TDockableForm; FormClass: TDockableFormClass);
procedure FreeDockableForm(var FormVar: TDockableForm);
procedure ShowDockableForm(Form: TDockableForm);

implementation

{$R *.dfm}


{$Region 'DockableForm Routines'}
procedure RegisterDockableForm(FormClass: TDockableFormClass;
  var FormVar; const FormName: string);
begin
  if @RegisterFieldAddress <> nil then
    RegisterFieldAddress(FormName, @FormVar);
  RegisterDesktopFormClass(FormClass, FormName, FormName);
end;

procedure UnRegisterDockableForm(var FormVar; const FormName: string);
begin
  if @UnregisterFieldAddress <> nil then
    UnregisterFieldAddress(@FormVar);
end;

procedure ShowDockableForm(Form: TDockableForm);
begin
  if not Assigned(Form) then
    Exit;
  if not Form.Floating then
  begin
    Form.ForceShow;
    FocusWindow(Form);
  end
  else
    Form.Show;
end;

procedure CreateDockableForm(var FormVar: TDockableForm; FormClass: TDockableFormClass);
begin
  TCustomForm(FormVar) := FormClass.Create(nil);
  RegisterDockableForm(FormClass, FormVar, TCustomForm(FormVar).Name);
end;

procedure FreeDockableForm(var FormVar: TDockableForm);
begin
  if Assigned(FormVar) then
  begin
    FormVar.Hide;
    UnRegisterDockableForm(FormVar, FormVar.Name);
    {FormVar.Release;
    FormVar:=nil;  }
    FreeAndNil(FormVar);
  end;
end;
{$EndRegion}

{ Tch2GUIDockableTree }

function Tch2GUIDockableTree.AddHelpItem(AHelpItem: Ich2HelpItem;
  AParent: Pointer): Pointer;
begin
  Result:=FForm.Frame.AddHelpItem(AHelpItem, AParent);
end;

procedure Tch2GUIDockableTree.AfterConstruction;
begin
  inherited;
  CreateDockableForm(TDockableForm(FForm), Tch2FormGUIDockableTree);
end;

procedure Tch2GUIDockableTree.BeforeDestruction;
begin
  FreeDockableForm(TDockableForm(FForm));
  inherited;
end;

function Tch2GUIDockableTree.GetDescription: String;
begin
  Result:='A dockable help with a treeview';
end;

function Tch2GUIDockableTree.GetGUID: TGUID;
begin
  Result:=StringToGUID('{CD0B4F74-6B2C-4851-8E0B-D652005AC7FB}');
end;

function Tch2GUIDockableTree.GetName: String;
begin
  Result:='DockableHelpGUI'
end;

procedure Tch2GUIDockableTree.Show(const AHelpString: String;
  const Ach2Keywords: TStringList);
begin
  FForm.Frame.Init(AHelpString, Ach2Keywords);
  ShowDockableForm(TDockableForm(Self.FForm));
end;

procedure Tch2FormGUIDockableTree.FormCreate(Sender: TObject);
begin
  Self.AutoSave:=True;
  Self.DeskSection:='CustomHelp2_GUI_DOCKABLE_TREE';
end;


initialization
  ch2Main.RegisterGUI(Tch2GUIDockableTree.Create as Ich2GUI);

end.
