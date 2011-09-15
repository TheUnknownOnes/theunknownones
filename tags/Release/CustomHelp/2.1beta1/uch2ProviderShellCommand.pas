unit uch2ProviderShellCommand;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uch2Main, StdCtrls, Spin, ExtCtrls, uch2FrameHelpItemDecoration,
  ComCtrls, Registry, Contnrs, ImgList, ToolWin, StrUtils;

type
  Tch2ShellCommand = class
  public
    Name,
    Command,
    Params : String;
    Deco : Tch2HelpItemDecoration;

    constructor Create();
    destructor Destroy; override;

    procedure LoadSettings(ARegistry : TRegistry);
    procedure SaveSettings(ARegistry : TRegistry);
  end;

  Tch2ProviderShellCommand = class(TInterfacedObject, Ich2Provider)
  private
    FPriority : Integer;
    FCommands : TObjectList;
    function Getcommand(AIndex: Integer): Tch2ShellCommand;

    procedure LoadSettings;
    procedure SaveSettings;
  public

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    {$REGION 'Ich2Provider'}
    function GetGUID : TGUID;
    function GetDescription : String;
    function GetName : String;

    procedure ProvideHelp(AKeyword : String; AGUI : Ich2GUI);
    procedure Configure;

    function GetPriority : Integer;
    procedure SetPriority(ANewPriority : Integer);
    {$ENDREGION}

    property Commands : TObjectList read FCommands;
    property Command[AIndex : Integer] : Tch2ShellCommand read Getcommand;

  end;

  Tch2FormConfigShellCommand = class(TForm)
    Panel2: TPanel;
    btn_Ok: TButton;
    GroupBox1: TGroupBox;
    lv: TListView;
    Panel3: TPanel;
    frame_Deco: Tch2FrameHelpItemDecoration;
    ed_Name: TLabeledEdit;
    ed_Command: TLabeledEdit;
    ed_Params: TLabeledEdit;
    TB: TToolBar;
    btn_Add: TToolButton;
    btn_Del: TToolButton;
    procedure btn_AddClick(Sender: TObject);
    procedure btn_DelClick(Sender: TObject);
    procedure lvSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure FormShow(Sender: TObject);
    procedure ed_NameChange(Sender: TObject);
    procedure ed_CommandChange(Sender: TObject);
    procedure ed_ParamsChange(Sender: TObject);
  private
    FProvider : Tch2ProviderShellCommand;

    procedure OnDecoChange(Sender : TObject);
  public
    class function Execute(AProvdier : Tch2ProviderShellCommand) : Boolean;
  end;


implementation

uses uch2Data;

{$R *.dfm}
{$I CustomHelp2.inc}

const
  Settings_Value_Priority = 'Priority';
  Settings_Key_Commands = '\Commands';
  Settings_Value_Command = 'Command';
  Settings_Value_Params = 'Params';
  Settings_Value_Name = 'Name';

type
  Tch2HIShellCommand = class(TInterfacedObject, Ich2HelpItem)
  private
    FCMD : Tch2ShellCommand;
    FKeyword : String;
    FPriority : Integer;
  public
    constructor Create(ACMD : Tch2ShellCommand; AKeyword : String; APriority : Integer);

    {$REGION 'Ich2HelpItem'}
    function GetGUID : TGUID;
    function GetCaption : String;
    function GetDescription : String;
    function GetDecoration : Tch2HelpItemDecoration;
    function GetFlags : Tch2HelpItemFlags;
    function GetPriority : Integer;
    procedure ShowHelp;
    {$ENDREGION}
  end;

{ Tch2ProviderShellCommand }

procedure Tch2ProviderShellCommand.AfterConstruction;
begin
  inherited;

  FPriority := 0;

  FCommands := TObjectList.Create(true);
  LoadSettings;
end;

procedure Tch2ProviderShellCommand.BeforeDestruction;
begin
  SaveSettings;

  FCommands.Free;

  inherited;
end;

procedure Tch2ProviderShellCommand.Configure;
begin
  Tch2FormConfigShellCommand.Execute(Self);
  SaveSettings;
end;

function Tch2ProviderShellCommand.Getcommand(AIndex: Integer): Tch2ShellCommand;
begin
  Result := Tch2ShellCommand(FCommands[AIndex]);
end;

function Tch2ProviderShellCommand.GetDescription: String;
begin
  Result := 'Search for help by executing a shell command';
end;

function Tch2ProviderShellCommand.GetGUID: TGUID;
const
  g : TGUID = '{A6349FDF-90E5-480D-A90D-4DDCFB4C719A}';
begin
  Result := g;
end;

function Tch2ProviderShellCommand.GetName: String;
begin
  Result := 'Shell Command'
end;

function Tch2ProviderShellCommand.GetPriority: Integer;
begin
  Result := FPriority;
end;

procedure Tch2ProviderShellCommand.LoadSettings;
var
  Reg : TRegistry;
  sl : TStringList;
  s : String;
  cmd : Tch2ShellCommand;
begin
  inherited;

  sl := TStringList.Create;
  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID], true) then
    begin
      if Reg.ValueExists(Settings_Value_Priority) then
        FPriority := reg.ReadInteger(Settings_Value_Priority)
      else
        FPriority := 0;

      Reg.CloseKey;
    end;

    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID] + Settings_Key_Commands, true) then
    begin
      Reg.GetKeyNames(sl);
      Reg.CloseKey;
    end;

    for s in sl do
    begin
      if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID] + Settings_Key_Commands + '\' + s, false) then
      begin
        cmd := Tch2ShellCommand.Create;
        cmd.LoadSettings(Reg);
        FCommands.Add(cmd);
        Reg.CloseKey;
      end;
    end;

  finally
    sl.Free;
    Reg.Free;
  end;

end;

procedure Tch2ProviderShellCommand.ProvideHelp(AKeyword: String; AGUI: Ich2GUI);
var
  o : Pointer;
  c : Tch2ShellCommand absolute o;
begin
  for o in FCommands do
  begin
    AGUI.AddHelpItem(Tch2HIShellCommand.Create(c, AKeyword, FPriority));
  end;
end;

procedure Tch2ProviderShellCommand.SaveSettings;
var
  Reg : TRegistry;
  sl : TStringList;
  s : String;
  idx : Integer;
begin
  sl := TStringList.Create;
  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID], true) then
    begin
      Reg.WriteInteger(Settings_Value_Priority, FPriority);

      Reg.CloseKey;
    end;

    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID] + Settings_Key_Commands, true) then
    begin
      Reg.GetKeyNames(sl);

      for s in sl do
      begin
        Reg.DeleteKey(ch2Main.RegRootKeyProvider[GetGUID] + Settings_Key_Commands + '\' + s);
      end;

      Reg.CloseKey;
    end;

    for idx := 0 to FCommands.Count - 1 do
    begin
      if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID] + Settings_Key_Commands + '\' + IntToStr(idx), true) then
      begin
        Command[idx].SaveSettings(Reg);
        Reg.CloseKey;
      end;
    end;

  finally
    sl.Free;
    Reg.Free;
  end;

end;

procedure Tch2ProviderShellCommand.SetPriority(ANewPriority: Integer);
var
  Reg : TRegistry;
begin
  FPriority:=ANewPriority;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID], true) then
    begin
      Reg.WriteInteger(Settings_Value_Priority, FPriority);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

{ Tch2ShellCommand }

constructor Tch2ShellCommand.Create;
begin

end;

destructor Tch2ShellCommand.Destroy;
begin

  inherited;
end;

procedure Tch2ShellCommand.LoadSettings(ARegistry: TRegistry);
begin
  Deco.LoadFromRegistry(ARegistry);

  if ARegistry.ValueExists(Settings_Value_Name) then
    Name := ARegistry.ReadString(Settings_Value_Name)
  else
    Name := EmptyStr;

  if ARegistry.ValueExists(Settings_Value_Command) then
    Command := ARegistry.ReadString(Settings_Value_Command)
  else
    Command := EmptyStr;

  if ARegistry.ValueExists(Settings_Value_Params) then
    Params := ARegistry.ReadString(Settings_Value_Params)
  else
    Params := EmptyStr;
end;

procedure Tch2ShellCommand.SaveSettings(ARegistry: TRegistry);
begin
  Deco.SaveToRegistry(ARegistry);

  ARegistry.WriteString(Settings_Value_Name, Name);
  ARegistry.WriteString(Settings_Value_Command, Command);
  ARegistry.WriteString(Settings_Value_Params, Params);
end;

{ Tch2FormConfigShellCommand }

procedure Tch2FormConfigShellCommand.btn_AddClick(Sender: TObject);
var
  cmd : Tch2ShellCommand;
begin
  cmd := Tch2ShellCommand.Create;
  cmd.Name := IfThen(ed_Name.Text = '', 'NewCommand', ed_Name.Text);
  cmd.Command := IfThen(ed_Command.Text = '', 'c:\', ed_Command.Text);
  cmd.Params := ed_Params.Text;
  cmd.Deco := frame_Deco.Decoration;
  FProvider.Commands.Add(cmd);
  with lv.Items.Add do
  begin
    Data := cmd;
    Caption := cmd.Name;
    SubItems.Add(cmd.Command);
    SubItems.Add(cmd.Params);
    Selected := true;
  end;
end;

procedure Tch2FormConfigShellCommand.btn_DelClick(Sender: TObject);
begin
  if Assigned(lv.Selected) then
  begin
    FProvider.Commands.Remove(lv.Selected.Data);
    lv.DeleteSelected;
  end;
end;

class function Tch2FormConfigShellCommand.Execute(
  AProvdier: Tch2ProviderShellCommand): Boolean;
var
  form : Tch2FormConfigShellCommand;
begin
  form := Tch2FormConfigShellCommand.Create(nil);
  try
    form.FProvider := AProvdier;
    Result := IsPositiveResult(form.ShowModal);
  finally
    form.Free;
  end;
end;

procedure Tch2FormConfigShellCommand.FormShow(Sender: TObject);
var
  o : Pointer;
  cmd : Tch2ShellCommand absolute o;
begin
  for o in FProvider.Commands do
  begin
    with lv.Items.Add do
    begin
      Caption := cmd.Name;
      SubItems.Add(cmd.Command);
      SubItems.Add(cmd.Params);
      Data := cmd;
    end;
  end;

  frame_Deco.OnChange := OnDecoChange;
end;

procedure Tch2FormConfigShellCommand.lvSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
  begin
    with Tch2ShellCommand(Item.Data) do
    begin
      ed_Name.Text := Name;
      ed_Command.Text := Command;
      ed_Params.Text := Params;
      frame_Deco.Decoration := Deco;
      frame_Deco.Caption := Name;
    end;
  end
  else
  begin
    ed_Name.Text := EmptyStr;
    ed_Command.Text := EmptyStr;
    ed_Params.Text := EmptyStr;
    frame_Deco.ResetToDefault;
  end;
end;

procedure Tch2FormConfigShellCommand.OnDecoChange(Sender: TObject);
begin
  if Assigned(lv.Selected) then
    Tch2ShellCommand(lv.Selected.Data).Deco := frame_Deco.Decoration;
end;

procedure Tch2FormConfigShellCommand.ed_CommandChange(Sender: TObject);
begin
  if Assigned(lv.Selected) then
  begin
    Tch2ShellCommand(lv.Selected.Data).Command := ed_Command.Text;
    lv.Selected.SubItems[0] := ed_Command.Text;
  end;
end;

procedure Tch2FormConfigShellCommand.ed_NameChange(Sender: TObject);
begin
  if Assigned(lv.Selected) then
  begin
    Tch2ShellCommand(lv.Selected.Data).Name := ed_Name.Text;
    lv.Selected.Caption := ed_Name.Text;
    frame_Deco.Caption := ed_Name.Text;
  end;
end;

procedure Tch2FormConfigShellCommand.ed_ParamsChange(Sender: TObject);
begin
  if Assigned(lv.Selected) then
  begin
    Tch2ShellCommand(lv.Selected.Data).Params := ed_Params.Text;
    lv.Selected.SubItems[1] := ed_Params.Text;
  end;
end;

{ Tch2HIShellCommand }

constructor Tch2HIShellCommand.Create(ACMD: Tch2ShellCommand; AKeyword: String; APriority : Integer);
begin
  FCMD := ACMD;
  FKeyword := AKeyword;
  FPriority := APriority;
end;

function Tch2HIShellCommand.GetCaption: String;
begin
  Result := FCMD.Name;
end;

function Tch2HIShellCommand.GetDecoration: Tch2HelpItemDecoration;
begin
  Result := Fcmd.Deco;
end;

function Tch2HIShellCommand.GetDescription: String;
begin
  Result := '';
end;

function Tch2HIShellCommand.GetFlags: Tch2HelpItemFlags;
begin
  Result := [ifProvidesHelp];
end;

function Tch2HIShellCommand.GetGUID: TGUID;
const
  g : TGUID = '{13A826AF-3BA7-4A22-BDE6-8C2C26382761}';
begin
  Result := g;
end;

function Tch2HIShellCommand.GetPriority: Integer;
begin
  Result := FPriority;
end;

procedure Tch2HIShellCommand.ShowHelp;
var
  c, p : String;
begin
  c := StringReplace(FCMD.Command, '$(HelpString)', FKeyword, [rfIgnoreCase, rfReplaceAll]);
  p := StringReplace(FCMD.Params, '$(HelpString)', FKeyword, [rfIgnoreCase, rfReplaceAll]);

  ch2Main.ShellOpen(c, p);
end;

initialization
  {$IFDEF ProviderShellCommand}ch2Main.RegisterProvider(Tch2ProviderShellCommand.Create as Ich2Provider);{$ENDIF}

end.
