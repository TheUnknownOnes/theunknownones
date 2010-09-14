unit uch2GUIDefault;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uch2Main, StdCtrls, ComCtrls, ExtCtrls, Registry, ImgList,
  uch2FrameHelpTree;

type
  Tch2GUIDefault = class;

  Tch2FormGUIDefault = class(TForm)
    Tree: Tch2FrameHelpTree;
    tmDoSearch: TTimer;
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tmDoSearchTimer(Sender: TObject);
  private
    procedure LoadSettings;
    procedure SaveSettings;
  public
  end;

  Tch2GUIDefault = class(TInterfacedObject, Ich2GUI)
  private
    FForm : Tch2FormGUIDefault;

    {$REGION 'Ich2HelpGUI'}
    function GetName : String;
    function GetDescription : String;
    function GetGUID : TGUID;

    procedure Show(const AHelpString : String; const Ach2Keywords : TStringList);

    function AddHelpItem(AHelpItem : Ich2HelpItem; AParent : Pointer = nil) : Pointer;
    {$ENDREGION}
  public

  end;

implementation

{$R *.dfm}

const
  Settings_Key_Stats = '\Stats\';
  Settings_Value_Expanded = 'Expanded';
  Settings_Value_Size = 'Size';

{ Tch2GUIDefault }

function Tch2GUIDefault.AddHelpItem(AHelpItem : Ich2HelpItem; AParent : Pointer = nil) : Pointer;
begin
  if Assigned(FForm) then
    Result := FForm.Tree.AddHelpItem(AHelpItem, AParent)
end;

function Tch2GUIDefault.GetDescription: String;
begin
  Result := 'The default GUI. Made without thinking about. :)'
end;

function Tch2GUIDefault.GetGUID: TGUID;
const
  g : TGUID = '{A533134A-065A-430B-960C-4EF5CACE470B}';
begin
  Result := g;
end;

function Tch2GUIDefault.GetName: String;
begin
  Result := 'DefaultGUI';
end;

procedure Tch2GUIDefault.Show(const AHelpString : String; const Ach2Keywords: TStringList);
begin
  FForm := Tch2FormGUIDefault.Create(nil);
  try
    Fform.Caption := 'Search Help for "' + AHelpString + '"';
    FForm.Tree.Init(AHelpString, Ach2Keywords, false);
    FForm.ShowModal;
  finally
    FForm.Free;
    FForm := nil;
  end;
end;

{ Tch2FormGUIDefault }


procedure Tch2FormGUIDefault.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  SaveSettings;
end;

procedure Tch2FormGUIDefault.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure Tch2FormGUIDefault.FormShow(Sender: TObject);
begin
  LoadSettings;
  tmDoSearch.Enabled := true;
end;

procedure Tch2FormGUIDefault.LoadSettings;
var
  Reg : TRegistry;
  r : TRect;
begin
  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    REG.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(ch2Main.RegRootKeyGUI[ch2Main.CurrentGUI.GetGUID], true) then
    begin
      if Reg.ValueExists(Settings_Value_Size) then
      begin
        Reg.ReadBinaryData(Settings_Value_Size, r, SizeOf(r));

        Left := r.Left;
        Top := r.Top;
        Width := r.Right - r.Left;
        Height := r.Bottom - r.Top;
      end;

      Reg.CloseKey;
    end;

  finally
    Reg.Free;
  end;
end;

procedure Tch2FormGUIDefault.SaveSettings;
var
  Reg : TRegistry;
  r : TRect;
begin
  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    REG.RootKey := HKEY_CURRENT_USER;

    r := Rect(Left, Top, Left + Width, Top + Height);

    if Reg.OpenKey(ch2Main.RegRootKeyGUI[ch2Main.CurrentGUI.GetGUID], true) then
    begin
      Reg.WriteBinaryData(Settings_Value_Size, r, SizeOf(r));

      Reg.CloseKey;
    end;

  finally
    Reg.Free;
  end;
end;

procedure Tch2FormGUIDefault.tmDoSearchTimer(Sender: TObject);
begin
  tmDoSearch.Enabled := false;
  Tree.ShowHelp(Tree.cbKeywords.Text);
end;

initialization
  ch2Main.RegisterGUI(Tch2GUIDefault.Create as Ich2GUI);

finalization


end.
