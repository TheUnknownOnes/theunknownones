unit uch2ProviderWindowsSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uch2Main, Spin, StdCtrls, ComCtrls, ToolWin,
  uch2FrameHelpItemDecoration, ExtCtrls, ImgList;

type
  Tch2ProviderWindowsSearch = class(TInterfacedObject, Ich2Provider)
  private
    FPriority : Integer;
    procedure SetPriority(APriority: Integer);
  protected
    function GetDecoration(AName: String): Tch2HelpItemDecoration;
    procedure SetDecoration(AName: String; ADeco: Tch2HelpItemDecoration);
  public
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;

    {$REGION 'Ich2Provider'}
    function GetGUID : TGUID;
    function GetDescription : String;
    function GetName : String;

    procedure ProvideHelp(AKeyword : String; AGUI : Ich2GUI);

    procedure Configure;

    function GetPriority : Integer;
    {$ENDREGION}
  end;

  Tuch2FormProviderWindowsSearch = class(TForm)
    GroupBox2: TGroupBox;
    LV: TListView;
    Panel2: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    ed_Name: TEdit;
    ed_Query: TEdit;
    frame_Deco: Tch2FrameHelpItemDecoration;
    ToolBar1: TToolBar;
    btn_Add: TToolButton;
    btn_Del: TToolButton;
    Panel1: TPanel;
    btn_OK: TButton;
    Panel3: TPanel;
    Label1: TLabel;
    ed_Prio: TSpinEdit;
    iml_TB: TImageList;
    procedure btn_AddClick(Sender: TObject);
    procedure btn_DelClick(Sender: TObject);
    procedure LVChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure ed_NameChange(Sender: TObject);
    procedure ed_QueryChange(Sender: TObject);
  private
    FProvider : Tch2ProviderWindowsSearch;
    procedure Init;
  public
    class procedure Execute(AProvider: Tch2ProviderWindowsSearch);
  end;

var
  uch2FormProviderWindowsSearch: Tuch2FormProviderWindowsSearch;

implementation

uses
  Registry;

{$R *.dfm}

{ Tch2ProviderWindowsSearch }
const
  REG_VALUE_PRIORITY = 'Priority';
  REG_KEY_SEARCHES = 'Searches';

procedure Tch2ProviderWindowsSearch.AfterConstruction;
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID], true) then
    begin
      if Reg.ValueExists(REG_VALUE_PRIORITY) then
        FPriority := reg.ReadInteger(REG_VALUE_PRIORITY)
      else
        FPriority := 0;

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure Tch2ProviderWindowsSearch.BeforeDestruction;
begin
  SetPriority(FPriority);
  inherited;
end;

procedure Tch2ProviderWindowsSearch.Configure;
begin
  Tuch2FormProviderWindowsSearch.Execute(self);
end;

function Tch2ProviderWindowsSearch.GetDecoration(
  AName: String): Tch2HelpItemDecoration;
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_SEARCHES+'\'+AName, true) then
    begin
      Result.LoadFromRegistry(Reg);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function Tch2ProviderWindowsSearch.GetDescription: String;
begin
  Result:='Search all indexed files on your computer.'
end;

function Tch2ProviderWindowsSearch.GetGUID: TGUID;
begin
  Result:=StringToGUID('{FA18E9E3-AF0F-413D-BC23-53E23C31B49C}');
end;

function Tch2ProviderWindowsSearch.GetName: String;
begin
  Result:='Windows Search';
end;

function Tch2ProviderWindowsSearch.GetPriority: Integer;
begin
  Result:=FPriority;
end;

procedure Tch2ProviderWindowsSearch.ProvideHelp(AKeyword: String;
  AGUI: Ich2GUI);
begin

end;

procedure Tch2ProviderWindowsSearch.SetDecoration(AName: String;
  ADeco: Tch2HelpItemDecoration);
var
  Reg : TRegistry;
begin
  inherited;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID]+'\'+REG_KEY_SEARCHES+'\'+AName, true) then
    begin
      ADeco.SaveToRegistry(Reg);

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure Tch2ProviderWindowsSearch.SetPriority(APriority: Integer);
var
  Reg : TRegistry;
begin
  FPriority:=APriority;

  Reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    if Reg.OpenKey(ch2Main.RegRootKeyProvider[GetGUID], true) then
    begin
      Reg.WriteInteger(REG_VALUE_PRIORITY, FPriority);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure Tuch2FormProviderWindowsSearch.btn_AddClick(Sender: TObject);
var
  item : TListItem;
begin
  item:=LV.Items.Add;
  item.Caption:='<new Windows Search>';
  item.SubItems.Add('$(Helpstring)');
  LV.Selected:=item;
end;

procedure Tuch2FormProviderWindowsSearch.btn_DelClick(Sender: TObject);
begin
  LV.DeleteSelected;
end;

procedure Tuch2FormProviderWindowsSearch.ed_NameChange(Sender: TObject);
begin
  if lv.Selected<>nil then
  begin
    LV.Selected.Caption:=ed_Name.Text;  
  end; 
end;

procedure Tuch2FormProviderWindowsSearch.ed_QueryChange(Sender: TObject);
begin
  if lv.Selected<>nil then
  begin
    LV.Selected.SubItems[0]:=ed_Query.Text;  
  end; 
end;

class procedure Tuch2FormProviderWindowsSearch.Execute(
  AProvider: Tch2ProviderWindowsSearch);
var
  Form: Tuch2FormProviderWindowsSearch;
begin
  Form:=Tuch2FormProviderWindowsSearch.Create(nil);
  try
    Form.FProvider:=AProvider;
    Form.Init;
    
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

procedure Tuch2FormProviderWindowsSearch.Init;
begin

end;

procedure Tuch2FormProviderWindowsSearch.LVChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if Item=LV.Selected then
  begin
    ed_Name.Text:=Item.Caption;
    ed_Query.Text:=Item.SubItems[0];
    frame_Deco.Decoration:=FProvider.GetDecoration(Item.Caption);
  end;
end;

initialization
  ch2Main.RegisterProvider(Tch2ProviderWindowsSearch.Create as Ich2Provider);


end.
