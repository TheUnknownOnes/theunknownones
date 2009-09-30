{-----------------------------------------------------------------------------
 Purpose: The config dialog of the custom help expert 
 
 (c) by TheUnknownOnes
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uFormConfigCustomHelp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Registry, CheckLst, uMSHelpServices;

type
  Tform_Config = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    ListView1: TListView;
    Panel2: TPanel;
    edName: TEdit;
    edDesc: TEdit;
    edURL: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel3: TGroupBox;
    cbcusthelpwp: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ListView2: TListView;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ListView1DblClick(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure edDescChange(Sender: TObject);
    procedure edURLChange(Sender: TObject);
    procedure ListView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FInsertItem : TListItem;
    procedure Save;
    procedure BuildNamespaceList;
  public
    class function Execute : Boolean; 
  end;

implementation

uses uCustomHelpMain;

{$R *.dfm}

{ Tform_Config }

procedure Tform_Config.Button1Click(Sender: TObject);
begin
  Save;
  ModalResult:=mrOk;
end;

procedure Tform_Config.edDescChange(Sender: TObject);
begin
  if (ListView1.ItemFocused<>FInsertItem) and
     Assigned(ListView1.ItemFocused) then
    ListView1.ItemFocused.SubItems[0]:=edDesc.Text;
end;

procedure Tform_Config.edNameChange(Sender: TObject);
begin
  if (ListView1.ItemFocused<>FInsertItem) and
     Assigned(ListView1.ItemFocused) then
    ListView1.ItemFocused.Caption:=edName.Text;
end;

procedure Tform_Config.edURLChange(Sender: TObject);
begin
  if (ListView1.ItemFocused<>FInsertItem) and
     Assigned(ListView1.ItemFocused) then
    ListView1.ItemFocused.SubItems[1]:=edURL.Text;
end;

class function Tform_Config.Execute: Boolean;
var
  form : Tform_Config;
begin
  form := Tform_Config.Create(nil);
  try
    Result := IsPositiveResult(form.ShowModal);
  finally
    form.Free;
  end;
end;

procedure Tform_Config.BuildNamespaceList;
var
  nsl : IHxRegNamespaceList;
  idx : integer;
  Enabled : TStringList;
begin
  Enabled:=TStringList.Create;
  try
    GlobalCustomHelp.ReadEnabledNamespacesFromRegistry(Enabled);

    nsl:=GlobalCustomHelp.Namespaces;
    for idx := 1 to nsl.Count do
    begin
      with ListView2.Items.Add do
      begin
        Caption:=nsl.Item(idx).Name;
        SubItems.Add(nsl.Item(idx).GetProperty(HxRegNamespaceDescription));
        if Enabled.IndexOf(Caption)>=0 then
          Checked:=True
        else
          Checked:=False;
      end;
    end;
  finally
    Enabled.Free;
  end;
end;

procedure Tform_Config.FormShow(Sender: TObject);
var
  Reg : TRegistry;
  sl : TStringList;
  s : String;
begin
  BuildNamespaceList;
  
  FInsertItem:=ListView1.Items.Add;

  with FInsertItem do
  begin
    Caption:='<new Item>';
    SubItems.Add('double click to add');
    SubItems.Add('');
  end;

  Reg := TRegistry.Create;
  sl := TStringList.Create;
  try
    TCustomHelp.ReadSettingsFromRegistry(sl);
    cbcusthelpwp.Checked:=sl.Values[SETTINGS_CUSTHELPWP]='1';

    if Reg.OpenKey(PROVIDER_ROOT_KEY, true) then
    begin
      Reg.GetKeyNames(sl);
      Reg.CloseKey;
    end;

    for s in sl do
    begin
      if Reg.OpenKey(PROVIDER_ROOT_KEY + '\' + s, false) then
      begin
        with ListView1.Items.Add do
        begin
          Caption:=Reg.ReadString(VALUE_NAME);
          SubItems.Add(Reg.ReadString(VALUE_DESCR));
          SubItems.Add(Reg.ReadString(VALUE_URL));
        end;

        Reg.CloseKey;
      end;
    end;
  finally
    sl.free;
    Reg.Free;
  end;
end;

procedure Tform_Config.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if Change = ctState then
  begin
    edName.Text:=Item.Caption;
    edDesc.Text:=Item.SubItems[0];
    edURL.Text:=Item.SubItems[1];
  end;
end;

procedure Tform_Config.ListView1DblClick(Sender: TObject);
var
  item : TListItem;
begin
  if ListView1.ItemIndex=0 then
  begin
    item:=ListView1.Items.Add;
    item.SubItems.Add('');
    item.SubItems.Add('');

    ListView1.Selected:=item;
    ListView1.ItemFocused:=item;
  end;
end;

procedure Tform_Config.ListView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if (ListView1.ItemFocused<>FInsertItem) and
     Assigned(ListView1.ItemFocused) and
     (Key=VK_DELETE) and
     (MessageDlg('Are you sure to delete '+ListView1.ItemFocused.Caption+'?',
     mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
   ListView1.DeleteSelected;

end;

procedure Tform_Config.Save;
var
  Reg : TRegistry;                        
  idx: Integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.DeleteKey(PROVIDER_ROOT_KEY);

    for idx := 0 to ListView1.Items.Count - 1 do
    begin
      if (ListView1.Items[idx]<>FInsertItem) and
         (Trim(ListView1.Items[Idx].Caption)<>EmptyStr) then
      begin
        TCustomHelp.WriteProviderToRegistry(IntToStr(idx),
                                            ListView1.Items[Idx].Caption,
                                            ListView1.Items[Idx].SubItems[0],
                                            ListView1.Items[Idx].SubItems[1]);
      end;
    end; 
  finally
    Reg.Free;
  end;

  TCustomHelp.WriteSettingToRegistry(SETTINGS_CUSTHELPWP, IntToStr(byte(cbcusthelpwp.checked)));

  for idx := 0 to ListView2.Items.Count-1 do
    GlobalCustomHelp.WriteNamespacesToRegistry(ListView2.Items[idx].Caption, ListView2.Items[idx].Checked);
    
end;

end.
