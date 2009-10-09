{-----------------------------------------------------------------------------
 Purpose: The config dialog of the custom help expert 
 
 (c) by TheUnknownOnes
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uFormConfigCustomHelp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Registry, CheckLst, uMSHelpServices,
  uCustomHelpMain;

type
  Tform_Config = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    ListView1: TListView;
    pnlOHSItem: TPanel;
    edName: TEdit;
    edDesc: TEdit;
    edURL: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    grpHelpDisplay: TGroupBox;
    cbcusthelpwp: TCheckBox;
    grpHelpNamespaces: TGroupBox;
    grpOtherHelpSources: TGroupBox;
    lvNamespaces: TListView;
    Panel4: TPanel;
    Panel5: TPanel;
    cbFullTextSearch: TCheckBox;
    cbReplaceDefaultViewer: TCheckBox;
    Label5: TLabel;
    edRedirectSchemes: TEdit;
    cbTrimNamespacesHX: TComboBox;
    Label4: TLabel;
    cbTrimNamespacesOHS: TComboBox;
    Label8: TLabel;
    cbOHSAtTop: TCheckBox;
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
    procedure FormCreate(Sender: TObject);
    procedure cbTrimNamespacesOHSChange(Sender: TObject);
  private
    FInsertItem : TListItem;
    procedure Save;
    procedure BuildNamespaceList;
    function TrimOptionFromString(AString: String): TNamespaceTrimOption;
  public
    class function Execute : Boolean; 
  end;

implementation

{$R *.dfm}


const
  OPTIONS_NAMESPACETRIM : array [nstoNoTrim..nstoTrimAll] of string = ('no trim',
                                                    'trim first namespace',
                                                    'trim all namespaces');

{ Tform_Config }

procedure Tform_Config.Button1Click(Sender: TObject);
begin
  Save;
  ModalResult:=mrOk;
end;

procedure Tform_Config.cbTrimNamespacesOHSChange(Sender: TObject);
begin
  if (ListView1.ItemFocused<>FInsertItem) and
     Assigned(ListView1.ItemFocused) then
    ListView1.ItemFocused.SubItems[2]:=cbTrimNamespacesOHS.Text;
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
      with lvNamespaces.Items.Add do
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

procedure Tform_Config.FormCreate(Sender: TObject);
var
  idx : TNamespaceTrimOption;
begin
  for idx := Low(OPTIONS_NAMESPACETRIM) to High(OPTIONS_NAMESPACETRIM) do
  begin
    cbTrimNamespacesHX.Items.Add(OPTIONS_NAMESPACETRIM[idx]);
    cbTrimNamespacesOHS.Items.Add(OPTIONS_NAMESPACETRIM[idx]);
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
    SubItems.Add('');
  end;

  Reg := TRegistry.Create;
  sl := TStringList.Create;
  try
    TCustomHelp.ReadSettingsFromRegistry(sl);
    cbcusthelpwp.Checked:=GlobalCustomHelp.ShowCustomHelpOnWelcomePage;
    cbFullTextSearch.Checked:=GlobalCustomHelp.PerformFullTextSearch;
    cbReplaceDefaultViewer.Checked := GlobalCustomHelp.ReplaceDefaultViewer;
    edRedirectSchemes.Text := GlobalCustomHelp.RedirectSchemes;
    cbTrimNamespacesHX.ItemIndex:=StrToIntDef(sl.Values[SETTINGS_TRIMNAMESPACES], 0);
    cbOHSAtTop.Checked:=GlobalCustomHelp.ShowOHSAtTop;

    if Reg.OpenKey(REG_ROOT_KEY + PROVIDER_SUB_KEY, true) then
    begin
      Reg.GetKeyNames(sl);
      Reg.CloseKey;
    end;

    for s in sl do
    begin
      if Reg.OpenKey(REG_ROOT_KEY + PROVIDER_SUB_KEY + '\' + s, false) then
      begin
        with ListView1.Items.Add do
        begin
          Caption:=Reg.ReadString(VALUE_NAME);
          SubItems.Add(Reg.ReadString(VALUE_DESCR));
          SubItems.Add(Reg.ReadString(VALUE_URL));
          SubItems.Add(OPTIONS_NAMESPACETRIM[TNamespaceTrimOption(StrToIntDef(Reg.ReadString(VALUE_TRIMNAMESPACE),0))]);
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
    cbTrimNamespacesOHS.ItemIndex:=Integer(TrimOptionFromString(Item.SubItems[2]));

    edName.Enabled := Assigned(ListView1.Selected) and (ListView1.Selected <> FInsertItem);
    edDesc.Enabled := edName.Enabled;
    edURL.Enabled := edName.Enabled;
    cbTrimNamespacesOHS.Enabled:= edName.Enabled;
  end;
end;

procedure Tform_Config.ListView1DblClick(Sender: TObject);
var
  item : TListItem;
begin
  if ListView1.Selected = FInsertItem  then
  begin
    item:=ListView1.Items.Add;
    item.SubItems.Add('');
    item.SubItems.Add('');
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

function Tform_Config.TrimOptionFromString(AString: String): TNamespaceTrimOption;
var
  idx: TNamespaceTrimOption;
begin
  Result:=nstoNoTrim;
  for idx := Low(OPTIONS_NAMESPACETRIM) to High(OPTIONS_NAMESPACETRIM) do
    if OPTIONS_NAMESPACETRIM[idx]=AString then
    begin
      Result:=idx;
      break;
    end;
end;

procedure Tform_Config.Save;
var
  Reg : TRegistry;
  idx: Integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.DeleteKey(REG_ROOT_KEY);

    for idx := 0 to ListView1.Items.Count - 1 do
    begin
      if (ListView1.Items[idx]<>FInsertItem) and
         (Trim(ListView1.Items[Idx].Caption)<>EmptyStr) then
      begin
        TCustomHelp.WriteProviderToRegistry(IntToStr(idx),
                                            ListView1.Items[Idx].Caption,
                                            ListView1.Items[Idx].SubItems[0],
                                            ListView1.Items[Idx].SubItems[1],
                                            TrimOptionFromString(ListView1.Items[Idx].SubItems[2]));
      end;
    end; 
  finally
    Reg.Free;
  end;

  GlobalCustomHelp.ShowCustomHelpOnWelcomePage:=cbcusthelpwp.Checked;
  GlobalCustomHelp.PerformFullTextSearch:=cbFullTextSearch.Checked;
  GlobalCustomHelp.TrimNamespacesUntilResultFound:=TNamespaceTrimOption(cbTrimNamespacesHX.ItemIndex);
  GlobalCustomHelp.ReplaceDefaultViewer := cbReplaceDefaultViewer.Checked;
  GlobalCustomHelp.RedirectSchemes := edRedirectSchemes.Text;
  GlobalCustomHelp.ShowOHSAtTop := cbOHSAtTop.Checked;

  with lvNamespaces do
    for idx := 0 to Items.Count-1 do
      GlobalCustomHelp.WriteNamespacesToRegistry(Items[idx].Caption, Items[idx].Checked);
    
end;

end.
