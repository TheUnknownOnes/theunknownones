unit uFrameConfigProviders;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, uCustomHelpConsts, Buttons,
  uCustomHelpMain;

type
  TConfigProvidersFilter = (cpfFileBased, cpfWebBased);

  TFrameConfigProviders = class(TFrame)
    pnlOHSItem: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    LabelURLPath: TLabel;
    Label8: TLabel;
    edName: TEdit;
    edDesc: TEdit;
    edURL: TEdit;
    cbTrimNamespacesOHS: TComboBox;
    ListView1: TListView;
    BtnBrowseForFile: TSpeedButton;
    OpenDialog1: TOpenDialog;
    procedure cbTrimNamespacesOHSChange(Sender: TObject);
    procedure edDescChange(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure edURLChange(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BtnBrowseForFileClick(Sender: TObject);
  private
    FInsertItem : TListItem;
    function IsURLAllowed(AFilter: TConfigProvidersFilter;
      AURL: String): Boolean;
    function TrimOptionFromString(AString: String): TNamespaceTrimOption;
  public
    procedure AfterConstruction; override;
    procedure InitContent(AFilter: TConfigProvidersFilter; AType : TProviderType);
    function Save(AOffset : Integer; AType : TProviderType) : Integer;
  end;

implementation

uses Registry, StrUtils;

{$R *.dfm}

procedure TFrameConfigProviders.AfterConstruction;
var
  idx : TNamespaceTrimOption;
begin
  inherited;
  for idx := Low(OPTIONS_NAMESPACETRIM) to High(OPTIONS_NAMESPACETRIM) do
  begin
    cbTrimNamespacesOHS.Items.Add(OPTIONS_NAMESPACETRIM[idx]);
  end;

end;

procedure TFrameConfigProviders.BtnBrowseForFileClick(Sender: TObject);
begin
  OpenDialog1.InitialDir:=ExtractFilePath(edUrl.Text);
  OpenDialog1.FileName:=edURL.Text;
  if OpenDialog1.Execute(Self.Handle) then
    edURL.Text:=OpenDialog1.FileName;
end;

procedure TFrameConfigProviders.cbTrimNamespacesOHSChange(Sender: TObject);
begin
  if (ListView1.ItemFocused<>FInsertItem) and
     Assigned(ListView1.ItemFocused) then
    ListView1.ItemFocused.SubItems[2]:=cbTrimNamespacesOHS.Text;
end;

procedure TFrameConfigProviders.edDescChange(Sender: TObject);
begin
  if (ListView1.ItemFocused<>FInsertItem) and
     Assigned(ListView1.ItemFocused) then
    ListView1.ItemFocused.SubItems[0]:=edDesc.Text;
end;

procedure TFrameConfigProviders.edNameChange(Sender: TObject);
begin
  if (ListView1.ItemFocused<>FInsertItem) and
     Assigned(ListView1.ItemFocused) then
    ListView1.ItemFocused.Caption:=edName.Text;
end;

procedure TFrameConfigProviders.edURLChange(Sender: TObject);
begin
  if (ListView1.ItemFocused<>FInsertItem) and
     Assigned(ListView1.ItemFocused) then
    ListView1.ItemFocused.SubItems[1]:=edURL.Text;
end;

function TFrameConfigProviders.IsURLAllowed(AFilter: TConfigProvidersFilter; AURL: String): Boolean;
begin
  Result := false;

  case AFilter of
    cpfFileBased: Result:=not AnsiContainsStr(AURL,'://');
    cpfWebBased: Result:=AnsiContainsStr(AURL,'://');
  end;
end;

procedure TFrameConfigProviders.ListView1Change(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if Change = ctState then
  begin
    try
      edName.Text:=Item.Caption;
      edDesc.Text:=Item.SubItems[0];
      edURL.Text:=Item.SubItems[1];
      if Integer(TrimOptionFromString(Item.SubItems[2])) < cbTrimNamespacesOHS.Items.Count then
        cbTrimNamespacesOHS.ItemIndex:=Integer(TrimOptionFromString(Item.SubItems[2]));

      edName.Enabled := Assigned(ListView1.Selected) and (ListView1.Selected <> FInsertItem);
      edDesc.Enabled := edName.Enabled;
      edURL.Enabled := edName.Enabled;
      BtnBrowseForFile.Enabled:=edName.Enabled;
      cbTrimNamespacesOHS.Enabled:= edName.Enabled;
    except

    end;
  end;
end;

procedure TFrameConfigProviders.ListView1DblClick(Sender: TObject);
var
  item : TListItem;
begin
  if ListView1.Selected = FInsertItem  then
  begin
    item:=ListView1.Items.Insert(1);
    item.SubItems.Add('');
    item.SubItems.Add('');
    item.SubItems.Add('');
    item.SubItems.Add('');
    item.SubItems.Add('');

    ListView1.ItemFocused:=item;
    ListView1.Selected:=item;
    edName.Text:='new provider';
    edDesc.Text:='add some description here';
  end;
end;

procedure TFrameConfigProviders.ListView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if (ListView1.ItemFocused<>FInsertItem) and
     Assigned(ListView1.ItemFocused) and
     (Key=VK_DELETE) and
     (MessageDlg('Are you sure to delete '+ListView1.ItemFocused.Caption+'?',
     mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
   ListView1.DeleteSelected;
end;

function TFrameConfigProviders.Save(AOffset : Integer; AType : TProviderType) : Integer;
var
  Reg : TRegistry;
  idx: Integer;
begin
  Result:=AOffset;
  Reg := TRegistry.Create;
  try
    for idx := 0 to ListView1.Items.Count - 1 do
    begin
      if (ListView1.Items[idx]<>FInsertItem) and
         (Trim(ListView1.Items[Idx].Caption)<>EmptyStr) then
      begin
        TCustomHelp.WriteProviderToRegistry(IntToStr(idx+AOffset),
                                            ListView1.Items[Idx].Caption,
                                            ListView1.Items[Idx].SubItems[0],
                                            ListView1.Items[Idx].SubItems[1],
                                            TrimOptionFromString(ListView1.Items[Idx].SubItems[2]),
                                            AType,
                                            ListView1.Items[Idx].Checked);

        inc(Result);
      end;
    end;
  finally
    Reg.Free;
  end;
end;

function TFrameConfigProviders.TrimOptionFromString(AString: String): TNamespaceTrimOption;
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


procedure TFrameConfigProviders.InitContent(AFilter: TConfigProvidersFilter; AType : TProviderType);
var
  Reg : TRegistry;
  sl : TStringList;
  s : String;
  Sub_Key : String;
begin
  case AType of
    ptStandard: Sub_Key:=PROVIDER_SUB_KEY;
    ptRSS: Sub_Key:=RSS_PROVIDER_SUB_KEY;
  end;

  FInsertItem:=ListView1.Items.Add;

  with FInsertItem do
  begin
    Caption:='<new Item>';
    SubItems.Add('double click to add');
    SubItems.Add('');
    SubItems.Add('');
    SubItems.Add('');
  end;

  case AFilter of
    cpfFileBased:
      begin
        ListView1.Columns[2].Caption:=Format(ListView1.Columns[2].Caption,['Path/Shell command']);
        LabelURLPath.Caption:=Format(LabelURLPath.Caption,['Path/Command']);
        edURL.Hint:='- Path to a windows *.hlp file (be sure to have winhlp32.exe installed)'+#13#10+
                    '- Path to a windows htmlHelp file (*.chm)'+#13#10+
                    '- Shell command to execute';

      end;
    cpfWebBased:
      begin
        ListView1.Columns[2].Caption:=Format(ListView1.Columns[2].Caption,['URL']);
        LabelURLPath.Caption:=Format(LabelURLPath.Caption,['URL']);
        BtnBrowseForFile.Visible:=False;
        edURL.Width:=edName.Width;
        edURL.Hint:='- URL to a webbased search provider (e.g. koders.com)';
      end;
  end;




  Reg := TRegistry.Create;
  sl := TStringList.Create;
  try
    TCustomHelp.ReadSettingsFromRegistry(sl);

    if Reg.OpenKey(REG_ROOT_KEY + SUB_KEY, true) then
    begin
      Reg.GetKeyNames(sl);
      Reg.CloseKey;
    end;

    for s in sl do
    begin
      if Reg.OpenKey(REG_ROOT_KEY + SUB_KEY + '\' + s, false) then
      begin
        if IsURLAllowed(AFilter, Reg.ReadString(VALUE_URL)) then
        begin
          with ListView1.Items.Insert(1) do
          begin
            Caption:=Reg.ReadString(VALUE_NAME);
            SubItems.Add(Reg.ReadString(VALUE_DESCR));
            SubItems.Add(Reg.ReadString(VALUE_URL));
            SubItems.Add(OPTIONS_NAMESPACETRIM[TNamespaceTrimOption(StrToIntDef(Reg.ReadString(VALUE_TRIMNAMESPACE),0))]);
            Checked:=Reg.ReadString(VALUE_ENABLED)<>'0';
          end;
        end;

        Reg.CloseKey;
      end;
    end;
  finally
    sl.free;
    Reg.Free;
  end;
end;


end.
