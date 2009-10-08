{-----------------------------------------------------------------------------
 Purpose: Form to select which help should be showed 
 
 (c) by TheUnknownOnes
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uCustomHelpSelector;
              
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HelpIntfs, OleCtrls, StdCtrls, ExtCtrls, ActiveX, mshtml, ComCtrls,
  CategoryButtons;

type
  TFormHelpSelector = class(TForm)
    CategoryButtons1: TCategoryButtons;
    Panel1: TPanel;
    cbFullTextSearch: TCheckBox;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListBox1Compare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListBox1Editing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure CategoryButtons1DrawText(Sender: TObject;
      const Button: TButtonItem; Canvas: TCanvas; Rect: TRect;
      State: TButtonDrawState);
    procedure CategoryButtons1ButtonClicked(Sender: TObject;
      const Button: TButtonItem);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FURL : String;
    FHelpIndex : Integer;
    procedure InitList(Keywords: TStrings);
    procedure SaveExpanded;
  public
    property URL : String read FURL;
    property SelectedHelpIndex : Integer read FHelpIndex;

    class function Execute(Keywords: TStrings; out SelectedIndex: Integer;
      out SelectedUrl: String): Boolean; static;
  end;

  THelpSelector = class(TInterfacedObject, IHelpSelector)
  protected
    function SelectKeyword(Keywords: TStrings) : Integer;
    function TableOfContents(Contents: TStrings): Integer;
  public
    constructor Create; 
    destructor destroy; override;
  end;

implementation

uses
  UrlMon, StrUtils, ComObj, ShellAPI, uCustomHelpMain, uCustomHelpIDEIntegration,
  Math, Registry;

{$R *.dfm}

const
  CAPTION_INDEXCHECK = 'Index search performed (check me to perform fulltext search next time)';
  CAPTION_FULLTEXTCHECK = 'Fulltext search performed (uncheck me to perform index search next time)';

type
  TCustomHelpButtonItem = class(TButtonItem)
  private
    FDesc: String;
    FURL: String;
    FIdx: Integer;
    FTO: TNamespaceTrimOption;
  public
    property URL: String read FURL write FURL;
    property Description: String read FDesc write FDesc;
    property HelpIndex: Integer read FIdx write FIdx;
    property TrimOption: TNamespaceTrimOption read FTO write FTO;
  end;

procedure TFormHelpSelector.CategoryButtons1ButtonClicked(Sender: TObject;
  const Button: TButtonItem);
begin
  if CategoryButtons1.SelectedItem<>Button then
    exit;

  if Button.Category.Collapsed then
    Button.Category.Collapsed:=False
  else
  if Button is TCustomHelpButtonItem then
  begin
    FHelpIndex:=TCustomHelpButtonItem(Button).HelpIndex;
    FURL:=TCustomHelp.EncodeURL(TCustomHelpButtonItem(Button).Caption,
                                TCustomHelpButtonItem(Button).Description,
                                TCustomHelpButtonItem(Button).URL,
                                '',
                                TCustomHelpButtonItem(Button).FTO);

    SaveExpanded;
    ModalResult:=mrOk;
  end;
end;

procedure TFormHelpSelector.CategoryButtons1DrawText(Sender: TObject;
  const Button: TButtonItem; Canvas: TCanvas; Rect: TRect;
  State: TButtonDrawState);
var
  drawrect : TRect;
  txt : String;
begin
  drawrect:=Rect;
  Canvas.Font.Style:=[fsBold];
  txt:=TCustomHelpButtonItem(Button).Caption;
  drawRect.Right:=drawrect.Left+(CategoryButtons1.Width div 3);
  Canvas.TextRect(drawrect, txt, [tfEndEllipsis]);

  drawrect:=Rect;
  Canvas.Font.Style:=[];
  txt:=TCustomHelpButtonItem(Button).Description;
  drawRect.Left:=drawrect.Left+(CategoryButtons1.Width div 3);
  Canvas.TextRect(drawrect, txt, [tfEndEllipsis]);
end;

class function TFormHelpSelector.Execute(Keywords: TStrings;
                                   out SelectedIndex: Integer;
                                   out SelectedUrl: String): Boolean;
var
  idx : Integer;
  c, d, u, g : String;
  o : Integer;
  selector : TFormHelpSelector;
begin
  Result:=False;
  SelectedIndex:=-1;
  SelectedUrl:='';
  Selector:=TFormHelpSelector.Create(nil);
  with Selector do
  begin
    InitList(Keywords);

    if (ShowModal=mrOk)  then
    begin
      Result:=True;
      SelectedIndex:=SelectedHelpIndex;
      SelectedUrl:=URL;
    end;
    Free;
  end;
end;

procedure TFormHelpSelector.InitList(Keywords: TStrings);
var
  cat : TButtonCategory;
  idx : integer;
  c, d, u, g: string;
  item : TCustomHelpButtonItem;
  Reg : TRegistry;
  CheckExpanded: Boolean;
  TrimOption: TNamespaceTrimOption;

  function GetCategoryFromLabel(ALabel: String): TButtonCategory;
  var
    i : Integer;
  begin
    Result:=nil;

    for i := 0 to CategoryButtons1.Categories.Count - 1 do
      if CategoryButtons1.Categories[i].Caption=ALabel then
      begin
        Result:=CategoryButtons1.Categories[i];
        break;
      end;

    if not Assigned(Result) then
    begin
      Result:=CategoryButtons1.Categories.Add;
      Result.Caption:=ALabel;
      Result.Color:=clActiveCaption;
      Result.TextColor:=clCaptionText;
      Result.Collapsed:=True;
      if CheckExpanded then
        Result.Collapsed:=Reg.ReadString(ALabel)='0';
    end;
  end;

begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    CheckExpanded:=Reg.OpenKey(REG_ROOT_KEY + EXPANDEDITEMS_SUB_KEY, true);

    GetCategoryFromLabel(GROUP_LABEL_DEFAULT);

    for idx := 0 to KeyWords.Count - 1 do
    begin
      if not AnsiStartsText('ms-help://',KeyWords[idx]) then
      begin
        if TCustomHelp.DecodeURL(Keywords[idx], c, d, u, g, TrimOption) then
        begin
          cat:=GetCategoryFromLabel(g);

          item:=TCustomHelpButtonItem.Create(cat.Items);
          item.Caption:=c;
          item.Description:=d;
          item.URL:=u;
          item.HelpIndex:=idx;
          item.TrimOption:=TrimOption;
        end;
      end;
    end;

    Reg.CloseKey;
  finally
    Reg.Free;
  end;
end;

constructor THelpSelector.Create;
begin
  
end;

destructor THelpSelector.destroy;
begin
  inherited;
end;

function THelpSelector.SelectKeyword(Keywords: TStrings): Integer;
var
  idx : integer;
  u : String;
begin
  Result:=-1;
  if TFormHelpSelector.Execute(Keywords, idx, u) then
    Result:=idx;
end;

function THelpSelector.TableOfContents(Contents: TStrings): Integer;
begin
  Result:=0;
end;

procedure TFormHelpSelector.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  GlobalCustomHelp.PerformFullTextSearch:=cbFullTextSearch.Checked;
end;

procedure TFormHelpSelector.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TFormHelpSelector.FormShow(Sender: TObject);
begin
  cbFullTextSearch.Checked:=GlobalCustomHelp.PerformFullTextSearch;
  if cbFullTextSearch.Checked then
    cbFullTextSearch.Caption:=CAPTION_FULLTEXTCHECK
  else
    cbFullTextSearch.Caption:=CAPTION_INDEXCHECK;

  Caption:=Caption+' (you searched for "'+GlobalCustomHelp.LastHelpCallKeyword+'")';
end;

procedure TFormHelpSelector.ListBox1Compare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare:=CompareValue(Integer(Item1.Data),Integer(Item2.Data));
end;

procedure TFormHelpSelector.ListBox1Editing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  AllowEdit:=False;
end;

procedure TFormHelpSelector.SaveExpanded;
var
  Reg : TRegistry;
  idx : Integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(REG_ROOT_KEY + EXPANDEDITEMS_SUB_KEY, true) then
    begin
      for idx := 0 to CategoryButtons1.Categories.Count - 1 do
        Reg.WriteString(CategoryButtons1.Categories[idx].Caption, ifthen(CategoryButtons1.Categories[idx].Collapsed,'0','1'));

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

end.
