unit uSuggestEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, Themes, Types, uLevenshtein, Math, ComCtrls, uKoelnerPhonetik,
  uSoundEx, uMetaphone, uDoubleMetaphone, uFuzzyMatching, Buttons;

type
  TSuggestMethod = (smDamerauLevenshtein,
                    smKoelnerPhonetik,
                    smSoundEx,
                    smMetaphone,
                    smDoubleMetaphone,
                    smFuzzyLogic);

  TSuggestEdit = class;

  TFormEditSuggest = class;

  TThreadEditSuggest = class(TThread)
  private
    FFormEditSuggest: TFormEditSuggest;
    FResults : TStringList;
  protected
    procedure Execute; override;
    procedure UpdateListView;
  public
    constructor Create(const AFormEditSuggest: TFormEditSuggest); reintroduce;
    destructor Destroy; override;
  end;

  TFormEditSuggest = class(TForm)
    ListView1: TListView;
    procedure ListView1Compare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FEdit : TSuggestEdit;
    FSuggestThread : TThreadEditSuggest;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMPrint(var Message: TMessage); message WM_PRINT;
    procedure NCPaint(DC: HDC);
  public
    procedure ActivateSuggestBox;
    procedure DeactivateSuggestBox;
    constructor Create(AEdit: TSuggestEdit); reintroduce;
    destructor Destroy; override;
  end;

  TSuggestEdit = class(TEdit)
  private
    FFormEditSuggest : TFormEditSuggest;
    FThreshold : Integer;
    FIgnoreCase: Boolean;
    FWordList : TStringList;
    FTestWord : String;
    FCS : TRTLCriticalSection;
    FDisplaySimilarity: Boolean;
    FSuggestBoxHeight: Integer;
    FSuggestMethod: TSuggestMethod;
    procedure SetIgnoreCase(const Value: Boolean);
    procedure SetThreshold(const Value: Integer);
    function GetWordList: TStrings;
    procedure SetWordList(const Value: TStrings);
    function GetTestWord: String;
    procedure SetSuggestMethod(const Value: TSuggestMethod);
  protected
    procedure Change; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property TestWord: String read GetTestWord;
  published
    property Threshold: Integer read FThreshold write SetThreshold;
    property IgnoreCase: Boolean read FIgnoreCase write SetIgnoreCase;
    property WordList: TStrings read GetWordList write SetWordList;
    property DisplaySimilarity: Boolean read FDisplaySimilarity write FDisplaySimilarity;
    property SuggestBoxHeight: Integer read FSuggestBoxHeight write FSuggestBoxHeight;
    property SuggestMethod: TSuggestMethod read FSuggestMethod write SetSuggestMethod;
  end;

  procedure Register;

implementation

{$R *.dfm}

{ TThreadEditSuggest }

constructor TThreadEditSuggest.Create(const AFormEditSuggest: TFormEditSuggest);
var
  I: Integer;
begin
  inherited Create(True);

  FreeOnTerminate:=True;

  FFormEditSuggest:= AFormEditSuggest;
  FResults:=TStringList.Create;
end;

destructor TThreadEditSuggest.Destroy;
begin
  FResults.Free;

  inherited;
end;

procedure TThreadEditSuggest.Execute;
var
  i : Integer;
  Ratio : Extended;
  myTestWord, tw, checkWord: String;
  myWordList : TStringList;
  myIgnoreCase : Boolean;
  myTheshold : Integer;
begin
  myTestWord:=#0;
  myWordList:=TStringList.Create;
  while not Terminated do
  begin
    sleep(100);
    tw:=FFormEditSuggest.FEdit.TestWord;
    if myTestWord<>tw then
    begin
      myTestWord:=tw;
      EnterCriticalSection(FFormEditSuggest.FEdit.FCS);
      myWordList.Text:=FFormEditSuggest.FEdit.FWordList.Text;
      LeaveCriticalSection(FFormEditSuggest.FEdit.FCS);
      myIgnoreCase:=FFormEditSuggest.FEdit.IgnoreCase;
      myTheshold:=FFormEditSuggest.FEdit.Threshold;

      FResults.Clear;

      for i := 0 to myWordList.Count - 1 do
      begin
        checkWord:=trim(myWordList[i]);

        case FFormEditSuggest.FEdit.FSuggestMethod of
          smDamerauLevenshtein: Ratio:=Round(100 * uLevenshtein.StringSimilarityRatio(myTestWord, checkWord, myIgnoreCase));
          smFuzzyLogic: Ratio:=Round(100 * uFuzzyMatching.StringSimilarityRatio(myTestWord, checkWord));
          smKoelnerPhonetik:
             begin
               if AnsiSameText(myTestWord, checkWord) then
                 Ratio:=100
               else
                 Ratio:=IfThen(uKoelnerPhonetik.SoundsSimilar(myTestWord, checkWord), 99, 0);
             end;
          smSoundEx:
             begin
               if AnsiSameText(myTestWord, checkWord) then
                 Ratio:=100
               else
                 Ratio:=IfThen(uSoundEx.SoundsSimilar(myTestWord, checkWord), 99, 0);
             end;
          smMetaphone:
             begin
               if AnsiSameText(myTestWord, checkWord) then
                 Ratio:=100
               else
                 Ratio:=IfThen(uMetaphone.SoundsSimilar(myTestWord, checkWord), 99, 0);
             end;
          smDoubleMetaphone:
             begin
               if AnsiSameText(myTestWord, checkWord) then
                 Ratio:=100
               else
                 Ratio:=IfThen(uDoubleMetaphone.SoundsSimilar(myTestWord, checkWord), 99, 0);
             end;
        end;

        if (Ratio>myTheshold) then
        begin
          FResults.AddObject(checkWord, TObject(Round(Ratio)));
        end;
      end;

      Synchronize(Self.UpdateListView);
    end;
  end;
  myWordList.Free;
end;

procedure TThreadEditSuggest.UpdateListView;
var
  i: Integer;
  countRlt100 : Integer; //count of Items where Ratio<100
  countReq100 : Integer; //count of Items where Ratio=100 ... should be 1
begin
  countRlt100:=0;
  countReq100:=0;
  FFormEditSuggest.ListView1.Clear;
  for i := 0 to FResults.Count - 1 do
  begin
    with FFormEditSuggest.ListView1.Items.Add do
    begin
      Caption:=FResults[i];
      SubItems.Add(IntToStr(Integer(FResults.Objects[i]))+'%');
      Data:=FResults.Objects[i];

      if Integer(FResults.Objects[i])<100 then
        inc(countRlt100)
      else
        inc(countReq100);

      if (not FFormEditSuggest.Visible) and
         ((countRlt100>0) or (countReq100>1)) then
      begin
        FFormEditSuggest.Show;
        FFormEditSuggest.FEdit.SetFocus;
      end;
    end;
  end;

  if (countRlt100=0) and (countReq100=1) then
    FFormEditSuggest.DeactivateSuggestBox;
end;

{ TFormEditSuggest }

procedure TFormEditSuggest.ActivateSuggestBox;
var
  pt : TPoint;
  s : String;
begin
  if not Visible then
  begin
    pt.X:=FEdit.Left;
    pt.Y:=FEdit.Top + FEdit.Height;
    pt:=FEdit.Parent.ClientToScreen(pt);
    Left:=pt.X;
    Top:=pt.Y;
    Width:=FEdit.Width;
    Height:=FEdit.SuggestBoxHeight;
    DoubleBuffered:=True;
    if FEdit.FDisplaySimilarity then
      ListView1.Columns[1].Width:=50
    else
      ListView1.Columns[1].Width:=0;

    ListView1.Realign;
  end;

  FSuggestThread.Resume;
end;

constructor TFormEditSuggest.Create(AEdit: TSuggestEdit);
begin
  inherited Create(nil);
  FEdit:=AEdit;

  FSuggestThread:=TThreadEditSuggest.Create(self);
end;

procedure TFormEditSuggest.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP;//or WS_BORDER;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    if NewStyleControls then
      ExStyle := WS_EX_TOOLWINDOW;
//       CS_DROPSHADOW requires Windows XP or above
    if CheckWin32Version(5, 1) then
      WindowClass.Style := WindowClass.style or CS_DROPSHADOW;
    if NewStyleControls then ExStyle := WS_EX_TOOLWINDOW;
    AddBiDiModeExStyle(ExStyle);
  end;
end;

procedure TFormEditSuggest.DeactivateSuggestBox;
begin
  if not Focused then
    Hide;
end;

destructor TFormEditSuggest.Destroy;
var
  i : Integer;
begin
  FSuggestThread.Terminate;
  WaitForSingleObject(FSuggestThread.Handle, 10000);
  inherited;
end;

procedure TFormEditSuggest.ListView1Compare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare:=CompareValue(Integer(Item2.Data), Integer(Item1.Data));
end;

procedure TFormEditSuggest.ListView1DblClick(Sender: TObject);
begin
  if ListView1.Selected<>nil then
  begin
    FEdit.Text:=ListView1.Selected.Caption;
    FEdit.SetFocus;
    FEdit.SelectAll;
    Hide;
  end;
end;

procedure TFormEditSuggest.ListView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then
    ListView1DblClick(Sender);
end;

procedure TFormEditSuggest.WMPrint(var Message: TMessage);
begin
  PaintTo(Message.WParam, 0, 0);
  NCPaint(Message.WParam);
end;

procedure TFormEditSuggest.NCPaint(DC: HDC);
var
  R: TRect;
  Details: TThemedElementDetails;
begin
  R := Types.Rect(0, 0, Width, Height);
  if not ThemeServices.ThemesEnabled then
    Windows.DrawEdge(DC, R, BDR_RAISEDOUTER, BF_RECT)
  else
  begin
    Details := ThemeServices.GetElementDetails(twWindowRoot);
    ThemeServices.DrawEdge(DC, Details, R, BDR_RAISEDOUTER, BF_RECT);
  end;
end;

{ TSuggestEdit }


procedure TSuggestEdit.Change;
begin
  FTestWord:=Trim(Text);
  if not (csDesigning in ComponentState) then
    FFormEditSuggest.ActivateSuggestBox;
  inherited;
end;

constructor TSuggestEdit.Create(AOwner: TComponent);
begin
  inherited;
  FFormEditSuggest:=TFormEditSuggest.Create(self);
  FWordList:=TStringList.Create;

  FSuggestBoxHeight:=100;
  FThreshold:=80;
  FIgnoreCase:=True;
  FDisplaySimilarity:=False;
  FSuggestMethod:=smDamerauLevenshtein;

  InitializeCriticalSection(FCS);
end;

destructor TSuggestEdit.Destroy;
begin
  FFormEditSuggest.Free;
  FWordList.Free;
  DeleteCriticalSection(FCS);
  inherited;
end;

procedure TSuggestEdit.DoExit;
begin
  inherited;
  FFormEditSuggest.DeactivateSuggestBox
end;

function TSuggestEdit.GetTestWord: String;
begin
  EnterCriticalSection(FCS);
  Result:=FTestWord;
  LeaveCriticalSection(FCS);
end;

function TSuggestEdit.GetWordList: TStrings;
begin
  Result:=FWordList;
end;

procedure TSuggestEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key=VK_DOWN then
  begin
    if FFormEditSuggest.Visible and (FFormEditSuggest.ListView1.Items.Count>0) then
    begin
      FFormEditSuggest.SetFocus;
      FFormEditSuggest.ListView1.SetFocus;
      FFormEditSuggest.ListView1.ItemIndex:=0;
      FFormEditSuggest.ListView1.Selected:=FFormEditSuggest.ListView1.Items[0];
      FFormEditSuggest.ListView1.ItemFocused:=FFormEditSuggest.ListView1.Selected;
    end;
  end
  else
  if KEY=VK_ESCAPE then
  begin
    FFormEditSuggest.DeactivateSuggestBox;
  end;
end;

procedure TSuggestEdit.SetIgnoreCase(const Value: Boolean);
begin
  FIgnoreCase:=Value;
end;

procedure TSuggestEdit.SetSuggestMethod(const Value: TSuggestMethod);
begin
  FSuggestMethod:=Value;
  Change;
end;

procedure TSuggestEdit.SetThreshold(const Value: Integer);
begin
  FThreshold:=Value;
end;

procedure TSuggestEdit.SetWordList(const Value: TStrings);
begin
  EnterCriticalSection(FCS);
  FWordList.Assign(Value);
  LeaveCriticalSection(FCS);
end;

procedure Register;
begin
  RegisterComponents('TUO',[TSuggestEdit]);
end;

end.
