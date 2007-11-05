//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitResEdEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ToolWin, ExtCtrls, Buttons,StrUtils, Grids,
  MPHexEditor, UnitResEdMain, unitEXIcon, pngimage, jpeg, GIFImage,
  ImgList;

type
  TFormResEdEditor = class(TForm)
    panRCDEdit: TPanel;
    PC: TPageControl;
    tsAT: TTabSheet;
    memATText: TMemo;
    ATTB: TToolBar;
    tbATSearch: TToolButton;
    gbATSearchRes: TGroupBox;
    lstATSearchRes: TListBox;
    panATSearchBottom: TPanel;
    pbATSearch: TProgressBar;
    btnATStopSearch: TBitBtn;
    btnATSearch: TBitBtn;
    edATSearch: TEdit;
    tsAP: TTabSheet;
    ATSB: TStatusBar;
    ToolButton1: TToolButton;
    tbATSave: TToolButton;
    imgAP: TImage;
    tsAH: TTabSheet;
    hexAH: TMPHexEditor;
    imlTB: TImageList;
    AHTB: TToolBar;
    btnAHSave: TToolButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tbATSearchClick(Sender: TObject);
    procedure btnATStopSearchClick(Sender: TObject);
    procedure btnATSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PCChange(Sender: TObject);
    procedure lstATSearchResClick(Sender: TObject);
    procedure memATTextClick(Sender: TObject);
    procedure memATTextKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tbATSaveClick(Sender: TObject);
    procedure btnAHSaveClick(Sender: TObject);
  private
    //Global-Vars
    Saved : Boolean;
  	//AsText-Vars
    ATSearching : Boolean;
    //AsPicture-Vars
    APPicture : TGraphic;

		//AsText-Functions
    procedure ATFreeSearchList();
    procedure ATRefreshRowCol();

  public
    Data    : TMemoryStream;
    ResType : UnitResEdMain.TResType;
  end;

  TFoundPos = record
    Line : Integer;
    Offset : Integer;
    WordStart,
    WordLen : Integer;
  end;
  PFoundPos = ^TFoundPos;

implementation


{$R *.dfm}

{$Region 'Formular-Kram'}
procedure TFormResEdEditor.FormCreate(Sender: TObject);
begin
  Data:=TMemoryStream.Create;
  ATRefreshRowCol;
  PCChange(Sender);
  Saved:=false;
end;

procedure TFormResEdEditor.FormShow(Sender: TObject);
begin
	case ResType of
		UnitResEdMain.rtICON,
    UnitResEdMain.rtBITMAP,
    UnitResEdMain.rtCURSOR,
    UnitResEdMain.rtJPEG,
    UnitResEdMain.rtGIF,
    UnitResEdMain.rtPNG :
    	Pc.ActivePageIndex:=1;
  end;
  PCChange(Sender);
end;

procedure TFormResEdEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	if (Saved) then
		ModalResult:=mrOk
	else
  	ModalResult:=mrCancel;
end;

procedure TFormResEdEditor.FormDestroy(Sender: TObject);
begin
	Data.Free;
end;

procedure TFormResEdEditor.PCChange(Sender: TObject);
begin
	Data.Seek(soFromBeginning,0);
  case PC.ActivePageIndex of
    0:  memATText.Lines.LoadFromStream(Data);
    1:
      begin
      	if (ResType in [UnitResEdMain.rtICON,
        								UnitResEdMain.rtBITMAP,
                        UnitResEdMain.rtCURSOR,
                        UnitResEdMain.rtJPEG,
                        UnitResEdMain.rtGIF,
                        UnitResEdMain.rtPNG]) then
        begin
          APPicture.Free;
          case ResType of
            UnitResEdMain.rtICON:
              APPicture:=TIcon.Create;
            UnitResEdMain.rtBITMAP:
              APPicture:=TBitmap.Create;
            UnitResEdMain.rtCURSOR:
              APPicture:=TExIconCursor.Create;
            UnitResEdMain.rtJPEG:
              APPicture:=TJPEGImage.Create;
            UnitResEdMain.rtGIF:
              APPicture:=TGIFImage.Create;
            UnitResEdMain.rtPNG:
              APPicture:=TPNGObject.Create;
          end;
          APPicture.Transparent:=true;
          APPicture.LoadFromStream(Data);
          imgAP.Picture.Graphic:=APPicture;
          imgAP.Invalidate;
        end;
      end;
    2:  hexAH.LoadFromStream(Data);

  end;
end;
{$EndRegion}

{$Region 'AsText'}
procedure TFormResEdEditor.tbATSearchClick(Sender: TObject);
begin
  gbATSearchRes.Visible:=(true xor gbATSearchRes.Visible);
  if (gbATSearchRes.Visible) then edATSearch.SetFocus;
  tbATSearch.Down:=gbATSearchRes.Visible;
  ATFreeSearchList;
end;

procedure TFormResEdEditor.btnATStopSearchClick(Sender: TObject);
begin
    ATSearching:=false;
end;

procedure TFormResEdEditor.ATFreeSearchList;
var
  i : Integer;
begin
  for i:=lstATSearchRes.Items.Count-1 downto 0 do
  begin
    Dispose(PFoundPos(lstATSearchRes.Items.objects[i]));
    lstATSearchRes.Items.Delete(i);
  end;
end;

procedure TFormResEdEditor.btnATSearchClick(Sender: TObject);
var
  i,
  posi,
  WordStart,
  WordEnd,
  CurOffset : integer;
  FP : PFoundPos;
  CurChar : ShortString;
  CurLine : String;
begin
  if (edATSearch.Text<>'') then
  begin
    ATSearching:=true;
    ATFreeSearchList;
    pbATSearch.Max:=memATText.Lines.Count-1;
    for i :=0 to memATText.Lines.Count-1 do
    begin
      pbATSearch.Position:=i;
      if (not ATSearching) then break;

      CurLine:=memATText.Lines[i];
      CurOffset:=0;
      posi:=AnsiPos(edATSearch.Text,CurLine);
      while posi>0 do
      begin
        New(FP);
        FP.Line:=i;
        FP.Offset:=posi+CurOffset;
        //Wort suchen, in dem der String vorkommt
        WordStart:=posi;
        WordEnd:=posi;
        CurChar:=Copy(CurLine,WordStart,1);
        while ((CurChar<>' ') and (WordStart>0)) do
        begin
          Dec(WordStart);
          CurChar:=Copy(CurLine,WordStart,1);
        end;
        if (CurChar=' ') then Inc(WordStart);
        
        CurChar:=Copy(CurLine,WordEnd,1);
        while ((CurChar<>' ') and (WordEnd<=Length(CurLine))) do
        begin
          Inc(WordEnd);
          CurChar:=Copy(CurLine,WordEnd,1);
        end;
        
        FP.WordStart:=WordStart+CurOffset-1;
        FP.WordLen:=WordEnd-WordStart;
        lstATSearchRes.Items.AddObject(Copy(CurLine,WordStart,WordEnd-WordStart),TObject(FP));
        CurOffset:=CurOffset+posi;
        CurLine:=Copy(CurLine,posi+1,Length(CurLine));
        posi:=AnsiPos(edATSearch.Text,CurLine);
        Application.ProcessMessages;
      end;
    end;
    ATSearching:=false;
    pbATSearch.Position:=0;
  end;
end;

procedure TFormResEdEditor.lstATSearchResClick(Sender: TObject);
var
  i : Integer;
  LI : integer;
begin
  i:=lstATSearchRes.ItemIndex;
  if (i<>-1) then
  begin
    LI:=SendMessage(memATText.Handle,EM_LINEINDEX,PFoundPos(lstATSearchRes.Items.Objects[i]).Line,0);
    memATText.SelStart:=LI+PFoundPos(lstATSearchRes.Items.Objects[i]).WordStart;
    memATText.SelLength:=PFoundPos(lstATSearchRes.Items.Objects[i]).WordLen;
    memATText.SetFocus;
  end;
end;

procedure TFormResEdEditor.ATRefreshRowCol;
begin
  ATSB.Panels[0].Text:='row: '+IntToStr(1+SendMessage(memATText.Handle,EM_LINEFROMCHAR,memATText.SelStart,0));
  ATSB.Panels[1].Text:='col: '+
    IntToStr(1+memATText.SelStart-SendMessage(memATText.Handle,EM_LINEINDEX,SendMessage(memATText.Handle,EM_LINEFROMCHAR,memATText.SelStart,0),0));
end;

procedure TFormResEdEditor.memATTextClick(Sender: TObject);
begin
  ATRefreshRowCol;
end;

procedure TFormResEdEditor.memATTextKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  ATRefreshRowCol;
end;

procedure TFormResEdEditor.tbATSaveClick(Sender: TObject);
begin
	Data.Clear;
  memATText.Lines.SaveToStream(Data);
  Saved:=true;
  Self.Close;
end;

{$EndRegion}

{$Region 'AsHex'}
procedure TFormResEdEditor.btnAHSaveClick(Sender: TObject);
begin
	Data.Clear;
  hexAH.SaveToStream(Data);
  Saved:=true;
  Self.Close;
end;
{$EndRegion}

end.
