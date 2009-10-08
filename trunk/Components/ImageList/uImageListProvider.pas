//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uImageListProvider;

interface

{$R 'ImagelistProvider.res'}

uses
  CommCtrl,
  ImgList,
  Windows,
  Graphics,
  Classes,
  uBaseImagelist,
  Dialogs;

type
  TImageListProvider = class(TCustomImageList)
  private
    FImageList: TBaseImagelist;
    FChangeLnk : TImageListChangeLink;
    FBackColor: TColor;
    FSupportCommCtrls: Boolean;
    FStates: TilDrawStates;

    procedure SetImagelist(const Value: TBaseImagelist);

    procedure OnChange(AImagelist : TBaseImagelist; AIndex : Integer);

    procedure SyncToImageList;
    procedure SetBackColor(const Value: TColor);
    procedure SetSupportCommCtrls(const Value: Boolean);
    procedure SetState(const Value: TilDrawStates);

  protected
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer;
      Style: Cardinal; Enabled: Boolean = True); override;
      
  public
    constructor Create(AOnwer : TComponent); override;
    destructor Destroy(); override;

    procedure GetImageSize(AIndex : Integer; out AWidth, AHeight : Integer);
  published
    property Source : TBaseImagelist read FImageList write SetImagelist;

    property SupportCommCtrls : Boolean read FSupportCommCtrls write SetSupportCommCtrls default false;
    property CommCtrlsBackColor : TColor read FBackColor write SetBackColor default clWhite;
    property CommCtrlsStyle : TilDrawStates read FStates write SetState default [ildEnabled];
  end;

implementation

{ TImageListProvider }

constructor TImageListProvider.Create(AOnwer: TComponent);
begin
  inherited;

  FSupportCommCtrls:=false;
  FBackColor:=clWhite;
  FStates:=[ildEnabled];

  FChangeLnk:=TImageListChangeLink.Create;
  FChangeLnk.OnChange:=OnChange;
end;

destructor TImageListProvider.Destroy;
begin
  FChangeLnk.Free;

  inherited;
end;

procedure TImageListProvider.DoDraw(Index: Integer; Canvas: TCanvas; X,
  Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  States : TilDrawStates;
begin
  if Enabled then
    States:=[ildEnabled]
  else
    States:=[ildDisabled];

  if (ILD_SELECTED and Style)=ILD_SELECTED then
    Include(States, ildSelected);

  if (ILD_FOCUS and Style)=ILD_FOCUS then
    Include(States, ildFocused);

  if Assigned(FImageList) then
    FImageList.Draw(Index, Canvas, Point(X, Y), States);

end;


procedure TImageListProvider.SyncToImageList;
var
  BMP : TBitmap;
  ImgWidth,
  ImgHeight,
  ListWidth,
  ListHeight : Integer;
  idx : Integer;
begin
  Clear;

  if not Assigned(FImageList) or (FImageList.Count=0) then
    exit;

  FImageList.GetImageSize(0, ListWidth, ListHeight);

  Width:=ListWidth;
  Height:=ListHeight;

  BMP:=TBitmap.Create;
  try
    BMP.Width:=ListWidth;
    BMP.Height:=ListHeight;

    if FSupportCommCtrls then
      BMP.PixelFormat:=pf32bit
    else
      BMP.PixelFormat:=pf1bit;

    for idx := 0 to FImageList.Count - 1 do
    begin
      if not SupportCommCtrls then
        AddMasked(BMP, clFuchsia)
      else
      begin
        BMP.Canvas.Brush.Color:=FBackColor;
        BMP.Canvas.FillRect(BMP.Canvas.ClipRect);

        FImageList.GetImageSize(idx, ImgWidth, ImgHeight);

        FImageList.Draw(idx,
                        BMP.Canvas.Handle,
                        Point(0,0),
                        FStates);
                        
        AddMasked(BMP, FBackColor);
      end;
    end;
  finally
    BMP.Free;
  end;
end;


procedure TImageListProvider.GetImageSize(AIndex: Integer; out AWidth,
  AHeight: Integer);
begin
  if Assigned(FImageList) then
    FImageList.GetImageSize(AIndex, AWidth, AHeight)
  else
  begin
    AWidth:=-1;
    AHeight:=-1;
  end;
end;

procedure TImageListProvider.OnChange(AImagelist: TBaseImagelist;
  AIndex: Integer);
begin
  SyncToImageList;
end;

procedure TImageListProvider.SetBackColor(const Value: TColor);
begin
  FBackColor := Value;
  SyncToImageList;
end;

procedure TImageListProvider.SetImagelist(const Value: TBaseImagelist);
begin
  FImageList := Value;
  
  if Assigned(FImageList) then
  begin
    SyncToImageList;
    FImageList.RegisterChangeLink(FChangeLnk);
  end;
end;

procedure TImageListProvider.SetState(const Value: TilDrawStates);
begin
  FStates := Value;
  SyncToImageList;
end;

procedure TImageListProvider.SetSupportCommCtrls(const Value: Boolean);
begin
  FSupportCommCtrls := Value;
  SyncToImageList;
end;

end.
