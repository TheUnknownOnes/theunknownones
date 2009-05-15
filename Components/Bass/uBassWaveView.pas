unit uBassWaveView;

interface

uses
  Classes,
  Controls,
  Windows,
  Graphics,
  uBassCommon;

type
  TCustomBassWaveView = class(TGraphicControl)
  private
    FWaveData: TBassWaveDataList;
    FBassChannel: Cardinal;
    FLineColor: TColor;
    FBackColor: TColor;
    FChannel: Cardinal;
    procedure SetBassChannel(const Value: Cardinal);
  protected
    property WaveData : TBassWaveDataList read FWaveData;
    property BassChannel : Cardinal read FBassChannel write SetBassChannel;
    property BackColor : TColor read FBackColor write FBackColor default clBlack;
    property LineColor : TColor read FLineColor write FLineColor default clLime;
    property Channel : Cardinal read FChannel write FChannel default 1;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;
  end;

  TBassWaveView = class(TCustomBassWaveView)
  public
    property WaveData;
    property BassChannel;
  published
    property Align;
    property BackColor;
    property LineColor;
    property Channel;
  end;

implementation

{ TCustomBassWaveView }

constructor TCustomBassWaveView.Create(AOwner: TComponent);
begin
  inherited;

  FChannel := 1;
  FWaveData := nil;

  FBackColor := clBlack;
  FLineColor := clLime;
end;

destructor TCustomBassWaveView.Destroy;
begin
  Channel := 0;

  inherited;
end;

procedure TCustomBassWaveView.SetBassChannel(const Value: Cardinal);
begin
  FBassChannel := Value;
  
  if Value > 0 then
  begin
    FWaveData := TBassWaveDataList.Create(FBassChannel);
  end
  else if Assigned(FWaveData) then
  begin
    FWaveData.Free;
    FWaveData := nil;
  end;
end;

procedure TCustomBassWaveView.Paint;
var
  bmp : TBitmap;
  List : TList;
  Data : Pointer;
  idx,
  midY : Integer;
  X,Y : Integer;
begin
  inherited;

  if not Assigned(FWaveData) then
    exit;  

  bmp := TBitmap.Create;
  bmp.Width := Width;
  bmp.Height := Height;
  bmp.PixelFormat := pf32bit;

  with bmp.Canvas do
  begin
    Brush.Color := FBackColor;
    Brush.Style := bsSolid;
    FillRect(ClipRect);

    pen.Color := FLineColor;
    pen.Style := psSolid;

    midY := bmp.Height div 2;

    FWaveData.DeleteExceptLast(bmp.Width);

    List := FWaveData.LockList;
    try
      MoveTo(0, midY);

      for idx := 0 to List.Count - 1 do
      begin
        x := idx;
        Data := List[idx];

        case FWaveData.SampleFormat of
          bsf16Bit:
          begin
            Inc(PSmallInt(Data), FChannel - 1);
            Y := Round(PSmallInt(Data)^ * midY + midY);
          end;

          bsf32Bit:
          begin
            Inc(PSingle(Data), FChannel - 1);
            Y := Round(PSingle(Data)^ * midY + midY);
          end;

          else
            Y := 0;
        end;


        LineTo(X, Y);
      end;
    finally
      FWaveData.UnlockList;
    end;
  end;

  Canvas.Draw(0, 0, bmp);

  bmp.Free;
end;

end.
