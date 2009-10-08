unit uBassWaveView;

interface

uses
  Classes,
  Controls,
  Windows,
  Graphics,
  Bass,
  SysUtils,
  uBassCommon;

type
  TCustomBassWaveViewLine = class(TCollectionItem)
  private
    FColor: TColor;
    FChannel: Cardinal;
  protected
    property Channel : Cardinal read FChannel write FChannel default 1;
    property Color : TColor read FColor write FColor default clLime;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  end;

  TCustomBassWaveView = class(TGraphicControl)
  private
    FBassChannel: Cardinal;
    FBackColor: TColor;
    FZoom: Single;
    FLines: TCollection;

    FOldData : Pointer;
    FOldDataSize : Integer;

    procedure SetLines(const Value: TCollection);

    procedure SetOldDataSize(ANewSize : Integer);
  protected
    property BassChannel : Cardinal read FBassChannel write FBassChannel;
    property BackColor : TColor read FBackColor write FBackColor default clBlack;
    property Zoom : Single read FZoom write FZoom;
    property Lines : TCollection read FLines write SetLines;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;
  end;

  TBassWaveViewLine = class(TCustomBassWaveViewLine)
  published
    property Channel;
    property Color;
  end;

  TBassWaveView = class(TCustomBassWaveView)
  public
    constructor Create(AOwner: TComponent); override;

    property BassChannel;
  published
    property Align;
    property BackColor;
    property Zoom;
    property Lines;
  end;

implementation

{ TCustomBassWaveView }

constructor TCustomBassWaveView.Create(AOwner: TComponent);
begin
  inherited;

  FOldData := nil;
  FOldDataSize := 0;

  FLines := nil;

  FBassChannel := 0;
  FZoom := 1;

  FBackColor := clBlack;
end;

destructor TCustomBassWaveView.Destroy;
begin
  if Assigned(FLines) then
    FLines.Free;

  FreeMem(FOldData, FOldDataSize);

  inherited;
end;

procedure TCustomBassWaveView.Paint;
var
  bmp : TBitmap;
  Data: PSingle;
  idx,
  midY,
  ChannelCnt : Cardinal;
  X,Y : Integer;
  Buffer : Pointer;
  BufferSize : Integer;
  ChunksWished : Integer;
  ChunkSize : Integer;
  ChunksReceived : Integer;
  LineIndex : Integer;
  Line : TCustomBassWaveViewLine;
  OldChunksAvailable : Integer;
  TempBuffer : Pointer;
begin
  inherited;

  if FBassChannel = 0 then
    exit;

  if not Assigned(FLines) then
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

    ChunksWished := Trunc(bmp.Width * FZoom);

    pen.Style := psSolid;

    midY := bmp.Height div 2;

    ChannelCnt := Bass_GetChannelsFromChannel(FBassChannel);
    ChunkSize := SizeOf(Single) * ChannelCnt;

    GetMem(Buffer, ChunkSize * ChunksWished);
    try
      BufferSize := BASS_ChannelGetData(FBassChannel, Buffer, BASS_DATA_FLOAT or (ChunksWished * ChunkSize));

      ChunksReceived := BufferSize div ChunkSize;

      if ChunksReceived < ChunksWished then
      begin
        OldChunksAvailable := (FOldDataSize div ChunkSize);

        if OldChunksAvailable >= ChunksWished - ChunksReceived then
        begin
          OldChunksAvailable := ChunksWished - ChunksReceived;

          GetMem(TempBuffer, BufferSize);

          CopyMemory(TempBuffer, Buffer, BufferSize);

          CopyMemory(Buffer, FOldData, OldChunksAvailable * ChunkSize);

          CopyMemory(Pointer(Integer(Buffer) + OldChunksAvailable * ChunkSize), TempBuffer, BufferSize);

          FreeMem(TempBuffer, BufferSize);

          Inc(ChunksReceived, OldChunksAvailable);
        end;
      end;

      SetOldDataSize(BufferSize);

      if Assigned(FOldData) then
        CopyMemory(FOldData, Buffer, BufferSize);

      if ChunksReceived > 0 then
      begin
        for LineIndex := 0 to FLines.Count - 1 do
        begin

          Line := TCustomBassWaveViewLine(FLines.Items[LineIndex]);

          Pen.Color := Line.Color;

          MoveTo(0, midY);
          Data := Buffer;
          for idx := 0 to ChunksReceived - 1 do
          begin
            x := Trunc((bmp.Width / ChunksReceived) * idx);

            Inc(Data, Line.Channel - 1);

            Y := Trunc(Data^ * midY + midY);

            LineTo(X, Y);

            Inc(Data, ChannelCnt - Line.Channel + 1);
          end;
        end;
      end;
    finally
      FreeMem(Buffer, ChunkSize * ChunksWished);
    end;
  end;

  Canvas.Draw(0, 0, bmp);

  bmp.Free;
end;

procedure TCustomBassWaveView.SetLines(const Value: TCollection);
begin
  if Assigned(FLines) then
    FLines.Assign(Value);
end;

procedure TCustomBassWaveView.SetOldDataSize(ANewSize: Integer);
begin
  if ANewSize <> FOldDataSize then
  begin
    if Assigned(FOldData) then
      FreeMem(FOldData, FOldDataSize);


    if ANewSize > 0 then
    begin
      FOldDataSize := ANewSize;
      GetMem(FOldData, FOldDataSize);
    end
    else
    begin
      FOldDataSize := 0;
      FOldData := nil;
    end;
  end;
end;

{ TCustomBassWaveViewLine }

constructor TCustomBassWaveViewLine.Create(Collection: TCollection);
begin
  inherited;

  FColor := clLime;
  FChannel := 1;
end;

destructor TCustomBassWaveViewLine.Destroy;
begin

  inherited;
end;

{ TBassWaveView }

constructor TBassWaveView.Create(AOwner: TComponent);
begin
  inherited;

  FLines := TCollection.Create(TBassWaveViewLine);
end;

end.
