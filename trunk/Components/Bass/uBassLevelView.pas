unit uBassLevelView;

interface

uses
  Bass,
  Classes,
  Controls,
  Windows,
  SysUtils,
  Graphics;

type
  TCustomBassWaveView = class(TGraphicControl)
  private
    FBassChannel: Cardinal;
    FBackColor: TColor;
  protected
    property BassChannel : Cardinal read FBassChannel write FBassChannel;
    property BackColor : TColor read FBackColor write FBackColor default clBlack;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;
  end;

implementation

{ TCustomBassWaveView }

constructor TCustomBassWaveView.Create(AOwner: TComponent);
begin
  inherited;

  FBassChannel := 0;
  FBackColor := clBlack;
end;

destructor TCustomBassWaveView.Destroy;
begin

  inherited;
end;

procedure TCustomBassWaveView.Paint;
var
  bmp : TBitmap;
begin
  inherited;

  if FBassChannel = 0 then
    exit;

  bmp := TBitmap.Create;
  try

    with bmp.Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := FBackColor;

    end;

  finally
    bmp.Free;
  end;
end;

end.
