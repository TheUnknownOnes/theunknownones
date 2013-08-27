unit uFormZintTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TformZintTest }

  TformZintTest = class(TForm)
    btnSaveSVG: TButton;
    comType: TComboBox;
    edData: TEdit;
    imgResult: TImage;
    lblError: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    SaveDialog1: TSaveDialog;
    procedure btnSaveSVGClick(Sender: TObject);
    procedure comTypeChange(Sender: TObject);
    procedure edDataChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure GenBarcode;
    function GetSymbology : Integer;
  public
    { public declarations }
  end;

var
  formZintTest: TformZintTest;

implementation

{$R *.lfm}

uses zint, zint_lmf, zint_render_lmf, zint_render_bmp, TADrawerSVG,
  zint_render_tadrawer, zint_helper, TADrawUtils;

type
  BCTypeEntry = record
    N: string;
    T: integer;
  end;
const
  SupportedTypes: array[0..42] of
    BCTypeEntry = (
    (N: '2 of 5 Matrix'; T: BARCODE_C25MATRIX),
    (N: '2 of 5 Industrial'; T: BARCODE_C25IND),
    (N: '2 of 5 Interleaved'; T: BARCODE_C25INTER),
    (N: '2 of 5 IATA'; T: BARCODE_C25IATA),
    (N: '2 of 5 Datalogic'; T: BARCODE_C25LOGIC),
    (N: 'Deutsche Post Leitcode'; T: BARCODE_DPLEIT),
    (N: 'Deutsche Post Identcode'; T: BARCODE_DPIDENT),
    (N: 'EAN 128'; T: BARCODE_EAN128),
    (N: 'Code 39'; T: BARCODE_CODE39),
    (N: 'Pharmazentral PZN'; T: BARCODE_PZN),
    (N: 'Extended Code 39'; T: BARCODE_EXCODE39),
    (N: 'Codabar'; T: BARCODE_CODABAR),
    (N: 'Code 93'; T: BARCODE_CODE93),
    (N: 'Logmars'; T: BARCODE_LOGMARS),
    (N: 'Code 128'; T: BARCODE_CODE128),
    (N: 'Code 128 B'; T: BARCODE_CODE128B),
    (N: 'NVE 18'; T: BARCODE_NVE18),
    (N: 'Code 11'; T: BARCODE_CODE11),
    (N: 'Pharmacode'; T: BARCODE_PHARMA),
    (N: 'ITF 14'; T: BARCODE_ITF14),
    (N: 'Australian Post Code'; T: BARCODE_AUSPOST),
    (N: 'Australian Post Reply'; T: BARCODE_AUSREPLY),
    (N: 'Australian Post Route'; T: BARCODE_AUSROUTE),
    (N: 'Australian Post Redirect'; T: BARCODE_AUSREDIRECT),
    (N: 'Pharma 2-track'; T: BARCODE_PHARMA_TWO),
    (N: 'Code 32 / Italian Pharmacode'; T: BARCODE_CODE32),
    (N: 'EAN 14'; T: BARCODE_EAN14),
    (N: 'Azrune'; T: BARCODE_AZRUNE),
    (N: 'HIBC 128'; T: BARCODE_HIBC_128),
    (N: 'HIBC 39'; T: BARCODE_HIBC_39),
    (N: 'HIBC Datamatrix'; T: BARCODE_HIBC_DM),
    (N: 'HIBC Aztec'; T: BARCODE_HIBC_AZTEC),
    (N: 'Datamatrix'; T: BARCODE_DATAMATRIX),
    (N: 'Maxicode'; T: BARCODE_MAXICODE),
    (N: 'Aztec'; T: BARCODE_AZTEC),
    (N: 'Code 16k'; T: BARCODE_CODE16K),
    (N: 'Code 49'; T: BARCODE_CODE49),
    (N: 'PDF417'; T: BARCODE_PDF417),
    (N: 'PDF417 Truncated'; T: BARCODE_PDF417TRUNC),
    (N: 'MicroPDF417'; T: BARCODE_MICROPDF417),
    (N: 'QR-Code'; T: BARCODE_QRCODE),
    (N: 'MicroQR-Code'; T: BARCODE_MICROQR),
    (N: 'Gridmatrix'; T: BARCODE_GRIDMATRIX)
    );

{ TformZintTest }

procedure TformZintTest.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := Low(SupportedTypes) to High(SupportedTypes) do
    comType.Items.Add(SupportedTypes[i].N);

  comType.ItemIndex := 0;
end;

procedure TformZintTest.FormShow(Sender: TObject);
begin
  GenBarcode;
end;

procedure TformZintTest.comTypeChange(Sender: TObject);
begin
  GenBarcode;
end;

procedure TformZintTest.btnSaveSVGClick(Sender: TObject);
var
  symbol: TZintSymbol;
  rt: TZintTADrawerRenderTarget;
  svgd: TSVGDrawer;
  fs: TFileStream;
begin
  if SaveDialog1.Execute then
  begin
    symbol := TZintSymbol.Create;
    symbol.symbology := GetSymbology;
    symbol.show_hrt := 1;
    fs := TFileStream.Create(SaveDialog1.FileName, fmCreate);

    try
      symbol.Encode(edData.Text, True);

      svgd := TSVGDrawer.Create(fs, True);
      rt := TZintTADrawerRenderTarget.Create(svgd, svgd, 10000, 10000); //the higher the values ... the more accurate the drawing (e.g. maxicode)
      rt.Transparent:=True;

      rt.RenderAdjustMode := ramScaleBarcode;
      try
        symbol.Render(rt);
      finally
        rt.Free;
      end;

    finally
      fs.Free;
      symbol.Free;
    end;
  end;
end;

procedure TformZintTest.edDataChange(Sender: TObject);
begin
  GenBarcode;
end;

{$DEFINE RenderBMP}

procedure TformZintTest.GenBarcode;
var
  symbol: TZintSymbol;
  {$IFDEF RenderBMP}
  rt: TZintBMPRenderTarget;
  img: TBitmap;
  {$ELSE}
  rt: TZintLMFRenderTarget;
  img: TlmfImage;
  {$ENDIF}
begin

  imgResult.Picture.Graphic := nil;
  lblError.Caption := '';

  symbol := TZintSymbol.Create;
  symbol.symbology := GetSymbology;
  symbol.input_mode := UNICODE_MODE;
  symbol.show_hrt := 1;
  try
    symbol.Encode(edData.Text, True);
    {$IFDEF RenderBMP}
    img := TBitmap.Create;
    img.PixelFormat := pf24bit;
    img.SetSize(imgResult.Width, imgResult.Height);
    img.Canvas.Brush.Color := clWhite;
    img.Canvas.Brush.Style := bsSolid;
    img.Canvas.FillRect(img.Canvas.ClipRect);
    rt := TZintBMPRenderTarget.Create(img);
    {$ELSE}
    img := TlmfImage.Create;
    img.Width := imgResult.Width;
    img.Height := imgResult.Height;
    rt := TZintLMFRenderTarget.Create(img);
    {$ENDIF}
    rt.RenderAdjustMode := ramScaleBarcode;
    try
      symbol.Render(rt);
      imgResult.Picture.Graphic := img;
    finally
      img.Free;
      rt.Free;
    end;
  except
    on E: Exception do
      lblError.Caption := e.Message;
  end;

  symbol.Free;
end;

function TformZintTest.GetSymbology: Integer;
var
  i: integer;
begin
  for i := Low(SupportedTypes) to High(SupportedTypes) do
  begin
    if comType.Text = SupportedTypes[i].N then
      Result := SupportedTypes[i].T;
  end;

end;

end.
