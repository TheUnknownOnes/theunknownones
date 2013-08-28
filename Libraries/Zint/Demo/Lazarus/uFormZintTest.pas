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
  public
    { public declarations }
  end;

var
  formZintTest: TformZintTest;

implementation

{$R *.lfm}

uses zint, zint_lmf, zint_render_lmf, zint_render_bmp, TADrawerSVG,
  zint_render_tadrawer, zint_helper, TADrawUtils;

{ TformZintTest }

procedure TformZintTest.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := Low(ZintSymbologies) to High(ZintSymbologies) do
    comType.Items.AddObject(ZintSymbologies[i].DisplayName, TObject(ZintSymbologies[i].Symbology));

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
    symbol.symbology := Integer(comType.Items.Objects[comType.ItemIndex]);
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
  symbol.symbology := Integer(comType.Items.Objects[comType.ItemIndex]);
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

end.
