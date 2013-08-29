unit uFormZintTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, zint, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, uRTTIHelper, ComCtrls, TypInfo;

type
  TForm46 = class(TForm)
    imgResult: TImage;
    Panel1: TPanel;
    edData: TEdit;
    comType: TComboBox;
    lblError: TLabel;
    btPrint: TButton;
    comPrinter: TComboBox;
    GroupBox1: TGroupBox;
    edWhitespaceWidth: TEdit;
    Label2: TLabel;
    Label1: TLabel;
    edFrameWidth: TEdit;
    rbBox: TRadioButton;
    rbBind: TRadioButton;
    rbNone: TRadioButton;
    cbHRT: TCheckBox;
    cbRAM: TComboBox;
    Label3: TLabel;
    edMHS: TEdit;
    Label4: TLabel;
    FontDialog1: TFontDialog;
    ButtonFont: TButton;
    btSVG: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Splitter1: TSplitter;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure edDataChange(Sender: TObject);
    procedure comTypeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btPrintClick(Sender: TObject);
    procedure ButtonFontClick(Sender: TObject);
    procedure btSVGClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSymbol : TZintSymbol; 
    procedure GenBarcode;
    function GenSymbol: TZintSymbol;
    procedure ProcessSymbol(ASymbol: TZintSymbol);
  public
    { Public-Deklarationen }
  end;

var
  Form46: TForm46;

implementation

{$R *.dfm}

uses zint_render_wmf, zint_render_canvas, Printers, zint_render_svg, zint_helper,
  uFrameOptions;

procedure TForm46.btPrintClick(Sender: TObject);
var
  symbol : TZintSymbol;
  rt  : TZintCanvasRenderTarget;
begin
  lblError.Caption := '';

  symbol:=GenSymbol;
  try
    symbol.Encode(edData.Text);

    Printer.PrinterIndex:=comPrinter.ItemIndex;
    Printer.BeginDoc;
    rt:=TZintCanvasRenderTarget.Create(Printer.Canvas);
    rt.RenderAdjustMode:=TZintRenderAdjustMode(cbRAM.ItemIndex);
    rt.Font.Assign(ButtonFont.Font);
    rt.HexagonScale:=StrToFloatDef(edMHS.Text, 1);
    rt.Left:=(Printer.Canvas.ClipRect.Right - Printer.Canvas.ClipRect.Left) / 3;
    rt.Top:=(Printer.Canvas.ClipRect.Bottom - Printer.Canvas.ClipRect.Top) / 3;
    rt.WidthDesired:=(Printer.Canvas.ClipRect.Right - Printer.Canvas.ClipRect.Left) / 3;
    rt.HeightDesired:=(Printer.Canvas.ClipRect.Right - Printer.Canvas.ClipRect.Left) / 3;
    try
      Symbol.Render(rt);
    finally
      rt.Free;
      Printer.EndDoc;
    end;

  except
    on E : Exception do
      lblError.Caption := e.Message;
  end;
end;

procedure TForm46.btSVGClick(Sender: TObject);
var
  sl : TStringList;
  symbol : TZintSymbol;
  rt  : TZintSVGRenderTarget;
begin
  if SaveDialog1.Execute then
  begin
    sl:=TStringList.Create;
    try
      lblError.Caption := '';

      symbol:=GenSymbol;
      try
        symbol.Encode(edData.Text);

        rt:=TZintSVGRenderTarget.Create(sl);
        rt.ForegroundColor:='black';
        rt.BackgroundColor:='white';
        rt.Transparent:=false;
        rt.RenderAdjustMode:=TZintRenderAdjustMode(cbRAM.ItemIndex);
        rt.Font:=ButtonFont.Font.Name;
        rt.HexagonScale:=StrToFloatDef(edMHS.Text, 1);
        rt.Left:=0;
        rt.Top:=0;
        rt.WidthDesired:=300;
        rt.HeightDesired:=300;
        try
          Symbol.Render(rt);
        finally
          rt.Free;
        end;

      except
        on E : Exception do
          lblError.Caption := e.Message;
      end;

      sl.SaveToFile(SaveDialog1.FileName);
    finally
      sl.free;
    end;
  end;
end;

procedure TForm46.ButtonFontClick(Sender: TObject);
begin
  if FontDialog1.Execute() then
    ButtonFont.Font.Assign(FontDialog1.Font);
  ButtonFont.Font.Size:=14;
  GenBarcode;
end;

procedure TForm46.comTypeChange(Sender: TObject);
begin
  GenBarcode;
end;

procedure TForm46.edDataChange(Sender: TObject);
begin
  GenBarcode;
end;

procedure TForm46.FormCreate(Sender: TObject);
var
  i : Integer;
begin
  for i := Low(ZintSymbologies) to High(ZintSymbologies) do
    comType.Items.AddObject(ZintSymbologies[i].DisplayName, TObject(ZintSymbologies[i].Symbology));

  comType.ItemIndex := 0;

  comPrinter.Items.Assign(Printer.Printers);
  comPrinter.ItemIndex:=0;

  FSymbol:=TZintSymbol.Create;

  ProcessSymbol(FSymbol);
end;

procedure TForm46.FormDestroy(Sender: TObject);
var
  i : Integer;
begin
  for i := PageControl1.PageCount-1 downto 0 do
    PageControl1.Pages[i].Free;
    
  FSymbol.Free;
end;

procedure TForm46.FormShow(Sender: TObject);
begin
  GenBarcode;
end;

function TForm46.GenSymbol: TZintSymbol;
begin
  Result := FSymbol;
  Result.Clear;
  Result.symbology := Integer(comType.Items.Objects[comType.ItemIndex]);
  Result.input_mode := UNICODE_MODE;

  if rbBox.Checked then
    Result.output_options:=Result.output_options or BARCODE_BOX
  else
  if rbBind.Checked then
    Result.output_options:=Result.output_options or BARCODE_BIND;

  Result.whitespace_width:=StrToIntDef(edWhitespaceWidth.Text, 0);
  Result.border_width:=StrToIntDef(edFrameWidth.Text, 0);
  if cbHRT.Checked then
    Result.show_hrt:= 1
  else
    Result.show_hrt:= 0;
end;

procedure TForm46.ProcessSymbol(ASymbol: TZintSymbol);
var
  i: Integer;
  Properties : TStringList;
  Property_Value: Variant;
  ts : TTabSheet;
  f : TFrameOptions;
begin
  Properties:=TStringList.Create;
  try
    rttihGetPropertiesList(ASymbol, Properties, false, [tkClass]);

    for i := 0 to Properties.Count - 1 do
    begin
      Property_Value:=rttihGetPropertyValue(ASymbol, Properties[i]);
      
      if TObject({$IFDEF declared(NativeInt)}NativeInt{$ELSE}Integer{$ENDIF}(Property_Value)).InheritsFrom(TCustomZintSymbolOptions) then
      begin
        ts:=TTabSheet.Create(PageControl1);
        ts.PageControl:=PageControl1;
        ts.Caption:=Properties[i];
        f := TFrameOptions.Create(ts);
        f.Parent:=ts;
        f.Init(ASymbol, Properties[i]);
        ts.OnShow:=f.RefreshFrame;
        f.OnChange:=edDataChange;
      end;
    end;
  finally
    Properties.Free;
  end;

end;

procedure TForm46.GenBarcode;
var
  symbol : TZintSymbol;
  wmf : TMetafile;
  rt  : TZintWMFRenderTarget;
begin
  imgResult.Picture.Graphic := nil;
  lblError.Caption := '';

  symbol:=GenSymbol;
  try
    symbol.Encode(edData.Text, true);

    wmf:=TMetafile.Create;
    wmf.SetSize(imgResult.Width, imgResult.Height);
    rt:=TZintWMFRenderTarget.Create(wmf);
    rt.HexagonScale:=StrToFloatDef(edMHS.Text, 1);
    rt.Font.Assign(ButtonFont.Font);
    rt.RenderAdjustMode:=TZintRenderAdjustMode(cbRAM.ItemIndex);
    try
      Symbol.Render(rt);
      imgResult.Picture.Graphic:=wmf;
    finally
      rt.Free;
      wmf.Free;
    end;

  except
    on E : Exception do
      lblError.Caption := e.Message;
  end;
end;

end.
