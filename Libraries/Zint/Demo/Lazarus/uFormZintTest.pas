unit uFormZintTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TformZintTest }

  TformZintTest = class(TForm)
    comType: TComboBox;
    edData: TEdit;
    imgResult: TImage;
    Panel1: TPanel;
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

uses zint, zint_lmf;

type
  BCTypeEntry = record
    N : String;
    T : Integer;
  end;

const
  SupportedTypes : array[0..36] of BCTypeEntry =((N : '2 of 5 Matrix'; T : BARCODE_C25MATRIX),
                                                (N : '2 of 5 Industrial'; T : BARCODE_C25IND),
                                                (N : '2 of 5 Interleaved'; T : BARCODE_C25INTER),
                                                (N : '2 of 5 IATA'; T : BARCODE_C25IATA),
                                                (N : '2 of 5 Datalogic'; T : BARCODE_C25LOGIC),
                                                (N : 'Deutsche Post Leitcode'; T : BARCODE_DPLEIT),
                                                (N : 'Deutsche Post Identcode'; T : BARCODE_DPIDENT),
                                                (N : 'EAN 128'; T : BARCODE_EAN128),
                                                (N : 'Code 39'; T : BARCODE_CODE39),
                                                (N : 'Pharmazentral PZN'; T : BARCODE_PZN),
                                                (N : 'Extended Code 39'; T : BARCODE_EXCODE39),
                                                (N : 'Codabar'; T : BARCODE_CODABAR),
                                                (N : 'Code 93'; T : BARCODE_CODE93),
                                                (N : 'Logmars'; T : BARCODE_LOGMARS),
                                                (N : 'Code 128'; T : BARCODE_CODE128),
                                                (N : 'Code 128 B'; T : BARCODE_CODE128B),
                                                (N : 'NVE 18'; T : BARCODE_NVE18),
                                                (N : 'Code 11'; T : BARCODE_CODE11),
                                                (N : 'Pharmacode'; T : BARCODE_PHARMA),
                                                (N : 'ITF 14'; T : BARCODE_ITF14),
                                                (N : 'Australian Post Code'; T : BARCODE_AUSPOST),
                                                (N : 'Australian Post Reply'; T : BARCODE_AUSREPLY),
                                                (N : 'Australian Post Route'; T : BARCODE_AUSROUTE),
                                                (N : 'Australian Post Redirect'; T : BARCODE_AUSREDIRECT),
                                                (N : 'Pharma 2-track'; T : BARCODE_PHARMA_TWO),
                                                (N : 'Code 32 / Italian Pharmacode'; T : BARCODE_CODE32),
                                                (N : 'EAN 14'; T : BARCODE_EAN14),
                                                (N : 'Azrune'; T : BARCODE_AZRUNE),
                                                (N : 'HIBC 128'; T : BARCODE_HIBC_128),
                                                (N : 'HIBC 39'; T : BARCODE_HIBC_39),
                                                (N : 'HIBC Datamatrix'; T : BARCODE_HIBC_DM),
                                                (N : 'HIBC Aztec'; T : BARCODE_HIBC_AZTEC),
                                                (N : 'Datamatrix'; T : BARCODE_DATAMATRIX),
                                                (N : 'Maxicode'; T : BARCODE_MAXICODE),
                                                (N : 'Aztec'; T : BARCODE_AZTEC),
                                                (N : 'Code 16k'; T : BARCODE_CODE16K),
                                                (N : 'Code 49'; T : BARCODE_CODE49)
                                               );

{ TformZintTest }

procedure TformZintTest.FormCreate(Sender: TObject);
var
  i : Integer;
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

procedure TformZintTest.edDataChange(Sender: TObject);
begin
  GenBarcode;
end;

procedure TformZintTest.GenBarcode;
var
  symbol : TZintSymbol;
  wmf : TZintMetafile;
begin
  imgResult.Picture.Graphic := nil;

  symbol := TZintSymbol.Create;
  symbol.symbology := SupportedTypes[comType.ItemIndex].T;
  symbol.show_hrt := 0;
  symbol.input_mode := UNICODE_MODE;
  symbol.show_hrt := 1;
  symbol.Encode(UTF8Encode(edData.Text), false);
  wmf:=TZintMetafile.Create;
  try
    symbol.Render(wmf);
    imgResult.Picture.Graphic := wmf;
  finally
    wmf.Free
  end;

  symbol.Free;
end;

end.

