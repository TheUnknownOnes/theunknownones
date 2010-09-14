unit uch2Data;

interface

uses
  SysUtils, Classes, ImgList, Controls;

type
  Tch2Data = class(TDataModule)
    ch2Images16: TImageList;
    ch2Images24: TImageList;
  private
    { Private-Deklarationen }
  public

  end;

var
  ch2Data: Tch2Data;

const
  iiHelp = 0;
  iiPlus = 1;
  iiMinus = 2;

implementation

{$R *.dfm}

initialization
  ch2Data := Tch2Data.Create(nil);

finalization
  ch2Data.Free;

end.
