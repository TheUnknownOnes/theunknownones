//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uImageTools;

interface

uses
  Windows, Controls, pngimage;

{$REGION 'ImageList Tools'}
procedure ReplaceImageInImageList(FromImageList: TImageList; FromIndex: Integer;
                                  ToImageList: TImageList; ToIndex: Integer);
{$ENDREGION}

{$REGION 'PNGs mischen'}
procedure MergePNGs(const APNG, APNGOnTop: TPNGObject; AMergeFactor : Single = 0.75);
{$ENDREGION}

implementation

uses
  CommCtrl, Graphics, Classes;

{$REGION 'ImageList Tools'}
procedure ReplaceImageInImageList(FromImageList: TImageList; FromIndex: Integer;
                                  ToImageList: TImageList; ToIndex: Integer);
var
  I: Integer;
  Image, Mask: TBitmap;
  ARect: TRect;
begin
  ARect := Rect(0, 0, ToImageList.Width, ToImageList.Height);
  Image := TBitmap.Create;
  try
    with Image do
    begin
      Height := ToImageList.Height;
      Width := ToImageList.Width;
    end;
    Mask := TBitmap.Create;
    try
      with Mask do
      begin
        Monochrome := True;
        Height := ToImageList.Height;
        Width := ToImageList.Width;
      end;

      with Image.Canvas do
      begin
        FillRect(ARect);
        ImageList_Draw(FromImageList.Handle, I, Handle, 0, 0, ILD_NORMAL);
      end;
      with Mask.Canvas do
      begin
        FillRect(ARect);
        ImageList_Draw(FromImageList.Handle, I, Handle, 0, 0, ILD_MASK);
      end;
      ToImageList.Replace(ToIndex, Image, Mask);

    finally
      Mask.Free;
    end;
  finally
    Image.Free;
  end;
end;
{$ENDREGION}


{$REGION 'PNGs mischen'}
procedure MergePNGs(const APNG, APNGOnTop: TPNGObject; AMergeFactor : Single);
var
  pngResult : TPNGObject;

  x, y : integer;

  TopPos,
  LeftPos : Integer;

  scanlin1,
  scanlin2,
  scanlinResult : pRGBLine;
  alphalin2 : PByteArray;

  BlendValue : byte;
begin
  pngResult:=TPNGObject.Create;
  pngResult.Assign(APNG);

  TopPos:=(APNG.Height-APNGOnTop.Height) div 2;
  LeftPos:=(APNG.Width-APNGOnTop.Width) div 2;

  for  y:= 0 to APNG.Height-1 do
  begin
    scanlin1:=APNG.Scanline[y];

    if (y>=TopPos) and (y<TopPos+APNGOnTop.Height) then
    begin
      scanlin2:=APNGOnTop.Scanline[y-TopPos];
      alphalin2:=APNGOnTop.AlphaScanline[y-TopPos];
    end;

    scanlinResult:=pngResult.Scanline[y];

    for x := 0 to APNG.Width-1 do
    begin
      if not ((y<TopPos) or (y>TopPos+APNGOnTop.Height-1) or
              (x<LeftPos) or (x>LeftPos+APNGOnTop.Width-1)) then
      begin
        blendvalue:=round(alphalin2[x-LeftPos] * AMergeFactor);
        scanlinResult[x].rgbtBlue:=((scanlin1[x].rgbtBlue * (255 - blendvalue)+
                                   (scanlin2[x-LeftPos].rgbtBlue * blendvalue)) div 255) ;
        scanlinResult[x].rgbtGreen:=((scanlin1[x].rgbtGreen * (255 - blendvalue)+
                                   (scanlin2[x-LeftPos].rgbtGreen * blendvalue)) div 255) ;
        scanlinResult[x].rgbtRed:=((scanlin1[x].rgbtRed * (255 - blendvalue)+
                                   (scanlin2[x-LeftPos].rgbtRed * blendvalue)) div 255) ;
      end;
    end;
  end;

  APNG.Assign(pngResult);
  pngResult.Free;
end;
{$ENDREGION}

end.
